;;;; Simple HTTP/HTML Package for LispWorks
;;;;
;;;; Copyright (c) 2013 by Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License.  You may obtain
;;;; a copy of the License at
;;;;
;;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied.  See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "comm"))

(defpackage :http
  (:use :cl :lw :system :comm :re :lexer :parsergen :base64)
  (:export
   #:*http-timeout*
   #:*http-error*

   ;; classes
   #:request
   #:response
   #:url

   ;; url helpers
   #:parse-url
   #:copy-url
   #:encode-url
   #:decode-url
   #:format-url

   ;; macros
   #:with-url
   #:with-response
   #:with-headers

   ;; data post body functions
   #:make-query-string
   #:parse-query-string

   ;; HTTP socket streams
   #:open-http-stream
   #:open-http-event-stream

   ;; decoding content
   #:decode-response-body

   ;; header accessors
   #:http-headers
   #:http-header

   ;; following redirects
   #:http-follow-request
   #:http-follow

   ;; request functions
   #:http-perform
   #:http-head
   #:http-get
   #:http-options
   #:http-trace
   #:http-delete
   #:http-put
   #:http-post
   #:http-patch

   ;; url accessors
   #:url-domain
   #:url-port
   #:url-auth
   #:url-scheme
   #:url-secure
   #:url-path
   #:url-query
   #:url-fragment
   
   ;; http request accessors
   #:request-url
   #:request-method
   #:request-data
   #:request-keep-alive
   #:request-read-body

   ;; response accessors
   #:response-stream
   #:response-code
   #:response-status
   #:response-body
   #:response-request))

(in-package :http)

(defparameter *http-timeout* nil
  "Used as the :timeout and :read-timeout to the HTTP stream object.")
(defparameter *http-error* nil
  "Set to T if you want http-perform to signal errors.")
(defparameter *http-semaphore* (mp:make-semaphore :name "HTTP Lock" :count 200)
  "Prevents many HTTP client connections simultaneously.")

(defclass url ()
  ((domain     :initarg :domain     :accessor url-domain)
   (port       :initarg :port       :accessor url-port           :initform nil)
   (auth       :initarg :auth       :accessor url-auth           :initform nil)
   (scheme     :initarg :scheme     :accessor url-scheme         :initform :http)
   (path       :initarg :path       :accessor url-path           :initform "/")
   (query      :initarg :query      :accessor url-query          :initform nil)
   (fragment   :initarg :fragment   :accessor url-fragment       :initform nil))
  (:documentation "Universal Resource Locator."))

(defclass headers ()
  ((headers    :initarg :headers    :accessor http-headers       :initform nil))
  (:documentation "Base class for requests and responses."))

(defclass request (headers)
  ((url        :initarg :url        :accessor request-url)
   (keep-alive :initarg :keep-alive :accessor request-keep-alive :initform nil)
   (read-body  :initarg :read-body  :accessor request-read-body  :initform t)
   (method     :initarg :method     :accessor request-method     :initform "GET")
   (data       :initarg :data       :accessor request-data       :initform nil))
  (:documentation "A request for a URL from an HTTP server."))

(defclass response (headers)
  ((request    :initarg :request    :accessor response-request)
   (stream     :initarg :stream     :accessor response-stream)
   (code       :initarg :code       :accessor response-code)
   (status     :initarg :status     :accessor response-status    :initform nil)
   (body       :initarg :body       :accessor response-body      :initform nil))
  (:documentation "The response from a request to an HTTP server."))

(defmethod print-object ((url url) s)
  "Output a URL to a stream."
  (print-unreadable-object (url s :type t) (format s "~s" (format-url url))))

(defmethod print-object ((req request) s)
  "Output an HTTP request to a stream."
  (print-unreadable-object (req s :type t)
    (with-slots (method url)
        req
      (format s "~a ~s" method (format-url url)))))

(defmethod print-object ((resp response) stream)
  "Output an HTTP response to a stream."
  (print-unreadable-object (resp stream :type t)
    (with-slots (code status)
        resp
      (format stream "~a ~s" code status))))

(defparameter *status-re* (compile-re "^HTTP/[^%s]+%s+(%d+)%s*([^%n]*)")
  "Pattern for parsing a response status code and message.")
(defparameter *header-re* (compile-re "^([^:]+):%s*([^%n]*)")
  "Pattern for parsing a response header line.")
(defparameter *charset-re* (compile-re "charset%s*=%s*([%w%-]+)")
  "Pattern for parsing the character encoding type.")

(defconstant +unreserved-chars+ "abcdefghijklmnopqrstuvwxyz0123456789-._~"
  "Unreserved URL characters that *must not* be encoded.")
(defconstant +http-schemes+ '((:http 80) (:https 443) (nil 80))
  "A list of valid HTTP schemes and their default ports.")
(defconstant +url-format+ "~@[~(~a~):~]//~@[~{~a:~a~}@~]~a~:[:~a~;~*~]~a~@[?~a~]~@[#~a~]"
  "The format options used to recreate a URL string.")

(defun http-scheme (name)
  "Return the scheme keyword for a given scheme or NIL."
  (flet ((scheme-equal (a b)
           (string-equal a (symbol-name b))))
    (or (first (assoc name +http-schemes+ :test #'scheme-equal))

        ;; signal an error
        (error "Unknown HTTP scheme ~s" name))))

(deflexer url-lexer
  ("%."                       (values :dot))
  ("^([^/:]+)://"             (values :scheme (http-scheme $1)))
  ("^//"                      (values :scheme nil))
  ("([^:]+):([^@]+)@"         (values :auth (list $1 $2)))
  ("/[^?#]*"                  (values :path $$))
  ("%?([^#]*)"                (values :query $1))
  ("#(.*)"                    (values :fragment $1))
  ("[%a%d%-]+"                (values :domain $$))
  (":(%d+)"                   (values :port (parse-integer $1))))

(defparser url-parser
  ((start url) $1)

  ;; http://login:password@www.host.com:80/index.html?query=string#fragment
  ((url scheme) $1)
  ((url :error) (error "Invalid URL"))

  ;; http://
  ((scheme :scheme auth) `(:scheme ,$1 ,@$2))
  ((scheme auth) `(:scheme :http ,@$1))

  ;; login:password@
  ((auth :auth domain) `(:auth ,$1 ,@$2))
  ((auth domain) `(:auth nil ,@$1))

  ;; www.host.com
  ((domain host port) `(:domain ,$1 ,@$2))

  ;; host.com
  ((host :domain :dot host) (string-append $1 #\. $3))
  ((host :domain) $1)

  ;; :80
  ((port :port path) `(:port ,$1 ,@$2))
  ((port path) `(:port nil ,@$1))

  ;; /index.html
  ((path :path query) `(:path ,$1 ,@$2))
  ((path query) `(:path "/" ,@$1))

  ;; ?q=value
  ((query :query fragment) `(:query ,(parse-query-string $1) ,@$2))
  ((query fragment) $1)

  ;; #anchor
  ((fragment :fragment) `(:fragment ,$1))
  ((fragment)) nil)

(defmethod initialize-instance :after ((url url) &key)
  "Fix-up work on URLs. Make sure paths are sanitized, etc."
  (with-slots (path)
      url
    (setf path (sanitize-path path))))

(defmethod initialize-instance :after ((req request) &key)
  "Make sure the URL - if a string - is parsed."
  (with-slots (url)
      req
    (when (stringp url)
      (setf url (parse-url url)))))

(defmacro with-url ((url url-expr &rest initargs &key scheme auth domain port path query fragment) &body body)
  "Parse a URL if necessary, bind it, and execute a body."
  (declare (ignore scheme auth domain port path query fragment))
  `(let ((,url (copy-url ,url-expr ,@initargs)))
     (progn ,@body)))

(defmacro with-response ((resp resp-expr &key timeout errorp) &body body)
  "Execute a request, if successful, execute body with the response variable."
  `(let* (,@(when errorp `((*http-error* ,errorp)))
          ,@(when timeout `((*http-timeout* ,timeout)))

          ;; execute the request, allow for redirects
          (,resp ,resp-expr))
     (if (and ,resp (<= 200 (response-code ,resp) 299))
         (values (progn ,@body) t)
       (values nil nil ,resp))))

(defmacro with-headers ((&rest bindings) object &body body)
  "Extract a value from an HTTP header assoc list."
  `(symbol-macrolet (,@(loop :for binding :in bindings
                             :collect (destructuring-bind (var key &key all)
                                          binding
                                        `(,var (http-header ,object ,key :all ,all)))))
     (progn ,@body)))

(defun parse-url (url &rest initargs &key scheme auth domain port path query fragment)
  "Parse a URL and return. If URL is relative, set what it's relative to."
  (declare (ignorable scheme auth domain port path query fragment))
  (let ((spec (parse #'url-parser (tokenize #'url-lexer url))))
    (apply #'make-instance 'url (nconc initargs spec))))

(defun copy-url (url &rest initargs &key scheme auth domain port path query fragment)
  "Create a new URL that's a copy, but with optionally overriding parse of it."
  (if (typep url 'url)
      (make-instance 'url
                     :scheme (or scheme (url-scheme url))
                     :auth (or auth (url-auth url))
                     :domain (or domain (url-domain url))
                     :port (or port (url-port url))
                     :path (or path (url-path url))
                     :query (or query (url-query url))
                     :fragment (or fragment (url-fragment url)))
    (apply #'parse-url url initargs)))

(defun format-url (url &optional stream)
  "Print a URL as a complete string."
  (with-slots (scheme auth domain port path query fragment)
      url
    (let ((qs (when query (make-query-string query))))
      (format stream +url-format+ scheme auth domain (null port) port path qs fragment))))

(defun escape-char-p (c)
  "T if a character needs to be escaped in a URL."
  (not (find c +unreserved-chars+ :test #'char-equal)))

(defun sanitize-path (path)
  "Make sure whitespaces in a path are actually %20."
  (with-output-to-string (s)
    (loop for c across path
          do (if (char= c #\space)
                  (princ "%20" s)
                (princ c s)))))

(defun encode-url (string)
  "Convert a string into a URL-safe, encoded string."
  (with-output-to-string (url)
    (loop for c across string
          do (if (escape-char-p c)
                 (format url "%~16,2,'0r" (char-code c))
               (princ c url)))))

(defun decode-url (url)
  "Decode an encoded URL into a string. Returns NIL if malformed."
  (with-output-to-string (string)
    (with-input-from-string (s url)
      (loop for c = (read-char s nil nil)
            while c
            do (case c
                 (#\+ (princ #\space string))
                 (#\% (let ((c1 (read-char s nil nil))
                            (c2 (read-char s nil nil)))
                        (when (and c1 c2)
                          (let ((code (logior (ash (parse-integer (string c1) :radix 16) 4)
                                              (ash (parse-integer (string c2) :radix 16) 0))))
                            (princ (code-char code) string)))))
                 (otherwise
                  (princ c string)))))))

(defun make-query-string (query)
  "Build a k=v&.. query string from an associative list, properly url-encoded."
  (with-output-to-string (qs)
    (loop for (key value) :in query
          when value
          do (let ((encoded-value (encode-url (princ-to-string value))))
               (format qs "~:[~;&~]~a=~a" (plusp (file-position qs)) key encoded-value)))))

(defun parse-query-string (qs)
  "Return an associative list of query string parameters."
  (let ((q (split-sequence "&=" qs)))
    (loop for k = (pop q)
          for v = (pop q)
          while k
          collect (list k (if v (decode-url v) "")))))

(defun basic-auth-string (login)
  "Create the basic auth value for the Authorization header."
  (let ((auth-string (format nil "~{~a:~a~}" login)))
    (format nil "Basic ~a" (base64-encode auth-string))))

(defun decode-response-body (resp &optional charset)
  "Use the charset identified in the response Content-Type to decode the response body."
  (with-headers ((content-type "Content-Type"))
      resp
    (let ((format (with-re-match (match (find-re *charset-re* content-type) :no-match charset)
                    (cond
                     ((string-equal $1 "utf-8") :utf-8)
                     ((string-equal $1 "iso-8859-1") :latin-1)
                     ((string-equal $1 "shift_jis") :sjis)
                     ((string-equal $1 "x-mac-roman") :macos-roman)
                     ((string-equal $1 "euc-jp" :euc-jp))
                     ((string-equal $1 "jis") :jis)))))
      (if (null format)
          (response-body resp)
        (let ((body (map '(vector (unsigned-byte 8)) #'char-code (response-body resp))))
          (values (external-format:decode-external-string body format) format))))))

(defun open-http-stream (req &key (errorp *http-error*) (timeout *http-timeout*))
  "Open a TCP stream to a given URL."
  (with-slots (scheme domain port)
      (request-url req)
    (let ((service (or port (second (assoc scheme +http-schemes+)) 80)))
      (when-let (stream (open-tcp-stream domain
                                         service
                                         :errorp errorp
                                         :timeout timeout
                                         :read-timeout timeout
                                         :write-timeout timeout
                                         :keepalive (request-keep-alive req)))
        (prog1 stream
          (when (member scheme '(:https))
            (attach-ssl stream :ssl-side :client)))))))

(defun http-header (hs header &key all)
  "Returns the value of a request header."
  (if all
      (mapcar #'second (remove header (http-headers hs) :test-not #'string-equal :key #'first))
    (second (assoc header (http-headers hs) :test #'string-equal))))

(defsetf http-header (headers header) (value)
  "Add or change the value of a request header."
  (let ((place (gensym))
        (h (gensym))
        (hs (gensym))
        (new-value (gensym)))
    `(let* ((,hs ,headers)
            (,h ,header)
            (,new-value (princ-to-string ,value))
            (,place (assoc ,h (http-headers ,hs) :test #'string-equal)))
       (prog1 ,new-value (if ,place
                             (setf (second ,place) ,new-value)
                           (push (list ,h ,new-value) (http-headers ,hs)))))))

(defun read-http-status (http)
  "Parse the line and parse it. Return the code and value."
  (with-re-match (match (match-re *status-re* (read-line http)))
    (values (parse-integer $1) $2)))

(defun read-http-headers (http)
  "Parse a header line. Return the key and value."
  (flet ((read-header ()
           (with-re-match (match (match-re *header-re* (read-line http)))
             (list $1 $2))))
    (loop for header = (read-header) while header collect header)))

(defun read-http-content-chunked (http)
  "Read the body from the HTTP server using a chunked Transfer-Encoding."
  (with-output-to-string (body)
    (loop for len = (parse-integer (read-line http) :radix 16 :junk-allowed t)
          while (plusp len)
          do (let ((chunk (make-string len)))
               (write-sequence chunk body :end (read-sequence chunk http))
               (read-line http))

          ;; chunked content will have a final, empty line
          finally (read-line http))))

(defun read-http-content (http &optional content-length)
  "Read the rest of the response from the HTTP server."
  (if content-length
      (let ((body (make-string (parse-integer content-length))))
        (prog1 body (read-sequence body http)))
    (with-output-to-string (body)
      (loop with chunk = (make-string 4000)
            for bytes-read = (read-sequence chunk http)
            while (plusp bytes-read)
            do (write-sequence chunk body :end bytes-read)))))

(defun read-http-response (stream req)
  "Read a response string from an HTTP socket stream and parse it."
  (multiple-value-bind (code status)
      (read-http-status stream)
    (let ((headers (read-http-headers stream)))
      (make-instance 'response
                     :request req
                     :stream stream
                     :code code
                     :status status
                     :headers headers
                     :body (when (request-read-body req)
                             (let ((encoding (second (assoc "Transfer-Encoding" headers :test #'string-equal)))
                                   (content-length (second (assoc "Content-Length" headers :test #'string-equal))))
                               (cond
                                ((null encoding) (read-http-content stream content-length))
                                ((string= encoding "identity") (read-http-content stream content-length))
                                ((string= encoding "chunked") (read-http-content-chunked stream))
                                (t
                                 (error "Unknown Transfer-Encoding ~s" encoding)))))))))

(defun send-http-request (stream req)
  "Write the request to the HTTP socket."
  (with-slots (url method headers data keep-alive)
      req

    ;; send the formal http request line
    (let ((qs (format nil "~:[~;?~a~]" (url-query url) (make-query-string (url-query url)))))
      (format stream "~a ~a~a HTTP/1.1~c~c" method (url-path url) qs #\return #\linefeed))

    ;; always send the host as it should be
    (format stream "Host: ~a~c~c" (url-domain url) #\return #\linefeed)

    ;; write default headers
    (unless (assoc "Accept" headers :test #'string=)
      (format stream "Accept: */*~c~c" #\return #\linefeed))
    (unless (assoc "Connection" headers :test #'string=)
      (format stream "Connection: ~:[close~;keep-alive~]~c~c" keep-alive #\return #\linefeed))
    (unless (assoc "User-Agent" headers :test #'string=)
      (format stream "User-Agent: lispworks~c~c" #\return #\linefeed))
          
    ;; send the content length if there is content
    (when data
      (format stream "Content-Length: ~a~c~c" (length data) #\return #\linefeed))

    ;; optionally send basic auth if in the url
    (when-let (auth (url-auth url))
      (format stream "Authorization: ~a~c~c" (basic-auth-string auth) #\return #\linefeed))
            
    ;; send all the headers
    (dolist (header headers)
      (format stream "~a: ~a~c~c" (first header) (second header) #\return #\linefeed))
          
    ;; complete the request
    (format stream "~c~c" #\return #\linefeed)
          
    ;; write optional data to the body
    (when data
      (write-sequence data stream))
          
    ;; send all data
    (force-output stream)))

(defun http-perform (req &optional stream)
  "Perform a generic HTTP request, return the response. Optionally provide a re-usable stream."
  (unwind-protect
      (progn
        (mp:semaphore-acquire *http-semaphore*)

        ;; if a stream wasn't provided create one
        (unless stream
          (setf stream (open-http-stream req)))

        ;; issue the request
        (send-http-request stream req)

        ;; parse the response
        (let ((resp (read-http-response stream req)))
          (prog1 resp
            (with-headers ((connection "Connection"))
                resp

              ;; check to see if the server is requesting to close the connection
              (when (string-equal connection "close")
                (close stream))))))

    ;; release the lock
    (progn
      (mp:semaphore-release *http-semaphore*) 

      ;; if the request doesn't want to keep the socket open, ensure it closes
      (unless (request-keep-alive req)
        (close stream)))))

(defun http-follow-request (resp)
  "Create a new request for a redirect response."
  (when (<= 300 (response-code resp) 399)
    (let* ((req (response-request resp))
           (url (request-url req))
           
           ;; get the target location
           (loc (http-header resp "Location"))
           
           ;; the redirect location might be relative (to the domain)
           (url (if (char= (char loc 0) #\/)
                    (with-slots (scheme domain port auth)
                        url
                      (parse-url (format nil +url-format+ scheme auth domain (null port) port loc nil nil)))
                  (parse-url loc))))
      (with-slots (query fragment)
          url
        
        ;; if the new url doesn't have a query or fragment, use the old one
        (unless (url-query url)
          (setf (url-query url) query))
        (unless (url-fragment url)
          (setf (url-fragment url) fragment))
        
        ;; create the new request - keep the same method unless a 303
        (make-instance 'request
                       :url url
                       :keep-alive (request-keep-alive req)
                       :read-body (request-read-body req)
                       :data (request-data req)
                       :headers (http-headers req)
                       :method (if (= (response-code resp) 303) "GET" (request-method req)))))))

(defun http-follow (resp &key (redirect-limit 3))
  "Create a response redirect."
  (case (response-code resp)
    ((301 302 303 304 305 307)
     (if (zerop redirect-limit)
         resp
       (let ((req (http-follow-request resp)))
         (http-follow (http-perform req) :redirect-limit (1- redirect-limit)))))
    (otherwise resp)))

(defun http-simple-perform (url redirect-limit &rest initargs &key method headers data keep-alive (read-body t))
  "Parse a URL, create a simple request, and perform the request."
  (declare (ignore method headers data keep-alive read-body))
  (let ((req (apply #'make-instance 'request :url url initargs)))
    (http-follow (http-perform req) :redirect-limit redirect-limit)))

(defun http-head (url &key headers keep-alive (redirect-limit 3))
  "Perform a HEAD request for a URL, return the response."
  (http-simple-perform url redirect-limit :method "HEAD" :headers headers :read-body nil :keep-alive keep-alive))

(defun http-get (url &key headers keep-alive (redirect-limit 3))
  "Perform a GET request for a URL, return the response."
  (http-simple-perform url redirect-limit :method "GET" :headers headers :keep-alive keep-alive))

(defun http-options (url &key headers keep-alive (redirect-limit 3))
  "Perform an OPTIONS request for a URL, return the response."
  (http-simple-perform url redirect-limit :method "OPTIONS" :headers headers :keep-alive keep-alive))

(defun http-trace (url &key headers keep-alive (redirect-limit 3))
  "Perform an TRACE request for a URL, return the response."
  (http-simple-perform url redirect-limit :method "TRACE" :headers headers :keep-alive keep-alive))

(defun http-delete (url &key headers keep-alive (redirect-limit 3))
  "Perform a DELETE request for a URL, return the response."
  (http-simple-perform url redirect-limit :method "DELETE" :headers headers :keep-alive keep-alive))

(defun http-put (url &key headers data keep-alive (redirect-limit 3))
  "Perform a PUT request for a URL, return the response."
  (http-simple-perform url redirect-limit :method "PUT" :headers headers :data data :keep-alive keep-alive))

(defun http-post (url &key headers data keep-alive (redirect-limit 3))
  "Perform a POST request for a URL, return the response."
  (http-simple-perform url redirect-limit :method "POST" :headers headers :data data :keep-alive keep-alive))

(defun http-patch (url &key headers data keep-alive (redirect-limit 3))
  "Perform a PATCH request for a URL, return the response."
  (http-simple-perform url redirect-limit :method "PATCH" :headers headers :data data :keep-alive keep-alive))
