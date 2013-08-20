;;;; Simple HTTP Package for LispWorks
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

(defpackage :http
  (:use :cl :lw :system :comm :re :lexer :parsergen :base64)
  (:export
   #:request
   #:response
   #:url

   ;; url helpers
   #:parse-url
   #:encode-url
   #:with-url

   ;; request functions
   #:http-perform
   #:http-follow
   #:http-head
   #:http-get
   #:http-delete
   #:http-put
   #:http-post

   ;; url accessors
   #:url-scheme
   #:url-auth
   #:url-domain
   #:url-port
   #:url-path
   
   ;; http request accessors
   #:request-url
   #:request-method
   #:request-headers
   #:request-data

   ;; response accessors
   #:response-code
   #:response-status
   #:response-headers
   #:response-body
   #:response-request))

(in-package :http)

(defclass url ()
  ((domain :initarg :domain :accessor url-domain)
   (auth   :initarg :auth   :accessor url-auth   :initform nil)
   (port   :initarg :port   :accessor url-port   :initform 80)
   (scheme :initarg :scheme :accessor url-scheme :initform "http")
   (path   :initarg :path   :accessor url-path   :initform "/"))
  (:documentation "Universal Resource Locator."))

(defclass request ()
  ((url     :initarg :url     :accessor request-url)
   (method  :initarg :method  :accessor request-method)
   (headers :initarg :headers :accessor request-headers :initform nil)
   (data    :initarg :data    :accessor request-data    :initform nil))
  (:documentation "A request for a URL from an HTTP server."))

(defclass response ()
  ((code    :initarg :code    :accessor response-code)
   (status  :initarg :status  :accessor response-status)
   (headers :initarg :headers :accessor response-headers)
   (body    :initarg :body    :accessor response-body)
   (request :initarg :request :accessor response-request))
  (:documentation "The response from a request to an HTTP server."))

(defmethod print-object ((url url) stream)
  "Output a URL to a stream."
  (print-unreadable-object (url stream :type t)
    (with-slots (scheme auth domain port path)
        url
      (format stream "\"~a://~@[~a@~]~a~:[:~a~;~*~]~a\"" scheme auth domain (= port 80) port path))))

(defmethod print-object ((req request) stream)
  "Output an HTTP request to a stream."
  (print-unreadable-object (req stream :type t)
    (with-slots (scheme domain path)
        (request-url req)
      (format stream "~a \"~a://~a~a\"" (request-method req) scheme domain path))))

(defmethod print-object ((resp response) stream)
  "Output an HTTP response to a stream."
  (print-unreadable-object (resp stream :type t)
    (with-slots (code status)
        resp
      (format stream "~a ~s" code status))))

(defparameter *status-re* (compile-re "^HTTP/[^%s]+%s+(%d+)%s+([^%n]+)")
  "Pattern for parsing a response status code and message.")
(defparameter *header-re* (compile-re "^([^:]+):%s*([^%n]*)")
  "Pattern for parsing a response header line.")

(defconstant +reserved-chars+ '(#\% #\$ #\& #\+ #\, #\/ #\: #\; #\= #\? #\@)
  "Reserved URL characters that *must* be encoded.")
(defconstant +unwise-chars+ '(#\{ #\} #\| #\\ #\^ #\[ #\] #\`)
  "Characters presenting a possibility of being misunderstood and should be encoded.")

(deflexer url-lexer
  ("^([^:]+)://"              (values :scheme $1))
  ("([^:]+:[^@]+)@"           (values :auth $1))
  ("/.*"                      (values :path $$))
  ("[%a%d%-]+"                (values :domain $$))
  ("%."                       (values :dot))
  (":(%d+)"                   (values :port (parse-integer $1))))

(defparser url-parser
  ((start url) $1)

  ;; http://login:password@www.host.com:80/index.html
  ((url scheme) (cons 'url $1))
  ((url :error) (error "Invalid URL"))

  ;; http://
  ((scheme :scheme auth) `(:scheme ,$1 ,@$2))
  ((scheme auth) `(:scheme "http" ,@$1))

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
  ((port path) `(:port 80 ,@$1))

  ;; /index.html
  ((path :path) `(:path ,$1))
  ((path) `(:path "/")))

(defmacro with-url ((url url-expr) &body body)
  "Parse a URL if necessary, bind it, and execute a body."
  (let ((p (gensym)))
    `(let ((,p ,url-expr))
       (let ((,url (if (eq (type-of ,p) 'url)
                       ,p
                     (parse-url ,p))))
         (progn ,@body)))))

(defun parse-url (url)
  "Parse a URL and return."
  (let ((spec (parse #'url-parser (tokenize #'url-lexer url))))
    (apply #'make-instance spec)))

(defun escape-char-p (c)
  "T if a character needs to be escaped in a URL."
  (or (char< c #\!)
      (char> c #\~)

      ;; reserved and unsafe characters
      (find c +unwise-chars+ :test #'char=)
      (find c +reserved-chars+ :test #'char=)))

(defun encode-url (string)
  "Convert a string into a URL-safe, encoded string."
  (with-output-to-string (url)
    (loop :for c :across string
          :do (if (escape-char-p c)
                  (if (char= c #\space)
                      (princ #\+ url)
                    (format url "%~16,2,'0r" (char-code c)))
                (princ c url)))))

(defun basic-auth-string (login)
  "Create the basic auth value for the Authorization header."
  (format nil "Basic ~a" (base64-encode login)))

(defun read-http-status (http)
  "Parse the line and parse it. Return the code and value."
  (with-re-match (match (match-re *status-re* (read-line http)))
    (values (parse-integer $1) $2)))

(defun read-http-headers (http)
  "Parse a header line. Return the key and value."
  (flet ((read-header ()
           (with-re-match (match (match-re *header-re* (read-line http)))
             (list $1 $2))))
    (loop :for header := (read-header) :while header :collect header)))

(defun read-http-body (http)
  "Read the rest of the response from the HTTP server."
  (with-output-to-string (s)
    (let ((seq (make-array 4000 :element-type 'unsigned-byte :fill-pointer t)))
      (loop :for bytes-read := (read-sequence seq http)
            :while (plusp bytes-read)
            :do (write-sequence seq s :end bytes-read)))))

(defun read-http-response (http req)
  "Read a response string from an HTTP socket stream and parse it."
  (multiple-value-bind (code status)
      (read-http-status http)
    (make-instance 'response
                   :request req
                   :code code
                   :status status
                   :headers (read-http-headers http)
                   :body (read-http-body http))))

(defun http-perform (req)
  "Perform a generic HTTP request, return the request and body."
  (with-slots (url method headers data)
      req
    (with-slots (domain port path auth)
        url
      (with-open-stream (http (open-tcp-stream domain port))
        (format http "~a ~a HTTP/1.1~c~c" method path #\return #\linefeed)

        ;; send headers
        (format http "Host: ~a~c~c" domain #\return #\linefeed)
        (format http "Connection: close~c~c" #\return #\linefeed)

        ;; optionally sent
        (when data
          (format http "Content-Length: ~a~c~c" (length data) #\return #\linefeed))
        (when auth
          (format http "Authorization: ~a~c~c" (basic-auth-string auth) #\return #\linefeed))

        ;; user headers
        (dolist (header headers)
          (format http "~a: ~a~c~c" (first header) (second header) #\return #\linefeed))

        ;; complete the request
        (format http "~c~c" #\return #\linefeed)

        ;; write optional data to the body
        (when data
          (write-sequence data http))

        ;; send all data
        (force-output http)

        ;; return the response
        (read-http-response http req)))))

(defun http-follow (resp)
  "Follow a reponse's redirection to a new resource location."
  (with-slots (headers request)
      resp
    (let ((location (assoc "Location" headers :test #'string=)))
      (when location
        (with-url (url (second location))
          (http-perform (make-instance 'request
                                       :url url
                                       :method (request-method request)
                                       :headers (request-headers request)
                                       :data (request-data request))))))))

(defun http-head (url &key headers)
  "Perform a HEAD request for a URL, return the response."
  (with-url (url url)
    (let ((req (make-instance 'request :url url :method "HEAD" :headers headers)))
      (http-perform req))))      

(defun http-get (url &key headers)
  "Perform a GET request for a URL, return the response and body."
  (with-url (url url)
    (let ((req (make-instance 'request :url url :method "GET" :headers headers)))
      (http-perform req))))

(defun http-delete (url &key headers)
  "Perform a DELETE request for a URL, return the response."
  (with-url (url url)
    (let ((req (make-instance 'request :url url :method "DELETE" :headers headers)))
      (http-perform req))))

(defun http-put (url &key headers data)
  "Perform a PUT request for a URL, return the response."
  (with-url (url url)
    (let ((req (make-instance 'request :url url :method "PUT" :headers headers :data data)))
      (http-perform req))))

(defun http-post (url &key headers data)
  "Perform a POST request for a URL, return the response."
  (with-url (url url)
    (let ((req (make-instance 'request :url url :method "POST" :headers headers :data data)))
      (http-perform req))))