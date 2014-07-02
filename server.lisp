;;;; Simple HTTP Server Package for LispWorks
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

(defpackage :http-server
  (:use :cl :lw :comm :mp :re :http)
  (:export
   #:*request*
   #:*route-map*

   ;; server startup
   #:simple-http

   ;; route declarations
   #:define-http-route

   ;; response generation
   #:http-continue
   #:http-switching-protocols
   #:http-ok
   #:http-created
   #:http-accepted
   #:http-non-authoritative
   #:http-no-content
   #:http-reset-content
   #:http-partial-content
   #:http-moved-permanently
   #:http-found
   #:http-see-other
   #:http-not-modified
   #:http-temporary-redirect
   #:http-bad-request
   #:http-unauthorized
   #:http-forbidden
   #:http-not-found
   #:http-method-not-allowed
   #:http-not-acceptable
   #:http-proxy-required
   #:http-request-timeout
   #:http-conflict
   #:http-gone
   #:http-length-required
   #:http-precondition-failed
   #:http-request-too-large
   #:http-request-uri-too-long
   #:http-unsupported-media-type
   #:http-range-not-satisfiable
   #:http-expectation-failed
   #:http-internal-server-error
   #:http-not-implemented
   #:http-bad-gateway
   #:http-service-unavailable
   #:http-gateway-timeout
   #:http-version-not-supported))

(in-package :http-server)

(defconstant +request-line-re+ (compile-re "(%u+)%s+([^%s]+)%s+HTTP/([^%s]+)")
  "HTTP request line pattern.")

(defvar *request* nil
  "The currently active request.")

(defparameter *route-map* nil
  "All routes defined with define-http-route are added here.")

(defun simple-http (&key (name "HTTP Simple Server") (port 8000))
  "Start a server process that will process incoming HTTP requests."
  (flet ((accept-request (h)
           (let ((http (make-instance 'socket-stream :socket h :direction :io :element-type 'base-char)))
             (unwind-protect
                 (let ((resp (let ((*request* (read-http-request http)))
                               (if *request*
                                   (simple-request-router)
                                 (http-bad-request)))))
                   (send-response resp http))
               (close http)))))
    (start-up-server :function #'accept-request
                     :service port
                     :process-name (format nil "~a on port ~d" name port))))

(defun simple-request-router ()
  "Given a request, match a route and execute it."
  (handler-case
      (dolist (route *route-map* (http-not-found))
        (multiple-value-bind (resp okp)
            (funcall route)
          (when okp
            (return resp))))
    (condition (e)
      (http-internal-server-error (princ-to-string e)))))

(defun read-http-request (http)
  "Read from a stream into a request."
  (with-re-match (m (match-re +request-line-re+ (read-line http nil "")))
    (let* ((headers (http::read-http-headers http))

           ;; extract the Host and Content-Length from the request
           (host (second (assoc "Host" headers :test #'string-equal)))
           (content-length (second (assoc "Content-Length" headers :test #'string-equal)))

           ;; parse the body if there is content
           (body (when content-length
                   (http::read-http-content http content-length))))

      ;; create the request object
      (make-instance 'request
                     :method $1
                     :headers headers
                     :data body
                     :url (parse-url (string-append (or host "localhost") $2))))))

(defun send-response (response http)
  "Send a response back over the wire."
  (unwind-protect
      (let ((code (response-code response))
            (status (response-status response))
            (body (response-body response))
            (headers (http-headers response))
            (req (response-request response)))

        ;; send the code and status string
        (format http "HTTP/1.1 ~d~@[ ~a~]~c~c" code status #\return #\linefeed)

        ;; send any and all headers back
        (dolist (header headers)
          (format http "~a: ~a~c~c" (first header) (second header) #\return #\linefeed))
        
        ;; write the length of the response body (if there is one)
        (when body 
          (format http "Content-Length: ~a~c~c" (length body) #\return #\linefeed))
        
        ;; close the connection when done
        (format http "Connection: close~c~c" #\return #\linefeed)
        
        ;; end of headers
        (format http "~c~c" #\return #\linefeed)
        
        ;; send the body if present and not a HEAD request
        (when (and body req (string/= (request-method req) "HEAD"))
          (princ body http)))

    ;; make sure anything that has been written flushes
    (force-output http)))

(defmacro define-http-response (reply code status (&rest headers) &body docstring)
  "Create an HTTP response generation."
  (let ((args (mapcar #'gensym headers))
        (hs (gensym))
        (body (gensym)))
    `(defun ,reply (,@args &optional ,body)
       ,@(let ((doc (first docstring)))
           (when (stringp doc) (list doc)))
       (let ((,hs (mapcar #'list ',headers (list ,@args))))
         (make-instance 'response :request *request* :code ,code :status ,status :headers ,hs :body ,body)))))

(define-http-response http-continue 100 "Continue" ()
  "The client should continue with its request.")
(define-http-response http-switching-protocols 101 "Switching Protocols" ()
  "The server is willing to comply with the client's request per the Upgrade message header field.")
(define-http-response http-ok 200 "OK" ()
  "The request hash-table succeeded.")
(define-http-response http-created 201 "Created" ()
  "The request has been fulfilled and resulted in a new resource being created.")
(define-http-response http-accepted 202 "Accepted" ()
  "The request has been fulfilled and resulted in a new resource being created.")
(define-http-response http-non-authoritative 203 "Non-Authoritative Information" ()
  "The returned information in the header is not the definitive set as available from the origin server.")
(define-http-response http-no-content 204 "No Content" ()
  "The server has fulfilled the request but does not need to return an entity-body.")
(define-http-response http-reset-content 205 "Reset Content" ()
  "The server has fulfilled the request and the user agent should reset the document view.")
(define-http-response http-partial-content 206 "Partial Content" ()
  "The server has fulfilled the partial GET request for the resource.")
(define-http-response http-moved-permanently 301 "Moved Permanently" ("Location")
  "The requested resource has been assigned a new permanent URI.")
(define-http-response http-found 302 "Found" ("Location")
  "The requested resource resides temporarily under a different URI.")
(define-http-response http-see-other 303 "See Other" ("Location")
  "The response to the request can be found under a different URI.")
(define-http-response http-not-modified 304 "Not Modified" ()
  "The response to the request can be found under a different URI.")
(define-http-response http-temporary-redirect 307 "Temporary Redirect" ("Location")
  "The requested resource resides temporarily under a different URI.")
(define-http-response http-bad-request 400 "Bad Request" ()
  "The request could not be understood by the server due to malformed syntax.")
(define-http-response http-unauthorized 401 "Unauthorized" ()
  "The request requires user authentication.")
(define-http-response http-forbidden 403 "Forbidden" ()
  "The server understood the request, but is refusing to fulfill it.")
(define-http-response http-not-found 404 "Not Found" ()
  "The server has not found anything matching the Request-URI.")
(define-http-response http-method-not-allowed 405 "Method Not Allowed" ()
  "The server has not found anything matching the Request-URI.")
(define-http-response http-not-acceptable 406 "Not Acceptable" ()
  "The resource is only capable of generating contents that are not acceptable according to the Accept header.")
(define-http-response http-proxy-required 407 "Proxy Required" ()
  "The client must first authenticate itself with the proxy.")
(define-http-response http-request-timeout 408 "Request Timeout" ()
  "The client did not produce a request within the time that the server was prepared to wait.")
(define-http-response http-conflict 409 "Conflict" ()
  "The request could not be completed due to a conflict with the current state of the resource.")
(define-http-response http-gone 410 "Gone" ()
  "The requested resource is no longer available at the server and no forwarding address is known.")
(define-http-response http-length-required 411 "Length Required" ()
  "The server refuses to accept the request without a defined Content-Length.")
(define-http-response http-precondition-failed 412 "Precondition Failed" ()
  "The precondition given evaluated to false when it was tested on the server.")
(define-http-response http-request-too-large 413 "Request Entity Too Large" ()
  "The server is refusing to process a request because the request entity is too large.")
(define-http-response http-request-uri-too-long 414 "Request URI Too Long" ()
  "The server is refusing to service the request because the Request-URI is too long.")
(define-http-response http-unsupported-media-type 415 "Unsupported Media Type" ()
  "The server is refusing to service the request because the it is in a format not supported.")
(define-http-response http-range-not-satisfiable 416 "Request Range Not Satisfiable" ()
  "The Range request-header field does not overlap the current extent of the selected resource.")
(define-http-response http-expectation-failed 417 "Expectation Failed" ()
  "The expectation given in an Expect request-header field could not be met by this server.")
(define-http-response http-internal-server-error 500 "Internal Server Error" ()
  "The server encountered an unexpected condition which prevented it from fulfilling the request.")
(define-http-response http-not-implemented 501 "Not Implemented" ()
  "The server does not support the functionality required to fulfill the request.")
(define-http-response http-bad-gateway 502 "Bad Gateway" ()
  "The server received an invalid response from the upstream server in attempting to fulfill the request.")
(define-http-response http-service-unavailable 503 "Service Unavailable" ()
  "The server is currently unable to handle the request.")
(define-http-response http-gateway-timeout 504 "Gateway Timeout" ()
  "The server, while acting as a gateway or proxy, did not receive a timely response from the upstream server.")
(define-http-response http-version-not-supported 505 "HTTP Version Not Supported" ()
  "The server does not support the HTTP protocol version that was used in the request message.")

(defmacro define-http-route (name (&rest path) (&rest guards &key &allow-other-keys) &body body)
  "Defines a function that will match a request and execute body if successful."
  (let ((route (gensym))
        (handler (gensym))
        (okp (gensym))
        (els (gensym)))
    `(let ((,route (defun ,name ()
                     (flet ((,handler (,@(route-symbols path)) ,@body))
                       (multiple-value-bind (,els ,okp)
                           (match-route ',path ,@guards)
                         (when ,okp
                           (values (apply #',handler ,els) t)))))))
       (prog1
           ,route
         (eval-when (:load-toplevel :execute)
           (unless (member ,route *route-map* :test 'eq)
             (push ,route *route-map*)))))))

(defun route-symbols (route-els)
  "Returns the symbol list for the route handler."
  (flet ((el-symbol (route-el)
           (etypecase route-el
             (list   (list (first route-el)))
             (symbol (list route-el))
             (string ()))))
    (mapcan #'el-symbol route-els)))

(defun match-route (route-els &key methods)
  "Attempts to match a request to various parameters."
  (when (and (guard-method methods))
    (loop :with path-els := (split-sequence "/" (url-path (request-url *request*)) :coalesce-separators t)
          
          ;; grab all the route elements to match
          :for route-el :in route-els

          ;; look for the rest of the arguments
          :when (eq route-el '&rest)
          :return (values (append match path-els) t)

          ;; symbols extract a path element
          :when (symbolp route-el)
          :collect (let ((path-el (pop path-els)))
                     (if path-el
                         path-el
                       (return-from match-route)))
          :into match

          ;; lists are path elements with guards
          :when (listp route-el)
          :collect (multiple-value-bind (path-el okp)
                       (apply #'guard-path (pop path-els) (rest route-el))
                     (if okp
                         path-el
                       (return-from match-route)))
          :into match

          ;; strings match exactly
          :when (stringp route-el)
          :do (unless (equal route-el (pop path-els))
                (return-from match-route))

          ;; make sure the path was consumed completely
          :finally (when (null path-els)
                     (return (values match t))))))

(defun guard-method (methods)
  "Checks to see if the method matches the request."
  (flet ((method-equal (a b)
           (string-equal (if (string-equal a :head) :get a)
                         (if (string-equal b :head) :get b))))
    (or (null methods)

        ;; does the method match any accepted?
        (find (request-method *request*) methods :test #'method-equal))))

(defun guard-path (path-el &key (ok-check #'identity) (value-function #'identity))
  "Additional guards for a particular path element."
  (and (funcall ok-check path-el)

       ;; all guards matched, return the value
       (values (funcall value-function path-el) t)))
