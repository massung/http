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
  (:use :cl :lw :system :comm :re :lexer :parsergen)
  (:export
   #:request
   #:response
   #:url

   ;; url helpers
   #:parse-url
   #:with-url

   ;; http stream functions
   #:read-http-response
   #:read-http-body

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

   ;; helper functions
   #:http-perform
   #:http-head
   #:http-get
   #:http-delete
   #:http-put
   #:http-post))

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
   (body    :initarg :body    :accessor response-body))
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
  ((url scheme)
   (cons 'url $1))
  ((url :error)
   (error "Invalid URL"))

  ;; http://
  ((scheme :scheme auth)
   `(:scheme ,$1 ,@$2))
  ((scheme auth)
   `(:scheme "http" ,@$1))

  ;; login:password@
  ((auth :auth domain)
   `(:auth ,$1 ,@$2))
  ((auth domain)
   `(:auth nil ,@$1))

  ;; www.host.com
  ((domain host port)
   `(:domain ,$1 ,@$2))

  ;; host.com
  ((host :domain :dot host)
   (string-append $1 #\. $3))
  ((host :domain) $1)

  ;; :80
  ((port :port path)
   `(:port ,$1 ,@$2))
  ((port path)
   `(:port 80 ,@$1))

  ;; /index.html
  ((path :path)
   `(:path ,$1))
  ((path)
   `(:path "/")))

(defmacro with-http-request ((http request &rest tcp-socket-opts) &body body)
  "Send a simple HTTP request to a server."
  (let ((auth (gensym "auth"))
        (domain (gensym "domain"))
        (port (gensym "port"))
        (path (gensym "path"))
        (header (gensym "header"))
        (headers (gensym "headers"))
        (req (gensym "req")))
    `(let ((,req ,request))
       (with-slots ((,domain domain) (,port port) (,path path) (,auth auth))
           (request-url ,req)
         (with-open-stream (,http (open-tcp-stream ,domain ,port ,@tcp-socket-opts))
           (format ,http "~a ~a HTTP/1.1~c~c" (request-method ,req) ,path #\return #\linefeed)

           ;; send all request headers
           (let ((,headers (request-headers ,req)))
             (push (list "Host" ,domain) ,headers)
             (push (list "Connection" "close") ,headers)

             ;; content length
             (when (request-data ,req)
               (push (list "Content-Length" (length (request-data ,req))) ,headers))

             ;; basic auth
             (when ,auth
               (push (list "Authorization" (basic-auth-string ,auth)) ,headers))

             ;; write the request headers
             (dolist (,header ,headers)
               (format ,http "~a: ~a~c~c" (first ,header) (second ,header) #\return #\linefeed)))

           ;; complete the request
           (format ,http "~c~c" #\return #\linefeed)

           ;; write the data in the body (if there is any)
           (when (request-data ,req)
             (write-sequence (request-data ,req) ,http))

           ;; send it
           (force-output ,http)
           
           ;; wait for a response
           (progn ,@body))))))

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

(defun basic-auth-string (userpass)
  "Create the value for an Authorization header."
  (format nil "Basic ~a" userpass))

(defun read-http-status (http)
  "Parse the line and parse it. Return the code and value."
  (with-re-match (match (match-re *status-re* (read-line http)))
    (values (parse-integer $1) $2)))

(defun read-http-header (http)
  "Parse a header line. Return the key and value."
  (with-re-match (match (match-re *header-re* (read-line http)))
    (list $1 $2)))

(defun read-http-response (http)
  "Read a response string from an HTTP socket stream and parse it."
  (multiple-value-bind (code status)
      (read-http-status http)
    (make-instance 'response
                   :code code
                   :status status
                   :body nil
                   :headers (loop :for header := (read-http-header http)
                                  :while header
                                  :collect header))))

(defun read-http-body (http)
  "Read the rest of the response from the HTTP server."
  (with-output-to-string (s)
    (loop :for line := (read-line http nil nil)
          :while line
          :do (write-line line s))))

(defun http-perform (req &key read-body follow-redirect)
  "Perform a generic HTTP request, return the request and body."
  (with-http-request (http req)
    (let ((resp (read-http-response http)))
      (cond
       ;; success!
       ((<= 200 (response-code resp) 299)
        (prog1
            resp
          (when read-body
            (setf (response-body resp) (read-http-body http)))))
           
       ;; redirected, reload at the new location
       ((<= 300 (response-code resp) 399)
        (let ((location (assoc "Location" (response-headers resp) :test #'string=)))
          (if (or (null follow-redirect)
                  (null location))
              resp
            (with-url (url (second location))
              (let ((req (make-instance 'request
                                        :url url
                                        :method (request-method req)
                                        :headers (request-headers req)
                                        :data (request-data req))))
                (http-perform req :read-body read-body))))))
       
       ;; failure
       (t resp)))))

(defun http-head (url &key headers follow-redirect)
  "Perform a HEAD request for a URL, return the response."
  (with-url (url url)
    (let ((req (make-instance 'request :url url :method "HEAD" :headers headers)))
      (http-perform req :follow-redirect follow-redirect))))

(defun http-get (url &key headers follow-redirect)
  "Perform a GET request for a URL, return the response and body."
  (with-url (url url)
    (let ((req (make-instance 'request :url url :method "GET" :headers headers)))
      (http-perform req :follow-redirect follow-redirect :read-body t))))

(defun http-delete (url &key headers)
  "Perform a DELETE request for a URL, return the response."
  (with-url (url url)
    (let ((req (make-instance 'request :url url :method "DELETE" :headers headers)))
      (http-perform req))))

(defun http-put (url &key headers data)
  "Perform a PUT request for a URL, return the response."
  (with-url (url url)
    (let ((req (make-instance 'request :url url :method "PUT" :headers headers :data data)))
      (http-perform req :read-body t))))

(defun http-post (url &key headers data)
  "Perform a POST request for a URL, return the response."
  (with-url (url url)
    (let ((req (make-instance 'request :url url :method "POST" :headers headers :data data)))
      (http-perform req :read-body t))))
