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

   ;; utility functions
   #:parse-url

   ;; functions
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

   ;; response accessors
   #:response-code
   #:response-status
   #:response-headers
   #:response-url
   
   ;; macros
   #:with-http-request
   #:with-url

   ;; helper functions
   #:http-head
   #:http-get
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
   (headers :initarg :headers :accessor request-headers :initform nil)
   (method  :initarg :method  :accessor request-method  :initform :get))
  (:documentation "A request for a URL from an HTTP server."))

(defclass response ()
  ((code    :initarg :code    :accessor response-code)
   (status  :initarg :status  :accessor response-status)
   (headers :initarg :headers :accessor response-headers)
   (url     :initarg :url     :accessor response-url))
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
    (with-slots (url method)
        req
      (format stream "~a \"~a://~a~a\"" method (url-scheme url) (url-domain url) (url-path url)))))

(defmethod print-object ((resp response) stream)
  "Output an HTTP response to a stream."
  (print-unreadable-object (resp stream :type t)
    (with-slots (code status)
        resp
      (format stream "~a ~s" code status))))

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

(defmacro with-http-request ((http req &rest tcp-socket-opts) &body body)
  "Send a simple HTTP request to a server."
  (let ((auth (gensym "auth"))
        (domain (gensym "domain"))
        (port (gensym "port"))
        (path (gensym "path"))
        (header (gensym "header"))
        (request (gensym "request")))
    `(let ((,request ,req))
       (with-slots ((,domain domain) (,port port) (,path path) (,auth auth))
           (request-url ,request)
         (with-open-stream (,http (open-tcp-stream ,domain ,port ,@tcp-socket-opts))
           (format ,http "~a ~a HTTP/1.1~c~c" (request-method ,request) ,path #\return #\linefeed)
           
           ;; send the host and force the server to close the connection
           (format ,http "Host: ~a~c~c" ,domain #\return #\linefeed)
           (format ,http "Connection: close~c~c" #\return #\linefeed)

           ;; send the auth header
           (when ,auth
             (format ,http "Authorization: ~a~c~c" (basic-auth-string ,auth) #\return #\linefeed))

           ;; send the other headers
           (dolist (,header (request-headers ,request))
             (format ,http "~a: ~a~c~c" (first ,header) (second ,header) #\return #\linefeed))

           ;; complete the request
           (format ,http "~c~c" #\return #\linefeed)
           (force-output ,http)
           
           ;; execute the body
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

(defun read-response-status (http)
  "Parse the line and parse it. Return the code and value."
  (with-re-match (match (match-re "^HTTP/[^%s]+%s+(%d+)%s+([^%n]+)" (read-line http)))
    (values (parse-integer $1) $2)))

(defun read-response-header (http)
  "Parse a header line. Return the key and value."
  (with-re-match (match (match-re "^([^:]+):%s*([^%n]*)" (read-line http)))
    (list $1 $2)))

(defun read-http-response (http)
  "Read a response string from an HTTP socket stream and parse it."
  (multiple-value-bind (code status)
      (read-response-status http)
    (make-instance 'response
                   :code code
                   :status status
                   :url nil
                   :headers (loop :for header := (read-response-header http)
                                  :while header
                                  :collect header))))

(defun read-http-body (http)
  "Read the rest of the response from the HTTP server."
  (with-output-to-string (s)
    (loop :for line := (read-line http nil nil)
          :while line
          :do (write-line line s))))

(defun http-head (url &key headers)
  "Create a HEAD request for a URL, send it, return the response."
  (with-url (url url)
    (let ((req (make-instance 'request :method :head :headers headers :url url)))
      (with-http-request (http req)
        (read-http-response http)))))

(defun http-get (url &key headers)
  "Create a request from a URL, send it, and read the response."
  (with-url (url url)
    (let ((req (make-instance 'request :method :get :headers headers :url url)))
      (with-http-request (http req)
        (let ((resp (read-http-response http)))
          (values resp (when (<= 200 (response-code resp) 299)
                         (read-http-body http))))))))
