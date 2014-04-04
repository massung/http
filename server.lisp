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
   #:start-http-server
   #:define-http-route))

(in-package :http-server)

(defconstant +request-re+ (compile-re "(%u+)%s+([^%s]+)%s+HTTP/([^%s]+)")
  "HTTP request line pattern.")

(defparameter *request* nil
  "The active request being routed.")
(defparameter *response* nil
  "Returned response for the active request.")

(defclass route ()
  ((method   :initarg :method   :reader route-method)
   (path     :initarg :path     :reader route-path)
   (callback :initarg :callback :reader route-callback))
  (:documentation ""))

(define-condition http-server-error (condition)
  ((code   :initarg :code   :initform 500)
   (status :initarg :status :initform "Internal Server Error"))
  (:documentation "A more specific error (e.g. Not found) other than a 500."))

(defun start-http-server (name routes &key (port 8000))
  "Spin up a server process that will accept HTTP requests, route, and return a response."
  (flet ((accept-request (h)
           (let ((stream (make-instance 'socket-stream :socket h :direction :io :element-type 'base-char)))
             (unwind-protect
                 (let ((response (handler-case
                                     (route-request (read-http-request stream) routes)

                                   ;; specific server errors
                                   (http-server-error (e)
                                     (with-slots (code status)
                                         e
                                       (make-instance 'response :code code :status status :request nil)))

                                   ;; undefined server errors
                                   (condition (e)
                                     (let ((status (princ-to-string e)))
                                       (make-instance 'response :code 500 :status status :request nil))))))
                   (send-response response stream))
               (close stream)))))
    (start-up-server :process-name name :service port :function #'accept-request)))

(defmacro define-http-route ((name (&rest path) &key (method "GET")) &body body)
  "Assign a variable to an HTTP route object."
  (let ((handle-route (gensym)))
    `(flet ((,handle-route (,@(remove-if #'stringp path)) ,@body))
       (defparameter ,name (make-instance 'route :path ',path :callback #',handle-route :method ,method)))))

(defun send-response (response stream)
  "Send a response back over the wire."
  (unwind-protect
      (let ((code (response-code response))
            (status (response-status response))
            (body (response-body response))
            (headers (response-headers response))
            (req (response-request response)))

        ;; send the code and status string
        (format stream "HTTP ~d~@[ ~a~]~c~c" code status #\return #\linefeed)

        ;; send any and all headers back
        (dolist (header headers)
          (format stream "~a: ~a~c~c" (first header) (second header) #\return #\linefeed))
        
        ;; write the length of the response body (if there is one)
        (when body 
          (format stream "Content-Length: ~a~c~c" (length body) #\return #\linefeed))
        
        ;; close the connection when done
        (format stream "Connection: close~c~c" #\return #\linefeed)
        
        ;; end of headers
        (format stream "~c~c" #\return #\linefeed)
        
        ;; send the body if present and not a HEAD request
        (when (and body req (string/= (request-method req) "HEAD"))
          (princ body stream)))

    ;; make sure anything that has been written flushes
    (force-output stream)))

(defun read-http-request (stream)
  "Read the request line and the headers."
  (let ((line (read-line stream nil)))
    (with-re-match (m (match-re +request-re+ line) :no-match (error "Invalid HTTP Request"))
      (let ((headers (http::read-http-headers stream)))
        (with-headers ((host "Host" :if-not-found "localhost")
                       (content-length "Content-Length"))
            headers
          (with-url (url (string-append host $2))
            (let ((body (when content-length
                          (http::read-http-content stream content-length))))
              (make-instance 'request :method $1 :url url :headers headers :data body))))))))

(defun route-request (req routes)
  "Compare the path in the request url to all the routes, match, and execute."
  (loop :with path-els := (rest (split-sequence "/" (url-path (request-url req))))
        :with method := (request-method req)
          
        ;; loop over all the routes, looking for a match
        :for name :in routes
        :for route := (symbol-value name)
        
        ;; attempt to match and run each route
        :for (route-els okp) := (multiple-value-list (match-route route method path-els))
        
        ;; if successful, set the body and return it
        :when okp
        :return (let ((*response* (make-instance 'response :request req)))
                  (setf (response-body *response*)
                        (with-output-to-string (*standard-output* nil :element-type 'base-char)
                          (apply (route-callback route) route-els)))
                  
                  ;; return the response and a success flag
                  (values *response* t))

        ;; otherwise, set a 404 error code
        :finally (error (make-instance 'http-server-error :code 404 :status "Not Found"))))

(defun match-route (route method path-els)
  "Returns T and a list of variable path values to pass to a route function."
  (when (string= (route-method route) method)
    (let ((route-els (loop :for route-el :in (route-path route)
                           :for path-el := (pop path-els)
                                      
                           ;; no more path elements, fail to match
                           :unless path-el
                           :return nil
                           
                           ;; string route elements match exactly
                           :when (and (stringp route-el) (string/= route-el path-el))
                           :do (return-from match-route)
                           
                           ;; symbol route elements are passed along
                           :when (symbolp route-el)
                           :collect path-el
                           
                           ;; make sure the passed in url was consumed in its entirety
                           :finally (when path-els
                                      (return-from match-route)))))
      (values route-els t))))

;;; Example routes
(define-http-route (hello ("hello" target) :method "GET")
  (html `(:html () (:body () (:h1 () ,(format nil "Hello, ~a" target))))))