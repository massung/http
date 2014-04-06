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
   #:simple-http

   ;; response generation
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

(defun simple-http (req-handler &key (name "HTTP Simple Server") (port 8000))
  "Start a server process that will process incoming HTTP requests."
  (flet ((accept-request (h)
           (let ((http (make-instance 'socket-stream :socket h :direction :io :element-type 'base-char)))
             (unwind-protect
                 (let ((resp (when-let (req (read-http-request http))
                               (handler-case
                                   (funcall req-handler req)
                                 (condition (e)
                                   (http-internal-server-error req (princ-to-string e)))))))
                   (send-response resp http))
               (close http)))))
    (start-up-server :process-name name :function #'accept-request :service port)))

(defun read-http-request (http)
  "Read from a stream into a request."
  (with-re-match (m (match-re +request-line-re+ (read-line http)))
    (let ((headers (http::read-http-headers http)))
      (with-headers ((host "Host" :if-not-found "localhost") (content-length "Content-Length"))
          headers
        (let ((body (when content-length
                      (http::read-http-content http content-length))))
          (make-instance 'request
                         :method $1
                         :headers headers
                         :data body
                         :url (parse-url (string-append host $2))))))))

(defun send-response (response http)
  "Send a response back over the wire."
  (unwind-protect
      (let ((code (response-code response))
            (status (response-status response))
            (body (response-body response))
            (headers (response-headers response))
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
                      
(defun http-ok (req &optional body)
  "The request has succeeded."
  (make-instance 'response :request req :code 200 :status "OK" :body body))

(defun http-created (req &optional body)
  "The request has been fulfilled and resulted in a new resource being created."
  (make-instance 'response :request req :code 201 :status "Created" :body body))

(defun http-accepted (req &optional body)
  "The request has been accepted for processing, but the processing has not been completed."
  (make-instance 'response :request req :code 202 :status "Accepted" :body body))

(defun http-non-authoritative (req &optional body)
  "The returned information in the header is not the definitive set as available from the origin server."
  (make-instance 'response :request req :code 203 :status "Non-Authoritative Information" :body body))

(defun http-no-content (req &optional body)
  "The server has fulfilled the request but does not need to return an entity-body."
  (make-instance 'response :request req :code 204 :status "No Content" :body body))

(defun http-reset-content (req &optional body)
  "The server has fulfilled the request and the user agent should reset the document view."
  (make-instance 'response :request req :code 205 :status "Reset Content" :body body))

(defun http-partial-content (req &optional body)
  "The server has fulfilled the partial GET request for the resource."
  (make-instance 'response :request req :code 206 :status "Partial Content" :body body))

(defun http-moved-permanently (req location &optional body)
  "The requested resource has been assigned a new permanent URI."
  (let ((headers `(("Location" ,location))))
    (make-instance 'response :request req :code 301 :status "Moved Permanently" :body body :headers headers)))

(defun http-found (req location &optional body)
  "The requested resource resides temporarily under a different URI."
  (let ((headers `(("Location" ,location))))
    (make-instance 'response :request req :code 302 :status "Found" :body body :headers headers)))

(defun http-see-other (req location &optional body)
  "The response to the request can be found under a different URI."
  (let ((headers `(("Location" ,location))))
    (make-instance 'response :request req :code 303 :status "See Other" :body body :headers headers)))

(defun http-not-modified (req &optional body)
  "The response to the request can be found under a different URI."
  (make-instance 'response :request req :code 304 :status "Not Modified" :body body))

(defun http-temporary-redirect (req location &optional body)
  "The requested resource resides temporarily under a different URI."
  (let ((headers `(("Location" ,location))))
    (make-instance 'response :request req :code 307 :status "Temporary Redirect" :body body :headers headers)))

(defun http-bad-request (req &optional body)
  "The request could not be understood by the server due to malformed syntax."
  (make-instance 'response :request req :code 400 :status "Bad Request" :body body))

(defun http-unauthorized (req &optional body)
  "The request requires user authentication."
  (make-instance 'response :request req :code 401 :status "Unauthorized" :body body))

(defun http-forbidden (req &optional body)
  "The server understood the request, but is refusing to fulfill it."
  (make-instance 'response :request req :code 401 :status "Unauthorized" :body body))

(defun http-not-found (req &optional body)
  "The server has not found anything matching the Request-URI."
  (make-instance 'response :request req :code 404 :status "Not Found" :body body))

(defun http-method-not-allowed (req &optional body)
  "The server has not found anything matching the Request-URI."
  (make-instance 'response :request req :code 405 :status "Method Not Allowed" :body body))

(defun http-not-acceptable (req &optional body)
  "The resource is only capable of generating contents that are not acceptable according to the Accept header."
  (make-instance 'response :request req :code 406 :status "Not Acceptable" :body body))

(defun http-proxy-required (req &optional body)
  "The client must first authenticate itself with the proxy."
  (make-instance 'response :request req :code 407 :status "Proxy Authentication Required" :body body))

(defun http-request-timeout (req &optional body)
  "The client did not produce a request within the time that the server was prepared to wait."
  (make-instance 'response :request req :code 408 :status "Request Timeout" :body body))

(defun http-conflict (req &optional body)
  "The request could not be completed due to a conflict with the current state of the resource."
  (make-instance 'response :request req :code 409 :status "Conflict" :body body))

(defun http-gone (req &optional body)
  "The requested resource is no longer available at the server and no forwarding address is known."
  (make-instance 'response :request req :code 410 :status "Gone" :body body))

(defun http-length-required (req &optional body)
  "The server refuses to accept the request without a defined Content-Length."
  (make-instance 'response :request req :code 411 :status "Length Required" :body body))

(defun http-precondition-failed (req &optional body)
  "The precondition given evaluated to false when it was tested on the server."
  (make-instance 'response :request req :code 412 :status "Precondition Failed" :body body))

(defun http-request-too-large (req &optional body)
  "The server is refusing to process a request because the request entity is too large."
  (make-instance 'response :request req :code 413 :status "Request Entity Too Large" :body body))

(defun http-request-uri-too-long (req &optional body)
  "The server is refusing to service the request because the Request-URI is too long."
  (make-instance 'response :request req :code 414 :status "Request URI Too Long" :body body))

(defun http-unsupported-media-type (req &optional body)
  "The server is refusing to service the request because the it is in a format not supported."
  (make-instance 'response :request req :code 415 :status "Unsupported Media Type" :body body))

(defun http-range-not-satisfiable (req &optional body)
  "The Range request-header field does not overlap the current extent of the selected resource."
  (make-instance 'response :request req :code 416 :status "Requested Range Not Satisfiable" :body body))

(defun http-expectation-failed (req &optional body)
  "The expectation given in an Expect request-header field could not be met by this server."
  (make-instance 'response :request req :code 417 :status "Expectation Failed" :body body))

(defun http-internal-server-error (req &optional body)
  "The server encountered an unexpected condition which prevented it from fulfilling the request."
  (make-instance 'response :request req :code 500 :status "Internal Server Error" :body body))

(defun http-not-implemented (req &optional body)
  "The server does not support the functionality required to fulfill the request."
  (make-instance 'response :request req :code 501 :status "Not Implemented" :body body))

(defun http-bad-gateway (req &optional body)
  "The server received an invalid response from the upstream server in attempting to fulfill the request."
  (make-instance 'response :request req :code 502 :status "Bad Gateway" :body body))

(defun http-service-unavailable (req &optional body)
  "The server is currently unable to handle the request."
  (make-instance 'response :request req :code 503 :status "Service Unavailable" :body body))

(defun http-gateway-timeout (req &optional body)
  "The server, while acting as a gateway or proxy, did not receive a timely response from the upstream server."
  (make-instance 'response :request req :code 504 :status "Gateway Timeout" :body body))

(defun http-version-not-supported (req &optional body)
  "The server does not support the HTTP protocol version that was used in the request message."
  (make-instance 'response :request req :code 505 :status "HTTP Version Not Supported" :body body))
