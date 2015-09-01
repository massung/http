;;;; Simple HTTP Server for ClozureCL
;;;;
;;;; Copyright (c) Jeffrey Massung
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
  (:use :cl :ccl :re :http :url)
  (:export
   #:http-simple-server

   ;; route and page macros
   #:define-http-router
   #:define-http-page

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

;;; ----------------------------------------------------

(defparameter *req-re* (compile-re "(%u+)%s+([^%s]+)%s+HTTP/([^%s]+)")
  "HTTP request line pattern.")

;;; ----------------------------------------------------

(defun http-simple-server (router &key (port 8000))
  "Start a server process that will process incoming HTTP requests."
  (let ((sock (make-socket :type :stream
                           :connect :passive
                           :reuse-address t
                           :local-port port)))
    (process-run-function "HTTP Server" 'server-loop sock router)))

;;; ----------------------------------------------------

(defun server-loop (socket router)
  "Infinite loop, accepting new connections and routing them."
  (flet ((handle-request (http)
           (unwind-protect
               (let ((resp (make-response http)))
                 (handler-case
                     (funcall router resp)
                   (condition (c)
                     (http-internal-server-error resp c)))

                 ;; send the response
                 (send-response resp http))
             (close http))))

    ;; loop forever, accepting new connections
    (unwind-protect
        (loop (let ((http (accept-connection socket :wait t)))
                (process-run-function "Request" #'handle-request http)))
      (close socket))))

;;; ----------------------------------------------------

(defun read-http-request (http)
  "Read from a stream into a request."
  (with-re-match (m (match-re *req-re* (read-line http nil "")))
    (let* ((headers (http::read-http-headers http))

           ;; extract the Host and Content-Length from the request
           (host (assoc "Host" headers :test #'string-equal))
           (len (assoc "Content-Length" headers :test #'string-equal))

           ;; create the url the request used from the host and path
           (url (concatenate 'string (or (second host) "localhost") $2))

           ;; parse the body if there is content
           (body (when len
                   (http::read-http-content http (second len)))))

      ;; create the request object
      (make-instance 'request
                     :method $1
                     :headers headers
                     :data body
                     :url (url-parse url)))))

;;; ----------------------------------------------------

(defun make-response (http)
  "Create a response from the stream and request."
  (make-instance 'response :stream http :request (read-http-request http)))

;;; ----------------------------------------------------

(defun send-response (resp http)
  "Send a response back over the wire."
  (unwind-protect
      (let ((code (resp-code resp))
            (status (resp-status resp))
            (body (resp-body resp))
            (headers (http-headers resp))
            (req (resp-request resp)))

        ;; send the code and status string
        (format http "HTTP/1.1 ~d~@[ ~a~]" code status)

        ;; begin headers
        (http::write-http-header http)

        ;; send any and all headers back
        (dolist (header headers)
          (http::write-http-header http (first header) (second header)))

        ;; close the connection when done
        (http::write-http-header http "Connection" "close")

        ;; end of headers
        (http::write-http-header http)

        ;; send the body if present and not a HEAD request
        (when (and body req (string/= (req-method req) "HEAD"))
          (write-sequence body http)))

    ;; make sure anything that has been written flushes
    (force-output http)))

;;; ----------------------------------------------------

(defmacro define-http-response (reply (code status &rest hs) doc)
  "Create an HTTP response generation."
  (let ((args (mapcar #'gensym hs))
        (resp (gensym "resp"))
        (body (gensym "body")))
    `(defun ,reply (,resp ,@args &optional ,body)
       ,doc
       (setf (resp-body ,resp) (when ,body (princ-to-string ,body))

             ;; set the headers from the arguments passed in
             ,@(loop for a in args and h in hs appending
                 `((http-header ,resp ,h) ,a))

             ;; set the response code and status
             (resp-code ,resp) ,code
             (resp-status ,resp) ,status))))

;;; ----------------------------------------------------

(define-http-response http-continue
    (100 "Continue")
  "The client should continue with its request.")

;;; ----------------------------------------------------

(define-http-response http-switching-protocols
    (101 "Switching Protocols")
  "The server is willing to comply with the client's request.")

;;; ----------------------------------------------------

(define-http-response http-ok
    (200 "OK")
  "The request hash-table succeeded.")

;;; ----------------------------------------------------

(define-http-response http-created
    (201 "Created")
  "The request has been fulfilled and a new resource created.")

;;; ----------------------------------------------------

(define-http-response http-accepted
    (202 "Accepted")
  "The request has been fulfilled and a new resource created.")

;;; ----------------------------------------------------

(define-http-response http-non-authoritative
    (203 "Non-Authoritative Information")
  "The returned information is not as definitive as from the origin server.")

;;; ----------------------------------------------------

(define-http-response http-no-content
    (204 "No Content")
  "The server has fulfilled the request but did return a body.")

;;; ----------------------------------------------------

(define-http-response http-reset-content
    (205 "Reset Content")
  "The server has fulfilled the request; the document view should reset.")

;;; ----------------------------------------------------

(define-http-response http-partial-content
    (206 "Partial Content")
  "The server has fulfilled the partial GET request for the resource.")

;;; ----------------------------------------------------

(define-http-response http-moved-permanently
    (301 "Moved Permanently" "Location")
  "The requested resource has been assigned a new permanent URI.")

;;; ----------------------------------------------------

(define-http-response http-found
    (302 "Found" "Location")
  "The requested resource resides temporarily under a different URI.")

;;; ----------------------------------------------------

(define-http-response http-see-other
    (303 "See Other" "Location")
  "The response to the request can be found under a different URI.")

;;; ----------------------------------------------------

(define-http-response http-not-modified
    (304 "Not Modified")
  "The response to the request can be found under a different URI.")
;;; ----------------------------------------------------

(define-http-response http-temporary-redirect
    (307 "Temporary Redirect" "Location")
  "The requested resource resides temporarily under a different URI.")

;;; ----------------------------------------------------

(define-http-response http-bad-request
    (400 "Bad Request")
  "The request contained malformed syntax.")

;;; ----------------------------------------------------

(define-http-response http-unauthorized
    (401 "Unauthorized")
  "The request requires user authentication.")

;;; ----------------------------------------------------

(define-http-response http-forbidden
    (403 "Forbidden")
  "The server understood the request, but is refusing to fulfill it.")

;;; ----------------------------------------------------

(define-http-response http-not-found
    (404 "Not Found")
  "The server has not found anything matching the Request-URI.")

;;; ----------------------------------------------------

(define-http-response http-method-not-allowed
    (405 "Method Not Allowed")
  "The server has not found anything matching the Request-URI.")

;;; ----------------------------------------------------

(define-http-response http-not-acceptable
    (406 "Not Acceptable")
  "The resource contents are not accepted by the request Accept header.")

;;; ----------------------------------------------------

(define-http-response http-proxy-required
    (407 "Proxy Required")
  "The client must first authenticate itself with the proxy.")

;;; ----------------------------------------------------

(define-http-response http-request-timeout
    (408 "Request Timeout")
  "The client did not produce a request within the time allotted.")

;;; ----------------------------------------------------

(define-http-response http-conflict
    (409 "Conflict")
  "The request could not be completed due to a conflict.")

;;; ----------------------------------------------------

(define-http-response http-gone
    (410 "Gone")
  "The requested resource is no longer available at the server.")

;;; ----------------------------------------------------

(define-http-response http-length-required
    (411 "Length Required")
  "The server refuses to accept the request without a Content-Length.")

;;; ----------------------------------------------------

(define-http-response http-precondition-failed
    (412 "Precondition Failed")
  "The precondition given evaluated to false.")

;;; ----------------------------------------------------

(define-http-response http-request-too-large
    (413 "Request Entity Too Large")
  "The server is refusing to process a request.")

;;; ----------------------------------------------------

(define-http-response http-request-uri-too-long
    (414 "Request URI Too Long")
  "The server is refusing to service the request.")

;;; ----------------------------------------------------

(define-http-response http-unsupported-media-type
    (415 "Unsupported Media Type")
  "The server is refusing to service the request.")

;;; ----------------------------------------------------

(define-http-response http-range-not-satisfiable
    (416 "Request Range Not Satisfiable")
  "The Range request-header field does not overlap the resource extent.")

;;; ----------------------------------------------------

(define-http-response http-expectation-failed
    (417 "Expectation Failed")
  "The Expect request-header field could not be met by this server.")

;;; ----------------------------------------------------

(define-http-response http-internal-server-error
    (500 "Internal Server Error")
  "The server encountered an unexpected condition.")

;;; ----------------------------------------------------

(define-http-response http-not-implemented
    (501 "Not Implemented")
  "The server does not support the functionality required.")

;;; ----------------------------------------------------

(define-http-response http-bad-gateway
    (502 "Bad Gateway")
  "The server received an invalid response from the upstream server.")

;;; ----------------------------------------------------

(define-http-response http-service-unavailable
    (503 "Service Unavailable")
  "The server is currently unable to handle the request.")

;;; ----------------------------------------------------

(define-http-response http-gateway-timeout
    (504 "Gateway Timeout")
  "The server, did not receive a timely response from an upstream server.")

;;; ----------------------------------------------------

(define-http-response http-version-not-supported
    (505 "HTTP Version Not Supported")
  "The server does not support the HTTP protocol version that was used.")
