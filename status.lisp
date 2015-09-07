;;;; HTTP interface for ClozureCL
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

(in-package :http)

;;; ----------------------------------------------------

(defmacro define-http-status (reply code (&key status headers))
  "Create an HTTP response generation."
  (let ((resp (gensym "resp"))
        (body (gensym "body"))

        ;; create an argument for each required header
        (args (mapcar #'gensym headers)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,reply (,resp ,@args &optional ,body)
         (setf (resp-code ,resp) ,code
               (resp-status ,resp) ,status

               ;; set the headers from the arguments passed in
               ,@(loop for a in args and h in headers appending
                      `((http-header ,resp ,h) ,a))

               ;; set the body if it was provided
               (resp-body ,resp) ,body))
       (export ',reply :http))))

;;; ----------------------------------------------------

(define-http-status http-continue 100
  (:status "Continue"))

;;; ----------------------------------------------------

(define-http-status http-switching-protocols 101
  (:status "Switching Protocols"))

;;; ----------------------------------------------------

(define-http-status http-ok 200
  (:status "OK"))

;;; ----------------------------------------------------

(define-http-status http-created 201
  (:status "Created"))

;;; ----------------------------------------------------

(define-http-status http-accepted 202
  (:status "Accepted"))

;;; ----------------------------------------------------

(define-http-status http-non-authoritative 203
  (:status "Non-Authoritative Information"))

;;; ----------------------------------------------------

(define-http-status http-no-content 204
  (:status "No Content"))

;;; ----------------------------------------------------

(define-http-status http-reset-content 205
  (:status "Reset Content"))

;;; ----------------------------------------------------

(define-http-status http-partial-content 206
  (:status "Partial Content"))

;;; ----------------------------------------------------

(define-http-status http-moved-permanently 301
  (:status "Moved Permanently" :headers ("Location")))

;;; ----------------------------------------------------

(define-http-status http-found 302
  (:status "Found" :headers ("Location")))

;;; ----------------------------------------------------

(define-http-status http-see-other 303
  (:status "See Other" :headers ("Location")))

;;; ----------------------------------------------------

(define-http-status http-not-modified 304
  (:status "Not Modified"))

;;; ----------------------------------------------------

(define-http-status http-temporary-redirect 307
  (:status "Temporary Redirect" :headers ("Location")))

;;; ----------------------------------------------------

(define-http-status http-bad-request 400
  (:status "Bad Request"))

;;; ----------------------------------------------------

(define-http-status http-unauthorized 401
  (:status "Unauthorized"))

;;; ----------------------------------------------------

(define-http-status http-forbidden 403
  (:status "Forbidden"))

;;; ----------------------------------------------------

(define-http-status http-not-found 404
  (:status "Not Found"))

;;; ----------------------------------------------------

(define-http-status http-method-not-allowed 405
  (:status "Method Not Allowed"))

;;; ----------------------------------------------------

(define-http-status http-not-acceptable 406
  (:status "Not Acceptable"))

;;; ----------------------------------------------------

(define-http-status http-proxy-required 407
  (:status "Proxy Required"))

;;; ----------------------------------------------------

(define-http-status http-request-timeout 408
  (:status "Request Timeout"))

;;; ----------------------------------------------------

(define-http-status http-conflict 409
  (:status "Conflict"))

;;; ----------------------------------------------------

(define-http-status http-gone 410
  (:status "Gone"))

;;; ----------------------------------------------------

(define-http-status http-length-required 411
  (:status "Length Required"))

;;; ----------------------------------------------------

(define-http-status http-precondition-failed 412
  (:status "Precondition Failed"))

;;; ----------------------------------------------------

(define-http-status http-request-too-large 413
  (:status "Request Entity Too Large"))

;;; ----------------------------------------------------

(define-http-status http-request-uri-too-long 414
  (:status "Request URI Too Long"))

;;; ----------------------------------------------------

(define-http-status http-unsupported-media-type 415
  (:status "Unsupported Media Type"))

;;; ----------------------------------------------------

(define-http-status http-range-not-satisfiable 416
  (:status "Request Range Not Satisfiable"))

;;; ----------------------------------------------------

(define-http-status http-expectation-failed 417
  (:status "Expectation Failed"))

;;; ----------------------------------------------------

(define-http-status http-internal-server-error 500
  (:status "Internal Server Error"))

;;; ----------------------------------------------------

(define-http-status http-not-implemented 501
  (:status "Not Implemented"))

;;; ----------------------------------------------------

(define-http-status http-bad-gateway 502
  (:status "Bad Gateway"))

;;; ----------------------------------------------------

(define-http-status http-service-unavailable 503
  (:status "Service Unavailable"))

;;; ----------------------------------------------------

(define-http-status http-gateway-timeout 504
  (:status "Gateway Timeout"))

;;; ----------------------------------------------------

(define-http-status http-version-not-supported 505
  (:status "HTTP Version Not Supported"))
