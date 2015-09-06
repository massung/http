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

(defpackage :http
  (:use :cl :ccl :sha1 :parse :re :lexer :url)
  (:export

   ;; macros
   #:with-headers
   #:with-response

   ;; HTTP socket streams
   #:http-open-stream
   #:http-open-event-stream

   ;; content type functions
   #:content-type-parse
   #:content-type-push
   #:content-type-text-p
   #:content-type-external-format
   #:content-type-mime-type
   #:content-type-mime-subtype
   #:content-type-parameters
   #:content-type-parameter

   ;; cookie functions
   #:cookie-parse
   #:cookie-push
   #:cookie-key
   #:cookie-value
   #:cookie-attributes
   #:cookie-attribute

   ;; header functions
   #:http-headers
   #:http-header
   #:http-read-headers
   #:http-write-headers

   ;; creating requests and responsed
   #:http-make-request
   #:http-make-response

   ;; content encodings
   #:http-register-content-encoding
   #:http-registered-content-encoding

   ;; request functions
   #:http-follow
   #:http-perform
   #:http-head
   #:http-get
   #:http-options
   #:http-trace
   #:http-delete
   #:http-put
   #:http-post
   #:http-patch

   ;; request accessors
   #:req-protocol
   #:req-url
   #:req-method
   #:req-body
   #:req-keep-alive
   #:req-read-body

   ;; response accessors
   #:resp-stream
   #:resp-protocol
   #:resp-code
   #:resp-status
   #:resp-body
   #:resp-request

   ;; server functions
   #:http-simple-server

   ;; base server configuration class
   #:http-server-config

   ;; routers
   #:define-http-router

   ;; continuations
   #:http-make-continuation))

(in-package :http)

;;; ----------------------------------------------------

(deftype octet () '(unsigned-byte 8))
(deftype octet-vector () '(vector octet))

;;; ----------------------------------------------------

(defun http-open-stream (req &key timeout)
  "Open a TCP stream to a given URL."
  (let ((url (req-url req)))
    (make-socket :remote-host (url-domain url)
                 :remote-port (url-port url)
                 :auto-close t
                 :format :bivalent
                 :connect-timeout timeout
                 :input-timeout timeout
                 :output-timeout timeout
                 :keepalive (req-keep-alive req))))

;;; ----------------------------------------------------

(defun http-perform (req &optional stream)
  "Perform a generic HTTP request, return the response."
  (let ((http (if stream
                  stream
                (http-open-stream req))))

    ;; send the request
    (http-write-request req http)

    ;; parse the response
    (let ((resp (http-read-response req http)))
      (prog1 resp

        ;; determine if the connection should terminate
        (cond ((null resp)
               (close http))

              ;; the request wants to close it
              ((null (req-keep-alive req))
               (close http))

              ;; see if the server wants to close it
              (t (let ((connection (http-header resp "Connection")))
                   (when (string-equal connection "close")
                     (close http)))))))))

;;; ----------------------------------------------------

(defun http-follow-request (resp)
  "Create a new request for a redirect response."
  (when (<= 300 (resp-code resp) 399)
    (let* ((req (resp-request resp))
           (url (req-url req))

           ;; get the target location
           (loc (http-header resp "Location"))

           ;; the redirect location might be relative (to the domain)
           (new-url (url-parse loc :relative-url url)))

      ;; create the new request - keep the same method unless a 303
      (make-instance 'request
                     :url new-url
                     :keep-alive (req-keep-alive req)
                     :read-body (req-read-body req)
                     :body (req-body req)
                     :headers (http-headers req)
                     :method (if (= (resp-code resp) 303)
                                 "GET"
                               (req-method req))))))

;;; ----------------------------------------------------

(defun http-follow (resp &key (redirect-limit 3))
  "Create a response redirect."
  (loop
     while (plusp redirect-limit)

     ;; if the response requires a redirect, follow it
     do (if (find (resp-code resp) '(301 302 303 304 305 307))
            (let ((req (http-follow-request resp)))
              (setf resp (http-perform req))
              (decf redirect-limit))
          (loop-finish))

     ;; quit returning the last response
     finally (return resp)))

;;; ----------------------------------------------------

(defun http-get (url &rest initargs)
  "Perform a GET request for a URL, return the response."
  (let ((req (apply #'http-make-request url initargs)))
    (http-follow (http-perform req) :redirect-limit 3)))

;;; ----------------------------------------------------

(defun http-head (url &rest initargs)
  "Perform a HEAD request for a URL, return the response."
  (apply #'http-get url :method "HEAD" :read-body nil initargs))

;;; ----------------------------------------------------

(defun http-options (url &rest initargs)
  "Perform an OPTIONS request for a URL, return the response."
  (apply #'http-get url :method "OPTIONS" initargs))

;;; ----------------------------------------------------

(defun http-trace (url &rest initargs)
  "Perform an TRACE request for a URL, return the response."
  (apply #'http-get url :method "TRACE" initargs))

;;; ----------------------------------------------------

(defun http-delete (url &rest initargs)
  "Perform a DELETE request for a URL, return the response."
  (apply #'http-get url :method "DELETE" initargs))

;;; ----------------------------------------------------

(defun http-put (url &rest initargs)
  "Perform a PUT request for a URL, return the response."
  (apply #'http-get url :method "PUT" initargs))

;;; ----------------------------------------------------

(defun http-post (url &rest initargs)
  "Perform a POST request for a URL, return the response."
  (apply #'http-get url :method "POST" initargs))

;;; ----------------------------------------------------

(defun http-patch (url &rest initargs)
  "Perform a PATCH request for a URL, return the response."
  (apply #'http-get url :method "PATCH" initargs))
