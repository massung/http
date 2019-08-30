;;;; HTTP interface for SBCL
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
  (:use :cl :sb-ext :sb-bsd-sockets :sb-alien :sb-thread :parse :re :lexer :tls :url)
  (:export

   ;; macros
   #:with-headers
   #:with-response

   ;; HTTP socket streams
   #:http-open-stream
   #:http-open-event-stream

   ;; content type class and functions
   #:content-type-parse
   #:content-type-push
   #:content-type-text-p
   #:content-type-of-pathname
   #:content-type-external-format
   #:content-type-mime-type
   #:content-type-mime-subtype
   #:content-type-parameters
   #:content-type-parameter

   ;; cookie class and functions
   #:cookie-parse
   #:cookie-parse-all
   #:cookie-push
   #:cookie-key
   #:cookie-value
   #:cookie-attributes
   #:cookie-attribute

   ;; creating content-types and cookies
   #:http-make-content-type
   #:http-make-cookie

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
   #:req-ssl-p
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

   ;; route macros
   #:define-http-router

   ;; server functions
   #:http-start-server

   ;; sessions and continuations
   #:http-make-continuation

   ;; response code return functions
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

(in-package :http)

;;; ----------------------------------------------------

(deftype octet () '(unsigned-byte 8))
(deftype octet-vector () '(vector octet))

;;; ----------------------------------------------------

(defun http-make-content-type (type subtype &key format parameters)
  "Make a content-type that can be pushed to a request/response."
  (make-instance 'content-type
                 :type type
                 :subtype subtype
                 :format format
                 :parameters parameters))

;;; ----------------------------------------------------

(defun http-make-cookie (key value &key attributes)
  "Create a cookie that can be pushed to a request/response."
  (make-instance 'cookie :key key :value value :attributes attributes))

;;; ----------------------------------------------------

(defun http-open-stream (req &key timeout)
  "Open a TCP stream to a given URL."
  (let* ((url (req-url req))
         (host (url-domain url))
         (port (url-port url)))

    ;; if the request requires ssl, then create a tls stream
    (if (req-ssl-p req)
        (make-tls-stream host port)
      (let ((address (host-ent-address (get-host-by-name host)))
            (sock (make-instance 'inet-socket :type :stream :protocol :tcp)))
        (setf (sockopt-keep-alive sock) (req-keep-alive req))

        ;; open the connection
        (socket-connect sock address port)
        (socket-make-stream sock
                            :element-type :default
                            :output t
                            :input t
                            :timeout timeout)))))

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
      (http-make-request new-url
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
