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
  (:use :cl :ccl :base64 :re :url)
  (:export

   ;; request/response classes
   #:request
   #:response

   ;; macros
   #:with-headers
   #:with-response

   ;; HTTP socket streams
   #:open-http-stream
   #:open-http-event-stream

   ;; content type class and functions
   #:content-type-parse
   #:content-mime-type
   #:content-parameters

   ;; header accessors
   #:http-headers
   #:http-header

   ;; following redirects
   #:http-follow-request
   #:http-follow

   ;; request functions
   #:http-perform
   #:http-head
   #:http-get
   #:http-options
   #:http-trace
   #:http-delete
   #:http-put
   #:http-post
   #:http-patch

   ;; http request accessors
   #:req-url
   #:req-method
   #:req-data
   #:req-keep-alive
   #:req-read-body

   ;; response accessors
   #:resp-stream
   #:resp-code
   #:resp-status
   #:resp-body
   #:resp-content-type
   #:resp-request))

(in-package :http)

;;; ----------------------------------------------------

(deftype octet () '(unsigned-byte 8))
(deftype octet-vector () '(vector octet))
