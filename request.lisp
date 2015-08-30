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

(defclass request (headers)
  ((url        :initarg :url        :accessor req-url        :initform nil)
   (method     :initarg :method     :accessor req-method     :initform "GET")
   (keep-alive :initarg :keep-alive :accessor req-keep-alive :initform nil)
   (read-body  :initarg :read-body  :accessor req-read-body  :initform t)
   (data       :initarg :data       :accessor req-data       :initform nil))
  (:documentation "A request for a URL from an HTTP server."))

;;; ----------------------------------------------------

(defmethod print-object ((req request) s)
  "Output an HTTP request to a stream."
  (print-unreadable-object (req s :type t)
    (with-slots (method url)
        req
      (format s "~a ~s" method (princ-to-string url)))))

;;; ----------------------------------------------------

(defmethod initialize-instance :after ((req request) &key &allow-other-keys)
  "Make sure the URL - if a string - is parsed."
  (with-slots (url)
      req
    (when (stringp url)
      (setf url (url-parse url)))))

;;; ----------------------------------------------------

(defun basic-auth-string (login)
  "Create the basic auth value for the Authorization header."
  (let ((auth-string (format nil "~{~a:~a~}" login)))
    (format nil "Basic ~a" (base64-encode auth-string))))

;;; ----------------------------------------------------

(defun open-http-stream (req &key timeout)
  "Open a TCP stream to a given URL."
  (let ((url (req-url req)))
    (make-socket
     :remote-host (url-domain url)
     :remote-port (url-port url)
     :auto-close t
     :format :bivalent
     :connect-timeout timeout
     :input-timeout timeout
     :output-timeout timeout
     :keepalive (req-keep-alive req))))

;;; ----------------------------------------------------

(defun send-http-request (stream req)
  "Write the request to the HTTP socket."
  (with-slots (url method headers data keep-alive)
      req

    ;; send the formal http request line
    (let* ((path (url-path url))
           (query (url-query url))
           (fragment (url-fragment url)))

      ;; write the HTTP request method
      (format stream "~a ~a~@[?~a~]~@[#~a~] HTTP/1.1"
              method
              path
              query
              fragment)

      ;; ready for the headers
      (write-char #\return stream)
      (write-char #\linefeed stream))

    ;; always send the host as it should be
    (write-http-header stream "Host" (url-domain url))

    ;; write default headers
    (unless (assoc "Accept" headers :test #'string=)
      (write-http-header stream "Accept" "*/*"))
    (unless (assoc "User-Agent" headers :test #'string=)
      (write-http-header stream "User-Agent" "lisp"))
    (unless (assoc "Connection" headers :test #'string=)
      (write-http-header stream "Connection" (if keep-alive
                                                "keep-alive"
                                              "close")))

    ;; send the content length if there is content
    (when data
      (write-http-header stream "Content-Length" (length data)))

    ;; optionally send basic auth if in the url
    (let ((auth (url-auth url)))
      (when auth
        (write-http-header stream "Authorization" (basic-auth-string auth))))

    ;; send all the headers
    (dolist (header headers)
      (write-http-header stream (first header) (second header)))

    ;; complete the request
    (write-http-header stream nil)

    ;; write optional data to the body
    (when data
      (write-sequence data stream))

    ;; send all data
    (force-output stream)))

;;; ----------------------------------------------------

(defun http-perform (req &optional stream)
  "Perform a generic HTTP request, return the response."
  (unwind-protect
       (progn
         (unless stream
           (setf stream (open-http-stream req)))

         ;; issue the request
         (send-http-request stream req)

         ;; parse the response
         (let ((resp (read-http-response stream req)))
           (prog1 resp
             (with-headers ((connection "Connection"))
               resp
               (when (string-equal connection "close")
                 (close stream))))))

    ;; if the request doesn't want to keep the socket open, close it
    (unless (req-keep-alive req)
      (close stream))))

;;; ----------------------------------------------------

(defun http-follow-request (resp)
  "Create a new request for a redirect response."
  (when (<= 300 (resp-code resp) 399)
    (let* ((req (resp-request resp))
           (url (req-url req))

           ;; get the target location
           (loc (http-header resp "Location"))

           ;; the redirect location might be relative (to the domain)
           (new-url (url-parse loc)))

      ;; keep the query and fragment if present in the request
      (unless (url-query new-url)
        (setf (url-query new-url) (url-query url)))
      (unless (url-fragment new-url)
        (setf (url-fragment new-url) (url-fragment url)))

      ;; create the new request - keep the same method unless a 303
      (make-instance 'request
                     :url new-url
                     :keep-alive (req-keep-alive req)
                     :read-body (req-read-body req)
                     :data (req-data req)
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

(defun http-get (url &rest args &key (redirect-limit 3) &allow-other-keys)
  "Perform a GET request for a URL, return the response."
  (let ((req (apply #'make-instance 'request :url url args)))
    (http-follow (http-perform req) :redirect-limit redirect-limit)))

;;; ----------------------------------------------------

(defun http-head (url &rest args)
  "Perform a HEAD request for a URL, return the response."
  (apply #'http-get url :method "HEAD" :read-body nil args))

;;; ----------------------------------------------------

(defun http-options (url &rest args)
  "Perform an OPTIONS request for a URL, return the response."
  (apply #'http-get url :method "OPTIONS" args))

;;; ----------------------------------------------------

(defun http-trace (url &rest args)
  "Perform an TRACE request for a URL, return the response."
  (apply #'http-get url :method "TRACE" args))

;;; ----------------------------------------------------

(defun http-delete (url &rest args)
  "Perform a DELETE request for a URL, return the response."
  (apply #'http-get url :method "DELETE" args))

;;; ----------------------------------------------------

(defun http-put (url &rest args)
  "Perform a PUT request for a URL, return the response."
  (apply #'http-get url :method "PUT" args))

;;; ----------------------------------------------------

(defun http-post (url &rest args)
  "Perform a POST request for a URL, return the response."
  (apply #'http-get url :method "POST" args))

;;; ----------------------------------------------------

(defun http-patch (url &rest args)
  "Perform a PATCH request for a URL, return the response."
  (apply #'http-get url :method "PATCH" args))
