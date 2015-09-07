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
  ((method     :initarg :method     :initform "GET" :accessor req-method)
   (protocol   :initarg :protocol   :initform "1.0" :accessor req-protocol)
   (url        :initarg :url        :initform nil   :accessor req-url)
   (keep-alive :initarg :keep-alive :initform nil   :accessor req-keep-alive)
   (read-body  :initarg :read-body  :initform t     :accessor req-read-body)
   (body       :initarg :body       :initform nil   :accessor req-body))
  (:documentation "An HTTP request to be responded to by an HTTP server."))

;;; ----------------------------------------------------

(defmethod print-object ((req request) s)
  "Output an HTTP request to a stream."
  (print-unreadable-object (req s :type t)
    (with-slots (method url)
        req
      (format s "~a ~a" method url))))

;;; ----------------------------------------------------

(defmethod cookie-push ((cookie cookie) (req request))
  "Add a header to the headers of a request or response."
  (with-slots (key value atts)
      cookie
    (let ((str (format nil "~a=~s~:{; ~a=~s~}" key value atts)))
      (push (list "Cookie" str) (http-headers req)))))

;;; ----------------------------------------------------

(defun http-make-request (url &rest initargs)
  "Create a new HTTP request."
  (with-url (url url)
    (apply 'make-instance 'request :url url initargs)))

;;; ----------------------------------------------------

(defun http-read-request-line (http)
  "Read the method, path, and protocol from a stream."
  (let ((line (read-line http nil "")))
    (let ((match (match-re #r"(%u+)%s+([^%s]+)%s+HTTP/([^%s]+)" line)))
      (when match
        (values-list (match-groups match))))))

;;; ----------------------------------------------------

(defun http-read-request (http)
  "Read from a stream into a request."
  (multiple-value-bind (method path protocol)
      (http-read-request-line http)
    (when method
      (let* ((headers (http-read-headers http))

             ;; get the headers needed to define the request
             (host (http-header headers "Host"))
             (connection (http-header headers "Connection"))
             (content-length (http-header headers "Content-Length"))

             ;; combine the host and path for the url
             (url (concatenate 'string host path))

             ;; determine if the connection should be kept alive
             (keep-alive (string-equal connection "keep-alive"))

             ;; get calculate the length of the body to read
             (n (when content-length
                  (parse-integer content-length :junk-allowed t)))

             ;; read the body of the request
             (body (when (and n (plusp n))
                     (http-read-content http n))))

        ;; create the request
        (http-make-request url
                           :method method
                           :protocol protocol
                           :keep-alive keep-alive
                           :headers (http-headers headers)
                           :body (http-decode body headers))))))

;;; ----------------------------------------------------

(defun http-write-request (req http)
  "Write the request to the HTTP socket."
  (with-slots (method url protocol headers body keep-alive)
      req

    ;; send the formal http request line
    (format http "~a ~a HTTP/~a" method (url-request-path url) protocol)

    ;; ready for the headers
    (write-char #\return http)
    (write-char #\linefeed http)

    ;; if sending a body, encode it
    (let ((bytes (when body
                   (http-encode body req))))

      ;; update the headers with the most recent information
      (setf (http-header req "Host")
            (url-domain url)

            ;; accept all registered content encodings
            (http-header req "Accept-Encoding")
            (http-registered-content-encodings)

            ;; if the URL has basic authorization, add it
            (http-header req "Authorization")
            (url-basic-auth url)

            ;; set the length of the encoded body
            (http-header req "Content-Length")
            (length bytes)

            ;; should the connection close or remain open?
            (http-header req "Connection")
            (format nil "~:[close~;keep-alive~]" keep-alive))


      ;; send all the headers
      (http-write-headers req http)

      ;; write optional data to the body
      (when bytes
        (write-sequence bytes http)))

    ;; send all data
    (force-output http)))
