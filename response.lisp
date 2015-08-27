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

(defparameter *status-re* (compile-re "^HTTP/%S+%s+(%d+)%s*(%N*)")
  "Pattern for parsing response code and status message.")

;;; ----------------------------------------------------

(defclass response (headers)
  ((http   :initarg :stream       :accessor resp-stream       :initform nil)
   (req    :initarg :request      :accessor resp-request      :initform nil)
   (code   :initarg :code         :accessor resp-code         :initform nil)
   (status :initarg :status       :accessor resp-status       :initform nil)
   (body   :initarg :body         :accessor resp-body         :initform nil)
   (type   :initarg :content-type :accessor resp-content-type :initform nil))
  (:documentation "The response from a request to an HTTP server."))

;;; ----------------------------------------------------

(defmethod print-object ((resp response) stream)
  "Output an HTTP response to a stream."
  (print-unreadable-object (resp stream :type t)
    (format stream "~a ~s" (resp-code resp) (resp-status resp))))

;;; ----------------------------------------------------

(defmethod (setf resp-content-type) :after (content-type (resp response))
  "Update the headers for the response when the content type changes."
  (write-content-type resp content-type))

;;; ----------------------------------------------------

(defmethod (setf resp-body) :after (body (resp response))
  "Update the headers with the content length of the body."
  (setf (http-header resp "Content-Length") (length body)))

;;; ----------------------------------------------------

(defmacro with-response ((resp resp-expr) &body body)
  "Perform request, bind response and exeucte body on success."
  `(let ((,resp ,resp-expr))
     (if (and ,resp (<= 200 (resp-code ,resp) 299))
         (values (progn ,@body) t)
       (values nil nil ,resp))))

;;; ----------------------------------------------------

(defun read-http-status (http)
  "Parse the line and parse it. Return the code and value."
  (with-re-match (match (match-re *status-re* (read-line http)))
    (values (parse-integer $1) $2)))

;;; ----------------------------------------------------

(defun read-http-content-chunked (http &optional chunk-size)
  "Read the body from the HTTP server using a chunked Transfer-Encoding."
  (loop

    ;; read the next chunk size (or use the one provided)
    for n = (if chunk-size
                chunk-size
              (parse-integer (read-line http) :radix 16 :junk-allowed t))

    ;; create a chunk large enough for the read
    for chunk = (make-array n :element-type '(unsigned-byte 8) :fill-pointer t)
    for bytes-read = (read-sequence chunk http)

    ;; update the fill pointer
    do (setf (fill-pointer chunk) bytes-read)

    ;; stop once nothing else was read
    while (plusp bytes-read)

    ;; collect all the chunks together for concatenation later
    collect chunk into body

    ;; return all the bytes as a single vector
    finally (return (apply #'concatenate '(vector (unsigned-byte 8)) body))))

;;; ----------------------------------------------------

(defun read-http-content (http &optional content-length)
  "Read the rest of the response from the HTTP server."
  (if content-length
      (let* ((n (parse-integer content-length))
             (body (make-array n :element-type '(unsigned-byte 8))))
        (prog1 body
          (read-sequence body http)))

    ;; read 4K chunks while there is still data to read
    (read-http-content-chunked http 4000)))

;;; ----------------------------------------------------

(defun read-http-response (http req)
  "Read a response string from an HTTP socket stream and parse it."
  (multiple-value-bind (code status)
      (read-http-status http)
    (let ((resp (make-instance 'response
                               :request req
                               :stream http
                               :code code
                               :status status
                               :content-type nil
                               :headers (read-http-headers http))))
      (prog1 resp

        ;; determine the content-type of the body
        (setf (resp-content-type resp) (read-content-type resp))

        ;; if the request wanted to read the body, then read it
        (when (req-read-body req)
          (read-http-response-body resp))))))

;;; ----------------------------------------------------

(defun read-http-response-body (resp)
  "Read the response body from the stream."
  (with-slots (http body type)
      resp

    ;; get the content length and the transfer type (e.g. chunked)
    (with-headers ((len "Content-Length")
                   (encoding "Transfer-Encoding"))
        resp

      ;; read the response body
      (setf body (cond ((null encoding)
                        (read-http-content http len))
                       ((string= encoding "identity")
                        (read-http-content http len))
                       ((string= encoding "chunked")
                        (read-http-content-chunked http))))

      ;; if the content-type is text, then decode the body
      (unless (binary-content-type-p type)
        (let ((f (content-type-external-format type)))
          (setf body (decode-string-from-octets body :external-format f)))))))

