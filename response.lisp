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

(in-package :http)

;;; ----------------------------------------------------

(defclass response (headers)
  ((stream   :initarg :stream   :initform nil   :accessor resp-stream)
   (req      :initarg :request  :initform nil   :accessor resp-request)
   (protocol :initarg :protocol :initform "1.0" :accessor resp-protocol)
   (code     :initarg :code     :initform nil   :accessor resp-code)
   (status   :initarg :status   :initform nil   :accessor resp-status)
   (body     :initarg :body     :initform nil   :accessor resp-body))
  (:documentation "The response from a request to an HTTP server."))

;;; ----------------------------------------------------

(defmethod print-object ((resp response) stream)
  "Output an HTTP response to a stream."
  (print-unreadable-object (resp stream :type t)
    (format stream "~a ~s" (resp-code resp) (resp-status resp))))

;;; ----------------------------------------------------

(defmethod cookie-header ((resp response)) "Set-Cookie")

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

(defun http-read-status (http)
  "Parse the line and parse it. Return the code and value."
  (let ((line (read-line http)))
    (with-re-match (match (match-re #r"^HTTP/(%S+)%s+(%d+)%s*(%N*)" line))
      (values $1 (parse-integer $2) $3))))

;;; ----------------------------------------------------

(defun http-read-content-chunked (http &optional chunk-size)
  "Read the body from the HTTP server using a chunked Transfer-Encoding."
  (loop

     ;; read the next chunk size (or use the one provided)
     for n = (if chunk-size
                 chunk-size
               (parse-integer (read-line http) :radix 16 :junk-allowed t))

     ;; if we failed to parse a chunk size, we're done
     while n

     ;; create a chunk for reading from the stream
     for chunk = (make-array n :element-type 'octet :fill-pointer t)

     ;; expand the chunk if needed, and read the sequence
     for bytes-read = (read-sequence chunk http)

     ;; make sure to clamp extra bytes
     do (setf (fill-pointer chunk) bytes-read)

     ;; stop once nothing else was read
     while (plusp bytes-read)

     ;; collect all the chunks together for concatenation later
     collect chunk into body

     ;; return all the bytes as a single vector
     finally (return (apply #'concatenate 'octet-vector body))))

;;; ----------------------------------------------------

(defun http-read-content (http &optional content-length)
  "Read the rest of the response from the HTTP server."
  (cond ((null content-length)
         (http-read-content-chunked http 4000))

        ;; if a string from a header, parse, and retry
        ((stringp content-length)
         (let ((n (parse-integer content-length :junk-allowed t)))
           (http-read-content http n)))

        ;; read the body, assuming length is ok
        (t (let ((body (make-array content-length :element-type 'octet)))
             (prog1 body
               (read-sequence body http))))))

;;; ----------------------------------------------------

(defun http-read-response (req http)
  "Read a response string from an HTTP socket stream and parse it."
  (multiple-value-bind (protocol code status)
      (http-read-status http)
    (let* ((headers (http-read-headers http))

           ;; create the response
           (resp (make-instance 'response
                                :request req
                                :stream http
                                :protocol protocol
                                :code code
                                :status status
                                :headers (http-headers headers))))

      ;; return the response, but read the body first if wanted
      (prog1 resp
        (when (req-read-body req)
          (http-read-response-body resp))))))

;;; ----------------------------------------------------

(defun http-read-response-body (resp)
  "Read the response body from the stream."
  (with-slots (stream body)
      resp

    ;; get the content length and the transfer type
    (let ((length (http-header resp "Content-Length"))
          (encoding (http-header resp "Transfer-Encoding")))

      ;; read the content based on length and transfer encoding
      (setf (resp-body resp)
            (cond ((null encoding)
                   (http-read-content stream length))
                  ((string= encoding "identity")
                   (http-read-content stream length))
                  ((string= encoding "chunked")
                   (http-read-content-chunked stream))))

      ;; attempt to decode the body
      (http-decode-response-body resp))))

;;; ----------------------------------------------------

(defun http-decode-response-body (resp)
  "Lookup the content type header and decode with the correct format."
  (setf (resp-body resp) (http-decode (resp-body resp) resp)))

;;; ----------------------------------------------------

(defun http-make-response (http)
  "Read a request and initialize a new response."
  (let ((req (http-read-request http)))
    (when req
      (make-instance 'response :stream http :request req))))

;;; ----------------------------------------------------

(defun http-write-response (resp)
  "Send a response back over the wire."
  (let ((http (resp-stream resp))
        (code (resp-code resp))
        (status (resp-status resp))
        (body (resp-body resp))
        (req (resp-request resp)))

    ;; send the code and status string
    (format http "HTTP/1.1 ~d~@[ ~a~]" code status)

    ;; begin headers
    (write-char #\return http)
    (write-char #\linefeed http)

    ;; send any and all headers back
    (http-write-headers resp http)

    ;; send the body if present and not a HEAD request
    (when (and body req (string/= (req-method req) "HEAD"))
      (let ((bytes (http-encode body resp)))
        (write-sequence bytes http)))

    ;; make sure anything that has been written flushes
    (force-output http)))
