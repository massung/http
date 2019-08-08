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

(defvar *http-content-encodings*
  '(("identity" :encoder #'identity :decoder #'identity))
  "Registered content encoding/decoding functions.")

;;; ----------------------------------------------------

(defun http-registered-content-encoding (name)
  "Find a content encoding with a given name."
  (rest (assoc name *http-content-encodings* :test #'string-equal)))

;;; ----------------------------------------------------

(defun http-registered-content-encodings ()
  "Return a comma-separated list of all registered encodings."
  (format nil "~{~a~^ ~}" (mapcar #'first *http-content-encodings*)))

;;; ----------------------------------------------------

(defun http-registered-content-encoder (name)
  "Find the content encoder for a given encoding."
  (or (getf (http-registered-content-encoding name) :encoder)

      ;; unknown content encoding, just use identity
      (prog1 #'identity
        (warn "Unrecognized Content-Encoding ~s" name))))

;;; ----------------------------------------------------

(defun http-registered-content-decoder (name)
  "Find the content decoder for a given encoding."
  (or (getf (http-registered-content-encoding name) :decoder)

      ;; unknown content encoding, just use identity
      (prog1 #'identity
        (warn "Unrecognized Content-Encoding ~s" name))))

;;; ----------------------------------------------------

(defun http-register-content-encoding (name &rest fns &key encoder decoder)
  "Replace or add a new content-encoding."
  (let ((encoding (http-registered-content-encoding name)))
    (if encoding
        (rplacd encoding fns)
      (let ((encoding (list name :encoder encoder :decoder decoder)))
        (push encoding *http-content-encodings*)))))

;;; ----------------------------------------------------

(defun http-encode-body (body encoding content-type)
  "Encode a response or request body."
  (let ((encoder (if (null encoding)
                     #'identity
                   (http-registered-content-encoder encoding)))

        ;; first, encode the body into octets
        (bytes (content-type-encode content-type body)))

    ;; encode the octets appropriately
    (funcall encoder bytes)))

;;; ----------------------------------------------------

(defun http-decode-body (body encoding content-type)
  "Decode a response or request body."
  (let* ((decoder (if (null encoding)
                      #'identity
                    (http-registered-content-decoder encoding)))

         ;; first, decode the body's bytes (e.g. gunzip)
         (bytes (funcall decoder body)))

    ;; next, use the content-type to decode octets to a string
    (content-type-decode content-type bytes)))

;;; ----------------------------------------------------

(defun http-encode (body headers)
  "Use headers in a response/request to encode the body."
  (when body
    (let* ((header (http-header headers "Content-Type"))
           (encoding (http-header headers "Content-Encoding"))

           ;; parse the content type header, or use the HTTP default
           (content-type (if (null header)
                             *application/octet-stream*
                           (content-type-parse header))))

      ;; encode the body into octets
      (http-encode-body body encoding content-type))))

;;; ----------------------------------------------------

(defun http-decode (body headers)
  "Use headers in a response/request to decode the body."
  (when body
    (let* ((header (http-header headers "Content-Type"))
           (encoding (http-header headers "Content-Encoding"))

           ;; parse the content type header, or use the HTTP default
           (content-type (if (null header)
                             *application/octet-stream*
                           (content-type-parse header))))

      ;; decode the body
      (http-decode-body body encoding content-type))))
