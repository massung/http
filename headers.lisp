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

(defclass headers ()
  ((headers :initarg :headers :accessor http-headers :initform nil))
  (:documentation "Base class for requests and responses."))

;;; ----------------------------------------------------

(defparameter *header-re* (compile-re "^([^:]+):%s*([^%n]*)")
  "Pattern for parsing a response header line.")

;;; ----------------------------------------------------

(defmacro with-headers ((&rest bindings) object &body body)
  "Extract a value from an HTTP header assoc list."
  `(symbol-macrolet
       (,@(loop
             for binding in bindings
             collect (destructuring-bind (var key &key all)
                         binding
                       `(,var (http-header ,object ,key :all ,all)))))
     (progn ,@body)))

;;; ----------------------------------------------------

(defun http-header (hs header &key all)
  "Returns the value of a request header."
  (if all
      (mapcar #'second (remove header
                               (http-headers hs)
                               :test-not #'string-equal
                               :key #'first))
    (second (assoc header (http-headers hs) :test #'string-equal))))

;;; ----------------------------------------------------

(defsetf http-header (headers header) (value)
  "Add or change the value of a request header."
  (let ((place (gensym))
        (h (gensym))
        (hs (gensym))
        (new-value (gensym)))
    `(let* ((,hs ,headers)
            (,h ,header)
            (,new-value (princ-to-string ,value))
            (,place (assoc ,h (http-headers ,hs) :test #'string-equal)))
       (prog1 ,new-value
         (if ,place
             (setf (second ,place) ,new-value)
           (push (list ,h ,new-value) (http-headers ,hs)))))))

;;; ----------------------------------------------------

(defun read-http-headers (http)
  "Parse a header line. Return the key and value."
  (flet ((read-header ()
           (with-re-match (match (match-re *header-re* (read-line http)))
             (list $1 $2))))
    (loop for header = (read-header) while header collect header)))

;;; ----------------------------------------------------

(defun write-http-header (http &optional key value)
  "Optionally send a header k/v pair to the request stream."
  (when key
    (format http "~a: ~@[~a~]" key value))

  ;; end the line
  (write-char #\return http)
  (write-char #\linefeed http))

;;; ----------------------------------------------------

(defun write-http-headers (http headers)
  "Send all the headers to an HTTP stream."
  (write-http-header http)

  ;; write all the headers
  (dolist (header headers)
    (apply #'write-http-header http header))

  ;; finish the headers with an empty line
  (write-http-header http))
