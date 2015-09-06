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
  ((headers :initarg :headers :initform nil :accessor http-headers))
  (:documentation "Base class for requests and responses."))

;;; ----------------------------------------------------

(defmacro with-headers ((&rest bindings) object &body body)
  "Extract a value from an HTTP header assoc list."
  `(symbol-macrolet
       (,@(loop
             for binding in bindings

             ;; create a binding for each header key
             collect (destructuring-bind (var key &key all)
                         binding
                       `(,var (http-header ,object ,key :all ,all)))))
     (progn ,@body)))

;;; ----------------------------------------------------

(defun http-header (headers header &key all)
  "Returns the value(s) of a request header."
  (if all
      (loop
         for (k v) in (http-headers headers)
         when (string-equal k header)
         collect v)
    (second (assoc header (http-headers headers) :test #'string-equal))))

;;; ----------------------------------------------------

(defsetf http-header (headers header) (value)
  "Add or change the value of a request header."
  (let ((place (gensym))
        (h (gensym))
        (hs (gensym))
        (new-value (gensym)))
    `(let* ((,hs ,headers)
            (,h ,header)
            (,new-value ,value)
            (,place (assoc ,h (http-headers ,hs) :test #'string-equal)))
       (prog1 ,new-value
         (if ,place
             (setf (second ,place) ,new-value)
           (push (list ,h ,new-value) (http-headers ,hs)))))))

;;; ----------------------------------------------------

(defun http-read-header (http)
  "Read a single header key/value pair from a stream."
  (let ((m (match-re #r"^([^:]+):%s*([^%n]*)" (read-line http))))
    (when m
      (match-groups m))))

;;; ----------------------------------------------------

(defun http-write-header (key value http)
  "Write a key/value pair header to a stream."
  (when (and key value)
    (format http "~a: ~a~c~c" key value #\return #\linefeed)))

;;; ----------------------------------------------------

(defun http-read-headers (http)
  "Read all the key/value pair headers from a stream."
  (loop
     for header = (http-read-header http) while header

     ;; build a list of all header (k v) pairs
     collect header into hs

     ;; return a headers instance
     finally (return (make-instance 'headers :headers hs))))

;;; ----------------------------------------------------

(defun http-write-headers (headers http)
  "Write all the key/value header pairs to a stream."
  (dolist (header (http-headers headers))
    (http-write-header (first header) (second header) http))

  ;; finish the headers with an empty line
  (write-char #\return http)
  (write-char #\linefeed http))
