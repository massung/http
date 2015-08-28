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

(defparameter *mime-re* (compile-re "([^%s%n()<>@,;:\\\"/%[%]?.=]+)/([^%s%n()<>@,;:\\\"/%[%]?.=]+)")
  "Pattern for parsing the MIME type from a Content-Type header.")

;;; ----------------------------------------------------

(defparameter *param-re* (compile-re ";%s*([^=%s%n()<>@,;:\\\"/%[%]?.]+)=(?\"(.-)\"|([^%s%n()<>@,;:\\\"/%[%]?.=]+))")
  "Pattern for parsing the parameters from a Content-Type header.")

;;; ----------------------------------------------------

(defclass content-type ()
  ((mime-type :initarg :mime-type :accessor content-mime-type  :initform nil)
   (params    :initarg :params    :accessor content-parameters :initform nil))
  (:documentation "A parsed content mime type and parameters."))

;;; ----------------------------------------------------

(defmethod print-object ((content content-type) stream)
  "Output a content-type object to a stream."
  (print-unreadable-object (content stream :type t)
    (format stream "\"~{~a/~a~}\"" (content-mime-type content))))

;;; ----------------------------------------------------

(defun content-type-parse (s)
  "Parse a string and return the content type."
  (with-re-match (m (match-re *mime-re* s))
    (let ((params (let ((i (match-pos-end m)))
                    (loop
                      for pos = (position #\; s :start i)
                      while pos

                      ;; match the next parameter
                      for m = (match-re *param-re* s :start pos)
                      while m

                      ;; update the offset to search from
                      do (setf i (match-pos-end m))

                      ;; add the parameter to the list
                      collect (match-groups m)))))

      ;; create the content-type object
      (make-instance 'content-type
                     :params params
                     :mime-type (match-groups m)))))

;;; ----------------------------------------------------

(defun binary-content-type-p (content-type)
  "T if the content-type is binary, NIL if text. Also returns the subtype."
  (destructuring-bind (type subtype)
      (content-mime-type content-type)
    (let ((binaryp (cond ((string-equal type "image") t)
                         ((string-equal type "audio") t)
                         ((string-equal type "video") t)

                         ;; text is always not binary
                         ((string-equal type "text") nil)

                         ;; check for octet-streams
                         ((string-equal type "application")
                          (string-equal subtype "octet-stream"))

                         ;; unknown, let's side towards caution
                         (t t))))
      (values binaryp subtype))))

;;; ----------------------------------------------------

(defun read-content-type (hs)
  "Read and parse the content-type from a header object."
  (with-headers ((content-type "Content-Type"))
      hs
    (if content-type
        (content-type-parse content-type)
      (make-instance 'content-type :mime-type '("text" "plain")))))

;;; ----------------------------------------------------

(defun write-content-type (hs content-type)
  "Outputs the content-type to a header object."
  (setf (http-header hs "Content-Type")
        (with-slots (mime-type params)
            content-type
          (format nil "~{~a/~a~}~:{;~a=~s~}" mime-type params))))

;;; ----------------------------------------------------

(defun content-type-external-format (content-type)
  "Return the external format for decoding the data of a request/response."
  (with-slots (params)
      content-type
    (let ((charset (second (assoc "charset" params :test #'string-equal))))
      (cond ((null charset) :utf-8)

            ;; common character set external formats
            ((string-equal charset "utf-8") :utf-8)
            ((string-equal charset "utf-16") :utf-16)
            ((string-equal charset "utf-32") :utf-32)
            ((string-equal charset "x-mac-roman") :macos-roman)
            ((string-equal charset "euc-jp") :euc-jp)
            ((string-equal charset "iso-8859-1") :iso-8859-1)
            ((string-equal charset "iso-8859-2") :iso-8859-2)
            ((string-equal charset "iso-8859-3") :iso-8859-3)
            ((string-equal charset "iso-8859-4") :iso-8859-4)
            ((string-equal charset "iso-8859-5") :iso-8859-5)
            ((string-equal charset "iso-8859-6") :iso-8859-6)
            ((string-equal charset "iso-8859-7") :iso-8859-7)
            ((string-equal charset "iso-8859-8") :iso-8859-8)
            ((string-equal charset "iso-8859-9") :iso-8859-9)
            ((string-equal charset "iso-8859-10") :iso-8859-10)
            ((string-equal charset "iso-8859-11") :iso-8859-11)
            ((string-equal charset "iso-8859-12") :iso-8859-12)
            ((string-equal charset "iso-8859-13") :iso-8859-13)
            ((string-equal charset "iso-8859-14") :iso-8859-14)
            ((string-equal charset "iso-8859-15") :iso-8859-15)
            ((string-equal charset "iso-8859-16") :iso-8859-16)

            ;; unknown, default to utf-8
            (t :utf-8)))))
