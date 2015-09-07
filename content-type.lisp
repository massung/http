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

(defclass content-type ()
  ((type       :initarg :type       :accessor content-type-mime-type)
   (subtype    :initarg :subtype    :accessor content-type-mime-subtype)
   (format     :initarg :format     :accessor content-type-mime-format)
   (parameters :initarg :parameters :accessor content-type-parameters))
  (:documentation "MIME type and optional parameters."))

;;; ----------------------------------------------------

(defconstant +text-subtypes+
  '("atom"
    "javascript"
    "json"
    "manifest"
    "rss"
    "xml"
    "x-web-app-manifest")
  "All known MIME subtypes that are text.")

;;; ----------------------------------------------------

(define-lexer content-type-lexer (s)

  ;; first is the MIME type/subtype
  ("^([^%s%n()<>@,;:\\\"/%[%]?.=]+)/([^+%s%n()<>@,;:\\\"/%[%]?.=]+)"
   (values :mime-type (list $1 $2)))

  ;; optional format for the subtype
  ("%+([^%s%n()<>@,;:\\\"/%[%]?.=]+)"
   (values :format $1))

  ;; followed by param keys
  (";%s*([^=%s%n()<>@,;:\\\"/%[%]?.]+)"
   (values :key $1))

  ;; and values
  ("%s*=%s*(?\"(.-)\"|([^%s%n()<>@,;:\\\"/%[%]?.=]+))"
   (values :value $1)))

;;; ----------------------------------------------------

(define-parser content-type-parser
  "Parse a Content-Type header string."
  (.let* ((type (.is :mime-type))

          ;; optionally parse the subtype format
          (format (.opt nil (.is :format)))

          ;; optionally parse many parameters
          (params (.many (.let* ((key (.is :key))

                                 ;; the value is optional
                                 (value (.opt nil (.is :value))))
                           (.ret (list key value))))))

    ;; create a new content-type and return it
    (.ret (http-make-content-type (first type)
                                  (second type)
                                  :format format
                                  :parameters params))))

;;; ----------------------------------------------------

(defun content-type-parse (str)
  "Parse a string and return the content type."
  (with-lexer (lexer 'content-type-lexer str)
    (with-token-reader (next-token lexer)
      (parse 'content-type-parser next-token))))

;;; ----------------------------------------------------

(defun content-type-push (content-type headers)
  "Outputs the content-type to a header object."
  (let ((str (with-slots (type subtype format parameters)
                 content-type
               (format nil "~a/~a~@[+~a~]~:{; ~a=~s~}"
                       type
                       subtype
                       format
                       parameters))))
    (setf (http-header headers "Content-Type") str)))

;;; ----------------------------------------------------

(defun content-type-text-p (content-type)
  "T if the content-type is binary, NIL if text."
  (with-slots (mime-type mime-subtype)
      content-type
    (cond ((string-equal mime-type "text") t)

          ;; various application formats are text
          ((string-equal mime-type "application")
           (member mime-subtype +text-subtypes+ :test #'string-equal)))))

;;; ----------------------------------------------------

(defun content-type-parameter (content-type param)
  "Lookup a parameter in the content-type."
  (with-slots (parameters)
      content-type
    (second (assoc param parameters :test #'string-equal))))

;;; ----------------------------------------------------

(defsetf content-type-parameter (content-type param) (value)
  "Add or change the value of a content-type parameter."
  (let ((place (gensym))
        (p (gensym))
        (ps (gensym))
        (new-value (gensym)))
    `(with-slots ((,ps parameters))
         ,content-type
       (let* ((,p ,param)
              (,new-value ,value)

              ;; lookup the parameter
              (,place (assoc ,p ,ps :test #'string-equal)))
         (prog1 ,new-value
           (if ,place
               (setf (second ,place) (princ-to-string ,new-value))
             (push (list ,p (princ-to-string ,new-value)) ,ps)))))))

;;; ----------------------------------------------------

(defun content-type-external-format (content-type)
  "Return the external format for decoding the data of a request/response."
  (let ((charset (content-type-parameter content-type "charset")))
    (cond ((null charset) :utf-8)

          ;; common character set external formats
          ((string-equal charset "us-ascii") :us-ascii)
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
          (t :utf-8))))

;;; ----------------------------------------------------

(defun content-type-encode (content-type str)
  "Use a content type to encode a string into bytes."
  (if (not (content-type-text-p content-type))
      str  ; assumed to be binary already!
    (let ((format (content-type-external-format content-type)))
      (encode-string-to-octets str :external-format format))))

;;; ----------------------------------------------------

(defun content-type-decode (content-type bytes)
  "Use a content type to decode bytes into a string."
  (when (content-type-text-p content-type)
    (let ((format (content-type-external-format content-type)))
      (decode-string-from-octets bytes :external-format format))))
