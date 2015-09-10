;;;; Simple HTTP Server for ClozureCL
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

(defclass cookie ()
  ((key   :initform nil :initarg :key        :accessor cookie-key)
   (value :initform nil :initarg :value      :accessor cookie-value)
   (atts  :initform nil :initarg :attributes :accessor cookie-attributes))
  (:documentation "A parsed - or set - HTTP cookie."))

;;; ----------------------------------------------------

(defgeneric cookie-header (headers)
  (:documentation "Cookie for request and Set-Cookie for response."))

;;; ----------------------------------------------------

(defun cookie-attribute (cookie att)
  "Lookup an attribute in the cookie."
  (with-slots (atts)
      cookie
    (second (assoc att atts :test #'string-equal))))

;;; ----------------------------------------------------

(defun cookie-attribute-set (cookie att value)
  "Add a new query parameter or update an existing one."

  (prog1 value
    (with-slots (atts)
        cookie
      (let ((q (assoc att atts :test #'string-equal)))
        (if q
            (rplacd q (list value))
          (push (list att value) atts))))))

;;; ----------------------------------------------------

(defsetf cookie-attribute (cookie att) (value)
  "Add or change the value of a content-type parameter."
  `(http::cookie-attribute-set ,cookie ,att ,value))

;;; ----------------------------------------------------

(define-lexer cookie-lexer (s)

  ;; first is the cookie's key
  ("^%s*([^%s=]+)" (values :key $$))

  ;; cookie values are optional
  ("%s*=%s*(?\"(.-)\"|([^;]+))" (values :value $1))

  ;; all other names are a cookie's attributes
  ("[;]%s*([^%s;=]+)" (values :att $1)))

;;; ----------------------------------------------------

(define-parser cookie-parser
  "Parse a Set-Cookie response header."
  (.let* ((key (.is :key))

          ;; values are optional
          (value (.opt nil (.is :value)))

          ;; attributes are optional
          (atts (.many (.let* ((att (.is :att))

                               ;; attribute values are optional
                               (value (.opt nil (.is :value))))
                         (.ret (list att value))))))

    ;; create a cookie with a key, value, and attributes
    (.ret (http-make-cookie key value :attributes atts))))

;;; ----------------------------------------------------

(defun cookie-parse (str)
  "Parse a Set-Cookie response header."
  (with-lexer (lexer 'cookie-lexer str)
    (with-token-reader (next-token lexer)
      (parse 'cookie-parser next-token))))

;;; ----------------------------------------------------

(defun cookie-parse-all (headers)
  "Find all the cookies and parse them all."
  (let ((h (cookie-header headers)))
    (mapcar #'cookie-parse (http-header headers h :all t))))

;;; ----------------------------------------------------

(defun cookie-push (cookie headers)
  "Add a header to the headers of a request or response."
  (with-slots (key value atts)
      cookie
    (let ((str (format nil "~a=~s~:{; ~a=~s~}" key value atts)))
      (push (list (cookie-header headers) str) (http-headers headers)))))
