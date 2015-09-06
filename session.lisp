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

(defclass http-session ()
  ((id    :initarg :id          :accessor http-session-id)
   (time  :initarg :time        :accessor http-session-time)

   ;; each session maintains a list of continuations
   (conts :initform ()          :accessor http-session-continuations))
  (:documentation "A unique ID for each client connection."))

;;; ----------------------------------------------------

(defun http-make-session (resp config)
  "Create a new session and add the cookie to the response."
  (let* ((sid (http-uuid-gen resp))

         ;; create a cookie for the session
         (cookie (make-instance 'cookie :key "_s" :value sid)))

    ;; create a cookie and add it to the response
    (cookie-push cookie resp "Set-Cookie")

    ;; create the session instance from the configuration
    (let ((session-class (slot-value config 'session-class)))
      (make-instance session-class :id sid :time (get-universal-time)))))

;;; ----------------------------------------------------

(defun http-read-session-id (req)
  "Search the Cookie headers in the request for a session id."
  (loop

     ;; search all the Cookie headers
     for h in (http-header req "Cookie" :all t)

     ;; parse the header
     for cookie = (cookie-parse h)

     ;; is this the session id cookie?
     when (and cookie (string-equal (cookie-key cookie) "_s"))

     ;; lookup the session and return it
     return (cookie-value cookie)))

;;; ----------------------------------------------------

(defun http-find-continuation (session resp)
  "Find a continuation in a given session for a request."
  (let* ((url (req-url (resp-request resp)))

         ;; continuations are broken into path and query
         (path (url-path url))
         (query (url-query url))

         ;; lookup the continuation id in the query
         (_k (second (assoc "_k" query :test #'string=))))

    ;; if there's a continuation, look it up
    (when _k
      (let ((conts (http-session-continuations session)))
        (third (assoc (list path _k) conts :test 'equal))))))

;;; ----------------------------------------------------

(defun http-make-continuation (session resp path route)
  "Create a new route continuation in a session for a given path."
  (let ((_k (http-uuid-gen resp)))
    (prog1 (format nil "~a?_k=~a" path _k)
      (push (list (list path _k) (get-universal-time) route)
            (http-session-continuations session)))))

;;; ----------------------------------------------------

(defun http-timeout-continuations (session timeout)
  "Loop over the continuations and remove any that are too old."
  (let ((now (get-universal-time)))
    (flet ((old-p (time)
             (> (- now time) timeout)))
      (with-slots (conts)
          session
        (setf conts (remove-if #'old-p conts :key #'second))))))

;;; ----------------------------------------------------

(defun http-uuid-gen (resp)
  "Generate a unique ID from local IP, time, and a random number."
  (let ((ip4 (remote-host (resp-stream resp)))
        (time (get-universal-time))
        (rnd (random #.(1- (ash 1 64))))

        ;; this is where all the bytes will go
        (digest (make-array 16 :fill-pointer 0)))

    ;; insert the local ip into the digest
    (dotimes (i 4) (vector-push (ldb (byte 8 (* i 8)) ip4) digest))

    ;; insert the universal time into the digest
    (dotimes (i 4) (vector-push (ldb (byte 8 (* i 8)) time) digest))

    ;; insert the random number into the digest
    (dotimes (i 8) (vector-push (ldb (byte 8 (* i 8)) rnd) digest))

    ;; return a hashed version of the digest
    (sha1-hex digest)))
