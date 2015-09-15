;;;; HTTP Interface for ClozureCL
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
  ((id    :initarg :id           :accessor session-id)
   (time  :initarg :time         :accessor session-time)
   (rng   :initarg :random-state :accessor session-random-state)

   ;; each session maintains a list of continuations
   (conts :initform nil          :accessor session-continuations))
  (:documentation "A unique ID for each client connection."))

;;; ----------------------------------------------------

(defclass http-continuation ()
  ((id    :initarg :id           :accessor continuation-id)
   (time  :initarg :time         :accessor continuation-time)
   (url   :initarg :url          :accessor continuation-url)
   (route :initarg :route        :accessor continuation-route)
   (args  :initarg :arguments    :accessor continuation-arguments))
  (:documentation "A unique ID for each continuation route."))

;;; ----------------------------------------------------

(defun http-make-session (resp config)
  "Create a new session and add the cookie to the response."
  (let ((sid (http-guid-gen resp)))

    ;; create a cookie and add it to the response
    (cookie-push (http-make-cookie "_s" sid) resp)

    ;; create the session instance from the configuration
    (let ((session-class (slot-value config 'session-class)))
      (make-instance session-class
                     :id sid
                     :time (get-universal-time)
                     :random-state (make-random-state t)))))

;;; ----------------------------------------------------

(defun http-read-session-id (req)
  "Search the Cookie headers in the request for a session id."
  (loop

     ;; search all the Cookie headers
     for cookie in (cookie-parse-all req)

     ;; is this the session id cookie?
     when (string-equal (cookie-key cookie) "_s")

     ;; lookup the session and return it
     return (cookie-value cookie)))

;;; ----------------------------------------------------

(defun http-session-timed-out-p (session timeout)
  "T if the session is too old and should be timed out."
  (> (- (get-universal-time) (session-time session)) (* timeout 60)))

;;; ----------------------------------------------------

(defun http-find-continuation (session resp)
  "Find a continuation in a given session for a request."
  (let* ((url (req-url (resp-request resp)))

         ;; lookup the continuation id in the query
         (_k (url-query-param url "_k"))

         ;; fetch all the continuations for this session
         (conts (session-continuations session)))

    ;; lookup the continuation by id
    (find _k conts :test #'string= :key #'continuation-id)))

;;; ----------------------------------------------------

(defun http-make-continuation (session resp route &rest args)
  "Create a new route continuation in a session for a given path."
  (let* ((*random-state* (session-random-state session))

         ;; generate a unique identifier for this continuation
         (id (http-luid-gen session))

         ;; get the target destination with the continuation
         (path (url-path (req-url (resp-request resp))))

         ;; create a local url path as the href
         (url (url-parse path :query `(("_k" ,id))))

         ;; create the continuation
         (cont (make-instance 'http-continuation
                              :id id
                              :url url
                              :route route
                              :arguments args
                              :time (get-universal-time))))

    ;; add the continuation, remove old continuations
    (setf (session-continuations session)
          (loop
             for c in (session-continuations session)

             ;; only keep up to 20 continuations
             for i below 20
             collect c into keep-list

             ;; make sure the new continuation is at the front
             finally (return (cons cont keep-list))))

    ;; return the url that will be used as the return link
    (continuation-url cont)))
