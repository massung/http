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

(defclass server-config ()
  ((process-name    :initarg :name            :initform "HTTP")
   (address         :initarg :address         :initform #(127 0 0 1))
   (port            :initarg :port            :initform 8000)
   (public-path     :initarg :public-path     :initform nil)
   (session-class   :initarg :session-class   :initform 'http-session)
   (session-timeout :initarg :session-timeout :initform 10)
   (not-found       :initarg :not-found-route :initform nil)
   (server-error    :initarg :error-route     :initform nil))
  (:documentation "Basic HTTP server configuration options."))

;;; ----------------------------------------------------

(defmethod initialize-instance :after ((config server-config) &key)
  "Make sure all the settings are valid."
  (with-slots (public-path)
      config
    (when public-path
      (setf public-path
            (let ((path (probe-file public-path)))
              (if path
                  path
                (warn "Invalid public path: ~s" public-path)))))))

;;; ----------------------------------------------------

(defvar *server-config*)
(defvar *session*)
(defvar *response*)

;;; ----------------------------------------------------

(defun http-start-server (router &key (config (make-instance 'server-config)))
  "Start a server process that will process incoming HTTP requests."
  (with-slots (address port process-name)
      config

    ;; create the server socket
    (let ((sock (make-instance 'inet-socket :type :stream :protocol :tcp)))
      (socket-bind sock address port)
      (socket-listen sock 20)

      ;; start accepting incoming connections
      (unwind-protect
           (http-server-loop sock router config)
        (socket-close sock)))))

;;; ----------------------------------------------------

(defun http-server-loop (socket router config)
  "Infinite loop, accepting new connections and routing them."
  (let ((session-map (make-hash-table :test 'equal))
        (session-lock (make-mutex)))
    (loop
       (let ((conn (make-thread #'http-accept-connection
                                :name "HTTP Server accept"
                                :arguments (list socket
                                                 router
                                                 config
                                                 session-map
                                                 session-lock))))

         ;; periodically see if should listen for new connection
         (loop
            until (join-thread conn :default nil :timeout 0)
            do (thread-yield))

         ;; remove old sessions from the map
         (http-timeout-sessions session-map session-lock config)))))

;;; ----------------------------------------------------

(defun http-accept-connection (socket router config session-map session-lock)
  "Wait forever for a new connection, then spawn a thread to handle it."
  (multiple-value-bind (connection address)
      (ignore-errors (socket-accept socket))
    (when connection
      (make-thread #'http-process-request
                   :name (format nil "Request from ~a" address)
                   :arguments (list connection
                                    router
                                    config
                                    session-map
                                    session-lock)))))

;;; ----------------------------------------------------

(defun http-timeout-sessions (session-map session-lock config)
  "Remove old sessions from the session map."
  (let ((time-limit (slot-value config 'session-timeout)))
    (flet ((timeout (sid session)
             (when (let ((*session* session))
                     (http-session-timed-out-p time-limit))
               (remhash sid session-map))))
      (with-mutex (session-lock)
        (maphash #'timeout session-map)))))

;;; ----------------------------------------------------

(defun http-process-request (socket router config session-map session-lock)
  "Process a single HTTP request and send a response."
  (let* ((http (socket-make-stream socket
                                   :output t
                                   :input t
                                   :element-type :default))

         ;; set global config and create the response object
         (*server-config* config)
         (*response* (http-make-response http)))
    (if *response*
        (let* ((req (resp-request *response*))

               ;; lookup the session from the request
               (session (let ((sid (http-read-session-id req)))
                          (when sid
                            (with-mutex (session-lock)
                              (gethash sid session-map))))))

          ;; if there's no session, make a new one
          (unless session
            (when (setf session (http-make-session))
              (with-slots (id)
                  session
                (with-mutex (session-lock)
                  (setf (gethash id session-map) session)))))

          ;; refresh the session with a new timestamp
          (when session
            (setf (session-time session) (get-universal-time)))

          ;; route the response, then send the response back
          (unwind-protect
               (let ((*session* session))
                 (http-route-response router)
                 (http-write-response *response*))

            ;; close the connection unless the server wants it alive
            (let ((connection (http-header *response* "Connection")))
              (unless (string-equal connection "keep-alive")
                (socket-shutdown socket :direction :output)))))

      ;; failed to parse the request and make a response, just quit
      (socket-close socket))))

;;; ----------------------------------------------------

(defun http-route-response (router)
  "Attempt to route the request to the appropriate handler."
  (with-slots (not-found server-error)
      *server-config*
    (handler-case

        ;; Process requests in the following order:
        ;;
        ;; 1. Check for a continuation
        ;; 2. Try the router
        ;; 3. Look for a public file
        ;; 4. Use the not-found config route
        ;; 5. Default http-not-found

        (unless (or (let ((cont (http-find-continuation)))
                      (when cont
                        (with-slots (route args)
                            cont
                          (apply route args))))

                    ;; router
                    (funcall router)

                    ;; public file
                    (http-route-public-file))

          ;; resource not found
          (if not-found
              (funcall not-found)
            (http-not-found)))

      ;; conditions trigger a server error
      (condition (c)
        (if server-error
            (funcall server-error c)
          (http-internal-server-error (princ-to-string c)))))))

;;; ----------------------------------------------------

(defun http-route-public-file ()
  "Check to see if the pathname exists in the public path."
  (with-slots (public-path)
      *server-config*
    (let ((pathname (when public-path
                      (let* ((url (req-url (resp-request *response*)))
                             (path (subseq (url-path url) 1)))
                        (probe-file (merge-pathnames path public-path))))))
      (when pathname
        (let* ((data (slurp pathname :element-type 'octet))
               (type (content-type-of-pathname pathname data)))
          (content-type-push type *response*)

          ;; return the content
          (http-ok (if (not (content-type-text-p type))
                       data
                     (content-type-decode type data))))))))
