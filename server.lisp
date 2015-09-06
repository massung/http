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

(defclass http-server-config ()
  ((port            :initarg :port            :initform 8000)
   (session-class   :initarg :session-class   :initform 'http-session)
   (session-timeout :initarg :session-timeout :initform 10)
   (not-found       :initarg :not-found-route :initform nil)
   (server-error    :initarg :error-route     :initform nil))
  (:documentation "Basic HTTP server configuration options."))

;;; ----------------------------------------------------

(defun http-simple-server (router &key config)
  "Start a server process that will process incoming HTTP requests."
  (let* ((config (if config
                     config
                   (make-instance 'http-server-config)))

         ;; create a server socket using the configuration
         (sock (make-socket :type :stream
                            :connect :passive
                            :reuse-address t
                            :local-port (slot-value config 'port))))

    ;; start the server process
    (process-run-function "HTTP"
                          'http-server-loop
                          sock
                          router
                          config)))

;;; ----------------------------------------------------

(defun http-server-loop (socket router config)
  "Infinite loop, accepting new connections and routing them."
  (let ((session-map (make-hash-table :test 'equal))
        (session-lock (make-read-write-lock)))
    (unwind-protect
         (loop
            (http-accept-request socket
                                 router
                                 config
                                 session-map
                                 session-lock)

            ;; remove old sessions from the map
            (http-timeout-sessions config session-map session-lock))

      ;; server process finished
      (close socket))))

;;; ----------------------------------------------------

(defun http-timeout-sessions (config session-map session-lock)
  "Loop over all sessions and remove ones that have timed out."
  (let ((now (get-universal-time))
        (timeout (* (slot-value config 'session-timeout) 60)))
    (flet ((timeout-session (sid session)
             (if (> (- now (http-session-time session)) timeout)
                 (remhash sid session-map)
               (http-timeout-continuations session timeout))))
      (with-write-lock (session-lock)
        (maphash #'timeout-session session-map)))))

;;; ----------------------------------------------------

(defun http-accept-request (socket router config session-map session-lock)
  "Wait for a new connection and then handle the request."
  (let ((http (accept-connection socket :wait t)))
    (process-run-function "Request"
                          'http-handle-request
                          http
                          router
                          config
                          session-map
                          session-lock)))

;;; ----------------------------------------------------

(defun http-handle-request (http router config session-map session-lock)
  "Process a single HTTP request and send a response."
  (let ((resp (http-make-response http)))
    (if resp
        (let* ((req (resp-request resp))

               ;; lookup the session from the request
               (session (let ((sid (http-read-session-id req)))
                          (when sid
                            (with-read-lock (session-lock)
                              (gethash sid session-map))))))

          ;; if there's no session, make a new one
          (unless session
            (setf session (http-make-session resp config))

            ;; add the session to the session map
            (with-write-lock (session-lock)
              (setf (gethash (http-session-id session) session-map) session)))

          ;; refresh the session with a new timestamp
          (setf (http-session-time session) (get-universal-time))

          ;; route the response, then send the response back
          (unwind-protect
               (let ((cont (http-find-continuation session resp)))
                 (if cont
                     (funcall cont session resp)
                   (http-route-response resp session router config))
                 (http-write-response resp))

            ;; close the connection unless the server wants it alive
            (let ((connection (http-header resp "Connection")))
              (unless (string-equal connection "keep-alive")
                (shutdown http :direction :output)))))

      ;; failed to parse the request and make a response, just quit
      (close http))))

;;; ----------------------------------------------------

(defun http-route-response (resp session router config)
  "Attempt to route the request to the appropriate handler."
  (with-slots (not-found server-error)
      config
    (handler-case
        (unless (funcall router session resp)
          (if not-found
              (funcall not-found session resp)
            (http-not-found resp)))

      ;; conditions trigger a server error
      (condition (c)
        (if server-error
            (funcall server-error session resp)
          (http-internal-server-error resp c))))))

;;; ----------------------------------------------------

(defmacro define-http-response (reply code (&key status headers))
  "Create an HTTP response generation."
  (let ((resp (gensym "resp"))
        (body (gensym "body"))

        ;; create an argument for each required header
        (args (mapcar #'gensym headers)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,reply (,resp ,@args &optional ,body)
         (setf (resp-code ,resp) ,code
               (resp-status ,resp) ,status

               ;; set the headers from the arguments passed in
               ,@(loop for a in args and h in headers appending
                      `((http-header ,resp ,h) ,a))

               ;; set the body if it was provided
               (resp-body ,resp) ,body))
       (export ',reply :http))))

;;; ----------------------------------------------------

(define-http-response http-continue 100
  (:status "Continue"))

;;; ----------------------------------------------------

(define-http-response http-switching-protocols 101
  (:status "Switching Protocols"))

;;; ----------------------------------------------------

(define-http-response http-ok 200
  (:status "OK"))

;;; ----------------------------------------------------

(define-http-response http-created 201
  (:status "Created"))

;;; ----------------------------------------------------

(define-http-response http-accepted 202
  (:status "Accepted"))

;;; ----------------------------------------------------

(define-http-response http-non-authoritative 203
  (:status "Non-Authoritative Information"))

;;; ----------------------------------------------------

(define-http-response http-no-content 204
  (:status "No Content"))

;;; ----------------------------------------------------

(define-http-response http-reset-content 205
  (:status "Reset Content"))

;;; ----------------------------------------------------

(define-http-response http-partial-content 206
  (:status "Partial Content"))

;;; ----------------------------------------------------

(define-http-response http-moved-permanently 301
  (:status "Moved Permanently" :headers ("Location")))

;;; ----------------------------------------------------

(define-http-response http-found 302
  (:status "Found" :headers ("Location")))

;;; ----------------------------------------------------

(define-http-response http-see-other 303
  (:status "See Other" :headers ("Location")))

;;; ----------------------------------------------------

(define-http-response http-not-modified 304
  (:status "Not Modified"))

;;; ----------------------------------------------------

(define-http-response http-temporary-redirect 307
  (:status "Temporary Redirect" :headers ("Location")))

;;; ----------------------------------------------------

(define-http-response http-bad-request 400
  (:status "Bad Request"))

;;; ----------------------------------------------------

(define-http-response http-unauthorized 401
  (:status "Unauthorized"))

;;; ----------------------------------------------------

(define-http-response http-forbidden 403
  (:status "Forbidden"))

;;; ----------------------------------------------------

(define-http-response http-not-found 404
  (:status "Not Found"))

;;; ----------------------------------------------------

(define-http-response http-method-not-allowed 405
  (:status "Method Not Allowed"))

;;; ----------------------------------------------------

(define-http-response http-not-acceptable 406
  (:status "Not Acceptable"))

;;; ----------------------------------------------------

(define-http-response http-proxy-required 407
  (:status "Proxy Required"))

;;; ----------------------------------------------------

(define-http-response http-request-timeout 408
  (:status "Request Timeout"))

;;; ----------------------------------------------------

(define-http-response http-conflict 409
  (:status "Conflict"))

;;; ----------------------------------------------------

(define-http-response http-gone 410
  (:status "Gone"))

;;; ----------------------------------------------------

(define-http-response http-length-required 411
  (:status "Length Required"))

;;; ----------------------------------------------------

(define-http-response http-precondition-failed 412
  (:status "Precondition Failed"))

;;; ----------------------------------------------------

(define-http-response http-request-too-large 413
  (:status "Request Entity Too Large"))

;;; ----------------------------------------------------

(define-http-response http-request-uri-too-long 414
  (:status "Request URI Too Long"))

;;; ----------------------------------------------------

(define-http-response http-unsupported-media-type 415
  (:status "Unsupported Media Type"))

;;; ----------------------------------------------------

(define-http-response http-range-not-satisfiable 416
  (:status "Request Range Not Satisfiable"))

;;; ----------------------------------------------------

(define-http-response http-expectation-failed 417
  (:status "Expectation Failed"))

;;; ----------------------------------------------------

(define-http-response http-internal-server-error 500
  (:status "Internal Server Error"))

;;; ----------------------------------------------------

(define-http-response http-not-implemented 501
  (:status "Not Implemented"))

;;; ----------------------------------------------------

(define-http-response http-bad-gateway 502
  (:status "Bad Gateway"))

;;; ----------------------------------------------------

(define-http-response http-service-unavailable 503
  (:status "Service Unavailable"))

;;; ----------------------------------------------------

(define-http-response http-gateway-timeout 504
  (:status "Gateway Timeout"))

;;; ----------------------------------------------------

(define-http-response http-version-not-supported 505
  (:status "HTTP Version Not Supported"))
