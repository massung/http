;;;; Simple HTTP/HTML Package for LispWorks
;;;;
;;;; Copyright (c) 2013 by Jeffrey Massung
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

(defstruct http-stream-event "Stream event data."
  id type data)

(defun open-http-event-stream (url event-callback &key (method "GET") headers (redirect-limit 3))
  "Create an HTTP socket to an event stream end-point and process events."
  (push '("Accept" "text/event-stream") headers)

  ;; create an initial request - don't read the body and keep the connection alive
  (let ((req (make-instance 'request :url url :method method :keep-alive t :read-body nil :headers headers)))

    ;; loop through redirects
    (do ((resp (http-perform req)
               (http-perform req)))
        ((null resp))

      ;; ensure that the socket is closed when done
      (unwind-protect
          (cond ((<= 200 (response-code resp) 299)
                 (return (process-http-event-stream resp event-callback)))
            
                ;; if the response requires a redirect, do so
                ((<= 300 (response-code resp) 399)
                 (if (minusp (decf redirect-limit))
                     (return nil)
                   (setf req (http-follow-request resp))))

                ;; anything else is an error and just return the response
                (t (return resp)))
        (close (response-stream resp))))))

(defun process-http-event-stream (resp event-callback)
  "Keep reading event data and handing it off to various handlers."
  (let ((http (response-stream resp))
        (event (make-http-stream-event)))

    ;; loop forever, reading incoming events and dispatching them
    (do ((line (read-line http nil nil)
               (read-line http nil nil)))
        ((null line))

      ;; fill in the event or dispatch if an empty line
      (cond ((zerop (length line))
             (with-slots (id type data)
                 event
               (funcall event-callback type id data))

             ;; clear the event data
             (setf event (make-http-stream-event)))

            ;; ignore lines beginning with ':'
            ((char= (char line 0) #\:))

            ;; set the type and value of the field
            (t (multiple-value-bind (key value)
                   (if-let (pivot (position #\: line :test #'char=))
                       (values (string-trim " " (subseq line 0 pivot))
                               (string-trim " " (subseq line (1+ pivot))))
                     (values line ""))

                 ;; HTTP event streams only support a few keys
                 (cond ((string= key "id")    (setf (http-stream-event-id event) value))
                       ((string= key "event") (setf (http-stream-event-type event) value))
                       ((string= key "data")  (setf (http-stream-event-data event) value))

                       ;; set the stream reconnect time
                       ((string= key "retry") (multiple-value-bind (n pos)
                                                  (parse-integer value :junk-allowed t)
                                                (when (= (length value) pos)
                                                  (setf (stream:stream-read-timeout http) n
                                                        (stream:stream-write-timeout http) n)))))))))))

