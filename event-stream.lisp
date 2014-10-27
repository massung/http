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

(defstruct http-stream-event "Stream event data." id type data)

(defun process-http-event-stream (http req code headers handlers)
  "Keep reading event data and handing it off to various handlers."
  (declare (ignore req headers))
  (when (<= 200 code 299)
    (let ((event (make-http-stream-event)))
      (flet ((dispatch-event ()
               (let ((type (http-stream-event-type event)))
                 (if-let (handler (second (assoc type handlers :test #'string-equal)))
                     (funcall handler event)
                   (warn "Unhandled HTTP stream event: ~s" type)))
               (setf event (make-http-stream-event))))
        
        ;; loop forever, reading incoming events and dispatching them
        (do ((line (read-line http nil nil)
                   (read-line http nil nil)))
            ((null line))

          ;; fill in the event or dispatch if an empty line
          (cond ((zerop (length line))
                 (dispatch-event))

                ;; ignore lines beginning with ':'
                ((char= (char line 0) #\:))

                ;; set the type and value of the field
                (t (multiple-value-bind (key value)
                       (if-let (pivot (position #\: line :test #'char=))
                           (values (string-trim " " (subseq line 0 pivot))
                                   (string-trim " " (subseq line (1+ pivot))))
                         (values line ""))
                     (cond ((string= key "id")    (setf (http-stream-event-id event) value))
                           ((string= key "event") (setf (http-stream-event-type event) value))
                           ((string= key "data")  (setf (http-stream-event-data event) value))

                           ;; set the stream reconnect time
                           ((string= key "retry") (multiple-value-bind (n pos)
                                                      (parse-integer value :junk-allowed t)
                                                    (declare (ignore n))
                                                    (when (= (length value) pos)
                                                      ;; TODO:
                                                      ))))))))))))

(defun open-http-event-stream (url handlers &key (method "GET") headers (redirect-limit 3))
  "Create an HTTP socket to an event stream end-point and process events."
  (push '("Accept" "text/event-stream") headers)
  (push '("Connection" "keep-alive") headers)

  ;; open the socket and process events
  (with-url (url url)
    (let ((http (open-http-stream url)))
      (unwind-protect
          (loop :with req := (make-instance 'request
                                            :url url
                                            :method method
                                            :headers headers
                                            :callback #'(lambda (s req code headers)
                                                          (process-http-event-stream s req code headers handlers)))

                ;; issue the request on the socket, this will process the events upon success
                :for resp := (http-perform req http)

                ;; if there was a redirect, follow it, otherwise fail with the response
                :do (if (and (plusp redirect-limit) (<= 300 (response-code resp) 399))
                        (progn
                          (close http)
                        
                          ;; assign the new socket and create a new request
                          (setf req (http-follow-request resp)
                                url (request-url req)
                                http (open-http-stream url))

                          ;; decrement the redirect limit and try again
                          (decf redirect-limit))

                      ;; failed or out of redirect tries
                      (return-from open-http-event-stream resp)))

        ;; ensure that the stream is closed
        (close http)))))
