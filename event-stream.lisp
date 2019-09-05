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

(defun http-open-event-stream (req event-callback &key (redirect-limit 3))
  "Create an HTTP socket to an event stream end-point and process events."
  (setf (req-keep-alive req) t
        (req-read-body req) nil

        ;; ensure that the accept header is accepting the proper type
        (http-header req "Accept") "text/event-stream")

  ;; loop through redirects
  (do ((resp (http-perform req)
             (http-perform req)))
      ((null resp))

    ;; ensure that the socket is closed when done
    (unwind-protect
        (cond ((<= 200 (resp-code resp) 299)
               (return (http-process-event-stream resp event-callback)))

              ;; if the response requires a redirect, do so
              ((<= 300 (resp-code resp) 399)
               (if (minusp (decf redirect-limit))
                   (return nil)
                 (setf req (http-follow-request resp))))

              ;; anything else is an error and just return the response
              (t (return resp)))
      (close (resp-stream resp)))))

;;; ----------------------------------------------------

(defun http-process-event-stream (resp event-callback)
  "Keep reading event data and handing it off to various handlers."
  (let ((http (resp-stream resp))

        ;; create a stream for aggregating the data
        (data (make-string-output-stream))

        ;; the event type and id
        (event nil)
        (last-id nil))

    ;; loop forever, reading incoming events and dispatching them
    (do ((line (read-line http nil nil)
               (read-line http nil nil)))
        ((null line))

      ;; fill in the event or dispatch if an empty line
      (cond ((zerop (length line))
             (when event
               (funcall event-callback
                        event
                        last-id
                        (get-output-stream-string data)))

             ;; clear the event and data
             (setf event nil data (make-string-output-stream)))

            ;; ignore lines beginning with ':'
            ((char= (char line 0) #\:))

            ;; set the type and value of the field
            (t (multiple-value-bind (key value)
                   (let ((pivot (position #\: line)))
                     (if pivot
                         (values (string-trim " " (subseq line 0 pivot))
                                 (string-trim " " (subseq line (1+ pivot))))
                       (values line "")))

                 ;; HTTP event streams only support a few keys
                 (cond ((string= key "id")    (setf last-id value))
                       ((string= key "event") (setf event value))

                       ;; the data field collects values with newlines
                       ((string= key "data")  (format data "~a~%" value))

                       ;; set the stream reconnect time (TODO)
                       ((string= key "retry")))))))))
