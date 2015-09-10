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

(defun http-guid-gen (&optional resp)
  "Generate a globally unique ID from IP, time, and a random number."
  (let ((d (with-output-to-vector (v)

             ;; add the IP address of the client
             (let ((ipv4  (if (null resp)
                              #x7f000001 ; localhost
                            (remote-host (resp-stream resp)))))
               (unsigned-integer-to-binary ipv4 4 v))

             ;; add the time to the digest
             (unsigned-integer-to-binary (get-universal-time) 4 v)

             ;; add a random, 64-bit number
             (unsigned-integer-to-binary (random #.(ash 1 64)) 8 v))))
    (sha1-hex d)))

;;; ----------------------------------------------------

(defun http-luid-gen (&optional session)
  "Generate a locally unique ID from a session, time, and random number."
  (let* ((*random-state* (if (null session)
                             *random-state*
                           (session-random-state session)))

         ;; the process id is unique for each request
         (pid (process-serial-number *current-process*))

         ;; the current time
         (time (get-universal-time))

         ;; generate the unique ID
         (n (logior (dpb time (byte 32 0) 0)

                    ;; add the process id, which is unique per request
                    (dpb pid (byte 12 0) 32)

                    ;; and a 28-bit random number at upper bits
                    (dpb (random #.(ash 1 28)) (byte 30 44) 0))))

    ;; convert the number to a base-36 string
    (format nil "~36r" n)))
