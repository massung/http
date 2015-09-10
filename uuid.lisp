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

(defun http-uuid-gen (&optional resp)
  "Generate a unique ID from local IP, time, and a random number."
  (let ((ip4 (if (null resp)
                 #x7f000001 ; localhost
               (remote-host (resp-stream resp))))

        ;; get the current time and a random number
        (time (get-universal-time))
        (rnd (random (ash 1 64)))

        ;; this is where all the bytes will go
        (digest (make-array 16 :element-type 'http::octet :fill-pointer 0)))

    ;; insert the local ip into the digest
    (dotimes (i 4) (vector-push (ldb (byte 8 (* i 8)) ip4) digest))

    ;; insert the universal time into the digest
    (dotimes (i 4) (vector-push (ldb (byte 8 (* i 8)) time) digest))

    ;; insert the random number into the digest
    (dotimes (i 8) (vector-push (ldb (byte 8 (* i 8)) rnd) digest))

    ;; return a hashed version of the digest
    (sha1-hex digest)))
