;;;; Simple HTTP Server Router for ClozureCL
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

(in-package :http-server)

;;; ----------------------------------------------------

(defparameter *path-spec-re* (compile-re "(?/([^/]+)?)+")
  "Path specification regular expression.")

;;; ----------------------------------------------------

(defmacro define-http-router (router &body routes)
  "Define a simple router function."
  (let ((req (gensym "req"))
        (resp (gensym "resp")))
    (labels ((try-routes (r rs)
               (if (null r)
                   `(http-not-found ,resp)
                 `(if ,(apply #'make-route req resp r)
                      t
                    ,(try-routes (first rs) (rest rs))))))
      `(defun ,router (,req ,resp)
         ,(try-routes (first routes) (rest routes))))))

;;; ----------------------------------------------------

(defun make-route (req resp method path-spec handler)
  "Create a path router that will call a handler function on match."
  (let ((okp (gensym "okp"))
        (args (gensym "args"))

        ;; the parameter list for the path-spec
        (plist (loop
                  with m = (match-re *path-spec-re* path-spec :exact t)

                  ;; each capture group is a path entry
                  for path in (match-groups m)

                  ;; path elements beginning with : are keywords
                  collect (cond ((zerop (length path))
                                 (error "Invalid path-spec ~s" path-spec))

                                ;; test for keyword argument to handler
                                ((char= (char path 0) #\:)
                                 (read-from-string path))

                                ;; otherwise just return the path
                                (t path)))))

    ;; match the method and path-spec, call the handler function
    `(when (string-equal (req-method ,req) ,method)
       (multiple-value-bind (,args ,okp)
           (path-equal ',plist (url-path (req-url ,req)))
         (when ,okp
           (apply ,handler ,req ,resp ,args))))))

;;; ----------------------------------------------------

(defun path-equal (spec path)
  "Return T if equal and a list of all arguments as a plist."
  (with-re-match (m (match-re *path-spec-re* path :exact t))
    (loop
       for spec-element = (pop spec)
       for path-element = (pop $*)

       ;; if both are null, it's a match
       when (and (null spec-element) (null path-element))
       return (values plist t)

       ;; if the spec is a keyword, then anything matches
       appending (if (keywordp spec-element)
                     (list spec-element path-element)
                   (unless (string= spec-element path-element)
                     (return-from path-equal nil)))

       ;; collect the parameter list that will be routed as keywords
       into plist)))

