;;;; HTTP Request Routing for LispWorks
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

(defpackage :http-router
  (:use :cl :lw :http :http-server)
  (:export
   #:define-http-route))

(in-package :http-router)

(defparameter *route-map* nil
  "All routes defined with define-http-route are added here.")

(defmacro define-http-route (name (req (&rest path) &rest match-args &key &allow-other-keys) &body body)
  "Defines a function that will match a request and execute body if successful."
  (let ((route (gensym))
        (handler (gensym))
        (okp (gensym))
        (els (gensym)))
    `(let ((,route (defun ,name (,req)
                     (flet ((,handler (,@(remove-if #'stringp path)) ,@body))
                       (multiple-value-bind (,els ,okp)
                           (match-route ,req ',path ,@match-args)
                         (when ,okp
                           (values (apply #',handler ,els) t)))))))
       (prog1
           ,route
         (unless (member ,route *route-map* :test 'eq)
           (push ,route *route-map*))))))

(define-http-route hello (req ("hello" target) :method "GET")
  (http-ok req (format nil "<h1>Hello, ~a</h1>" target)))

(defun route-request (req)
  "Given a request, match a route and execute it."
  (dolist (route *route-map* (http-not-found req))
    (multiple-value-bind (resp okp)
        (funcall route req)
      (when okp
        (return resp)))))

(defun match-route (req path &key method)
  "Attempts to match a request to various parameters."
  (when (and (or (null method) (string= (request-method req) method)))
    (loop :with path-els := (split-sequence "/" (url-path (request-url req)) :coalesce-separators t)
        
          ;; grab all the path elements
          :for route-el :in path
          :for path-el := (pop path-els)

          ;; make sure there is a path element
          :when (null path-el)
          :do (return-from match-route)
          
          ;; if the route element is a string, match it exactly
          :when (stringp route-el)
          :do (unless (string= path-el route-el)
                (return-from match-route))

          ;; if it's a symbol, then bind it
          :when (symbolp route-el)
          :collect path-el
          :into match

          ;; make sure the path was consumed completely
          :finally (unless path-els
                     (return (values match t))))))