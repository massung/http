(defpackage :http-asd
  (:use :cl :asdf))

(in-package :http-asd)

(defsystem :http
  :name "http"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "HTTP client interface for LispWorks."
  :serial t
  :components ((:file "http")
               (:file "event-stream")
               (:file "server"))
  :depends-on ("lexer" "base64"))
