(defpackage :http-asd
  (:use :cl :asdf))

(in-package :http-asd)

(defsystem :http
  :name "http"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "HTTP interface for SBCL."
  :serial t
  :components ((:file "http")
               (:file "headers")
               (:file "cookie")
               (:file "content-type")
               (:file "content-encoding")
               (:file "request")
               (:file "response")
               (:file "status")
               (:file "event-stream")

               ;; server files
               (:file "server")
               (:file "router")
               (:file "session"))
  :depends-on ("sb-bsd-sockets" "sha1" "parse" "re" "tls" "url"))
