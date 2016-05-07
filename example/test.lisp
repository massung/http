;;;; Example TODO Server
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :http)
  (use-package :html))

;;; ----------------------------------------------------

(define-http-router example-router
  (:get "/counter" 'counter))

;;; ----------------------------------------------------

;(http-start-server 'example-router :port 9000)

;;; ----------------------------------------------------

(defun counter (session resp &optional (n 0))
  "Generate the page."
  (flet ((href (n)
           (http-make-continuation session resp 'counter n)))

    ;; set the response type
    (content-type-push (http-make-content-type "text" "html") resp)

    ;; build the page and return it
    (http-ok resp
             (html-render (<html>
                           (<head> (<title> "Counter Example"))
                           (<body>
                            (<h1> n)
                            (<hr>)

                            ;; add the increment and decrement links
                            (<code> (<a> :href (href (1+ n)) "++")
                                    #\space
                                    (<a> :href (href (1- n)) "--"))))))))
