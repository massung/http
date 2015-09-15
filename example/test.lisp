;;;; Example TODO Server
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :http)
  (use-package :html))

;;; ----------------------------------------------------

(define-http-router example-router
  (:get "/counter" 'counter))

;;; ----------------------------------------------------

;(http-start-server 'example-router)

;;; ----------------------------------------------------

(defun counter (session resp &optional (n 0))
  "Generate the page."

  ;; set the content-type of the response
  (content-type-push (http-make-content-type "text" "html") resp)

  (flet ((href (n)
           (http-make-continuation session resp 'counter n)))
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
