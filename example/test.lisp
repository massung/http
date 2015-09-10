;;;; Example TODO Server
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :http)
  (use-package :html))

;;; -- First, define all the basic routes.

(define-http-router example-router
  (:get "/counter" 'counter))

;;; -- Evaluate this to get the server going.

;(http-start-server 'example-router)

;;; -- This is the homepage.

(defun counter (session resp &optional (n 0))
  "Show a number that can be incremented and decremented."
  (content-type-push (http-make-content-type "text" "html") resp)

  ;; create a continuation link
  (flet ((href (n)
           (http-make-continuation session resp 'counter n)))

    ;; generate the response body
    (let ((page (<html>
                 (<head>
                  (<title> "Counter Example"))
                 (<body>
                  (<h1> n)
                  (<hr>)
                  (<code>
                   (<a> :href (href (1+ n)) "++")
                   #\space
                   (<a> :href (href (1- n)) "--"))))))
      (http-ok resp (html-render page)))))
