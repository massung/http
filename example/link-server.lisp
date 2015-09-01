;;;; A simple Reddit-like clone using http-simple-server.
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :html)
  (use-package :http)
  (use-package :http-server))

(defparameter *link-id* 0
  "Unique identifier for a link.")

(defparameter *links* nil
  "All the submitted links.")

(defclass link ()
  ((url   :accessor link-url   :initform nil :initarg :url)
   (title :accessor link-title :initform nil :initarg :title)
   (votes :accessor link-votes :initform 0)
   (id    :accessor link-id    :initform (incf *link-id*))))

(defmethod initialize-instance :after ((link link) &key &allow-other-keys)
  ""
  (future:with-promise (resp (http-get (link-url link)))

    ;; if a title wasn't set, then set it now
    (unless (link-title link)
      (let* ((body (decode-response-body resp))

             ;; look for the <title> tag in the body
             (title (search "<title>" body :test #'string-equal)))
        (when title
          (re:with-re-match (m (re:find-re ">([^<]+)<" body :start title))
            (setf (link-title link) $1)))))

    ;; set the url to the full url of the final end-point
    (setf (link-url link) (req-url (resp-request resp)))

    ;; finally, add the link to the list of all links
    (push link *links*)))

(define-http-router link-app
  (:get "/" 'homepage))

(defun start-server ()
  "Start the link server running."
  (http-simple-server 'link-app :port 3000))

(define-http-page homepage (req)
  (:title "Reddit Clone")
  (:metadata (("charset" "utf-8")))
  (:stylesheets ("./css/dark-theme.css"))
  (:scripts ("./js/jquery.js"))
  (:body "{{links}}" (:links render-links)))

(defun render-links (req)
  "Render all the links."
  (declare (ignore req))
  (html nil '(:div () "Hello, world!")))

#{
<html foo='bar'>

</html>
}#
