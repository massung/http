;;;; HTTP interface for SBCL
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

(defclass content-type ()
  ((type       :initarg :type       :accessor content-type-mime-type)
   (subtype    :initarg :subtype    :accessor content-type-mime-subtype)
   (format     :initarg :format     :accessor content-type-mime-format)
   (parameters :initarg :parameters :accessor content-type-parameters))
  (:documentation "MIME type and optional parameters."))

;;; ----------------------------------------------------

(defmethod print-object ((content-type content-type) stream)
  "Output a content type to a stream."
  (print-unreadable-object (content-type stream :type t)
    (with-slots (type subtype format)
        content-type
      (format stream "~a/~a~@[+~a~]" type subtype format))))

;;; ----------------------------------------------------

(defparameter *application/octet-stream*
  (http-make-content-type "application" "octet-stream"))

;;; ----------------------------------------------------

(defparameter *content-types*
  `(("acx"     "application/internet-property-stream")
    ("ai"      "application/postscript")
    ("aif"     "audio/x-aiff")
    ("aifc"    "audio/x-aiff")
    ("aiff"    "audio/x-aiff")
    ("asf"     "video/x-ms-asf")
    ("asr"     "video/x-ms-asf")
    ("asx"     "video/x-ms-asf")
    ("au"      "audio/basic")
    ("avi"     "video/x-msvideo")
    ("axs"     "application/olescript")
    ("bas"     "text/plain")
    ("bcpio"   "application/x-bcpio")
    ("bin"     "application/octet-stream")
    ("bmp"     "image/bmp")
    ("c"       "text/plain")
    ("cat"     "application/vnd.ms-pkiseccat")
    ("cdf"     "application/x-cdf")
    ("cdf"     "application/x-netcdf")
    ("cer"     "application/x-x509-ca-cert")
    ("class"   "application/octet-stream")
    ("clp"     "application/x-msclip")
    ("cmx"     "image/x-cmx")
    ("cod"     "image/cis-cod")
    ("cpio"    "application/x-cpio")
    ("crd"     "application/x-mscardfile")
    ("crl"     "application/pkix-crl")
    ("crt"     "application/x-x509-ca-cert")
    ("csh"     "application/x-csh")
    ("css"     "text/css")
    ("dcr"     "application/x-director")
    ("der"     "application/x-x509-ca-cert")
    ("dir"     "application/x-director")
    ("dll"     "application/x-msdownload")
    ("dms"     "application/octet-stream")
    ("doc"     "application/msword")
    ("dot"     "application/msword")
    ("dvi"     "application/x-dvi")
    ("dxr"     "application/x-director")
    ("eps"     "application/postscript")
    ("etx"     "text/x-setext")
    ("evy"     "application/envoy")
    ("exe"     "application/octet-stream")
    ("fif"     "application/fractals")
    ("flr"     "x-world/x-vrml")
    ("gif"     "image/gif")
    ("gtar"    "application/x-gtar")
    ("gz"      "application/x-gzip")
    ("h"       "text/plain")
    ("hdf"     "application/x-hdf")
    ("hlp"     "application/winhlp")
    ("hqx"     "application/mac-binhex40")
    ("hta"     "application/hta")
    ("htc"     "text/x-component")
    ("htm"     "text/html")
    ("html"    "text/html")
    ("htt"     "text/webviewhtml")
    ("ico"     "image/x-icon")
    ("ief"     "image/ief")
    ("iii"     "application/x-iphone")
    ("ins"     "application/x-internet-signup")
    ("isp"     "application/x-internet-signup")
    ("jfif"    "image/pipeg")
    ("jpe"     "image/jpeg")
    ("jpeg"    "image/jpeg")
    ("jpg"     "image/jpeg")
    ("js"      "application/x-javascript")
    ("latex"   "application/x-latex")
    ("lha"     "application/octet-stream")
    ("lsf"     "video/x-la-asf")
    ("lsx"     "video/x-la-asf")
    ("lzh"     "application/octet-stream")
    ("m13"     "application/x-msmediaview")
    ("m14"     "application/x-msmediaview")
    ("m3u"     "audio/x-mpegurl")
    ("man"     "application/x-troff-man")
    ("mdb"     "application/x-msaccess")
    ("me"      "application/x-troff-me")
    ("mht"     "message/rfc822")
    ("mhtml"   "message/rfc822")
    ("mid"     "audio/mid")
    ("mny"     "application/x-msmoney")
    ("mov"     "video/quicktime")
    ("movie"   "video/x-sgi-movie")
    ("mp2"     "video/mpeg")
    ("mp3"     "audio/mpeg")
    ("mpa"     "video/mpeg")
    ("mpe"     "video/mpeg")
    ("mpeg"    "video/mpeg")
    ("mpg"     "video/mpeg")
    ("mpp"     "application/vnd.ms-project")
    ("mpv2"    "video/mpeg")
    ("ms"      "application/x-troff-ms")
    ("msg"     "application/vnd.ms-outlook")
    ("mvb"     "application/x-msmediaview")
    ("nc"      "application/x-netcdf")
    ("nws"     "message/rfc822")
    ("oda"     "application/oda")
    ("p10"     "application/pkcs10")
    ("p12"     "application/x-pkcs12")
    ("p7b"     "application/x-pkcs7-certificates")
    ("p7c"     "application/x-pkcs7-mime")
    ("p7m"     "application/x-pkcs7-mime")
    ("p7r"     "application/x-pkcs7-certreqresp")
    ("p7s"     "application/x-pkcs7-signature")
    ("pbm"     "image/x-portable-bitmap")
    ("pdf"     "application/pdf")
    ("pfx"     "application/x-pkcs12")
    ("pgm"     "image/x-portable-graymap")
    ("pko"     "application/ynd.ms-pkipko")
    ("pma"     "application/x-perfmon")
    ("pmc"     "application/x-perfmon")
    ("pml"     "application/x-perfmon")
    ("pmr"     "application/x-perfmon")
    ("pmw"     "application/x-perfmon")
    ("pnm"     "image/x-portable-anymap")
    ("pot"     "application/vnd.ms-powerpoint")
    ("ppm"     "image/x-portable-pixmap")
    ("pps"     "application/vnd.ms-powerpoint")
    ("ppt"     "application/vnd.ms-powerpoint")
    ("prf"     "application/pics-rules")
    ("ps"      "application/postscript")
    ("pub"     "application/x-mspublisher")
    ("qt"      "video/quicktime")
    ("ra"      "audio/x-pn-realaudio")
    ("ram"     "audio/x-pn-realaudio")
    ("ras"     "image/x-cmu-raster")
    ("rgb"     "image/x-rgb")
    ("rmi"     "audio/mid")
    ("roff"    "application/x-troff")
    ("rtf"     "application/rtf")
    ("rtx"     "text/richtext")
    ("scd"     "application/x-msschedule")
    ("sct"     "text/scriptlet")
    ("setpay"  "application/set-payment-initiation")
    ("setreg"  "application/set-registration-initiation")
    ("sh"      "application/x-sh")
    ("shar"    "application/x-shar")
    ("sit"     "application/x-stuffit")
    ("snd"     "audio/basic")
    ("spc"     "application/x-pkcs7-certificates")
    ("spl"     "application/futuresplash")
    ("src"     "application/x-wais-source")
    ("sst"     "application/vnd.ms-pkicertstore")
    ("stl"     "application/vnd.ms-pkistl")
    ("stm"     "text/html")
    ("sv4cpio" "application/x-sv4cpio")
    ("sv4crc"  "application/x-sv4crc")
    ("svg"     "image/svg+xml")
    ("swf"     "application/x-shockwave-flash")
    ("t"       "application/x-troff")
    ("tar"     "application/x-tar")
    ("tcl"     "application/x-tcl")
    ("tex"     "application/x-tex")
    ("texi"    "application/x-texinfo")
    ("texinfo" "application/x-texinfo")
    ("tgz"     "application/x-compressed")
    ("tif"     "image/tiff")
    ("tiff"    "image/tiff")
    ("tr"      "application/x-troff")
    ("trm"     "application/x-msterminal")
    ("tsv"     "text/tab-separated-values")
    ("txt"     "text/plain")
    ("uls"     "text/iuls")
    ("ustar"   "application/x-ustar")
    ("vcf"     "text/x-vcard")
    ("vrml"    "x-world/x-vrml")
    ("wav"     "audio/x-wav")
    ("wcm"     "application/vnd.ms-works")
    ("wdb"     "application/vnd.ms-works")
    ("wks"     "application/vnd.ms-works")
    ("wmf"     "application/x-msmetafile")
    ("wps"     "application/vnd.ms-works")
    ("wri"     "application/x-mswrite")
    ("wrl"     "x-world/x-vrml")
    ("wrz"     "x-world/x-vrml")
    ("xaf"     "x-world/x-vrml")
    ("xbm"     "image/x-xbitmap")
    ("xla"     "application/vnd.ms-excel")
    ("xlc"     "application/vnd.ms-excel")
    ("xlm"     "application/vnd.ms-excel")
    ("xls"     "application/vnd.ms-excel")
    ("xlt"     "application/vnd.ms-excel")
    ("xlw"     "application/vnd.ms-excel")
    ("xof"     "x-world/x-vrml")
    ("xpm"     "image/x-xpixmap")
    ("xwd"     "image/x-xwindowdump")
    ("z"       "application/x-compress")
    ("zip"     "application/zip")))

;;; ----------------------------------------------------

(defparameter *text-mime-subtypes*
  '("atom"
    "javascript"
    "json"
    "manifest"
    "rss"
    "xml"
    "x-web-app-manifest")
  "All known MIME subtypes that are text.")

;;; ----------------------------------------------------

(define-lexer content-type-lexer (s)

  ;; first is the MIME type/subtype
  ("^([^%s%n()<>@,;:\\\"/%[%]?.=]+)/([^+%s%n()<>@,;:\\\"/%[%]?=]+)"
   (values :mime-type (list $1 $2)))

  ;; optional format for the subtype
  ("%+([^%s%n()<>@,;:\\\"/%[%]?.=]+)"
   (values :format $1))

  ;; followed by param keys
  (";%s*([^=%s%n()<>@,;:\\\"/%[%]?.]+)"
   (values :key $1))

  ;; and values
  ("%s*=%s*(?\"(.-)\"|([^%s%n()<>@,;:\\\"/%[%]?.=]+))"
   (values :value $1)))

;;; ----------------------------------------------------

(define-parser content-type-parser
  "Parse a Content-Type header string."
  (.let* ((type (.is :mime-type))

          ;; optionally parse the subtype format
          (format (.opt nil (.is :format)))

          ;; optionally parse many parameters
          (params (.many (.let* ((key (.is :key))

                                 ;; the value is optional
                                 (value (.opt nil (.is :value))))
                           (.ret (list key value))))))

    ;; create a new content-type and return it
    (.ret (http-make-content-type (first type)
                                  (second type)
                                  :format format
                                  :parameters params))))

;;; ----------------------------------------------------

(defun content-type-parse (str)
  "Parse a string and return the content type."
  (with-lexer (lexer 'content-type-lexer str)
    (with-token-reader (next-token lexer)
      (parse 'content-type-parser next-token))))

;;; ----------------------------------------------------

(defun content-type-push (content-type headers)
  "Outputs the content-type to a header object."
  (let ((str (with-slots (type subtype format parameters)
                 content-type
               (format nil "~a/~a~@[+~a~]~:{; ~a=~s~}"
                       type
                       subtype
                       format
                       parameters))))
    (setf (http-header headers "Content-Type") str)))

;;; ----------------------------------------------------

(defun content-type-text-p (content-type)
  "T if the content-type is binary, NIL if text."
  (with-slots (type subtype)
      content-type
    (cond ((string-equal type "text") t)

          ;; various application formats are text
          ((string-equal type "application")
           (member subtype *text-mime-subtypes* :test #'string-equal)))))

;;; ----------------------------------------------------

(defun content-type-parameter (content-type param)
  "Lookup a parameter in the content-type."
  (with-slots (parameters)
      content-type
    (second (assoc param parameters :test #'string-equal))))

;;; ----------------------------------------------------

(defun content-type-parameter-set (content-type param value)
  "Set the value of a parameter in the content-type."
  (prog1 value
    (with-slots (parameters)
        content-type
      (let ((q (assoc param parameters :test #'string-equal)))
        (if q
            (rplacd q (list value))
          (push (list param value) parameters))))))

;;; ----------------------------------------------------

(defsetf content-type-parameter (content-type param) (value)
  "Add or change the value of a content-type parameter."
  `(http::content-type-parameter-set ,content-type ,param ,value))

;;; ----------------------------------------------------

(defun content-type-of-pathname (pathname)
  "Using the pathname type, return a known content type."
  (let* ((type (pathname-type pathname))

         ;; lookup the type from the known content types
         (mime-type (assoc type *content-types* :test #'string-equal)))
    (if mime-type
        (apply 'content-type-parse (rest mime-type))
      *application/octet-stream*)))

;;; ----------------------------------------------------

(defun content-type-external-format (content-type)
  "Return the external format for decoding the data of a request/response."
  (let ((charset (content-type-parameter content-type "charset")))
    (cond ((null charset) :utf-8)

          ;; common character set external formats
          ((string-equal charset "us-ascii") :us-ascii)
          ((string-equal charset "utf-8") :utf-8)
          ((string-equal charset "utf-16") :utf-16)
          ((string-equal charset "utf-32") :utf-32)
          ((string-equal charset "x-mac-roman") :macos-roman)
          ((string-equal charset "euc-jp") :euc-jp)
          ((string-equal charset "iso-8859-1") :iso-8859-1)
          ((string-equal charset "iso-8859-2") :iso-8859-2)
          ((string-equal charset "iso-8859-3") :iso-8859-3)
          ((string-equal charset "iso-8859-4") :iso-8859-4)
          ((string-equal charset "iso-8859-5") :iso-8859-5)
          ((string-equal charset "iso-8859-6") :iso-8859-6)
          ((string-equal charset "iso-8859-7") :iso-8859-7)
          ((string-equal charset "iso-8859-8") :iso-8859-8)
          ((string-equal charset "iso-8859-9") :iso-8859-9)
          ((string-equal charset "iso-8859-10") :iso-8859-10)
          ((string-equal charset "iso-8859-11") :iso-8859-11)
          ((string-equal charset "iso-8859-12") :iso-8859-12)
          ((string-equal charset "iso-8859-13") :iso-8859-13)
          ((string-equal charset "iso-8859-14") :iso-8859-14)
          ((string-equal charset "iso-8859-15") :iso-8859-15)
          ((string-equal charset "iso-8859-16") :iso-8859-16)

          ;; unknown, default to utf-8
          (t :utf-8))))

;;; ----------------------------------------------------

(defun content-type-encode (content-type str)
  "Use a content type to encode a string into bytes."
  (if (not (content-type-text-p content-type))
      str  ; assumed to be binary already!
    (let ((format (content-type-external-format content-type)))
      (string-to-octets str :external-format format))))

;;; ----------------------------------------------------

(defun content-type-decode (content-type bytes)
  "Use a content type to decode bytes into a string."
  (when (content-type-text-p content-type)
    (let ((format (content-type-external-format content-type)))
      (octets-to-string bytes :external-format format))))
