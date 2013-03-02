;;;; System definition for CL-RFC2047.

(defpackage cl-rfc2047-asd
  (:use :cl :asdf))

(in-package :cl-rfc2047-asd)

(defsystem cl-rfc2047
  :description "Implemntation of RFC2047."
  :version "0.3"
  :author "Christian Haselbach"
  :license "MIT"
  :components ((:file "package")
	       (:file "constants" :depends-on ("package"))
               (:file "encode" :depends-on ("package"
					    "constants"))
               (:file "decode" :depends-on ("package"
					    "constants")))
  :depends-on (:cl-base64 :babel))
