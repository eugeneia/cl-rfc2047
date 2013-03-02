;;;; System definition for CL-RFC2047-TEST

(defpackage cl-rfc2047-test-asd
  (:use :cl :asdf))

(in-package :cl-rfc2047-test-asd)

(defsystem cl-rfc2047-test
  :description "Testpackage for cl-rfc2047"
  :version "0.2"
  :author "Christian Haselbach"
  :license "MIT"
  :components ((:file "package")
               (:file "cl-rfc2047-test" :depends-on ("package")))
  :depends-on (:cl-rfc2047 :cl-ppcre :lift))
