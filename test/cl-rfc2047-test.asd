(in-package #:asdf)

(defsystem klatschbase
  :description "Testpackage for cl-rfc2047"
  :version "0.2"
  :author "Christian Haselbach"
  :license "MIT"
  :components ((:file "package")
               (:file "cl-rfc2047-test" :depends-on ("package")))
  :depends-on (:cl-rfc2047 :lift))
