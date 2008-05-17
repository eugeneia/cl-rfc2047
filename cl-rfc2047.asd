(in-package #:asdf)

(defsystem cl-rfc2047
  :description "Support for rfc-2047 de-/encoding"
  :version "0.1"
  :author "Christian Haselbach"
  :license "MIT"
  :components ((:file "package")
               (:file "cl-rfc-2047" :depends-on ("package")))
  :depends-on (:cl-base64 :flexi-streams :cl-ppcre))
