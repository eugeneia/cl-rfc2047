(defpackage :cl-rfc2047
  (:use :common-lisp :flexi-streams :cl-base64)
  (:export encode decode))