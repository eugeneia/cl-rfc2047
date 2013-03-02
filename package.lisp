;;;; Package definition for Cl-RFC2047

(defpackage cl-rfc2047
  (:documentation "Implmentation of RFC2047.")
  (:use :cl
	:babel
	:cl-base64)
  (:export :encode
	   :decode
	   :decode*))
