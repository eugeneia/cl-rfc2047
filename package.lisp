;;;; Package definition for Cl-RFC2047

(defpackage cl-rfc2047
  (:documentation
   "Implementation of the Email header encoding defined in
    [RFC 2047](http://tools.ietf.org/html/rfc2047).")
  (:use :cl
	:babel
	:cl-base64)
  (:export :should-encode-p
	   :encode
	   :decode
	   :decode*))
