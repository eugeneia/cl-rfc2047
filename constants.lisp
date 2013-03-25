;;;; Cl-RFC2047 constants.

(in-package :cl-rfc2047)

(defparameter *encoded-word-length* 75
  "Maximum length of an encoded word.")

(defparameter *encoded-word-length-overhead* 7
  "Number of syntax overhead characters for encoded words.")

(defparameter *crlfsp* (format nil "~a~a " #\Cr #\Lf)
  "<CRLFSP>")

(defparameter *ascii-boundary* 127
  "ASCII boundary.")

(defparameter *ascii-space* 32
  "ASCII space character code.")

(defparameter *ascii-newline* 10
  "ASCII newline character code.")

(defparameter *ascii-return* 13
  "ASCII return character code.")

(defparameter *ascii-equals* 61
  "ASCII equals sign character code.")

(defparameter *ascii-question-mark* 63
  "ASCII question mark character code.")

(defparameter *ascii-underscore* 95
  "ASCII underscore character code.")

(defparameter *underscore-value*
    (make-array 1 :element-type '(unsigned-byte 8)
		  :initial-element #x20)
  "RFC2047 mandaed value for underscore character in q encoded strings.")
