;;;; RFC2047 encoding.

(in-package :cl-rfc2047)

(defun encoded-word (encoding charset string)
  "Return encoded word for ENCODING, CHARSET and STRING."
  (format nil "=?~a?~a?~a?="
	  (symbol-name charset)
	  (symbol-name encoding)
	  string))

(defun encoded-words (encoding charset strings)
  "Return encoded words for ENCODING, CHARSET and STRINGS."
  (format nil (format nil "~~{~~a~~^~a~~}" *crlfsp*)
	  (loop for string in strings
	     collect (encoded-word encoding charset string))))

(defun encoded-word-content-length (charset)
  "Return maximum length of encoded word contents for CHARSET."
  (- *encoded-word-length* (+ *encoded-word-length-overhead*
			      (length (symbol-name charset)))))

(defun limited-words (pieces length word-type)
  "Return list of words no longer than LENGTH concatenated from PIECES
destructively."
  (flet ((next-word ()
	   (loop for count = 0 then (+ count (length piece))
	      for piece = (car pieces)
	      while (and piece
			 (<= (+ count (length piece))
			     length))
	      collect (pop pieces))))
    (loop for word = (next-word)
       while word collect (apply #'concatenate word-type word))))

(defun string-to-grouped-bytes (string charset)
  "Return list of byte vectors for STRING using CHARSET."
  (loop for i from 0 to (1- (length string))
     collect (string-to-octets (subseq string i (1+ i))
			       :encoding charset)))

(defun encoded-word-content-bytes (charset)
  "Return number of maximum bytes per b-encoded word using CHARSET."
  (floor (* (encoded-word-content-length charset) 0.7))) ; magic number

(defun b-split (string charset)
  "Return STRING split up in parts for b-encoding according to CHARSET."
  (limited-words (string-to-grouped-bytes string charset)
		 (encoded-word-content-bytes charset)
		 '(vector (unsigned-byte 8))))

(defun b-encode (string charset)
  "Return list of base64 encoded words for STRING using CHARSET."
  (loop for buffer in (b-split string charset)
       collect (usb8-array-to-base64-string buffer)))

(defun char-utf-8 (character)
  "Return UTF-8 buffer for CHARACTER."
  (string-to-octets (make-string 1 :initial-element character)
		    :encoding :utf-8))

(defun q-encode-p (character)
  "Predicate to test if CHARACTER needs to be q-encoded."
  (let ((code (aref (char-utf-8 character) 0)))
    (or (> code *ascii-boundary*)
	(= code *ascii-newline*)
	(= code *ascii-return*)
	(= code *ascii-space*)
	(= code *ascii-equals*)
	(= code *ascii-question-mark*)
	(= code *ascii-underscore*))))

(defun q-encode-string (string charset)
  "Return q encoded STRING using CHARSET."
  (with-output-to-string (out)
    (loop for character across string
       do (if (q-encode-p character)
	      (loop for byte across (string-to-octets
				     string :encoding charset)
		 do (format out "=~2,'0,X" byte))
	      (write-char character out)))))

(defun q-encode-characters (string charset)
  "Return list of q encoded characters for STRING using CHARSET."
  (loop for i from 0 to (1- (length string))
     collect (q-encode-string (subseq string i (1+ i)) charset)))

(defun q-encode (string charset)
  "Return list of q encoded words for STRING using CHARSET."
  (limited-words (q-encode-characters string charset)
		 (encoded-word-content-length charset)
		 'string))

(defun encode (string &key (encoding :b) (charset :utf-8))
  "Return encoded STRING using ENCODING and CHARSET."
  (encoded-words encoding charset (ecase encoding
				    (:b (b-encode string charset))
				    (:q (q-encode string charset)))))
