(in-package :cl-rfc2047)

(defparameter *crlfsp*
  (concatenate 'string
	       (string #\return) (string #\linefeed) (string #\space)))

(defparameter *decoded-line-regexp*
  "=\\?([a-zA-Z0-9-]+)\\?([qQbB])\\?([^ ?]+)\\?=")

(defun string+ (&rest strs)
  (apply #'concatenate `(string ,@strs)))

(defun char-max-len (charset)
  "returns the maximum number of bytes a character uses in the given
charset and as second value a boolean denoting whether the length is fixed.
Throws an error if the charset is unknown."
  (case charset
    (:utf-8       (values 4 nil))
    (:utf-16      (values 4 nil))
    (:ISO-8859-1  (values 1 t))
    (:ISO-8859-2  (values 1 t))
    (:ISO-8859-3  (values 1 t))
    (:ISO-8859-4  (values 1 t))
    (:ISO-8859-5  (values 1 t))
    (:ISO-8859-6  (values 1 t))
    (:ISO-8859-7  (values 1 t))
    (:ISO-8859-8  (values 1 t))
    (:ISO-8859-9  (values 1 t))
    (:ISO-8859-10 (values 1 t))
    (:ISO-8859-11 (values 1 t))
    (:ISO-8859-12 (values 1 t))
    (:ISO-8859-13 (values 1 t))
    (:ISO-8859-14 (values 1 t))
    (:ISO-8859-15 (values 1 t))
    (:ISO-8859-16 (values 1 t))
    (:US-ASCII    (values 1 t))
    (:KOI8-R      (values 1 t))
    (otherwise    (error "charset ~A unknown" charset))))

(defun direct-q-p (c)
  "Whether a character can be used directly in Q encoding."
  (or (<= (char-code #\a) c (char-code #\z))
      (<= (char-code #\A) c (char-code #\Z))
      (<= (char-code #\0) c (char-code #\9))))

(defun map-to-external-format (name)
  "Maps a character encoding name to the internal symbol.
Throws an error if the character encoding is not known."
  (make-external-format
   (cond
     ((string-equal name "UTF-8")       :UTF-8)
     ((string-equal name "ISO-8859-1")  :ISO-8859-1)
     ((string-equal name "ISO-8859-2")  :ISO-8859-2)
     ((string-equal name "ISO-8859-3")  :ISO-8859-3)
     ((string-equal name "ISO-8859-4")  :ISO-8859-4)
     ((string-equal name "ISO-8859-5")  :ISO-8859-5)
     ((string-equal name "ISO-8859-6")  :ISO-8859-6)
     ((string-equal name "ISO-8859-7")  :ISO-8859-7)
     ((string-equal name "ISO-8859-8")  :ISO-8859-8)
     ((string-equal name "ISO-8859-9")  :ISO-8859-9)
     ((string-equal name "ISO-8859-10") :ISO-8859-10)
     ((string-equal name "ISO-8859-11") :ISO-8859-11)
     ((string-equal name "ISO-8859-12") :ISO-8859-12)
     ((string-equal name "ISO-8859-13") :ISO-8859-13)
     ((string-equal name "ISO-8859-14") :ISO-8859-14)
     ((string-equal name "ISO-8859-15") :ISO-8859-15)
     ((string-equal name "ISO-8859-16") :ISO-8859-16)
     ((string-equal name "US-ASCII")    :US-ASCII)
     ((string-equal name "KOI8-R")      :KOI8-R)
     (t (error "charset ~A unknown" name)))))

(defun encode (str &optional &key (encoding :b) (charset :utf-8))
  "encodes the string using the given encoding and character set"
  (let ((slen (length str))
	(wlen (floor (+ 75 -8 (- (length (symbol-name charset))))
		     (char-max-len charset)))
	(ext-format (make-external-format charset))
	(stream (make-string-output-stream)))
    (labels
	((b-enc (i n)
	   (if (>= i slen)
	       n
	       (let* ((j      (min slen (+ i (* 4 (ceiling wlen 5)))))
		      (substr (subseq str i j))
		      (octets (string-to-octets substr
						:external-format ext-format))
		      (b64str (usb8-array-to-base64-string octets)))
		 (unless (zerop n) (princ *crlfsp* stream))
		 (format stream "=?~A?~A?~A?=" charset encoding b64str)
		 (b-enc j (1+ n)))))
	 (q-enc* (i len octets)
	   (if (or (>= i slen) (>= len wlen))
	       i
	       (let ((c (elt octets i)))
		 (if (direct-q-p c)
		     (progn
		       (princ (code-char c) stream)
		       (q-enc* (1+ i) (1+ len) octets))
		     (progn
		       (if (>= (+ len 3) wlen)
			   i
			   (progn
			     (format stream "=~2,'0,X" c)
			     (q-enc* (1+ i) (+ len 3) octets))))))))
	 (q-enc (i n octets)
	   (if (>= i slen)
	       n
	       (progn
		 (unless (zerop n) (princ *crlfsp* stream))
		 (format stream "=?~A?~A?" charset encoding)
		 (let ((i* (q-enc* i 0 octets)))
		   (princ "?=" stream)
		   (q-enc i* (1+ n) octets))))))
      (case encoding
	(:b (b-enc 0 0))
	(:q (if (= (char-max-len charset) 1)
		(q-enc 0 0 (string-to-octets str :external-format ext-format))
		(error
		 "can only use q encoding on 1 byte charset encodings")))
	(otherwise (error "encoding ~A unknown" encoding)))
      (get-output-stream-string stream))))

(defun decode-q-to-octets (str)
  "decodes q encoded strings to octet arrazs"
  (let* ((len (length str))
	 (octets (make-array len :fill-pointer 0 :adjustable t)))
    (labels
	((q-loop (i)
	   (when (< i len)
	     (let ((c (elt str i)))
	       (if (char= c #\=)
		   (progn
		     (vector-push-extend
		      (parse-integer str :start(+ i 1) :end (+ i 3) :radix 16)
		      octets)
		     (q-loop (+ i 3)))
		   (progn
		     (vector-push-extend
		      (if (char= c #\_) 32 (char-code c))
		      octets)
		     (q-loop (+ i 1))))))))
      (q-loop 0)
      octets)))

(defun decode-one-word (str)
  (multiple-value-bind (start end reg-starts reg-ends)
      (ppcre:scan *decoded-line-regexp* str)
    (if (null start)
	(error "Not a valid encoded word ~A" str)
	(let ((charset  (subseq str (elt reg-starts 0) (elt reg-ends 0)))
	      (encoding (subseq str (elt reg-starts 1) (elt reg-ends 1)))
	      (code     (subseq str (elt reg-starts 2) (elt reg-ends 2))))
	  (cond
	    ((string-equal encoding "b")
	     (octets-to-string
	      (base64-string-to-usb8-array code)
	      :external-format (map-to-external-format charset)))
	    ((string-equal encoding "q")
	     (octets-to-string
	      (decode-q-to-octets code)
	      :external-format (map-to-external-format charset)))
	    (t (error "unknown encoding ~A" encoding)))))))

(defun decode (str &key (start 0) (end (length str)))
  (loop
     :with stream = (make-string-output-stream)
     :for word :in (ppcre:split *crlfsp* str :start start :end end)
     :do (princ (decode-one-word word) stream)
     :finally (return (get-output-stream-string stream))))

(defun decode* (str)
  (loop
     :with stream = (make-string-output-stream)
     :for x = 0 :then end
     :for (start end)
     :on (ppcre:all-matches (string+ *decoded-line-regexp*
				     "(" *crlfsp* *decoded-line-regexp* ")*")
			    str)
     :by #'cddr
     :if (not (= x start)) :do (princ (subseq str x start) stream)
     :do (princ (decode str :start start :end end) stream)
     :finally (let ((strlen (length str)))
		(unless (= end strlen)
		  (princ (subseq str end strlen) stream))
		(return (get-output-stream-string stream)))))
