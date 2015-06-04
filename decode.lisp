;;;; RFC2037 decoding.

(in-package :cl-rfc2047)

(defun intern* (string)
  "Intern STRING upcased in KEYWORD package."
  (intern (string-upcase string) :keyword))

(defun parse-encoded-word (encoded-word)
  "Parse encoding, charset and encoded string of ENCODED-WORD."
  (let* ((charset-start 2)
	 (charset-end (position #\? encoded-word :start charset-start))
	 (encoding-start (1+ charset-end))
	 (encoding-end (1+ encoding-start))
	 (encoded-string-start (1+ encoding-end))
	 (encoded-string-end (- (length encoded-word) 2)))
    (values
     (intern* (subseq encoded-word encoding-start encoding-end))
     (intern* (subseq encoded-word charset-start charset-end))
     (subseq encoded-word encoded-string-start encoded-string-end))))

(defun decode-bytes (encoded-bytes charset)
  "Decode ENCODED-BYTES according to CHARSET."
  (let ((length (length encoded-bytes)))
    (octets-to-string
     (coerce (loop for i = 1 then (+ i 3) while (< i length)
		collect (parse-integer encoded-bytes
				       :start i
				       :end (+ i 2)
				       :radix 16))
	     '(vector (unsigned-byte 8)))
     :encoding charset)))

(defun write-decoded-underscore-value (charset stream)
  "Write decoded *UNDERSCORE-VALUE* to STREAM according to CHARSET."
  (write-string (octets-to-string *underscore-value* :encoding charset)
		stream))

(defun q-decode (encoded-string charset)
  "Decode ENCODED-STRING according to CHARSET."
  (flet ((token-p (char)
	   (or (char= #\= char)
	       (char= #\_ char)))
	 (encoded-to (from)
	   (loop for i = from then (+ i 3)
	      while (< i (length encoded-string))
	      unless (char= #\= (aref encoded-string i))
	      return i)))
    (with-output-to-string (out)
      (loop for start = 0 then to
	    for from = (position-if #'token-p encoded-string
				    :start start)
	    for token = (when from
			  (aref encoded-string from))
	    for to = (case token
		       (#\= (encoded-to from))
		       (#\_ (1+ from)))
	 do (write-string encoded-string out :start start :end from)
	 when token
	 do (ecase token
	      (#\= (write-string
		    (decode-bytes (subseq encoded-string from to)
				  charset)
		    out))
	      (#\_ (write-decoded-underscore-value charset out)))
	 while to))))

(defun decode-word (encoded-word)
  "Decode ENCODED-WORD."
  (multiple-value-bind (encoding charset encoded-string)
      (parse-encoded-word encoded-word)
    (case encoding
      (:b (octets-to-string (base64-string-to-usb8-array encoded-string)
			    :encoding charset))
      (:q (q-decode encoded-string charset)))))

(defun decode (string &key (start 0) end)
  "*Arguments and Values:*

   _string_—a _string_.

   _start_, _end_—_bounding index designators_ of _string_. The default
   for _start_ is 0 and the default for _end_ is {nil}.

   *Description*:

   {decode} returns the decoded word in _string_.


   *Exceptional Situations:*

   If {decode} fails an _error condition_ is signaled."
  (decode-word (subseq string start end)))

(defun crlfsp-p (string start end)
  "Predicate to test if STRING equals to *CRLFSP* from START to END."
  (unless (> end (length string))
    (string= *crlfsp* string :start2 start :end2 end)))

(defun encoded-word-to (string from)
  "Find end of encoded word in STRING starting at FROM."
  (let* ((?1 (position #\? string :start from))
	 (?2 (position #\? string :start (1+ ?1)))
	 (?3 (position #\? string :start (1+ ?2))))
    (+ 2 (search "?=" string :start2 (1+ ?3)))))

(defun decode-word* (string)
  "Decode mixed STRING."
  (with-output-to-string (out)
    (loop for start = 0 then (if (crlfsp-p string to (+ to 3))
				 (+ to 3)
				 to)
       for from = (search "=?" string :start2 start)
       for to = (when from (encoded-word-to string from))
       do (write-string (subseq string start from) out)
       when from
       do (write-string (decode string :start from :end to) out)
       while (and from to))))

(defun decode* (string &key (start 0) end (errorp t))
  "*Arguments and Values:*

   _string_—a _string_.

   _start_, _end_—_bounding index designators_ of _string_. The default
   for _start_ is 0 and the default for _end_ is {nil}.

   _error-p_—a _generalized boolean_. The default is _true_.

   *Description*:

   {decode*} returns a decoded copy of _string_ containing encoded as
   well as unencoded words.

   *Exceptional Situations:*

   If {decode*} fails and _error-p_ is _true_ an _error condition_ is
   signaled."
  (handler-case (decode-word* (subseq string start end))
    (error (error) (if errorp
		       (error error)
		       string))))
