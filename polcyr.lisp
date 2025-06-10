#!/usr/bin/env -S sbcl --script
(defconstant +lat-cyr-map+
  '(#\a #\а
    #\b #\б
    #\w #\в
    #\g #\г
    #\h #\г
    #\d #\д
    #\ż #\ж
    #\z #\з
    #\i #\и
    #\j #\й
    #\k #\к
    #\l #\л
    #\m #\м
    #\n #\н
    #\o #\о
    #\p #\п
    #\r #\р
    #\s #\с
    #\t #\т
    #\u #\у
    #\f #\ф
    :ch #\х
    #\c #\ц
    :cz #\ч ; (:sz . :y), (:ż . :y) and (:cz . :y) become (:cz . :i), etc.
    :sz #\ш ; (:sz . :cz) becomes сч at the beginning of words and щ elsewhere.
    :szcz #\щ
    #\y #\ы
    #\e #\э
    #\ę #\ѧ
    #\ą #\ѫ))

(defconstant +softness-transmitters+
  '(#\a #\w #\d #\z #\i #\m #\n #\o #\p #\s #\t #\u #\y #\e #\ę #\ą #\l))

(defconstant +need-hard-sign+
  '(#\z #\s #\n))

(defconstant +hard-soft-map+
  '(#\a #\я
    #\e #\е
    #\o #\ё
    #\y #\и
    #\u #\ю
    #\i #\и
    #\ę #\ѩ
    #\ą #\ѭ))

(defconstant +accents-sound-map+
  '(#\ś #\s
    #\ć #\t
    #\ź #\z
    #\ń #\n
    #\ẃ #\w
    #\ḿ #\m
    #\ṕ #\p))

(defclass abstract-letter ()
  ((uppercase :accessor uppercase
	      :initarg :up
	      :type bool
	      :documentation "Whether or not the letter is uppercase.")
   (sound :accessor sound
	  :initarg :sound
	  :type keyword
	  :documentation "The sound represented by the letter.")
   (soft :accessor soft
	 :initarg :soft
	 :type bool
	 :documentation "Whether or not the letter's sound is soft."))
  (:documentation "The class serving as an IR for latin -> cyrillic transliteration."))

(defun process-next-soft-vowel (s default push-default &optional keep-case)
  (if (null s)
      (cons (list default) s)
      (let* ((nx (car s))
	     (up (upper-case-p nx))
	     (nx (char-downcase nx)))
	(case nx
	  ((#\a #\e #\i #\y #\o #\u #\ę #\ą)
	   (cons
	    (if push-default
		(list default
		      (make-instance 'abstract-letter
				     :up (if keep-case (uppercase default) up)
				     :sound nx
				     :soft t))
		(list (make-instance 'abstract-letter
				     :up (if keep-case (uppercase default) up)
				     :sound nx
				     :soft t)))
	    (cdr s)))
	  (otherwise (cons (list default) s))))))
  
(defmacro z-digraph-handler (base zd soft)
  `(if (null (cdr s))
		   (list (make-instance 'abstract-letter
					:up up
					:sound ,base
					:soft nil))
		   (let ((nx (char-downcase (cadr s))))
		     (case nx
		       ; disgusting
		       (#\h ,(if (equal base #\c)
				 (list 'cons
				       (list 'make-instance
					     ''abstract-letter
					     :up 'up
					     :sound :ch
					     :soft nil)
				       (list 'analyse-list
					     (list 'cddr 's)))
				 (list 'cons
				       (list 'make-instance
					     ''abstract-letter
					     :up 'up
					     :sound base
					     :soft nil)
				       (list 'analyse-list
					     (list 'cdr 's)))))
		       (#\z (cons (make-instance 'abstract-letter
						 :up up
						 :sound ,zd
						 :soft nil)
				  (analyse-list (cddr s))))
		       (#\i (destructuring-bind (pr . cd)
				(process-next-soft-vowel (cddr s)
							 (make-instance 'abstract-letter
									:up up
									:sound #\y
									:soft t)
							 nil)
			      (cons (make-instance 'abstract-letter
						   :up up
						   :sound ,soft
						   :soft t)
				    (append pr (analyse-list cd)))))
		       (otherwise (cons (make-instance 'abstract-letter
						       :up up
						       :sound ,base
						       :soft nil)
					(analyse-list (cdr s))))))))

(defmacro i-digraph-handler (ch i sound)
    `(if (null (cdr s))
	 (cons (make-instance 'abstract-letter
			      :up up
			      :sound ,sound
			      :soft nil)
	       nil)
	 (let ((nx (char-downcase (cadr s))))
	   (case nx
	     (,i (destructuring-bind (pr . cd)
		     (process-next-soft-vowel (cddr s)
					      (make-instance 'abstract-letter
							     :up up
							     :sound ,(if (equal ch #\r) #\r #\y)
							     :soft t)
					      ,(if (equal ch #\r) t nil))
		   ,(let ((remainder (list 'append 'pr
					   (list 'analyse-list 'cd))))
		      (if (equal ch #\r)
			  remainder
			  (list 'cons
				(list 'make-instance ''abstract-letter
				      :up 'up :sound sound :soft t)
				remainder)))))
	     (otherwise (cons (make-instance 'abstract-letter
					     :up up
					     :sound ,sound
					     :soft nil)
			      (analyse-list (cdr s))))))))

(defun analyse-list (s)
  "Turn a list of Polish characters into a list of ABSTRACT-LETTER."
  (if (null s)
      nil
      (let* ((ch (car s))
	     (up (upper-case-p ch))
	     (ch (char-downcase ch)))
	(case ch
	  ((#\a #\e #\i #\y #\o #\ó #\u #\ę #\ą)
	   (cons (make-instance 'abstract-letter
				:up up
				:sound ch
				:soft nil)
		 (analyse-list (cdr s))))
	  (#\h (cons (make-instance 'abstract-letter
				    :up up
				    :sound #\h
				    :soft nil)
		     (analyse-list (cdr s))))
	  (#\c (z-digraph-handler #\c :cz #\t))
	  (#\s (z-digraph-handler #\s :sz #\s))
	  (#\l (if (null (cdr s))
		   (cons (make-instance 'abstract-letter
					:up up
					:sound #\l
					:soft t)
			 nil)
		   (let* ((nx (cadr s))
			  (up1 (upper-case-p nx))
			  (nx (char-downcase (cadr s))))
		     (case nx
		       ((#\a #\e #\i #\y #\o #\u #\ę #\ą)
			(append (list (make-instance 'abstract-letter
						     :up up
						     :sound #\l
						     :soft t)
				      (make-instance 'abstract-letter
						     :up up1
						     :sound nx
						     :soft t))
				(analyse-list (cddr s))))
		       (otherwise (cons (make-instance 'abstract-letter
						       :up up
						       :sound #\l
						       :soft t)
					(analyse-list (cdr s))))))))
	  ((#\ś #\ź #\ć #\ń #\ẃ #\ḿ #\ṕ) (cons (make-instance 'abstract-letter
				    :up up
				    :sound (getf +accents-sound-map+ ch)
				    :soft t)
					   (analyse-list (cdr s))))
	  (#\ł (cons (make-instance 'abstract-letter
				    :up up
				    :sound #\l
				    :soft nil)
		     (analyse-list (cdr s))))
	  ((#\b #\w #\g #\k #\m #\n #\p #\r #\f #\z)
	   (i-digraph-handler ch
			      (if (equal ch #\r) #\z #\i)
			      ch))
	  (#\LATIN_SMALL_LETTER_Z_WITH_DOT_ABOVE (cons
						(make-instance 'abstract-letter
							       :up up
							       :sound #\ż
							       :soft nil)
						(analyse-list (cdr s))))
	  (#\t (cons
		(make-instance 'abstract-letter
			       :up up
			       :sound #\t
			       :soft nil)
		(analyse-list (cdr s))))
	  (#\d (if (null (cdr s))
		   (cons (make-instance 'abstract-letter
					:up up
					:sound #\d
					:soft nil)
			 nil)
		   (let ((nx (char-downcase (cadr s))))
		     (case nx
		       (#\ź (cons (make-instance 'abstract-letter
						 :up up
						 :sound #\d
						 :soft t)
				  (analyse-list (cddr s))))
		       (#\z (if (null (cddr s))
				(list (make-instance 'abstract-letter
						     :up up
						     :sound #\d
						     :soft nil)
				      (make-instance 'abstract-letter
						     :up up
						     :sound #\z
						     :soft nil))
				(let ((sd (char-downcase (caddr s))))
				  (case sd
				    (#\i (destructuring-bind (pr . cd)
					     (process-next-soft-vowel (cdddr s)
								      (make-instance 'abstract-letter
										     :up up
										     :sound #\y
										     :soft t)
								      nil)
					   (cons (make-instance 'abstract-letter
								:up up
								:sound #\d
								:soft t)
						 (append pr (analyse-list cd)))))
				    (otherwise (append (list (make-instance 'abstract-letter
									    :up up
									    :sound #\d
									    :soft nil)
							     (make-instance 'abstract-letter
									    :up up
									    :sound #\z
									    :soft nil))
						       (analyse-list (cddr s))))))))
		       (otherwise (cons (make-instance 'abstract-letter
						       :up up
						       :sound #\d
						       :soft nil)
					(analyse-list (cdr s))))))))
	  (#\j (destructuring-bind (pr . cd)
		   (process-next-soft-vowel (cdr s)
					    (make-instance 'abstract-letter
							   :up up
							   :sound #\j
							   :soft t)
					    nil
					    t)
		 (append pr (analyse-list cd))))
	  (otherwise (cons ch (analyse-list (cdr s))))))))

(defun pre-processing (lst)
  (if (null lst)
      nil
      (let ((x (car lst)))
	(cons (case x
		(#\ó #\o)
		(#\Ó #\O)
		(otherwise x))
	      (pre-processing (cdr lst))))))

(defun post-processing (prev lst)
  (if (null lst)
      nil
      (let ((x (car lst)))
	(if (equal (type-of x) 'abstract-letter)
	    (case (sound x)
	      (:sz (if (next-abstract-p lst)
			(let ((nx (cadr lst)))
			  (case (sound nx)
			    (#\y (append (list x (make-instance 'abstract-letter
								:up (uppercase nx)
								:sound #\i
								:soft nil))
					 (post-processing nx (cddr lst))))
			    (#\e (append (list x (make-instance 'abstract-letter
								:up (uppercase nx)
								:sound #\e
								:soft t))
					 (post-processing nx (cddr lst))))
			    (:cz 
					 (post-processing nx (append (if prev
									 (list (make-instance 'abstract-letter
											      :up (uppercase x)
											      :sound :szcz
											      :soft nil))
									 (list (make-instance 'abstract-letter
											      :up (uppercase x)
											      :sound #\s
											      :soft nil)
									       (make-instance 'abstract-letter
											      :up (uppercase nx)
											      :sound :cz
											      :soft nil)))
								     (cddr lst))))
			    (otherwise (cons x (post-processing x (cdr lst))))))
			(cons x (post-processing x (cdr lst)))))
	      ((:cz #\c #\ż :szcz) (if (next-abstract-p lst)
				 (let ((nx (cadr lst)))
				   (case (sound nx)
				     (#\e (append (list x (make-instance 'abstract-letter
									 :up (uppercase nx)
									 :sound #\e
									 :soft t))
						  (post-processing nx (cddr lst))))
				     (#\y (append (list x (make-instance 'abstract-letter
									 :up (uppercase nx)
									 :sound #\i
									 :soft nil))
						  (post-processing nx (cddr lst))))
				     (otherwise (cons x (post-processing x (cdr lst))))))
				 (cons x (post-processing x (cdr lst)))))
	      (otherwise (cons x (post-processing x (cdr lst)))))
	    (cons x (post-processing nil (cdr lst)))))))

(defun vowel-p (x)
  (member (sound x) +hard-soft-map+))

(defun next-abstract-p (lst)
  (and (not (null (cdr lst)))
       (equal (type-of (cadr lst)) 'abstract-letter)))

(defun next-transmits-p (lst)
  (and (next-abstract-p lst)
       (member (sound (cadr lst)) +softness-transmitters+)))

(defun make-agreement (chr cse)
  (if (uppercase cse)
      (char-upcase chr)
      (char-downcase chr)))

(defun transliterate-list (lst)
  "Transform a list of ABSTRACT-LETTER into a list of Cyrillic characters."
  (if (null lst)
      nil
      (let ((x (car lst)))
	(if (equal (type-of x) 'abstract-letter)
	    (let* ((cyr (getf +lat-cyr-map+ (sound x)))
		   (cyrs (if (and (soft x)
				  (vowel-p x))
			     (getf +hard-soft-map+ (sound x))
			     cyr))
		   (cyru (if (uppercase x) (char-upcase cyrs) cyrs)))
	      (append (if (vowel-p x)
			  (cons cyru nil)
			  (if (soft x)
			      (if (and (next-transmits-p lst)
				       (or
					(not (equal (sound x) #\l))
					(vowel-p (cadr lst)))
				       (soft (cadr lst)))
				  (cons cyru nil)
				  (if (equal (sound x) #\j)
				      (cons cyru nil)
				      (list cyru (make-agreement #\ь x))))
			      (if (and (next-transmits-p lst)
				       (member (sound x) +need-hard-sign+)
				       (soft (cadr lst)))
				  (list cyru (make-agreement #\ъ x))
				  (cons cyru nil))))
		      (transliterate-list (cdr lst))))
	    (cons x (transliterate-list (cdr lst)))))))

(defun transliteration-pipeline (output-stream str)
  "Run the full pre-processing, analysis, post-processing and transilteration pipeline.

FORMATs the string on OUTPUT-STREAM"
  (format output-stream
	  "~{~A~}~%"
	  (transliterate-list
	   (post-processing nil
			    (analyse-list
			     (pre-processing
			      (coerce str 'list)))))))

(loop
  (let ((line (read-line)))
    (transliteration-pipeline t line)))
