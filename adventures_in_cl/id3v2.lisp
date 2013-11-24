;; -*- mode: lisp; ; ; fill-column: 100; coding: utf-8; -*-

;;; Nic M, Nov 2013
;;; Ref: http://www.gigamonkeys.com/book/practical-an-id3-parser.html

(in-package :id3v2)

;;; Integer Types
(define-binary-type unsigned-integer (bytes bits-per-byte)
  (:reader (in)
	   (loop :with value = 0
	      :for low-bit :downfrom (* bits-per-byte (1- bytes)) :to 0 :by bits-per-byte
	      :do (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
	      :finally (return value)))
  (:writer (out value)
	   (loop :for low-bit :downfrom (* bits-per-byte (1- bytes)) :to 0 :by bits-per-byte
	      :do (write-byte (ldb (byte bits-per-byte low-bit) value) out))))

(define-binary-type id3-tag-size ()
  (unsigned-integer :bytes 4 :bits-per-byte 7))

(define-binary-type u1 () (unsigned-integer :bytes 1 :bits-per-byte 8))
(define-binary-type u2 () (unsigned-integer :bytes 2 :bits-per-byte 8))
(define-binary-type u3 () (unsigned-integer :bytes 3 :bits-per-byte 8))
(define-binary-type u4 () (unsigned-integer :bytes 4 :bits-per-byte 8))

;;; String Types
(define-binary-type generic-string (length character-type)
  (:reader (in)
	   (let ((string (make-string length)))
	     (dotimes (i length)
	       (setf (char string i) (read-value character-type in)))
	     string))
  (:writer (out string)
	   (dotimes (i length)
	     (write-value character-type out (char string i)))))

(define-binary-type generic-terminated-string (terminator character-type)
  (:reader (in)
	   (with-output-to-string (s)
	     (loop :for char = (read-value character-type in)
		:until (char= char terminator)
		:do (write-char char s))))
  (:writer (out string)
	   (loop :for char :across string
	      :do (write-value character-type out char)
	      :finally (write-value character-type out terminator))))

(define-binary-type iso-8859-1-char ()
  (:reader (in)
	   (let ((code (read-byte in)))
	     #-sbcl 			;SBCL never return nil for CODE-CHAR/CHAR-CODE 
	     (or (code-char code)
		 (error "Character code ~d not supported" code))
	     #+sbcl
	     (code-char code)))
  (:writer (out char)
	   (let ((code (char-code char)))
	     (if (<= 0 code #xff)
		 (write-byte code out)
		 (error "Illigal character for iso-8859-1 encoding: character: ~c with code: ~d"
			char code)))))

(define-binary-type iso-8859-1-string (length)
  (generic-string :length length :character-type 'iso-8859-1-char))

(define-binary-type iso-8859-1-terminated-string (terminator)
  (generic-terminated-string :terminator terminator :character-type 'iso-8859-1-char))