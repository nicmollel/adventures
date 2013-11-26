;; -*- mode: lisp; fill-column: 72; coding: utf-8; -*-

;;; Nic M, Nov 2013

(in-package :cl-user)

(defpackage :id3v2-asd
  (:use :cl :asdf))

(in-package :id3v2-asd)

(defvar *id3v2-version* "1.0"
  "A string denoting current version of ID3v2")

(export '*id3v2-version*)

(asdf:defsystem :id3v2
  :serial t
  :version #.*id3v2-version*
  :depends-on (:alexandria :binary-io)	;UIOP/PATHNAME is part of ASDF
					;and should be loaded loaded at
					;this time
  :components ((:file "packages")
	      (:file "id3v2" :depends-on ("packages")))) 

