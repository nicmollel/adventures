;; -*- mode: lisp; fill-column: 72; coding: utf-8; -*-
;;; Nic M, Nov 2013

;;; Binary IO

(in-package :cl-user)

(defpackage :binary-io
  (:nicknames :binary-data)		;Name in the book
  (:use :cl :alexandria)
  (:export :define-binary-class
	   :define-tagged-binary-class
	   :define-binary-type
	   :read-value
	   :write-value
	   :*in-progress-objects*
	   :parent-of-type
	   :current-binary-object
	   :+null+))

(defpackage :id3v2
  (:use :cl
	:alexandria
	:binary-io
	:uiop/pathname)
  (:export
   :read-id3
   :mp3-p
   :id3-p
   :album
   :composer
   :genre
   :encoding-program
   :artist
   :part-of-set
   :track
   :song
   :year
   :size
   :translated-genre))