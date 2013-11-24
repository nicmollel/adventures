;; -*- mode: lisp; fill-column: 72; coding: utf-8; -*-

;;; Nic M, Nov 2013

(in-package :cl-user)

(defpackage :binary-io-asd
  (:use :cl :asdf))

(in-package :binary-io-asd)

(defvar *binary-io-version* "1.0"
  "A string denoting current version of BINARY-IO")

(export '*binary-io-version*)

(asdf:defsystem :binary-io
  :serial t
  :version #.*binary-io-version*
  :depends-on (:alexandria)
  :components ((:file "packages")
	       (:file "binary-io" :depends-on ("packages"))))