;; -*- mode: lisp; fill-column: 72; coding: utf-8; -*-
;;; Nic M, Nov 2013

;;; Binary IO

(in-package :cl-user)

(defpackage :binary-io
  (:nicknames :binary-data)		;Name in the book
  (:use :cl :alexandria))