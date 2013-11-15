;; -*- mode: lisp; ; fill-column: 82; coding: utf-8; -*-

;;; Nic M, Nov 2013

(in-package :binary-io)

(defgeneric read-value (type stream &key)
  (:documentation "Read a value of the given type from the stream"))

(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(make-keyword name) :accessor ,name)))

(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defun normalize-slot-spec (spec)
  (list (first spec) (ensure-list (second spec)))) ;ensure-list instead mklist 
