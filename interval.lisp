(in-package #:dwcalgorithms)

(defclass interval ()
  ((lower :initform 0 :initarg :lower :accessor lower)
   (upper :initform 0 :initarg :upper :accessor upper)))

(defun make-interval (lower upper)
  (assert (< lower upper))
  (make-instance 'interval :lower lower :upper upper))

(defmethod <=> ((one interval) (two interval))
  (cond
    ((< (lower one) (lower two)) -1)
    ((and (= (lower one) (lower two))
          (< (upper one) (upper two))) -1)
    ((and (= (lower one) (lower two))
          (= (upper one) (upper two))) 0)
    (t 1)))

(defmethod print-object ((i interval) stream)
  (print-unreadable-object (i stream :type t)
    (with-slots (lower upper) i
      (format stream "(lower: ~a, upper: ~a)" lower upper))))

(defun left-of (one two)
  (< (upper one) (lower two)))

(defun right-of (one two)
  (< (upper two) (lower one)))

(defun overlap? (one two)
  (not (or (left-of one two)
           (right-of one two))))

(defun interval-search (tree i)
  (loop
     with next-i = (root tree)
     while (and next-i (not (overlap? i (data next-i))))
     do (progn
          (if (and (left next-i) (>=? #'<=> (upper (data (left next-i))) (lower i)))
              (setf next-i (left next-i))
              (setf next-i (right next-i))))
     finally (return (if next-i (data next-i) nil))))
