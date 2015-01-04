(in-package #:dwcalgorithms)

(defun random-num-array (size max)
  (loop with ary = (make-array size)
     for i from 0 below size
     do (setf (aref ary i) (random max))
     finally (return ary)))

(defun random-string-array (size strlength)
  (loop with ary = (make-array size)
     for i from 0 below size
     do (loop with str = (make-array strlength :element-type 'character)
	   for j from 0 below strlength
	   do (setf (aref str j) (code-char (+ 97 (random 26))))
	   finally (setf (aref ary i) str))
     finally (return ary)))

