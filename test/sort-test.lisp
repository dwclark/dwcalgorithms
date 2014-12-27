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

(5am:def-suite sort-suite :description "Sort Test Suite")
(5am:in-suite sort-suite)

(5am:test test-<=>
  (5am:is (= -1 (<=> 1 2)))
  (5am:is (= 0 (<=> 1 1)))
  (5am:is (= 1 (<=> 2 1)))

  (5am:is (= -1 (<=> 1.1 1.2)))
  (5am:is (= 0 (<=> 1.1 1.1)))
  (5am:is (= 1 (<=> 1.2 1.1)))

  (5am:is (= -1 (<=> "first" "second")))
  (5am:is (= 0 (<=> "first" "first")))
  (5am:is (= 1 (<=> "second" "first")))

  (5am:is (= -1 (<=> #\a #\b)))
  (5am:is (= 0 (<=> #\a #\a)))
  (5am:is (= 1 (<=> #\b #\a))))

(5am:test test-<=>-ignore-case
  (5am:is (= -1 (<=>-ignore-case "FIRST" "second")))
  (5am:is (= 0 (<=>-ignore-case "FIRST" "FIRst")))
  (5am:is (= 1 (<=>-ignore-case "SECOND" "first"))))

(5am:test test-sorted?
  (5am:is (sorted? (vector 1 2 3 4 5)))
  (5am:is (not (sorted? (vector 1 2 4 5 3)))))

(5am:test test-<=>-reverse
  (5am:is (= -1 (funcall (<=>-reverse #'<=>) 2 1)))
  (5am:is (= 1 (funcall (<=>-reverse #'<=>) 1 2)))
  (5am:is (= 0 (funcall (<=>-reverse #'<=>) #\a #\a)))
  (5am:is (= -1 (funcall (<=>-reverse #'<=>-ignore-case) "SECOND" "first"))))

(5am:test test-insertion-sort
  (5am:is (sorted? (sort (random-num-array 200 1000) :type :insertion)))
  (5am:is (equalp (vector "BUckle" "mY" "ONE" "SHOe" "two")
	      (sort (vector "ONE" "two" "BUckle" "mY" "SHOe") 
		    :cmp #'<=>-ignore-case :type :insertion)))
  (5am:is (sorted? (sort (random-string-array 300 10) :type :insertion))))

(5am:test test-partition!
  (let* ((vec (vector 15 9 8 1 4 11 7 12 13 6 5 3 16 2 10 14))
	 (pindex (partition! vec #'<=> 0 15 9)))
    (5am:is (= 5 pindex))
    (5am:is (equalp (vector 1 4 5 3 2 6 7 12 13 14 8 15 16 9 10 11) vec))))

(5am:test test-median-sort
  (5am:is (sorted? (sort (random-num-array 100 500) :type :median)))
  (5am:is (sorted? (sort (random-string-array 100 20) :type :median))))

(5am:test test-quick-sort
  (5am:is (sorted? (sort (random-num-array 100 500) :type :median)))
  (5am:is (sorted? (sort (random-string-array 100 20) :type :median))))

(5am:test test-selection-sort
  (5am:is (sorted? (sort (random-num-array 100 500) :type :selection)))
  (5am:is (sorted? (sort (random-string-array 100 20) :type :selection))))

(5am:test test-heap-sort
  (5am:is (sorted? (sort (random-num-array 100 500) :type :heap)))
  (5am:is (sorted? (sort (random-string-array 100 20) :type :heap))))

(5am:test test-counting-sort
  (5am:is (sorted? (sort (random-num-array 200 200) :aux 200 :type :counting)))
  (5am:is (sorted? (sort (random-num-array 100 100) :type :counting))))

(5am:test test-hash-sort
  (let ((hash (lambda (num element)
		(truncate (* num element)))))
    (5am:is (sorted? (sort (random-num-array 100 1.0) :aux hash :type :hash)))
    (5am:is (sorted? (sort (random-num-array 1000 1.0) :aux hash :type :hash)))))

(5am:test test-merge-sort
  (5am:is (sorted? (sort (random-num-array 100 500) :type :merge)))
  (5am:is (sorted? (sort (random-string-array 100 25) :type :merge))))
