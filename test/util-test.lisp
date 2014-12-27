(in-package #:dwcalgorithms)

(5am:def-suite util-suite :description "Util Test Suite")
(5am:in-suite util-suite)

(5am:test test-shift-left!
  (let ((zero-element (make-array 5 :adjustable t :fill-pointer 0))
	(one-element (make-array 1 :initial-contents '(1) :adjustable t :fill-pointer 1))
	(ten-element (make-array 10 :initial-contents '(1 2 3 4 5 6 7 8 9 10) 
				 :adjustable t :fill-pointer 10)))
    (5am:is (null (shift-left! zero-element)))
    (5am:is (equalp 1 (shift-left! one-element)))
    (5am:is (= 0 (length one-element)))

    (5am:is (= 1 (shift-left! ten-element)))
    (5am:is (= 9 (length ten-element)))
    (5am:is (= 2 (shift-left! ten-element)))
    (5am:is (= 8 (length ten-element)))))

(5am:test test-comparisons
  (let ((cmp #'<=>))
    
    (5am:is (<? cmp 1 2))
    (5am:is (not (<? cmp 2 2)))
    (5am:is (not (<? cmp 4 2)))

    (5am:is (>? cmp 2 1))
    (5am:is (not (>? cmp 2 2)))
    (5am:is (not (>? cmp 2 3)))

    (5am:is (=? cmp 2 2))
    (5am:is (not (=? cmp 2 3)))
    (5am:is (not (=? cmp 3 2)))

    (5am:is (<=? cmp 2 3))
    (5am:is (<=? cmp 2 2))
    (5am:is (not (<=? cmp 2 1)))

    (5am:is (>=? cmp 3 2))
    (5am:is (>=? cmp 3 3))
    (5am:is (not (>=? cmp 3 4)))))
