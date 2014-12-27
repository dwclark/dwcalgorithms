(in-package #:dwcalgorithms)

(5am:def-suite heap-suite :description "Heap Test Suite")
(5am:in-suite heap-suite)

(5am:test test-parent-index
  (5am:is (= 0 (parent-index 0)))
  (5am:is (= 0 (parent-index 1)))
  (5am:is (= 0 (parent-index 2)))
  (5am:is (= 1 (parent-index 3)))
  (5am:is (= 1 (parent-index 4)))
  (5am:is (= 2 (parent-index 5)))
  (5am:is (= 2 (parent-index 6)))
  (5am:is (= 4 (parent-index 9))))

(5am:test test-left-index
  (5am:is (= 1 (left-index 0)))
  (5am:is (= 7 (left-index 3)))
  (5am:is (= 5 (left-index 2))))

(5am:test test-right-index
  (5am:is (= 2 (right-index 0)))
  (5am:is (= 4 (right-index 1)))
  (5am:is (= 8 (right-index 3))))

(5am:test test-enqueue-min-heap
  (let ((dyna (make-instance 'heap)))
    (enqueue dyna 20)
    (5am:is (equalp (vector 20) (ary dyna)))
    (enqueue dyna 25)
    (5am:is (equalp (vector 20 25) (ary dyna)))
    (enqueue dyna 7)
    (5am:is (equalp (vector 7 25 20) (ary dyna)))
    (enqueue dyna 30)
    (5am:is (equalp (vector 7 25 20 30) (ary dyna)))
    (enqueue dyna 1)
    (5am:is (equalp (vector 1 7 20 30 25) (ary dyna)))))

(5am:test test-enqueue-max-heap
  (let ((dyna (make-instance 'heap :cmp-func (<=>-reverse #'<=>))))
    (enqueue dyna 20)
    (5am:is (equalp (vector 20) (ary dyna)))
    (enqueue dyna 25)
    (5am:is (equalp (vector 25 20) (ary dyna)))
    (enqueue dyna 7)
    (5am:is (equalp (vector 25 20 7) (ary dyna)))
    (enqueue dyna 30)
    (5am:is (equalp (vector 30 25 7 20) (ary dyna)))
    (enqueue dyna 1)
    (5am:is (equalp (vector 30 25 7 20 1) (ary dyna)))))

(5am:test test-dequeue-min-heap
  (let* ((ary (make-array 5 :initial-contents '(1 7 20 30 25) :adjustable t :fill-pointer 5))
	 (the-heap (make-instance 'heap :init-ary ary)))
    (loop for should-be in (list 1 7 20 25 30)
       do (5am:is (= should-be (dequeue the-heap))))
    (5am:is (null (dequeue the-heap)))
    (5am:is (= 0 (size the-heap)))))

(5am:test test-dequeue-max-heap
  (let* ((ary (make-array 5 :initial-contents '(30 25 7 20 1) :adjustable t :fill-pointer 5))
	 (the-heap (make-instance 'heap :init-ary ary :cmp-func (<=>-reverse #'<=>))))
    (loop for should-be in (list 30 25 20 7 1)
       do (5am:is (= should-be (dequeue the-heap))))
    (5am:is (null (dequeue the-heap)))
    (5am:is (= 0 (size the-heap)))))

(5am:test test-initialize
  (let* ((min-ary (make-array 5 :initial-contents '(20 25 7 30 1) :adjustable t :fill-pointer 5))
	 (max-ary (make-array 5 :initial-contents '(20 25 7 30 1) :adjustable t :fill-pointer 5)) 
 	 (min-heap (make-instance 'heap :init-ary min-ary :build? t))
	 (max-heap (make-instance 'heap :init-ary max-ary :build? t :cmp-func (<=>-reverse #'<=>))))
    (loop for item in (list 1 7 20 25 30)
       do (5am:is (= item (dequeue min-heap))))
    
    (loop for item in (list 30 25 20 7 1)
       do (5am:is (= item (dequeue max-heap))))))
    
(5am:test test-sort
  (let* ((min-ary (vector 1 3 5 7 9 2 4 6 8 10))
	 (max-ary (vector 1 3 5 7 9 2 4 6 8 10))
	 (min-heap (make-instance 'heap :init-ary min-ary :build? t))
	 (max-heap (make-instance 'heap :init-ary max-ary :build? t :cmp-func (<=>-reverse #'<=>))))
    
    (heap->sorted min-heap)
    (heap->sorted max-heap)
    (5am:is (equalp (vector 10 9 8 7 6 5 4 3 2 1) (ary min-heap)))
    (5am:is (equalp (vector 1 2 3 4 5 6 7 8 9 10) (ary max-heap)))))
    
