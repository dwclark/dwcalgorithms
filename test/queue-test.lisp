(in-package #:dwcalgorithms)

(5am:def-suite queue-suite :description "Queue Test Suite")
(5am:in-suite queue-suite)

(5am:test test-vector-queue
  (let ((queues (list (make-instance 'vector-queue)
		      (make-instance 'vector-queue :init-size 20))))
    (loop for queue in queues
	 do (progn
	      (5am:is (null (dequeue queue)))
	      (enqueue queue 10)
	      (5am:is (= 10 (peek queue)))
	      (5am:is (= 10 (dequeue queue)))
	      (loop for i from 0 below 10
		   do (enqueue queue i))
	      (loop for i from 0 below 10
		   do (5am:is (= i (dequeue queue))))))))

(5am:test test-min-priorty-queue
  (let ((queues (list (make-instance 'vector-queue :priority-cmp #'<=>)
		      (make-instance 'heap-queue :priority-cmp #'<=>))))
    (loop for queue in queues
       do (let ((collector (make-array 10 :adjustable t :fill-pointer 0)))
	    (loop for i in (list 45 1 3 99 87 34 23 87 44 22 88 44 101 44 3 17)
	       do (enqueue queue i))
	    (loop until (null (peek queue))
	       do (vector-push-extend (dequeue queue) collector))
	    (5am:is (sorted? collector))))))

(5am:test test-max-priority-queue
  (let* ((cmp (<=>-reverse #'<=>))
	 (queues (list (make-instance 'vector-queue :priority-cmp cmp)
		       (make-instance 'heap-queue :priority-cmp cmp))))
    (loop for queue in queues
	 do (let ((collector (make-array 10 :adjustable t :fill-pointer 0)))
	      (loop for i in (list 45 1 3 99 87 34 23 87 44 22 88 44 101 44 3 17)
		 do (enqueue queue i))
	      (loop until (null (peek queue))
		 do (vector-push-extend (dequeue queue) collector))
	      (5am:is (sorted? collector :cmp cmp))))))
    
