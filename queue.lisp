(in-package #:dwcalgorithms)

(defconstant +initial-queue-size+ 10)

(defclass vector-queue ()
  ((ary)
   (cmp)))

(defmethod initialize-instance :after ((the-queue vector-queue) 
				       &key (init-size +initial-queue-size+)
					 (priority-cmp #'<=>))
  (with-slots (ary cmp) the-queue
    (setf ary (make-array init-size :adjustable t :fill-pointer 0))
    (setf cmp priority-cmp)))

(defmethod peek ((the-q vector-queue))
  (with-slots (ary) the-q
    (if (> (length ary) 0)
	(aref ary 0)
	nil)))

(defmethod enqueue ((the-q vector-queue) element)
  (with-slots (ary cmp) the-q
    (vector-push-extend element ary)
    (if (not (null cmp))
	(insertion-sort! ary cmp :start-index (1- (length ary)))))
  the-q)

(defmethod dequeue ((the-q vector-queue))
  (with-slots (ary) the-q
    (shift-left! ary)))

(defclass heap-queue ()
  ((the-heap)))

(defmethod initialize-instance :after 
    ((the-queue heap-queue) &key (init-size +initial-queue-size+) (priority-cmp #'<=>))
    (with-slots (the-heap) the-queue
      (setf the-heap (make-instance 'heap :init-size init-size :cmp-func priority-cmp))))
			      
(defmethod peek ((the-q heap-queue))
  (with-slots (the-heap) the-q
    (with-slots (ary size) the-heap
      (if (= 0 size)
	  nil
	  (aref ary 0)))))

(defmethod enqueue ((the-q heap-queue) element)
  (with-slots (the-heap) the-q
    (enqueue the-heap element)))

(defmethod dequeue ((the-q heap-queue))
  (with-slots (the-heap) the-q
    (dequeue the-heap)))

