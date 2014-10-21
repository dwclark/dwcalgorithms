(in-package #:dwcalgorithms)

;by default is a min-heap.
;to convert to max-heap, swap order of comparisons
(defclass heap ()
  ((ary :reader ary)
   (size :reader size :initform 0)
   (cmp)))

(defmethod initialize-instance :after 
    ((the-heap heap) &key (cmp-func #'<=>) (init-size 10)
		     (init-ary nil) (build? nil))
  (with-slots (ary cmp size) the-heap
    (setf cmp cmp-func)
    (if init-ary
	(progn
	  (setf ary init-ary)
	  (setf size (length ary))
	  (if build?
	      (build the-heap)))
	
	(progn
	  (setf ary (make-array init-size :adjustable t :fill-pointer 0))))))

(defmacro parent-index (index)
  (once-only (index)
    `(truncate (/ (1- ,index) 2))))

(defgeneric parent-value (the-heap index))
(defmethod parent-value ((the-heap heap) index)
  (with-slots (ary) the-heap
    (aref ary (parent-index index))))

(defmacro left-index (index)
  (once-only (index)
    `(+ 1 (* 2 ,index))))

(defgeneric left-value (the-heap index))
(defmethod left-value ((the-heap heap) index)
  (with-slots (ary) the-heap
    (aref ary (left-index index))))

(defmacro right-index (index)
  (once-only (index)
    `(+ 2 (* 2 ,index))))

(defgeneric right-value (the-heap index))
(defmethod right-value ((the-heap heap) index)
  (with-slots (ary) the-heap
    (aref ary (right-index index))))

(defmethod enqueue ((the-heap heap) element)
  (with-slots (ary cmp size) the-heap
    (vector-push-extend element ary)
    (incf size)

    (let ((index (1- size)))
      (loop while (and (not (= 0 index))
		       (<=? cmp element (parent-value the-heap index)))
	 do (progn 
	      (swap! ary index (parent-index index))
	      (setf index (parent-index index)))))))

(defmethod dequeue ((the-heap heap))
  (with-slots (ary cmp size) the-heap
    (let ((ret nil))
      (if (not (= 0 size))
	  (progn
	    (setf ret (aref ary 0))
	    (swap! ary 0 (1- size))
	    (setf (fill-pointer ary) (1- (fill-pointer ary)))
	    (decf size)
	    (if (not (= 0 size))
		(heapify the-heap 0))))
      ret)))

(defgeneric heap->sorted (the-heap))
(defmethod heap->sorted ((the-heap heap))
  (with-slots (ary cmp size) the-heap
    (loop for i from (1- size) downto 1
       do (progn
	    (swap! ary 0 i)
	    (decf size)
	    (heapify the-heap 0)))))

(defmethod heapify ((the-heap heap) i)
  (with-slots (ary cmp size) the-heap
    (let ((left (left-index i))
	  (right (right-index i))
	  (extremum i))
      
      (if (and (< left size) (<? cmp (aref ary left) (aref ary extremum)))
	  (setf extremum left))
      
      (if (and (< right size) (<? cmp (aref ary right) (aref ary extremum)))
	  (setf extremum right))
      
      (if (not (= extremum i))
	  (progn 
	    (swap! ary extremum i)
	    (heapify the-heap extremum))))))

(defmethod build ((the-heap heap))
  (with-slots (ary cmp size) the-heap
    (loop with n = size
       for i from (1- (truncate (/ n 2))) downto 0
       do (heapify the-heap i))))
