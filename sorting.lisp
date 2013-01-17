(in-package #:dwcalgorithms)

(defgeneric <=> (first second))

(defmethod <=> ((first real) (second real))
  (cond ((< first second) -1)
	((= first second) 0)
	(t 1)))

(defmethod <=> ((first string) (second string))
  (cond ((string< first second) -1)
	((string= first second) 0)
	(t 1)))

(defmethod <=> ((first character) (second character))
  (cond ((char< first second) -1)
	((string= first second) 0)
	(t 1)))

(defgeneric <=>-ignore-case (first second))

(defmethod <=>-ignore-case ((first string) (second string))
  (cond ((string-lessp first second) -1)
	((string-equal first second) 0)
	(t 1)))

(defmethod <=>-ignore-case ((first character) (second character))
  (cond ((char-lessp first second) -1)
	((char-equal first second) 0)
	(t 1)))

(defun sorted? (vec func)
  (loop with result = t
     for index from 1 below (length vec)
     while (not (null result))
     do (setf result (not (= 1 (funcall func (aref vec (1- index)) (aref vec index)))))
     finally (return result)))

(defun <=>-reverse (func)
  (lambda (first second)
    (let ((val (funcall func first second)))
      (cond ((= val -1) 1)
	    ((= val 1) -1)
	    (t 0)))))

(defun insertion-sort! (vec func)
  (loop with i = 0
     for j from 1 below (length vec)
     do (progn
	  (setf i (1- j))
	  (loop with value = (aref vec j)
	       while (and (> i -1) (> (funcall func (aref vec i) value) 0))
	       do (progn
		    (setf (aref vec (1+ i)) (aref vec i))
		    (decf i))
	       finally (setf (aref vec (1+ i)) value))))
  vec)

(defun insertion-sort (vec func)
  (insertion-sort! (copy-array vec) func))

(defun partition! (vec func left right pivot-index)
  (let ((pivot (aref vec pivot-index)))
    (swap! vec right pivot-index)
    (loop with store = left
       for idx from left below right
       do (progn
	    (when (<= (funcall func (aref vec idx) pivot) 0)
	      (swap! vec idx store)
	      (incf store)))
       finally (progn
		 (swap! vec right store)
		 (return store)))))

(defun random-in-interval (left right)
  (if (= left right)
      left
      (+ left (random (- right left)))))

(defun select-kth! (vec func k left right)
  (let* ((idx (random-in-interval left right))
	 (pivot-index (partition! vec func left right idx))
	 (test-for (1- (+ left k))))
    (cond ((= pivot-index test-for) pivot-index)
	  ((< test-for pivot-index) (select-kth! vec func k left (1- pivot-index)))
	  (t (select-kth! vec func (- k (+ (- pivot-index left) 1)) (1+ pivot-index) right)))))
	  
(defun median-sort! (vec func)
  (labels ((internal-median-sort (left right)
	     (if (not (<= right left))
		 (let* ((mid (truncate (/ (+ (- right left) 1) 2)))
			(me (select-kth! vec func (1+ mid) left right)))
		   (internal-median-sort left (- (+ left mid) 1))
		   (internal-median-sort (+ left mid 1) right)))))
    (internal-median-sort 0 (1- (length vec))))
  vec)

(defun median-sort (vec func)
  (median-sort! (copy-array vec) func))

(defun quick-sort! (vec func)
  (labels ((internal-quick-sort (left right)
	     (if (> right left)
		 (let ((pivot-index (partition! vec func left right (random-in-interval left right))))
		   (internal-quick-sort left (1- pivot-index))
		   (internal-quick-sort (1+ pivot-index) right)))))
    (internal-quick-sort 0 (1- (length vec)))))

(defun quick-sort (vec func)
  (quick-sort! (copy-array vec) func))

(defun min-pos (vec func start)
  (loop with pos = start
     for i from (1+ start) below (length vec)
     do (if (> (funcall func (aref vec pos) (aref vec i)) 0)
	    (setf pos i))
     finally (return pos)))

(defun selection-sort! (vec func)
  (loop for i from 0 below (1- (length vec))
     do (let ((the-min (min-pos vec func i)))
	  (if (not (= the-min i))
	      (swap! vec the-min i))))
  vec)

(defun selection-sort (vec func)
  (selection-sort! (copy-array vec) func))

(defun heapify! (vec func idx max)
  (let ((left (+ 1 (* 2 idx)))
	(right (+ 2 (* 2 idx)))
	(largest nil))
    (if (and (< left max) (> (funcall func (aref vec left) (aref vec idx)) 0))
	(setf largest left)
	(setf largest idx))
    (if (and (< right max) (> (funcall func (aref vec right) (aref vec largest)) 0))
	(setf largest right))
    (if (not (= largest idx))
	(progn
	  (swap! vec largest idx)
	  (heapify! vec func largest max)))))

(defun build-heap! (vec func n)
  (loop for i from (1- (truncate (/ n 2))) downto 0
     do (heapify! vec func i n)))

(defun heap-sort! (vec func)
  (build-heap! vec func (length vec))
  (loop for i from (1- (length vec)) downto 1
     do (progn
	  (swap! vec 0 i)
	  (heapify! vec func 0 i)))
  vec)

(defun heap-sort (vec func)
  (heap-sort! (copy-array vec) func))
    
(defun counting-sort! (vec &optional (k (reduce (lambda (x y) (max x y)) vec)))
  (let ((tmp (make-array (1+ k) :initial-element 0)))
    (loop for val across vec
       do (incf (aref tmp val)))
    (loop with idx = 0
       for tmpidx from 0 below (length tmp)
       do (dotimes (i (aref tmp tmpidx))
	    (setf (aref vec idx) tmpidx)
	    (incf idx))))
  vec)

(defun counting-sort (vec &optional (k (reduce (lambda (x y) (max x y)) vec)))
  (counting-sort! (copy-array vec) k))

(defun hash-sort! (vec cmp hash)
  (let ((working (make-array (length vec) :initial-element nil)))
    (loop for element across vec
       do (let ((the-hash (funcall hash (length vec) element)))
	    (push element (aref working the-hash))))
    (loop with idx = 0
       for lst across working
       do (cond ((= 1 (length lst))
		 (setf (aref vec idx) (car lst))
		 (incf idx))
		((< 1 (length lst))
		 (let ((ary (make-array (length lst) :initial-contents lst)))
		   (loop for element across (insertion-sort! ary cmp)
		      do (progn
			   (setf (aref vec idx) element)
			   (incf idx))))))))
  vec)

(defun hash-sort (vec cmp hash)
  (hash-sort! (copy-array vec) cmp hash))
