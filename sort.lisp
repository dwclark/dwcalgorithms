(in-package #:dwcalgorithms)

(defmethod <=> ((first real) (second real))
  (cond 
    ((< first second) -1)
    ((= first second) 0)
    (t 1)))

(defmethod <=> ((first string) (second string))
  (cond 
    ((string< first second) -1)
    ((string= first second) 0)
    (t 1)))

(defmethod <=> ((first character) (second character))
  (cond 
    ((char< first second) -1)
    ((string= first second) 0)
    (t 1)))

(defmethod <=>-ignore-case ((first string) (second string))
  (cond 
    ((string-lessp first second) -1)
    ((string-equal first second) 0)
    (t 1)))

(defmethod <=>-ignore-case ((first character) (second character))
  (cond 
    ((char-lessp first second) -1)
    ((char-equal first second) 0)
    (t 1)))

(defun sorted? (vec &key (cmp #'<=>))
  (loop 
     with result = t
     for index from 1 below (length vec)
     while (not (null result))
     do (setf result (<=? cmp (aref vec (1- index)) (aref vec index)))
     finally (return result)))

(defun <=>-reverse (cmp)
  (lambda (first second)
    (let ((val (funcall cmp first second)))
      (cond 
        ((= val -1) 1)
        ((= val 1) -1)
        (t 0)))))

(defun sort (vec &key (cmp #'<=>) (type :quick) (in-place t) (aux nil))
  (let ((ary (if in-place vec (copy-array vec))))
    (ecase type
      (:quick (quick-sort! ary cmp))
      (:insertion (insertion-sort! ary cmp :start-index (if aux aux 1)))
      (:selection (selection-sort! ary cmp))
      (:median (median-sort! ary cmp))
      (:heap (heap-sort! ary cmp))
      (:counting (if aux (counting-sort! ary aux) (counting-sort! ary)))
      (:hash (hash-sort! ary cmp aux))
      (:merge (merge-sort! ary cmp)))
    ary))

(defun insertion-sort! (vec cmp &key start-index)
  (loop 
     with i = 0
     for j from start-index below (length vec)
     do (progn
          (setf i (1- j))
          (loop 
             with value = (aref vec j)
             while (and (> i -1) (>=? cmp (aref vec i) value))
             do (progn
                  (setf (aref vec (1+ i)) (aref vec i))
                  (decf i))
             finally (setf (aref vec (1+ i)) value))))
  vec)

(defun partition! (vec cmp left right pivot-index)
  (let ((pivot (aref vec pivot-index)))
    (swap! vec right pivot-index)
    (loop with store = left
       for idx from left below right
       do (progn
            (when (<=? cmp (aref vec idx) pivot)
              (swap! vec idx store)
              (incf store)))
       finally (progn
                 (swap! vec right store)
                 (return store)))))

(defun random-in-interval (left right)
  (if (= left right)
      left
      (+ left (random (- right left)))))

(defun select-kth! (vec cmp k left right)
  (let* 
      ((idx (random-in-interval left right))
       (pivot-index (partition! vec cmp left right idx))
       (test-for (1- (+ left k))))
    
    (cond ((= pivot-index test-for) pivot-index)
          ((< test-for pivot-index) (select-kth! vec cmp k left (1- pivot-index)))
          (t (select-kth! vec cmp (- k (+ (- pivot-index left) 1)) (1+ pivot-index) right)))))
	  
(defun median-sort! (vec cmp)
  (labels 
      ((internal-median-sort (left right)
         (if (not (<= right left))
             (let ((mid (truncate (/ (+ (- right left) 1) 2))))
               (select-kth! vec cmp (1+ mid) left right)
               (internal-median-sort left (- (+ left mid) 1))
               (internal-median-sort (+ left mid 1) right)))))

    (internal-median-sort 0 (1- (length vec))))
  vec)

(defun quick-sort! (vec cmp)
  (labels 
      ((internal-quick-sort (left right)
	     (if (> right left)
             (let ((pivot-index (partition! vec cmp left right (random-in-interval left right))))
               (internal-quick-sort left (1- pivot-index))
               (internal-quick-sort (1+ pivot-index) right)))))
    
    (internal-quick-sort 0 (1- (length vec)))))

(defun selection-sort! (vec cmp)
  (labels 
      ((next-minimum-position (start)
         (loop 
            with pos = start
            for i from (1+ start) below (length vec)
            if (<? cmp (aref vec i) (aref vec pos))
            do (setf pos i)
            finally (return pos))))
    
    (loop 
       for i from 0 below (1- (length vec))
       do (let ((the-min (next-minimum-position i)))
            (if (not (= the-min i))
                (swap! vec the-min i))))
    vec))

(defun heap-sort! (vec cmp)
  (heap->sorted (make-instance 'heap :init-ary vec :build? t
			       :cmp-func (<=>-reverse cmp))))

(defun counting-sort! (vec &optional (k (reduce (lambda (x y) (max x y)) vec)))
  (let ((tmp (make-array (1+ k) :initial-element 0)))
    (loop 
       for val across vec
       do (incf (aref tmp val)))
    (loop 
       with idx = 0
       for tmpidx from 0 below (length tmp)
       do (dotimes (i (aref tmp tmpidx))
            (setf (aref vec idx) tmpidx)
            (incf idx))))
  vec)

(defun hash-sort! (vec cmp hash)
  (let ((working (make-array (length vec) :initial-element nil)))
    (loop 
       for element across vec
       do (let ((the-hash (funcall hash (length vec) element)))
            (cl:push element (aref working the-hash))))

    (loop 
       with idx = 0
       for lst across working
       do (cond 
            ;case of null list at position 
            ((null lst) nil)
            
            ;single element list, just add to next slot
            ((= 1 (length lst))
             (setf (aref vec idx) (car lst))
             (incf idx))
            
            ;multiple element list, need to do a quick insertion sort
            ((< 1 (length lst))
             (let ((ary (make-array (length lst) :initial-contents lst)))
               (loop for element across (insertion-sort! ary cmp :start-index 1)
                  do (progn
                       (setf (aref vec idx) element)
                       (incf idx))))))))
  vec)

(defun merge-sort! (vec cmp)
  ;Allocate temporary arrays for the sort. 
  (let* 
      ((tmp-size (ceiling (/ (length vec) 2)))
       (left-array (make-array tmp-size :initial-element nil))
       (right-array (make-array tmp-size :initial-element nil)))
    
    (labels
        ;utility function for copying working data into the temporary arrays
        ((inner-copy (target lower upper)
           (loop for i from lower below upper
              do (setf (aref target (- i lower)) (aref vec i))))
         
         ;Do the actual merge here
         (inner-merge (lower-bound pivot upper-bound)
           (let ((left-size (- pivot lower-bound))
                 (right-size (- upper-bound pivot)))
             (inner-copy left-array lower-bound pivot)
             (inner-copy right-array pivot upper-bound)
             (loop
                with left-i = 0
                with right-i = 0
                for i from lower-bound below upper-bound
                do  (labels
                        ((choose-left ()
                           (setf (aref vec i) (aref left-array left-i))
                           (incf left-i))
                         (choose-right ()
                           (setf (aref vec i) (aref right-array right-i))
                           (incf right-i)))

                      (cond 
                        ((= right-i right-size) (choose-left))
                        
                        ((= left-i left-size) (choose-right))
                        
                        ((<? cmp (aref left-array left-i) (aref right-array right-i)) (choose-left))
                        
                        (t (choose-right)))))))
         
         ;This is the main merge sort algorithm
         (inner-merge-sort (lower-bound upper-bound)
           (if (< (1+ lower-bound) upper-bound)
               (let ((pivot (floor (/ (+ upper-bound lower-bound) 2))))
                 (inner-merge-sort lower-bound pivot)
                 (inner-merge-sort pivot upper-bound)
                 (inner-merge lower-bound pivot upper-bound)))))
      
      ;Start the actual merge sort
      (inner-merge-sort 0 (length vec))
      vec)))
