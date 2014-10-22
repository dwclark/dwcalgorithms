(in-package #:dwcalgorithms)

(defun linear-search (vec test-for cmp)
  (loop 
     for idx from 0 below (length vec)
     do (if (=? cmp test-for (aref vec idx))
            (return idx))
     finally (return nil)))

(defun binary-search (vec test-for cmp)
  (let* 
      ((low 0)
       (high (1- (length vec))))
    (loop 
       while (<= low high)
       do (let* ((idx (ash (+ high low) -1))
                 (result (funcall cmp test-for (aref vec idx))))
            (cond 
              ((< result 0) (setf high (1- idx)))
              ((> result 0) (setf low (1+ idx)))
              (t (return-from binary-search idx))))))
  nil)

(defclass comparable-array ()
  ((ary :initarg :ary :initform (make-array 10 :fill-pointer 0 :adjustable t) :reader ary)
   (cmp :initarg :cmp :initform #'<=> :reader cmp)))

(defmethod size ((vec comparable-array))
  (length (ary vec)))

(defclass sorted-array (comparable-array) ())

(defclass sorted-array-set (sorted-array) ())

(defclass sorted-map (sorted-array) ())

(defmethod initialize-instance :after ((vec sorted-array) &key)
  (if (eq (type-of vec) 'sorted-array)
      (with-slots (ary cmp) vec
        (if (not (sorted? ary :cmp cmp))
            (sort ary :cmp cmp)))))

(defmethod initialize-instance :after ((vec sorted-array-set) &key)
  (if (eq (type-of vec) 'sorted-array-set)
      (with-slots (ary cmp) vec
        (let* ((tmp ary))
          (setf ary (make-array (length tmp) :fill-pointer 0 :adjustable t))
          (reduce #'insert tmp :initial-value vec)))))

(defmethod initialize-instance :after ((vec sorted-map) &key)
  (if (eq (type-of vec) 'sorted-map)
      (progn
        (with-slots (cmp) vec
          (let ((old-cmp cmp))
            (setf cmp (lambda (one two)
                        (funcall old-cmp (car one) (car two))))))
        
        (with-slots (ary) vec
          (let ((tmp ary))
            (setf ary (make-array (length tmp) :fill-pointer 0 :adjustable t))
            (reduce #'insert tmp :initial-value vec))))))

(defmethod search ((vec comparable-array) value)
  (with-slots (ary cmp) vec
    (let ((idx (linear-search ary value cmp)))
      (if idx
          (aref ary idx)
          nil))))

(defmethod search ((vec sorted-array) value)
  (with-slots (ary cmp) vec
    (let ((idx (binary-search ary value cmp)))
      (if idx
          (aref ary idx)
          nil))))

(defmethod insert ((vec comparable-array) value)
  (with-slots (ary sorted) vec
    (vector-push-extend value ary))
  vec)

(defmethod insert ((vec sorted-array) value)
  (with-slots (ary cmp) vec
    (vector-push-extend value ary)
    (sort ary :type :insertion :cmp cmp :aux (1- (length ary))))
  vec)

(defmethod insert ((vec sorted-array-set) value)
  (with-slots (ary cmp) vec
    (if (null (binary-search ary value cmp))
        (call-next-method)))
  vec)

(defmethod insert ((vec sorted-map) cell)
  (with-slots (ary cmp) vec
    (let ((idx (binary-search ary cell cmp)))
      (if (null idx)
          (call-next-method)
          (setf (aref ary idx) cell)))))

(defmethod [] ((vec sorted-map) key)
  (let ((cell (search vec (cons key nil))))
    (if (not (null cell))
        (cdr cell)
        nil)))

(defmethod (setf []) (val (vec sorted-map) key)
  (insert vec (cons key val)))

(defmethod delete ((vec comparable-array) value)
  (with-slots (ary) vec
    (setf ary (cl:delete value ary)))
  vec)

(defmethod minimum ((vec comparable-array))
  (with-slots (ary cmp) vec
    (if (< 0 (length ary))
        
        (let ((min (aref ary 0)))
          (loop 
             for i from 1 below (length ary)
             do (if (<? cmp (aref ary i) min)
                    (setf min (aref ary i)))
             finally (return min)))
        
        nil)))

(defmethod minimum ((vec sorted-array))
  (with-slots (ary cmp) vec
    (if (< 0 (length ary))
        (aref ary 0)
        nil)))

(defmethod kth ((vec comparable-array) k)
  (let ((idx (select-kth! (ary vec) (cmp vec) k 0 (1- (size vec)))))
    (aref (ary vec) idx)))

(defmethod kth ((vec sorted-array) k)
  (aref (ary vec) (1- k)))

(defmethod median ((vec comparable-array))
  (let ((k (ash (size vec) -1)))
    (kth vec k)))

(defmethod maximum ((vec comparable-array))
  (with-slots (ary cmp) vec
    (if (< 0 (length ary))
        
        (let ((max (aref ary 0)))
          (loop 
             for i from 1 below (length ary)
             do (if (>? cmp (aref ary i) max)
                    (setf max (aref ary i)))
             finally (return max)))

        nil)))

(defmethod maximum ((vec sorted-array))
  (with-slots (ary cmp) vec
    (if (< 0 (length ary))
        (aref ary (1- (length ary)))
        nil)))

(defmethod successor ((vec comparable-array) value)
  (with-slots (ary cmp) vec
    (if (< 0 (length ary))
        
        (loop 
           with next = nil
           for i from 0 below (length ary)
           do (if (and (<? cmp value (aref ary i))
                       (or (null next)
                           (<? cmp (aref ary i) next)))
                  (setf next (aref ary i)))
           finally (return next))
        
        nil)))

(defmethod successor ((vec sorted-array) value)
  (with-slots (ary cmp) vec
    (if (< 0 (length ary))

        (let ((idx (binary-search ary value cmp))
              (result nil))
          (if (and (not (null idx))
                   (< idx (1- (length ary))))
              (loop 
                 with iter = (1+ idx)
                 while (and (null result) 
                            (< iter (length ary)))
                 do (if (>? cmp (aref ary iter) value)
                        (setf result (aref ary iter))
                        (incf iter))))
          result)

        nil)))

(defmethod predecessor ((vec comparable-array) value)
  (with-slots (ary cmp) vec
    (if (< 0 (length ary))
        
        (loop 
           with next = nil
           for i from 0 below (length ary)
           do (if (and (<? cmp (aref ary i) value)
                       (or (null next)
                           (<? cmp next (aref ary i))))
                  (setf next (aref ary i)))
           finally (return next))
        
        nil)))

(defmethod predecessor ((vec sorted-array) value)
  (with-slots (ary cmp) vec
    (if (< 0 (length ary))
        
        (let ((idx (binary-search ary value cmp))
              (result nil))
          (if (and (not (null idx))
                   (<= 0 idx))
              (loop 
                 with iter = (1- idx)
                 while (and (null result) 
                            (<= 0 iter))
                 do (if (<? cmp (aref ary iter) value)
                        (setf result (aref ary iter))
                        (incf iter))))
          result)
        
        nil)))
