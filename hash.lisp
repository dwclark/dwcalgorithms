(in-package #:dwcalgorithms)

(defclass hash-search ()
  ((table)
   (filled-buckets :initform 0)
   (hash-func :initform #'sxhash :initarg :hash-func)
   (equal-func :initform #'equal :initarg :equal-func)
   (load-factor :initform 0.75 :initarg :load-factor)
   (size :initform 0 :reader size)))

(defun standard-initialize (hs capacity elements)
  (let ((tmp-size (cond 
                    ((not (null elements)) (truncate (* 1.5 (length elements))))
                    (t capacity))))
    (setf (slot-value hs 'table) (make-array tmp-size :initial-element nil))
    (if (not (null elements))
        (reduce #'insert elements :initial-value hs))))

(defun hash-position (hash-collection element)
  (with-slots (table hash-func) hash-collection
    (mod (funcall hash-func element) (length table))))

(defun exceeds-load-factor (hash-collection)
  (with-slots (table filled-buckets load-factor) hash-collection
    (< load-factor (/ filled-buckets (length table)))))

(defun rehash (hash-collection)
  (with-slots (table filled-buckets size) hash-collection
    (let ((tmp table))
      (setf table (make-array (* 2 (length tmp)) :initial-element nil))
      (setf filled-buckets 0)
      (setf size 0)
      (loop 
         for lst across tmp
         do (if (not (null lst))
                (loop 
                   for e in lst
                   do (put-element-internal hash-collection e)))))))

(defun put-element-internal (hash-collection element)
  (with-slots (table filled-buckets allow-duplicates size) hash-collection
    (let ((pos (hash-position hash-collection element)))
      (if (null (aref table pos))
          (progn
            (cl:push element (aref table pos))
            (incf filled-buckets)
            (incf size))
          (place-in-list hash-collection pos element)))))

(defmethod print-object ((hs hash-search) stream)
  (print-unreadable-object (hs stream :type t)
    (with-slots (table size filled-buckets) hs
      (format stream "~&size: ~a, filled-buckets: ~a~&table: ~a" 
              size filled-buckets table))))

(defmethod initialize-instance :after ((hs hash-search) &key (capacity 100) (elements nil))
  (if (eq 'hash-search (type-of hs))
      (standard-initialize hs capacity elements)))

(defgeneric place-in-list (h pos e))
(defmethod place-in-list ((h hash-search) pos e)
  (with-slots (table size) h
    (cl:push e (aref table pos))
    (incf size)))

(defgeneric get-from-list (hs pos e))
(defmethod get-from-list ((hs hash-search) pos e)
  (with-slots (table equal-func) hs
    (remove-if-not (lambda (to-test)
                     (funcall equal-func e to-test)) (aref table pos))))

(defmethod insert ((hs hash-search) e)
  (if (exceeds-load-factor hs)
      (rehash hs))
  
  (put-element-internal hs e)
  hs)

(defmethod search ((hs hash-search) e)
  (with-slots (table equal-func) hs
    (let ((pos (hash-position hs e)))
      (cond 
        ((null (aref table pos)) nil)
        (t (get-from-list hs pos e))))))

(defmethod delete ((hs hash-search) e)
  (with-slots (table equal-func size filled-buckets) hs
    (let* 
        ((pos (hash-position hs e))
         (num (count-if (lambda (x)
                          (funcall equal-func e x)) (aref table pos))))
      (if (< 0 num)
          (progn
            (setf (aref table pos)
                  (remove-if (lambda (x)
                               (funcall equal-func e x)) (aref table pos)))
            (decf size num)
            (if (null (aref table pos))
                (decf filled-buckets)))))))

(defclass hash-set (hash-search) ())

(defmethod initialize-instance :after ((hs hash-set) &key (capacity 100) (elements nil))
  (if (eq 'hash-set (type-of hs))
      (standard-initialize hs capacity elements)))

(defmethod place-in-list ((h hash-set) pos e)
  (with-slots (table size equal-func) h
    (if (null (find-if (lambda (x)
                         (funcall equal-func x e)) (aref table pos)))
        (progn
          (cl:push e (aref table pos))
          (incf size)))))

(defmethod get-from-list ((h hash-set) pos e)
  (with-slots (table equal-func) h
    (find-if (lambda (x)
               (funcall equal-func x e)) (aref table pos))))

(defclass hash-map (hash-set) ())

(defun initialize-map-functions (hm)
  (with-slots (hash-func equal-func) hm
    (let ((old-hash-func hash-func)
          (old-equal-func equal-func))
      (setf hash-func (lambda (cell)
                        (funcall old-hash-func (car cell))))
      (setf equal-func (lambda (cell-1 cell-2)
                         (funcall old-equal-func (car cell-1) (car cell-2)))))))

(defmethod initialize-instance :after ((hm hash-map) &key (capacity 100) (elements nil))
  (initialize-map-functions hm)
  
  (if (eq 'hash-map (type-of hm))
      (standard-initialize hm capacity elements)))

(defmethod place-in-list ((h hash-map) pos e)
  (with-slots (table size equal-func) h
    (let ((cell (find-if (lambda (cell)
                           (funcall equal-func cell e)) (aref table pos))))
      (cond 
        ((null cell)
         (cl:push e (aref table pos))
         (incf size))
        
        (t (setf (cdr cell) (cdr e)))))))

(defmethod delete ((hm hash-map) key)
  (call-next-method hm (cons key nil)))

(defmethod [] ((hm hash-map) key)
  (let ((cell (search hm (cons key nil))))
    (if (not (null cell))
        (cdr cell)
        nil)))

(defmethod (setf []) (val (hm hash-map) key)
  (insert hm (cons key val)))

(defclass multi-hash-map (hash-search) ())

(defmethod initialize-instance :after ((hm multi-hash-map) &key (capacity 100) (elements nil))
  (initialize-map-functions hm)
  
  (if (eq 'multi-hash-map (type-of hm))
      (standard-initialize hm capacity elements)))

(defmethod [] ((hm multi-hash-map) key)
  (let ((lst (search hm (cons key nil))))
    (if lst
        (mapcar #'cdr lst)
        nil)))

(defmethod (setf []) (val (hm multi-hash-map) key)
  (insert hm (cons key val)))

(defmethod delete ((hm multi-hash-map) key)
  (call-next-method hm (cons key nil)))
