(in-package #:dwcalgorithms)

(defclass hash-search ()
  ((table)
   (filled-buckets :initform 0)
   (hash-func :initform #'sxhash :initarg :hash-func)
   (equal-func :initform #'equal :initarg :equal-func)
   (load-factor :initform 0.75 :initarg :load-factor)
   (size :initform 0 :reader size)))

(defmethod print-object ((hs hash-search) stream)
  (print-unreadable-object (hs stream :type t)
    (with-slots (table size filled-buckets) hs
      (format stream "~&size: ~a, filled-buckets: ~a~&table: ~a" 
	      size filled-buckets table))))

(defun standard-initialize (hs capacity elements)
  (let ((tmp-size (cond ((not (null elements)) (truncate (* 1.5 (length elements))))
			(t capacity))))
    (setf (slot-value hs 'table) (make-array tmp-size :initial-element nil))
    (if (not (null elements))
	(put-all-elements hs elements))))

(defmethod initialize-instance :after ((hs hash-search) &key (capacity 100) (elements nil))
  (if (eq 'hash-search (type-of hs))
      (standard-initialize hs capacity elements)))
      
(defgeneric hash-position (h e))
(defmethod hash-position ((h hash-search) e)
  (with-slots (table hash-func) h
    (mod (funcall hash-func e) (length table))))

(defgeneric exceeds-load-factor (h))
(defmethod exceeds-load-factor ((h hash-search))
  (with-slots (table filled-buckets load-factor) h
    (< load-factor (/ filled-buckets (length table)))))

(defgeneric place-in-list (h pos e))
(defmethod place-in-list ((h hash-search) pos e)
  (with-slots (table size) h
    (push e (aref table pos))
    (incf size)))

(defgeneric put-element (hs e))
(defmethod put-element ((hs hash-search) e)
  (if (exceeds-load-factor hs)
      (rehash hs))

  (put-element-internal hs e))

(defun put-element-internal (hs e)
  (with-slots (table filled-buckets allow-duplicates size) hs
    (let ((pos (hash-position hs e)))
      (if (null (aref table pos))
	  (progn
	    (push e (aref table pos))
	    (incf filled-buckets)
	    (incf size))
	  (place-in-list hs pos e)))))

(defgeneric put-all-elements (hs lst))
(defmethod put-all-elements ((hs hash-search) elements)
  (loop for element in elements do (put-element hs element)))

(defgeneric get-from-list (hs pos e))
(defmethod get-from-list ((hs hash-search) pos e)
  (with-slots (table equal-func) hs
    (remove-if-not (lambda (to-test)
		     (funcall equal-func e to-test)) (aref table pos))))

(defgeneric get-element (hs e))
(defmethod get-element ((hs hash-search) e)
  (with-slots (table equal-func) hs
    (let ((pos (hash-position hs e)))
      (cond ((null (aref table pos)) nil)
	    (t (get-from-list hs pos e))))))

(defgeneric remove-element (hs e))
(defmethod remove-element ((hs hash-search) e)
  (with-slots (table equal-func size filled-buckets) hs
    (let* ((pos (hash-position hs e))
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

(defgeneric remove-all-elements (hs lst))
(defmethod remove-all-elements ((hs hash-search) lst)
  (loop for element in lst do (remove-element hs element)))

(defgeneric rehash (h))
(defmethod rehash ((h hash-search))
  (with-slots (table filled-buckets size) h
    (let ((tmp table))
      (setf table (make-array (* 2 (length tmp)) :initial-element nil))
      (setf filled-buckets 0)
      (setf size 0)
      (loop for lst across tmp
	 do (if (not (null lst))
		(loop for e in lst
		   do (put-element-internal h e)))))))

(defclass hash-set (hash-search) ())

(defmethod initialize-instance :after ((hs hash-set) &key (capacity 100) (elements nil))
  (if (eq 'hash-set (type-of hs))
      (standard-initialize hs capacity elements)))

(defmethod place-in-list ((h hash-set) pos e)
  (with-slots (table size equal-func) h
    (if (null (find-if (lambda (x)
			 (funcall equal-func x e)) (aref table pos)))
	(progn
	  (push e (aref table pos))
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
      (cond ((null cell)
	     (push e (aref table pos))
	     (incf size))

	    (t (setf (cdr cell) (cdr e)))))))

(defmethod remove-element ((hm hash-map) e)
  (call-next-method hm (cons e nil)))

(defmethod [] ((hm hash-map) key)
  (let ((cell (get-element hm (cons key nil))))
    (if (not (null cell))
	(cdr cell)
	nil)))

(defmethod (setf []) (val (hm hash-map) key)
  (put-element hm (cons key val)))

(defclass multi-hash-map (hash-search) ())

(defmethod initialize-instance :after ((hm multi-hash-map) &key (capacity 100) (elements nil))
  (initialize-map-functions hm)
  
  (if (eq 'multi-hash-map (type-of hm))
      (standard-initialize hm capacity elements)))

(defmethod [] ((hm multi-hash-map) key)
  (let ((lst (get-element hm (cons key nil))))
    (if lst
	(mapcar #'cdr lst)
	nil)))

(defmethod (setf []) (val (hm multi-hash-map) key)
  (put-element hm (cons key val)))

(defmethod remove-element ((hm multi-hash-map) e)
  (call-next-method hm (cons e nil)))
