(in-package #:dwcalgorithms)		

(defclass tree-map (red-black-tree) ())

(defmethod initialize-instance :after ((map tree-map) &key (elements nil))
  (let ((old-cmp (cmp map)))
    (setf (slot-value map 'cmp)
	  (lambda (one two)
	    (funcall old-cmp (car one) (car two))))
    (loop for e in elements do (insert map e))))

(defmethod search ((map tree-map) key)
  (let ((cell (call-next-method map (cons key nil))))
    (if (not (null cell))
	(cdr (data cell))
	nil)))

(defmethod delete ((map tree-map) key)
  (call-next-method (cons key nil)))

(defmethod [] ((map tree-map) key)
  (search map key))

(defmethod (setf []) (val (map tree-map) key)
  (insert map (cons key val)))
