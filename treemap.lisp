(in-package #:dwcalgorithms)		

(defclass tree-map (rb-tree)
  ((size :initform 0 :reader size)))

(defmethod initialize-instance :after ((map tree-map) &key (elements nil))
  (let ((old-cmp (cmp map)))
    (setf (slot-value map 'cmp)
	  (lambda (one two)
	    (funcall old-cmp (car one) (car two)))))
  (load-elements map elements))

(defmethod insert ((map tree-map) val)
  (let ((node (search-node map (root map) val)))
    (if (not (nil? node))
	(setf (cdr (data node)) (cdr val))
	(progn
	  (call-next-method)
	  (incf (slot-value map 'size))))))

(defmethod search ((map tree-map) key)
  (let ((cell (call-next-method map (cons key nil))))
    (if (not (null cell))
	(cdr cell)
	nil)))

(defmethod delete ((map tree-map) key)
  (let* ((cell (cons key nil))
	 (node (search-node map (root map) cell)))
    (if (not (nil? node))
	(progn
	  (delete-node map cell)
	  (decf (slot-value map 'size))))))

(defmethod [] ((map tree-map) key)
  (search map key))

(defmethod (setf []) (val (map tree-map) key)
  (insert map (cons key val)))
