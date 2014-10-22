(in-package #:dwcalgorithms)

(defclass order-statistic-node (red-black-node)
  ((size :initform 1 :initarg :size :accessor size)))

(defparameter *nil-order-statistic-node* 
  (make-instance 'order-statistic-node :size 0))

(defmethod initialize-instance :after ((node order-statistic-node) &key)
  (if (eq (type-of node) 'order-statistic-node)
      (init-rb-node node *nil-order-statistic-node*)))

(defmethod nil? ((node order-statistic-node))
  (eq *nil-order-statistic-node* node))

(defclass order-statistic-tree (red-black-tree)
  ((root :initform *nil-order-statistic-node* :initarg :root :accessor root)))

(defmethod new-node ((tree order-statistic-tree) value)
  (make-instance 'order-statistic-tree :data value))

(defmethod nil-node ((tree order-statistic-tree))
  *nil-order-statistic-node*)

(defmethod insert-fixup ((tree order-statistic-tree) (new-node order-statistic-node))
  (labels ((increment-size (node)
	     (if (not (nil? node))
		 (progn
		   (incf (size node))
		   (increment-size (parent node))))))
    (increment-size (parent new-node)))
  (call-next-method))

(defmethod delete-fixup ((tree order-statistic-tree) (the-node order-statistic-node)
			 (splice-node order-statistic-node))
  (labels ((decrement-size (node)
	     (if (not (nil? node))
		 (progn
		   (decf (size node))
		   (decrement-size (parent node))))))
    (decrement-size (parent splice-node)))
  (call-next-method))

(defmethod left-rotate ((tree order-statistic-tree) (x order-statistic-node))
  (let ((y (right x)))
    (setf (size y) (size x))
    (setf (size x) (+ (size (left x)) (size (right x)) 1)))
  (call-next-method))

(defmethod right-rotate ((tree order-statistic-tree) (x order-statistic-node))
  ;;TODO: finish this method
)
