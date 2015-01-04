(in-package #:dwcalgorithms)

(defclass red-black-node (binary-search-node)
  ((color :initform :red :accessor color)))

(defun red? (node)
  (and (not (null node)) (eq (color node) :red)))

(defmethod left-rotate-node ((x red-black-node))
  (multiple-value-bind (upper lower) (call-next-method)
    (setf (color upper) :black)
    (setf (color lower) :red)
    (values upper lower)))

(defmethod right-rotate-node ((x red-black-node))
  (multiple-value-bind (upper lower) (call-next-method)
    (setf (color upper) :black)
    (setf (color lower) :red)
    (values upper lower)))

(defclass red-black-tree (binary-search-tree)
  ((node-type :initform 'red-black-node :reader node-type :allocation :class)))

(defmethod new-node ((tree red-black-tree) data)
  (make-instance (node-type tree) :data data))

