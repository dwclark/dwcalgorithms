(in-package #:dwcalgorithms)

(defclass os-node ()
  ((rank :initform 1 :accessor rank)))

(defclass os-tree () ())

(defmethod kth ((tree os-tree) k)
  (assert (and (<= 1 k) (<= k (size tree))))
  (labels ((kth-node (node i)
             (let ((r (if (left node) 
                          (+ 1 (rank (left node)))
                          1)))
               (cond
                 ((= i r) node)
                 ((< i r) (kth-node (left node) i))
                 (t (kth-node (right node) (- i r)))))))
    (data (kth-node (root tree) k))))

(defun modify-ranks (node func)
  (loop
     with n = node
     while n
     do (progn
          (funcall func n)
          (setf n (parent n)))))

(defmethod insert-left ((tree os-tree) node data)
  (modify-ranks node #'(lambda (n) (incf (rank n))))
  (call-next-method))

(defmethod insert-right ((tree os-tree) node data)
  (modify-ranks node #'(lambda (n) (incf (rank n))))
  (call-next-method))

(defmethod remove-node ((tree os-tree) node)
  (modify-ranks (parent node) #'(lambda (n) (decf (rank n))))
  (call-next-method))

(defun compute-new-rank (node)
  (+ (if (left node) (rank (left node)) 0)
     (if (right node) (rank (right node)) 0) 1))     
  
(defun modify-ranks-after-rotate (parent child)
  (setf (rank child) (compute-new-rank child))
  (setf (rank parent) (compute-new-rank parent)))

(defmethod left-rotate ((tree os-tree) node)
  (multiple-value-bind (parent child) (call-next-method)
    (modify-ranks-after-rotate parent child)
    (values parent child)))

(defmethod right-rotate ((tree os-tree) node)
  (multiple-value-bind (parent child) (call-next-method)
    (modify-ranks-after-rotate parent child)
    (values parent child)))

(defclass os-red-black-node (os-node red-black-node) ())

(defclass os-red-black-tree (os-tree red-black-tree)
  ((node-type :initform 'os-red-black-node :reader node-type :allocation :class)))

(defclass os-avl-node (os-node avl-node) ())

(defclass os-avl-tree (os-tree avl-tree)
  ((node-type :initform 'os-avl-node :reader node-type :allocation :class)))
  
(defclass os-binary-search-node (os-node binary-search-node) ())

(defclass os-binary-search-tree (os-tree binary-search-tree)
  ((node-type :initform 'os-binary-search-node :reader node-type :allocation :class)))
