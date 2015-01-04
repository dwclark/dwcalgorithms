(in-package #:dwcalgorithms)

(defclass avl-node (binary-search-node)
  ((height :initform 1 :accessor height)))

(defmethod left-rotate-node ((x avl-node))
  (multiple-value-bind (upper lower) (call-next-method)
    (recompute-height lower)
    (recompute-height upper)
    (values upper lower)))

(defmethod right-rotate-node ((x avl-node))
  (multiple-value-bind (upper lower) (call-next-method)
    (recompute-height lower)
    (recompute-height upper)
    (values upper lower)))

(defun left-height (the-avl-node)
  (if (not (null (left the-avl-node)))
      (height (left the-avl-node))
      0))

(defun right-height (the-avl-node)
  (if (not (null (right the-avl-node)))
      (height (right the-avl-node))
      0))

(defun taller-side (the-avl-node)
  (let ((the-left (left-height the-avl-node))
        (the-right (right-height the-avl-node)))
    (cond
      ((and (not (null (left the-avl-node)))
            (> the-left the-right))
       (left the-avl-node))
      
      ((and (not (null (right the-avl-node)))
            (> the-right the-left))
       (right the-avl-node))
      
      (t nil))))

(defun balance-factor (the-avl-node)
  (- (right-height the-avl-node) (left-height the-avl-node)))

(defun recompute-height (node)
  (with-accessors ((height height)) node
    (let ((prev height))
      (setf height (1+ (max (left-height node) (right-height node))))
      (not (= prev height)))))

(defun perform-rotations (tree node)
  (let* ((tallest-sub-node (taller-side node))
         (top-imbalance (balance-factor node))
         (sub-imbalance (balance-factor tallest-sub-node)))
    
    (if (not (= 2 (abs top-imbalance)))
        (error 'illegal-state :message "All rotations should start with +2 or -2 imbalances"))
    
    (let ((upper (if (= 2 top-imbalance)
                     (if (>= sub-imbalance 0)
                         (left-rotate-node node)
                         (progn
                           (right-rotate-node tallest-sub-node)
                           (left-rotate-node node)))
                     (if (<= sub-imbalance 0)
                         (right-rotate-node node)
                         (progn
                           (left-rotate-node tallest-sub-node)
                           (right-rotate-node node))))))

      (if (root? upper)
          (setf (root tree) upper))
      upper)))

(defun compute-and-rebalance-insert (tree node)
  (if (not (null node))
      (let ((changed (recompute-height node)))
        (if changed
            (let ((bf (abs (balance-factor node))))
              (cond
                ((= 0 bf) nil) ;return nothing, 
                
                ((= 1 bf)
                 (if (not (null (parent node)))
                     (compute-and-rebalance-insert tree (parent node))))
                
                ((= 2 bf)
                 (compute-and-rebalance-insert tree (perform-rotations tree node)))))
            nil))
      nil))

(defun compute-and-rebalance-delete (tree node)
  (if (not (null node))
      (progn
        (recompute-height node)
        (let ((bf (abs (balance-factor node))))
          (if (= 2 bf)
              (compute-and-rebalance-delete tree (perform-rotations tree node))
              (compute-and-rebalance-delete tree (parent node)))))
      nil))

(defclass avl-tree (binary-search-tree)
  ((node-type :initform 'avl-node :reader node-type :allocation :class)
   (cmp :initform #'<=> :initarg :cmp :reader cmp)))

(defmethod new-node ((tree avl-tree) data)
  (make-instance (node-type tree) :data data))

(defmethod insert ((tree avl-tree) val)
  (let ((new-node (call-next-method)))
    (if (not (null new-node))
        (compute-and-rebalance-insert tree (parent new-node)))
    new-node))

(defmethod delete ((tree avl-tree) val)
  (let ((parent-of-deleted (call-next-method)))
    (if (not (null parent-of-deleted))
        (compute-and-rebalance-delete tree parent-of-deleted))
    parent-of-deleted))

(defmethod merge-trees ((left-tree avl-tree) (right-tree avl-tree) data)
  (error 'unsupported-operation :message "It is not possible to merge avl trees efficiently"))
