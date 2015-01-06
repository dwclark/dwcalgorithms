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

(defun red-violation? (node)
  (with-accessors ((parent parent)) node
    (and (red? node) (red? parent))))

(defun red-black-rebalance-insert (tree node)
  (let* ((parent-node (parent node))
         (grand-parent-node (if (not (null parent-node)) (parent parent-node) nil))
         (uncle-node (sibling parent-node)))
         
    (cond
      ((red? uncle-node)
       (progn
         (setf (color parent-node) :black)
         (setf (color uncle-node) :black)
         (setf (color grand-parent-node) :red)
         (if (red-violation? grand-parent-node)
             (red-black-rebalance-insert tree grand-parent-node))))
      
      ;;from here on out, the uncle has to be black
      ((right? parent-node)
       (if (right? node) ;;R-R case
           (left-rotate-node grand-parent-node)
           (progn ;;R-L case
             (right-rotate-node parent-node)
             (left-rotate-node grand-parent-node))))
      (t
       (if (left? node)
           (right-rotate-node grand-parent-node) ;;L-L case
           (progn ;;L-R case
             (left-rotate-node parent-node)
             (right-rotate-node grand-parent-node)))))))
  
(defmethod insert ((tree red-black-tree) val)
  (let ((new-node (call-next-method)))
    (if (and (not (null new-node)) (red-violation? new-node))
        (red-black-rebalance-insert tree new-node))
    (setf (color (root tree)) :black)
    new-node))

(defmethod delete ((tree red-black-tree) val)
  (let ((ret (call-next-method)))
    (if (not (null (root tree)))
        (setf (root tree) :black))
    ret))

;; (defmethod remove-node ((node red-black-node))
  
;;   (let ((children (number-of-children node)))
;;     (if (< children 2)
;;         (call-next-method)
;;         (let* ((next (next-node node))
;;                (data (data next)))
;;           (setf (data node) data)
;;           (call-next-method next)))))

