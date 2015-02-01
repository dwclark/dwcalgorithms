(in-package #:dwcalgorithms)

;An alternate definition of scapegoat-tree would be to implement it in
;terms of order statistics. This would simplify the implementation.
;However, keeping the rank of individual nodes nullifies one of the
;selling points of scapegoat trees, that no extra per node information is required.
(defclass scapegoat-tree (binary-search-tree)
  ((alpha :initform 0.67 :initarg :alpha :accessor alpha)
   (max-node-count :initform 0 :accessor max-node-count)))

(defun alpha-height-balanced? (tree height)
  (<= height (1+ (log (size tree) (/ 1 (alpha tree))))))

(defun scapegoat (tree node)
  (labels ((next (n child-size sibling-size)
             (if (>= (/ child-size (+ 1 child-size sibling-size)) (alpha tree))
                 n
                 (next (parent n) (+ 1 child-size sibling-size) (size-node (sibling n))))))
    (next (parent node) 1 (size-node (sibling node)))))

(defmethod insert ((tree scapegoat-tree) val)
  (multiple-value-bind (new-node height) (call-next-method)
    (if new-node
        (progn
          (setf (max-node-count tree) (max (max-node-count tree) (size tree)))
          (if (not (alpha-height-balanced? tree height))
              (let* ((the-scapegoat (scapegoat tree new-node))
                     (top (rebalance the-scapegoat)))
                (if (root? top)
                    (setf (root tree) top))))))))

(defmethod delete ((tree scapegoat-tree) val)
  (let ((deleted (call-next-method)))
    (if (and deleted (<= (size tree) (* (alpha tree) (max-node-count tree))))
        (progn
          (setf (root tree) (rebalance (root tree)))
          (setf (max-node-count tree) (size tree))))
    deleted))
          
                     
