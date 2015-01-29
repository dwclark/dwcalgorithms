(in-package #:dwcalgorithms)

(defclass scapegoat-tree (binary-search-tree)
  ((alpha :initform 0.67 :initarg :alpha :accessor alpha)))

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
    (if (not (alpha-height-balanced? tree height))
        (let* ((the-scapegoat (scapegoat tree new-node))
               (top (rebalance the-scapegoat)))
          (if (root? top)
              (setf (root tree) top))))))
                     
