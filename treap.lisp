(in-package #:dwcalgorithms)

(defclass treap-node (binary-search-node)
  ((priority :initform (random most-positive-fixnum) :initarg :priority :accessor priority)))

(defclass treap (binary-search-tree)
  ((node-type :initform 'treap-node :reader node-type :allocation :class)))

(defmacro violation-on? (direction node)
  (once-only (node)
    `(and (not (null (,direction ,node)))
          (< (priority ,node) (priority (,direction ,node))))))

(defun priority-violations? (tree)
  (let ((violations nil))
    (in-order-node (root tree)
                   #'(lambda (node)
                       (with-accessors ((left left) (right right)) node
                         (if (or (violation-on? left node)
                                 (violation-on? right node))
                             (setf violations t)))))
    violations))

(defun float-up (tree node)
  (if node
      (loop
         with next = node
         while (and (not (root? next))
                    (< (priority (parent next)) (priority next)))
         do (progn
              (if (left? next)
                  (setf next (right-rotate tree (parent next)))
                  (setf next (left-rotate tree (parent next))))))))

(defun sink-down (tree node)
  (labels ((direction-of-rotation ()
             (let ((on-left? (violation-on? left node))
                   (on-right? (violation-on? right node)))
               (cond
                 ((and on-left? on-right?)
                  (if (< (priority (left node)) (priority (right node))) 
                      #'left-rotate 
                      #'right-rotate))
                 
                 (on-left? #'right-rotate)
                 (on-right? #'left-rotate)
                 (t nil)))))
    (if node
        (loop 
           with dir = (direction-of-rotation)
           while (not (null dir))
           do (progn
                (funcall dir tree node)
                (setf dir (direction-of-rotation)))))))

(defmethod insert ((tree treap) data)
  (float-up tree (call-next-method)))

(defmethod delete ((tree treap) data)
  (multiple-value-bind (point succ) (find-deletion-points tree data)
    (perform-deletion tree point succ)
    (if (and point succ)
        (progn
          (setf (priority point) (priority succ))
          (sink-down tree point)))))
