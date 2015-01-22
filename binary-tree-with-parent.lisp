(in-package #:dwcalgorithms)

(defclass binary-node-with-parent (binary-node)
  ((parent :initform nil :initarg :parent :accessor parent)))

(defmacro direction? (node direction-func)
  (once-only (node)
    (with-unique-names (p)
      `(let ((,p (parent ,node)))
         (if (or (null ,p)
                 (null (,direction-func ,p)))
             nil
             (eq (,direction-func ,p) ,node))))))

(defun right? (node)
  (direction? node right))

(defun left? (node)
  (direction? node left))

(defun root? (node)
  (null (parent node)))

(defun child (node)
  (assert (> 2 (number-of-children node)))
  (if (not (null (left node)))
      (left node)
      (right node)))

(defmacro link-on (direction node p-node)
  (once-only (node p-node)
    `(progn
       (if (not (null ,node))
           (setf (parent ,node) ,p-node))
       (if (not (null ,p-node))
           (setf (,direction ,p-node) ,node))
       ,node)))

(defun remove-node (node)
  (let ((children (number-of-children node)))
    (assert (< children 2))
    
    (with-accessors ((parent parent)) node
      (if (not (null (left node)))
          (if (right? node)
              (link-on right (left node) parent)
              (link-on left (left node) parent))
          (if (right? node)
              (link-on right (right node) parent)
              (link-on left (right node) parent)))
      node)))

(defmacro direction-rotate (the-direction x)
  (once-only (x)
    (with-unique-names (permanent-root y beta)
      (let ((func (if (eq the-direction 'right) 'left 'right)))
        `(progn
           (assert (not (null (,func ,x))))
           (let* ((,permanent-root (parent ,x))
                  (,y (,func ,x))
                  (,beta (,the-direction ,y)))
             
             (if (right? ,x)
                 (link-on right ,y ,permanent-root)
                 (link-on left ,y ,permanent-root))
             
             (link-on ,the-direction ,x ,y)
             (link-on ,func ,beta ,x)
             (values ,y ,x)))))))

(defun left-rotate-node (node)
  (direction-rotate left node))

(defun right-rotate-node (node)
  (direction-rotate right node))

(defun double-left-rotate-node (node)
  (right-rotate-node (right node))
  (left-rotate-node node))

(defun double-right-rotate-node (node)
  (left-rotate-node (left node))
  (right-rotate-node node))

(defclass binary-tree-with-parent (binary-tree)
  ((node-type :initform 'binary-node-with-parent :reader node-type :allocation :class)))

(defmethod new-node ((tree binary-tree-with-parent) data)
  (make-instance (node-type tree) :data data))

(defmethod insert-left ((tree binary-tree-with-parent) node data)
  (link-on left (call-next-method) node))

(defmethod insert-right ((tree binary-tree-with-parent) node data)
  (link-on right (call-next-method) node))

(defmethod left-rotate ((tree binary-tree-with-parent) node)
  (multiple-value-bind (parent child) (left-rotate-node node)
    (if (root? parent)
        (setf (root tree) parent))
    (values parent child)))

(defun double-left-rotate (tree node)
  (right-rotate tree (right node))
  (left-rotate tree node))

(defmethod right-rotate ((tree binary-tree-with-parent) node)
  (multiple-value-bind (parent child) (right-rotate-node node)
    (if (root? parent)
        (setf (root tree) parent))
    (values parent child)))

(defun double-right-rotate (tree node)
  (left-rotate tree (left node))
  (right-rotate tree node))

(defmethod merge-trees ((left-tree binary-tree-with-parent) (right-tree binary-tree-with-parent) data)
  (let* ((left-root (root left-tree))
         (right-root (root right-tree))
         (new-tree (call-next-method)))
    (link-on left left-root (root new-tree))
    (link-on right right-root (root new-tree))
    new-tree))

