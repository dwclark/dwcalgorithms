(in-package #:dwcalgorithms)

(defgeneric remove-node (node))
(defgeneric left-rotate-node (node))
(defgeneric right-rotate-node (node))

(defclass binary-node-with-parent (binary-node)
  ((parent :initform nil :initarg :parent :accessor parent)))

(defmacro direction? (node direction-func)
  `(with-accessors ((parent parent)) node
     (if (or (null parent)
             (null (,direction-func parent)))
         nil
         (eq (,direction-func parent) node))))

(defun right? (node)
  (direction? node right))

(defun left? (node)
  (direction? node left))

(defun root? (node)
  (null (parent node)))

(defun sibling (node)
  (with-accessors ((parent parent)) node
    (if (not (null parent))
        (if (right? node)
            (left parent)
            (right parent))
        nil)))

(defmacro link-on (direction node p-node)
  (once-only (node p-node)
    `(progn
       (if (not (null ,node))
           (setf (parent ,node) ,p-node))
       (if (not (null ,p-node))
           (setf (,direction ,p-node) ,node))
       ,node)))

(defmethod remove-node ((node binary-node-with-parent))
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
      parent)))

(defmacro direction-rotate (the-direction x)
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
           (values ,y ,x))))))

(defmethod left-rotate-node ((x binary-node-with-parent))
  (direction-rotate left x))

(defmethod right-rotate-node ((x binary-node-with-parent))
  (direction-rotate right x))

(defclass binary-tree-with-parent (binary-tree)
  ((node-type :initform 'binary-node-with-parent :reader node-type :allocation :class)))

(defmethod new-node ((tree binary-tree-with-parent) data)
  (make-instance (node-type tree) :data data))

(defmethod insert-left ((tree binary-tree-with-parent) node data)
  (link-on left (call-next-method) node))

(defmethod insert-right ((tree binary-tree-with-parent) node data)
  (link-on right (call-next-method) node))

(defmethod merge-trees ((left-tree binary-tree-with-parent) (right-tree binary-tree-with-parent) data)
  (let* ((left-root (root left-tree))
         (right-root (root right-tree))
         (new-tree (call-next-method)))
    (link-on left left-root (root new-tree))
    (link-on right right-root (root new-tree))
    new-tree))

