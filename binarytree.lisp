(in-package #:dwcalgorithms)

(defclass binary-node ()
  ((data :initform nil :initarg :data :accessor data)
   (right :initform nil :initarg :right :accessor right)
   (left :initform nil :initarg :left :accessor left)))

(defgeneric leaf? (node))
(defgeneric pre-order (node proc))
(defgeneric in-order (node proc))
(defgeneric post-order (node proc))

(defmethod leaf? ((node binary-node))
  (and (null (right node))
       (null (left node))))

(defmethod pre-order ((node binary-node) proc)
  (labels
      ((inner (node)
         (if (not (null node))
             (progn
               (funcall proc (data node))
               
               (if (not (null (left node)))
                   (inner (left node)))

               (if (not (null (right node)))
                   (inner (right node)))))))
    (inner node)))

(defmethod in-order ((node binary-node) proc)
  (labels
      ((inner (node)
         (if (not (null node))
             (progn
               (if (not (null (left node)))
                   (inner (left node)))
               
               (funcall proc (data node))

               (if (not (null (right node)))
                   (inner (right node)))))))
    (inner node)))

(defmethod post-order ((node binary-node) proc)
  (labels
      ((inner (node)
         (if (not (null node))
             (progn
               (if (not (null (left node)))
                   (inner (left node)))
               
               (if (not (null (right node)))
                   (inner (right node)))
               
               (funcall proc (data node))))))
    (inner node)))

(defclass binary-tree ()
  ((root :initform nil :accessor root)
   (size :initform 0 :accessor size)
   (node-type :initform 'binary-node :reader node-type :allocation :class)))

(defgeneric clear (tree))
(defgeneric new-node (tree data))
(defgeneric insert-left (tree node data))
(defgeneric insert-right (tree node data))
(defgeneric remove-left (tree node))
(defgeneric remove-right (tree node))
(defgeneric merge-trees (left-tree right-tree root-data))

(defmethod clear ((tree binary-tree))
  (setf (size tree) 0)
  (setf (root tree) nil))

(defmethod new-node ((tree binary-tree) data)
  (make-instance (node-type tree) :data data))

(defmacro insert-node-at (tree node data func)
  `(let ((nn (new-node tree data)))
     (if (null node)
         (progn
           (assert (= 0 (size tree)))
           (setf (root tree) nn))
         
         (progn
           (assert (null (,func node)))
           (setf (,func node) nn)))
     (incf (size tree))
     nn))

(defmethod insert-left ((tree binary-tree) node data)
  (insert-node-at tree node data left))

(defmethod insert-right ((tree binary-tree) node data)
  (insert-node-at tree node data right))

(defmacro remove-at (tree node func)
  `(if (> (size tree) 0)
       (let ((position (if (null node) 
                           (root tree) 
                           (,func node))))
         
         (if (not (null position))
             (progn
               (remove-left tree position)
               (remove-right tree position)
               (if (not (null node))
                   (setf (,func node) nil))
               (decf (size tree)))))))

(defmethod remove-left ((tree binary-tree) node)
  (remove-at tree node left))

(defmethod remove-right ((tree binary-tree) node)
  (remove-at tree node right))

(defmethod merge-trees ((left-tree binary-tree) (right-tree binary-tree) data)
  (let ((merged (make-instance (type-of left-tree))))
    (insert-left merged nil data)
    (setf (left (root merged)) (root left-tree))
    (setf (right (root merged)) (root right-tree))
    (setf (size merged) (+ 1 (size left-tree) (size right-tree)))
    (clear left-tree)
    (clear right-tree)
    merged))

(defmethod pre-order ((tree binary-tree) proc)
  (pre-order (root tree) proc))

(defmethod in-order ((tree binary-tree) proc)
  (in-order (root tree) proc))

(defmethod post-order ((tree binary-tree) proc)
  (post-order (root tree) proc))

(defclass binary-node-with-parent (binary-node)
  ((parent :initform nil :initarg :parent :accessor parent)))

(defgeneric right? (node))
(defgeneric left? (node))

(defmacro direction? (node direction-func)
  `(with-accessors ((parent parent)) node
     (if (or (null parent)
             (null (,direction-func parent)))
         nil
         (eq (,direction-func parent) node))))

(defmethod right? ((node binary-node-with-parent))
  (direction? node right))

(defmethod left? ((node binary-node-with-parent))
  (direction? node left))

(defclass binary-tree-with-parent (binary-tree)
  ((node-type :initform 'binary-node-with-parent :reader node-type :allocation :class)))

(defmethod new-node ((tree binary-tree-with-parent) data)
  (make-instance (node-type tree) :data data))

(defun set-my-parent (new-node node)
  (if (not (null node))
      (setf (parent new-node) node))
  new-node)

(defmethod insert-left ((tree binary-tree-with-parent) node data)
  (set-my-parent (call-next-method) node))

(defmethod insert-right ((tree binary-tree-with-parent) node data)
  (set-my-parent (call-next-method) node))

(defmethod merge-trees ((left-tree binary-tree-with-parent) (right-tree binary-tree-with-parent) data)
  (let* ((left-root (root left-tree))
         (right-root (root right-tree))
         (new-tree (call-next-method))
         (new-root (root new-tree)))
    (setf (parent left-root) new-root)
    (setf (parent right-root) new-root)
    new-tree))
