(in-package #:dwcalgorithms)

;;TODO
;;1) Clean up return values
;;2) Separate into files
;;3) Re-vist inheritance hierarchy (mainly make sure contracts are obeyed as things are extended
;;4) AVL balanced tree, finish it up
;;5) Implement red/black tree for this interface

(defclass binary-node ()
  ((data :initform nil :initarg :data :accessor data)
   (right :initform nil :initarg :right :accessor right)
   (left :initform nil :initarg :left :accessor left)))

(defgeneric number-of-children (node))
(defgeneric leaf? (node))
(defgeneric pre-order (node proc))
(defgeneric in-order (node proc))
(defgeneric post-order (node proc))

(defmethod number-of-children ((node binary-node))
  (if (null node)
      nil
      (+ (if (null (right node)) 0 1)
         (if (null (left node)) 0 1))))

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

(defmacro insert-node-at (func tree node data)
  (once-only (tree node data)
    (with-unique-names (nn)
      `(let ((,nn (new-node ,tree ,data)))
         (if (null ,node)
             (progn
               (assert (= 0 (size ,tree)))
               (setf (root ,tree) ,nn))
             
             (progn
               (assert (null (,func ,node)))
               (setf (,func ,node) ,nn)))
         (incf (size ,tree))
         ,nn))))

(defmethod insert-left ((tree binary-tree) node data)
  (insert-node-at left tree node data))

(defmethod insert-right ((tree binary-tree) node data)
  (insert-node-at right tree node data))

(defmacro remove-at (tree node func)
  (once-only (tree node)
    (with-unique-names (position)
      `(if (> (size ,tree) 0)
           (let ((,position (if (null ,node) 
                                (root ,tree) 
                                (,func ,node))))
             
             (if (not (null ,position))
                 (progn
                   (remove-left ,tree ,position)
                   (remove-right ,tree ,position)
                   (if (not (null ,node))
                       (setf (,func ,node) nil))
                   (decf (size ,tree)))))))))

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
  (if (not (null (root tree)))
      (pre-order (root tree) proc)))

(defmethod in-order ((tree binary-tree) proc)
  (if (not (null (root tree)))
      (in-order (root tree) proc)))

(defmethod post-order ((tree binary-tree) proc)
  (if (not (null (root tree)))
      (post-order (root tree) proc)))

(defclass binary-node-with-parent (binary-node)
  ((parent :initform nil :initarg :parent :accessor parent)))

(defgeneric right? (node))
(defgeneric left? (node))
(defgeneric root? (node))
(defgeneric remove-node (node))
(defgeneric remove-leaf-node (node))
(defgeneric remove-single-child-node (node))
(defgeneric left-rotate-node (node))
(defgeneric right-rotate-node (node))

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

(defmethod root? ((node binary-node-with-parent))
  (null (parent node)))

(defmacro link-on (direction node p-node)
  (once-only (node p-node)
    `(progn
       (if (not (null ,node))
           (setf (parent ,node) ,p-node))
       (if (not (null ,p-node))
           (setf (,direction ,p-node) ,node))
       ,node)))

(defmethod remove-leaf-node ((node binary-node-with-parent))
  (with-accessors ((parent parent)) node
    (cond
      ((left? node)
       (link-on left nil parent)
       (values parent :left))
      ((right? node)
       (link-on right nil parent)
       (values parent :right)))))

(defmethod remove-single-child-node ((node binary-node-with-parent))
  (with-accessors ((parent parent)) node
    (cond
      ((not (null (left node)))
       (link-on left (left node) parent)
       (values (left node) :left))
      
      ((not (null (right node)))
       (link-on right (right node) parent)
       (values (right node) :right)))))

(defmethod remove-node ((node binary-node-with-parent))
  (with-accessors ((parent parent)) node
    (let ((children (number-of-children node)))
      (assert (< children 2))
      
      (if (= 0 children)
          (remove-leaf-node node)
          (remove-single-child-node node)))))

(defmacro direction-rotate (the-direction x other-direction)
  (with-unique-names (permanent-root y beta)
    `(progn
       (assert (not (null (,other-direction ,x))))
       (let* ((,permanent-root (parent ,x))
              (,y (,other-direction ,x))
              (,beta (,the-direction ,y)))
         
         (if (right? ,x)
             (link-on right ,y ,permanent-root)
             (link-on left ,y ,permanent-root))
         
         (link-on ,the-direction ,x ,y)
         (link-on ,other-direction ,beta ,x)
         (values ,y ,x))))) ;return new root and new child

(defmethod left-rotate-node ((x binary-node-with-parent))
  (direction-rotate left x right))

(defmethod right-rotate-node ((x binary-node-with-parent))
  (direction-rotate right x left))

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

(defclass binary-search-node (binary-node-with-parent) ())

(defmacro extreme (func node)
  (once-only (node)
    (with-unique-names (var)
      `(loop
          with ,var = ,node
          while (not (null (,func ,var)))
          do (setf ,var (,func ,var))
          finally (return ,var)))))

(defmethod minimum ((node binary-search-node))
  (extreme left node))

(defmethod maximum ((node binary-search-node))
  (extreme right node))

(defmacro one-node-to-the (func node other-func)
  (once-only (node)
    (with-unique-names (x y)
      `(if (not (null (,func ,node)))
           (extreme ,other-func (,func ,node))
           (let ((,y (parent ,node))
                 (,x ,node))
             (loop
                while (and (not (null ,y)) (eq ,x (,func ,y)))
                do (progn
                     (setf ,x ,y)
                     (setf ,y (parent ,y)))
                finally (return ,y)))))))

(defmethod next-node ((node binary-search-node))
  (one-node-to-the right node left))

(defmethod previous-node ((node binary-search-node))
  (one-node-to-the left node right))

(defmethod remove-node ((node binary-search-node))
  (let ((children (number-of-children node)))
    (if (< children 2)
        (call-next-method)
        (let* ((next (next-node node))
               (data (data next)))
          (setf (data node) data)
          (call-next-method next)))))

(defclass binary-search-tree (binary-tree-with-parent)
  ((node-type :initform 'binary-search-node :reader node-type :allocation :class)
   (cmp :initform #'<=> :initarg :cmp :reader cmp)))

(defmethod new-node ((tree binary-search-tree) data)
  (make-instance (node-type tree) :data data))

(defun find-insertion-point (tree data)
  (with-accessors ((cmp cmp)) tree
    (let ((iter (root tree))
          (insertion-func #'insert-left)
          (insertion-point nil))

      (loop
         while (not (null iter))
         do (progn
              (setf insertion-point iter)
              (let ((data-to-cmp (data insertion-point)))
                (cond 
                  ;;Go Left
                  ((<? cmp data data-to-cmp)
                   (progn
                     (setf insertion-func #'insert-left)
                     (setf iter (left insertion-point))))
                  ;;Found exact match, don't allow a duplicate
                  ((=? cmp data data-to-cmp)
                   (progn
                     (setf insertion-func nil)
                     (setf iter nil)))
                  ;;Go Right
                  (t
                   (progn
                     (setf insertion-func #'insert-right)
                     (setf iter (right insertion-point)))))))
         finally (return (values insertion-point insertion-func))))))

(defmethod insert ((tree binary-search-tree) val)
  (multiple-value-bind (point func) (find-insertion-point tree val)
    (if (not (null func))
        (funcall func tree point val)
        nil)))

(defun found? (point func)
  (and (null func) (not (null point))))

(defmethod search ((tree binary-search-tree) val)
  (multiple-value-bind (point func) (find-insertion-point tree val)
    (if (found? point func)
        (data point)
        nil)))

(defmethod minimum ((tree binary-search-tree))
  (let ((min-node (minimum (root tree))))
    (if (not (null min-node))
        (data min-node)
        nil)))

(defmethod maximum ((tree binary-search-tree))
  (let ((max-node (maximum (root tree))))
    (if (not (null max-node))
        (data max-node)
        nil)))

(defmethod predecessor ((tree binary-search-tree) val)
  (multiple-value-bind (point func) (find-insertion-point tree val)
    (if (found? point func)
        (let ((pred-node (previous-node point)))
          (if (not (null pred-node))
              (data pred-node)
              nil))
        nil)))

(defmethod successor ((tree binary-search-tree) val)
  (multiple-value-bind (point func) (find-insertion-point tree val)
    (if (found? point func)
        (let ((next-node (next-node point)))
          (if (not (null next-node))
              (data next-node)
              nil))
        nil)))

(defmethod delete ((tree binary-search-tree) val)
  (multiple-value-bind (point func) (find-insertion-point tree val)
    (if (not (null point))
        (if (and (root? point) (= 0 (number-of-children point)))
            (progn
              (clear tree)
              (values nil nil))
            (multiple-value-bind (ret-node direction) (remove-node point)
              (decf (size tree))
              (if (root? ret-node)
                  (setf (root tree) ret-node))
              (values ret-node direction)))
        (values nil nil))))

;; AVL Nodes and Trees
(defclass avl-node (binary-search-node)
  ((height :initform 1 :accessor height)))

(defun left-height (the-avl-node)
  (if (not (null (left the-avl-node)))
      (height (left the-avl-node))
      0))

(defun right-height (the-avl-node)
  (if (not (null (right the-avl-node)))
      (height (right the-avl-node))
      0))

(defun balance-factor (the-avl-node)
  (- (right-height the-avl-node) (left-height the-avl-node)))

(defun recompute-height (node)
  (with-accessors ((height height)) node
    (let ((prev height))
      (setf height (1+ (max (left-height node) (right-height node))))
      (not (= prev height)))))

(defun rebalance-insertion (node))
  
(defun compute-and-rebalance-insertion (node)
  (let ((changed (recompute-height node)))
    (if changed
        (let ((bf (abs (balance-factor node))))
          (cond
            ((= 0 bf) nil) ;return nothing, 
            
            ((= 1 bf)
             (if (not (null (parent node)))
                 (compute-and-rebalance-insertion (parent node))
                 nil))
            
            ((= 2 bf)
             (rebalance-insertion node))))
        nil)))

(defclass avl-tree (binary-search-tree)
  ((node-type :initform 'avl-node :reader node-type :allocation :class)
   (cmp :initform #'<=> :initarg :cmp :reader cmp)))

(defmethod new-node ((tree avl-tree) data)
  (make-instance (node-type tree) :data data))

(defmethod insert-left ((tree avl-tree) node data)
  (let ((new-node (call-next-method)))
    (if (not (null node))
        (compute-and-rebalance-insertion node))
    new-node))

(defmethod insert-right ((tree avl-tree) node data)
  (let ((new-node (call-next-method)))
    (if (not (null node))
        (compute-and-rebalance-insertion node))
    new-node))

(defmethod merge-trees ((left-tree avl-tree) (right-tree avl-tree) data)
  (error 'unsupported-operation :message "It is not possible to merge avl trees efficiently"))
