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

(defun pre-order-node (node proc)
  (labels
      ((inner (node)
         (if (not (null node))
             (progn
               (funcall proc node)
               
               (if (not (null (left node)))
                   (inner (left node)))

               (if (not (null (right node)))
                   (inner (right node)))))))
    (inner node)))


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
  (format t "In remove-leaf-node, node data: ~A~%" (data node))
  (with-accessors ((parent parent)) node
    (cond
      ((left? node)
       (link-on left nil parent)
       parent)
      ((right? node)
       (link-on right nil parent)
       (format t "in remove-leaf-node parent data: ~A, parent right: ~A~%" (data parent) (right parent))
       parent))))

(defmethod remove-single-child-node ((node binary-node-with-parent))
  (format t "In remove-single-child-node, node data: ~A~%" (data node))
  (with-accessors ((parent parent)) node
    (cond
      ((not (null (left node)))
       (if (right? node)
           (link-on right (left node) parent)
           (link-on left (left node) parent))
       parent)
      
      ((not (null (right node)))
       (if (right? node)
           (link-on right (right node) parent)
           (link-on left (right node) parent))
       parent))))

(defmethod remove-node ((node binary-node-with-parent))
  (let ((children (number-of-children node)))
    (assert (< children 2))
    
    (if (= 0 children)
        (remove-leaf-node node)
        (remove-single-child-node node))))

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

(defmacro one-node-to-the (the-direction node)
  (once-only (node)
    (with-unique-names (x y)
      (let ((other-direction (if (eq the-direction 'right) 'left 'right)))
        `(if (not (null (,the-direction ,node)))
             (extreme ,other-direction (,the-direction ,node))
             (let ((,y (parent ,node))
                   (,x ,node))
               (loop
                  while (and (not (null ,y)) (eq ,x (,the-direction ,y)))
                  do (progn
                       (setf ,x ,y)
                       (setf ,y (parent ,y)))
                  finally (return ,y))))))))

(defmethod next-node ((node binary-search-node))
  (one-node-to-the right node))

(defmethod previous-node ((node binary-search-node))
  (one-node-to-the left node))

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
        ;;case where we have something to delete
        (progn
          ;;handle the root differently
          (if (and (root? point) (< (number-of-children point) 2))
              (progn
                (if (= 0 (number-of-children point))
                    ;;root has no children, will be empty tree
                    (progn
                      (clear tree)
                      nil)
                    ;;root has one child, it will become the root
                    (let ((other (if (not (null (left point))) (left point) (right point))))
                      (remove-node point)
                      (decf (size tree))
                      (setf (root tree) other)
                      other)))
          
              ;;not root case
              (let ((parent-of-deleted (remove-node point)))
                (format t "Removing either non-root or root with more than one child~%")
                (format t "parent-of-deleted ~A~%" (data parent-of-deleted))
                (decf (size tree))
                (if (root? parent-of-deleted)
                    (setf (root tree) parent-of-deleted))
                parent-of-deleted)))
        nil)))

;; AVL Nodes and Trees
(defclass avl-node (binary-search-node)
  ((height :initform 1 :accessor height)))

(defmethod left-rotate-node ((x avl-node))
  (format t "In avl-node::left-rotate-node~%")
  (multiple-value-bind (upper lower) (call-next-method)
    (recompute-height lower)
    (recompute-height upper)
    (values upper lower)))

(defmethod right-rotate-node ((x avl-node))
  (format t "In avl-node::right-rotate-node~%")
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
                     (if (<= 0 sub-imbalance)
                         (right-rotate-node node)
                         (progn
                           (left-rotate-node tallest-sub-node)
                           (right-rotate-node node))))))

      (if (root? upper)
          (setf (root tree) upper))
      upper)))
      

(defun compute-and-rebalance-insert (tree node)
  (format t "In compute-and-rebalance-insert~%")
  
  (if (not (null node))
      (let ((changed (recompute-height node)))
        (format t "In compute-and-rebalance-insert node ~A, changed ~A~%" (data node) changed)
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
  (format t "In compute-and-rebalance-delete~%")
  
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
    (compute-and-rebalance-insert tree (parent new-node))
    new-node))

(defmethod delete ((tree avl-tree) val)
  (let ((parent-of-deleted (call-next-method)))
    (compute-and-rebalance-delete tree parent-of-deleted)
    parent-of-deleted))

(defmethod merge-trees ((left-tree avl-tree) (right-tree avl-tree) data)
  (error 'unsupported-operation :message "It is not possible to merge avl trees efficiently"))
