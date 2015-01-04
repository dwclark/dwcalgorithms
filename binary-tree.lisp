(in-package #:dwcalgorithms)

;;TODO
;;1) Implement red/black tree for this interface

(defgeneric insert-left (tree node data))
(defgeneric insert-right (tree node data))
(defgeneric merge-trees (left-tree right-tree root-data))

(defclass binary-node ()
  ((data :initform nil :initarg :data :accessor data)
   (right :initform nil :initarg :right :accessor right)
   (left :initform nil :initarg :left :accessor left)))

(defun number-of-children (node)
  (if (null node)
      nil
      (+ (if (null (right node)) 0 1)
         (if (null (left node)) 0 1))))

(defun leaf? (node)
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

(defun in-order-node (node proc)
  (labels
      ((inner (node)
         (if (not (null node))
             (progn
               (if (not (null (left node)))
                   (inner (left node)))
               
               (funcall proc node)

               (if (not (null (right node)))
                   (inner (right node)))))))
    (inner node)))

(defun post-order-node (node proc)
  (labels
      ((inner (node)
         (if (not (null node))
             (progn
               (if (not (null (left node)))
                   (inner (left node)))
               
               (if (not (null (right node)))
                   (inner (right node)))
               
               (funcall proc node)))))
    (inner node)))

(defclass binary-tree ()
  ((root :initform nil :accessor root)
   (size :initform 0 :accessor size)
   (node-type :initform 'binary-node :reader node-type :allocation :class)))

(defun clear (tree)
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

(defun remove-left (tree node)
  (remove-at tree node left))

(defun remove-right (tree node)
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
      (pre-order-node (root tree) #'(lambda (node) (funcall proc (data node))))))

(defmethod in-order ((tree binary-tree) proc)
  (if (not (null (root tree)))
      (in-order-node (root tree) #'(lambda (node) (funcall proc (data node))))))

(defmethod post-order ((tree binary-tree) proc)
  (if (not (null (root tree)))
      (post-order-node (root tree) #'(lambda (node) (funcall proc (data node))))))

