(in-package #:dwcalgorithms)

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

(defun next-node (node)
  (one-node-to-the right node))

(defun previous-node (node)
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
         while (and (not (null iter)) (not (null insertion-func)))
         do (progn
              (setf insertion-point iter)
              (let ((data-to-cmp (data insertion-point)))
                (cond 
                  ;;Go Left
                  ((<? cmp data data-to-cmp)
                   (progn
                     (setf insertion-func #'insert-left)
                     (setf iter (left insertion-point))))
                  ;;Found exact match, copy new data in, set insertion-func, iter -> nil
                  ((=? cmp data data-to-cmp)
                   (progn
                     (setf insertion-func nil)
                     (setf insertion-point iter)))
                     
                  ;;Go Right
                  (t
                   (progn
                     (setf insertion-func #'insert-right)
                     (setf iter (right insertion-point)))))))
         finally (return (values insertion-point insertion-func))))))

(defun found-insertion-point? (point func)
  (not (null func)))

(defun found-point? (point func)
  (and (not (null point)) (null func)))

(defmethod insert ((tree binary-search-tree) val)
  (multiple-value-bind (point func) (find-insertion-point tree val)
    (cond 
      ((found-insertion-point? point func)
       (funcall func tree point val))
      
      ((found-point? point func)
       (setf (data point) val)
       nil)
      
      (t  nil))))

(defmethod search ((tree binary-search-tree) val)
  (multiple-value-bind (point func) (find-insertion-point tree val)
    (if (found-point? point func)
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
    (if (found-point? point func)
        (let ((pred-node (previous-node point)))
          (if (not (null pred-node))
              (data pred-node)
              nil))
        nil)))

(defmethod successor ((tree binary-search-tree) val)
  (multiple-value-bind (point func) (find-insertion-point tree val)
    (if (found-point? point func)
        (let ((next-node (next-node point)))
          (if (not (null next-node))
              (data next-node)
              nil))
        nil)))

(defmethod delete ((tree binary-search-tree) val)
  (let ((point (find-insertion-point tree val)))
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
                (decf (size tree))
                (if (root? parent-of-deleted)
                    (setf (root tree) parent-of-deleted))
                parent-of-deleted)))
        nil)))

