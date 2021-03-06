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

(defun rebalance (node)
  (assert (not (null node)))
  (let* ((vec (make-array 10 :adjustable t :fill-pointer 0))
         (original-parent (parent node))
         (node-direction (if (right? node) :right :left)))
          
    (in-order-node node #'(lambda (n) 
                            (vector-push-extend n vec)))
    (labels 
        ((calculate-median (left right)
           (truncate (/ (+ left right) 2)))
         
         (median-insert (left right parent link-direction)
           (if (< left right)
               (let* ((mid (calculate-median left right))
                      (current (aref vec mid)))
                 (if (eq :left link-direction)
                     (link-on left current parent)
                     (link-on right current parent))
                 (median-insert left mid current :left)
                 (median-insert (1+ mid) right current :right)))))
      (loop 
         for node across vec
         do (progn
              (setf (parent node) nil)
              (setf (left node) nil)
              (setf (right node) nil)))
      (median-insert 0 (length vec) original-parent node-direction)
      (aref vec (calculate-median 0 (length vec))))))

(defclass binary-search-tree (binary-tree-with-parent)
  ((node-type :initform 'binary-search-node :reader node-type :allocation :class)
   (cmp :initform #'<=> :initarg :cmp :reader cmp)
   (allow-duplicates? :initform nil :initarg :allow-duplicates? :reader allow-duplicates?)))

(defun find-insertion-point (tree data)
  (with-accessors ((cmp cmp)) tree
    (let ((iter (root tree))
          (insertion-func #'insert-left)
          (insertion-point nil))

      (loop
         with height = 1
         while (and (not (null iter)) (not (null insertion-func)))
         do (progn
              (incf height)
              (setf insertion-point iter)
              (let ((data-to-cmp (data insertion-point)))
                (cond 
                  ;;Go Left
                  ((<? cmp data data-to-cmp)
                   (progn
                     (setf insertion-func #'insert-left)
                     (setf iter (left insertion-point))))

                  ;;Found exact match, set insertion-func -> nil
                  ((=? cmp data data-to-cmp)
                   (progn
                     (setf insertion-func nil)
                     (setf insertion-point iter)))
                     
                  ;;Go Right
                  (t
                   (progn
                     (setf insertion-func #'insert-right)
                     (setf iter (right insertion-point)))))))
         finally (return (values insertion-point insertion-func height))))))

(defun find-deletion-points (tree data)
  (multiple-value-bind (point func) (find-insertion-point tree data)
    (if (and (not (null point))
             (null func))
        (values point
                (if (= 2 (number-of-children point)) 
                    (next-node point) 
                    nil))
        (values nil nil))))

(defun found-insertion-point? (func)
  (not (null func)))

(defun found-point? (point func)
  (and (not (null point)) (null func)))

(defmethod insert ((tree binary-search-tree) val)
  (multiple-value-bind (point func height) (find-insertion-point tree val)
    (cond 
      ((found-insertion-point? func)
       (values (funcall func tree point val) height))
      
      ((found-point? point func)
       (setf (data point) val)
       (values nil nil))
      
      (t  (values nil nil)))))

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

(defun setup-final-deletion-point (point succ)
  (if succ
      (progn
        (setf (data point) (data succ))
        succ)
      point))

(defun perform-deletion (tree point succ)
  (if point
        (let ((to-delete (setup-final-deletion-point point succ)))
          (remove-node tree to-delete)
          (decf (size tree))
          (if (root? to-delete)
              (setf (root tree) (child to-delete)))
          to-delete)
        nil))

(defmethod delete ((tree binary-search-tree) val)
  (multiple-value-bind (point succ) (find-deletion-points tree val)
    (perform-deletion tree point succ)))

