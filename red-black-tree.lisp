(in-package #:dwcalgorithms)

(defclass red-black-node (binary-search-node)
  ((color :initform :red :initarg :color :accessor color)))

(defun red? (node)
  (and (not (null node)) (eq (color node) :red)))

(defun black? (node)
  (or (null node) (eq (color node) :black)))

(defun black-non-leaf? (node)
  (and (not (null node)) (eq (color node) :black)))

(defun sibling (node &optional cmp)
  (with-accessors ((parent parent)) node
    (if (not (null parent))
        (if (null cmp)
            (if (right? node)
                (left parent)
                (right parent))
            (if (<? cmp (data parent) (data node))
                (left parent)
                (right parent)))
        nil)))

(defclass red-black-tree (binary-search-tree)
  ((node-type :initform 'red-black-node :reader node-type :allocation :class)))

(defmethod new-node ((tree red-black-tree) data)
  (make-instance (node-type tree) :data data))

(defun red-violation? (node)
  (with-accessors ((parent parent)) node
    (and (red? node) (red? parent))))

(defun red-violations? (node)
  (if (null node)
      nil
      (if (and (red? node) (or (red? (left node)) (red? (right node))))
          t
          (or (red-violations? (left node))
              (red-violations? (right node))))))

(defun black-height (node)
  (let ((adder (if (black? node) 1 0)))
    (if (null node)
        adder
        (+ adder (max (black-height (left node)) (black-height (right node)))))))

(defun black-violations? (start-node)
  (let ((ret nil))
    (in-order-node start-node 
                   (lambda (node)
                     (if (not (= (black-height (left node))
                                 (black-height (right node))))
                         (setf ret t))))
    ret))

(defun red-black-violations? (start-node)
  (or (red-violations? start-node)
      (black-violations? start-node)))

(defmethod search ((tree red-black-tree) val)
  (multiple-value-bind (point func) (find-insertion-point tree val)
    (if (found-point? point func)
        point
        nil)))

(defmethod insert ((tree red-black-tree) val)
  (let ((new-node (call-next-method)))
    (if (not (null new-node))
        (progn
          (insert-case-1 tree new-node)
          (setf (color (root tree)) :black)))
    new-node))

(defun insert-case-1 (tree node)
  (if (null (parent node))
      (setf (color node) :black)
      (insert-case-2 tree node)))

(defun insert-case-2 (tree node)
  (if (eq (color (parent node)) :red)
      (insert-case-3 tree node)))

(defun insert-case-3 (tree node)
  (let* ((p (parent node))
         (u (sibling p))
         (g (parent p)))
    (if (and (not (null u))
             (eq (color u) :red))
        (progn
          (setf (color (parent node)) :black)
          (setf (color u) :black)
          (setf (color g) :red)
          (insert-case-1 tree g))
        (insert-case-4 tree node))))

(defun insert-case-4 (tree node)
  (let* ((p (parent node))
         (g (parent p))
         (next-n node))

    (cond
      ((and (right? node)
            (left? p))
       (left-rotate tree p)
       (setf next-n (left node)))

      ((and (left? node)
            (right? p))
       (right-rotate tree p)
       (setf next-n (right node))))

    (insert-case-5 tree next-n)))

(defun insert-case-5 (tree node)
  (let* ((p (parent node))
         (g (parent p)))
    (setf (color p) :black)
    (setf (color g) :red)
    (if (left? node)
        (right-rotate tree g)
        (left-rotate tree g))))

(defmethod delete ((tree red-black-tree) val)
  (multiple-value-bind (point succ) (find-deletion-points tree val)
    (if point
        (let* ((to-delete (setup-final-deletion-point point succ))
               (num (number-of-children to-delete)))
          (decf (size tree))
          (if (= 1 num)
              (let ((the-child (child to-delete)))
                (remove-node to-delete)
                (if (black? to-delete)
                    (if (red? the-child)
                        (setf (color the-child) :black)
                        (delete-case-1 tree the-child)))
                (if (= 1 (size tree))
                    (setf (root tree) the-child)))
              (progn
                (if (black? to-delete)
                    (delete-case-1 tree to-delete))
                (remove-node to-delete)
                (if (= 0 (size tree))
                    (setf (root tree) nil))))))))

(defun delete-case-1 (tree node)
  (if (not (null (parent node)))
      (delete-case-2 tree node)))

(defun delete-case-2 (tree node)
  (let ((s (sibling node))
        (p (parent node)))
    (if (red? s)
        (progn
          (setf (color p) :red)
          (setf (color s) :black)
          (if (left? node)
              (left-rotate tree p)
              (right-rotate tree p))))
    (delete-case-3 tree node)))

(defun delete-case-3 (tree node)
  (let ((s (sibling node))
        (p (parent node)))
    (if (and (black? p)
             (black-non-leaf? s)
             (black? (left s))
             (black? (right s)))
        (progn
          (setf (color s) :red)
          (delete-case-1 tree p))
        (delete-case-4 tree node))))

(defun delete-case-4 (tree node)
  (let ((s (sibling node))
        (p (parent node)))
    (if (and (red? p)
             (black-non-leaf? s)
             (black? (left s))
             (black? (right s)))
        (progn
          (setf (color s) :red)
          (setf (color p) :black))
        (delete-case-5 tree node))))

(defun delete-case-5 (tree node)
  (let ((s (sibling node)))
    (if (black-non-leaf? s)
        (progn
          (if (and (left? node)
                   (black? (right s))
                   (red? (left s)))
              (progn
                (setf (color s) :red)
                (setf (color (left s)) :black)
                (right-rotate tree s))
              (if (and (right? node)
                       (black? (left s))
                       (red? (right s)))
                  (progn
                    (setf (color s) :red)
                    (setf (color (right s)) :black)
                    (left-rotate tree s))))
          (delete-case-6 tree node)))))
                
(defun delete-case-6 (tree node)
  (let* ((s (sibling node))
         (p (parent node)))
    (setf (color s) (color p))
    (setf (color p) :black)
    (if (left? node)
        (progn
          (if (not (null (right s))) (setf (color (right s)) :black))
          (left-rotate tree p))
        (progn
          (if (not (null (left s))) (setf (color (left s)) :black))
          (right-rotate tree p)))))
