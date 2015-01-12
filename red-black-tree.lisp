(in-package #:dwcalgorithms)

(defclass red-black-node (binary-search-node)
  ((color :initform :red :initarg :color :accessor color)))

(defun red? (node)
  (and (not (null node)) (eq (color node) :red)))

(defun black? (node)
  (or (null node) (eq (color node) :black)))

(defun black-non-leaf? (node)
  (and (not (null node)) (eq (color node) :black)))

(defmethod left-rotate-node ((x red-black-node))
  (multiple-value-bind (upper lower) (call-next-method)
    (setf (color upper) :black)
    (setf (color lower) :red)
    (values upper lower)))

(defmethod right-rotate-node ((x red-black-node))
  (multiple-value-bind (upper lower) (call-next-method)
    (setf (color upper) :black)
    (setf (color lower) :red)
    (values upper lower)))

(defclass red-black-tree (binary-search-tree)
  ((node-type :initform 'red-black-node :reader node-type :allocation :class)))

(defmethod new-node ((tree red-black-tree) data)
  (make-instance (node-type tree) :data data))

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

(defun red-black-rebalance-insert (tree node)
  (let* ((parent-node (parent node))
         (grand-parent-node (if (not (null parent-node)) (parent parent-node) nil))
         (uncle-node (sibling parent-node)))
         
    (cond
      ((red? uncle-node)
       (progn
         (setf (color parent-node) :black)
         (setf (color uncle-node) :black)
         (setf (color grand-parent-node) :red)
         (if (red-violation? grand-parent-node)
             (red-black-rebalance-insert tree grand-parent-node))))
      
      ;;from here on out, the uncle has to be black
      ((right? parent-node)
       (if (right? node) ;;R-R case
           (left-rotate-node grand-parent-node)
           (progn ;;R-L case
             (right-rotate-node parent-node)
             (left-rotate-node grand-parent-node))))
      (t
       (if (left? node)
           (right-rotate-node grand-parent-node) ;;L-L case
           (progn ;;L-R case
             (left-rotate-node parent-node)
             (right-rotate-node grand-parent-node)))))))
  
(defmethod insert ((tree red-black-tree) val)
  (let ((new-node (call-next-method)))
    (if (and (not (null new-node)) (red-violation? new-node))
        (red-black-rebalance-insert tree new-node))
    (setf (color (root tree)) :black)
    new-node))

;; (defmethod delete ((tree red-black-tree) val)
;;   (let ((ret (call-next-method)))
;;     (if (not (null (root tree)))
;;         (setf (color (root tree)) :black))
;;     ret))

;; (defmethod remove-node ((node red-black-node))
;;   (multiple-value-bind (parent-node deleted-node) (call-next-method)
;;     (if (black? parent-node)
;;         (cond
;;           ((red? deleted-node)
;;            (setf (color parent-node) :black))

;;           (t 
;;            (delete-case-1 parent-node))))
;;     (values parent-node deleted-node)))

;; (defun delete-case-1 (node)
;;   (if (not (root? node))
;;       (delete-case-2 node)))

;; (defun delete-case-2 (node)
;;   (let ((s (sibling node)))
;;     (if (red? s)
;;         (progn
;;           (setf (color (parent node)) :red)
;;           (setf (color s) :black)
;;           (if (left? node)
;;               (left-rotate-node node)
;;               (right-rotate-node node)))
;;         (delete-case-3 node))))

;; (defun delete-case-3 (node)
;;   (let ((s (sibling node)))
;;     (if (and (black? (parent node))
;;              (black? node)
;;              (black? (left s))
;;              (black? (right s)))
;;         (progn
;;           (setf (color s) :red)
;;           (delete-case-1 (parent node)))
;;         (delete-case-4 node))))

;; (defun delete-case-4 (node)
;;   (let ((s (sibling node)))
;;     (if (and (red? (parent node))
;;              (black? s)
;;              (black? (left s))
;;              (black? (right s)))
;;         (progn
;;           (setf (color s) :red)
;;           (setf (color (parent node)) :black))
;;         (delete-case-5 node))))

;; (defun delete-case-5 (node)
;;   (let ((s (sibling node)))
;;     (if (black? s)
;;         (progn
;;           (if (and (left? node)
;;                    (black? (right s))
;;                    (red? (left s)))
;;               (progn
;;                 (setf (color s) :red)
;;                 (setf (color (left s)) :black)
;;                 (right-rotate-node s)))
          
;;           (if (and (right? node)
;;                    (black? (left s))
;;                    (red? (left s)))
;;               (progn
;;                 (setf (color s) :red)
;;                 (setf (right s) :black)
;;                 (left-rotate-node s))))
;;         (delete-case-6 node))))

;; (defun delete-case-6 (node)
;;   (let ((s (sibling node)))
;;     (setf (color s) (color (parent node)))
;;     (setf (color (parent node)) :black)

;;     (if (left? node)
;;         (progn
;;           (setf (color (right s)) :black)
;;           (left-rotate-node (parent node)))
;;         (progn
;;           (setf (color (left s)) :black)
;;           (right-rotate-node (parent node))))))
