(in-package #:dwcalgorithms)

(5am:def-suite binary-tree-suite :description "Binary Tree Test Suite")
(5am:in-suite binary-tree-suite)

(5am:test create-node
  (let ((the-node (make-instance 'binary-tree-node :data 1)))
    (5am:is (= 1 (data the-node)))
    (5am:is (null (right the-node)))
    (5am:is (null (left the-node)))
    (5am:is (not (null the-node)))))

(5am:test is-leaf
  (let ((the-node (make-instance 'binary-tree-node)))
    (5am:is (leaf? the-node))
    (setf (right the-node) 1)
    (5am:is (not (leaf? the-node)))
    (setf (right the-node) 1)
    (setf (left the-node) 2)
    (5am:is (not (leaf? the-node)))))

(defun make-traversal-test-node ()
  (let ((the-node (make-instance 'binary-tree-node :data "root data")))
    (setf (left the-node) (make-instance 'binary-tree-node :data "left data"))
    (setf (right the-node) (make-instance 'binary-tree-node :data "right data"))
    the-node))

(defun add-to-vector (vec)
  (lambda (data)
    (vector-push-extend data vec)))

(5am:test pre-order-traversal
  (let ((the-node (make-traversal-test-node))
        (the-vector (make-array 3 :adjustable t :fill-pointer 0)))
    (pre-order the-node (add-to-vector the-vector))
    (5am:is (string= (elt the-vector 0) "root data"))
    (5am:is (string= (elt the-vector 1) "left data"))
    (5am:is (string= (elt the-vector 2) "right data"))))

(5am:test in-order-traversal
  (let ((the-node (make-traversal-test-node))
        (the-vector (make-array 3 :adjustable t :fill-pointer 0)))
    (in-order the-node (add-to-vector the-vector))
    (5am:is (string= (elt the-vector 0) "left data"))
    (5am:is (string= (elt the-vector 1) "root data"))
    (5am:is (string= (elt the-vector 2) "right data"))))

(5am:test post-order-traversal
  (let ((the-node (make-traversal-test-node))
        (the-vector (make-array 3 :adjustable t :fill-pointer 0)))
    (post-order the-node (add-to-vector the-vector))
    (5am:is (string= (elt the-vector 0) "left data"))
    (5am:is (string= (elt the-vector 1) "right data"))
    (5am:is (string= (elt the-vector 2) "root data"))))

(defun make-traversal-test-tree ()
  (let* ((tree (make-instance 'binary-tree))
         (root-node (insert-left tree nil "root data"))
         (left-node (insert-left tree root-node "left data"))
         (right-node (insert-right tree root-node "right data")))
    tree))

(5am:test pre-order-tree
  (let ((tree (make-traversal-test-tree))
        (the-vector (make-array 3 :adjustable t :fill-pointer 0)))
    (pre-order tree (add-to-vector the-vector))
    (5am:is (string= (elt the-vector 0) "root data"))
    (5am:is (string= (elt the-vector 1) "left data"))
    (5am:is (string= (elt the-vector 2) "right data"))))

(5am:test in-order-traversal
  (let ((tree (make-traversal-test-tree))
        (the-vector (make-array 3 :adjustable t :fill-pointer 0)))
    (in-order tree (add-to-vector the-vector))
    (5am:is (string= (elt the-vector 0) "left data"))
    (5am:is (string= (elt the-vector 1) "root data"))
    (5am:is (string= (elt the-vector 2) "right data"))))

(5am:test post-order-traversal
  (let ((tree (make-traversal-test-tree))
        (the-vector (make-array 3 :adjustable t :fill-pointer 0)))
    (post-order tree (add-to-vector the-vector))
    (5am:is (string= (elt the-vector 0) "left data"))
    (5am:is (string= (elt the-vector 1) "right data"))
    (5am:is (string= (elt the-vector 2) "root data"))))

(5am:test tree-remove
  (let* ((tree (make-traversal-test-tree))
         (the-root (root tree)))
    (5am:is (= 3 (size tree)))
    (remove-left tree the-root)
    (5am:is (= 2 (size tree)))
    (remove-right tree the-root)
    (5am:is (= 1 (size tree)))
    (remove-left tree nil)
    (5am:is (= 0 (size tree)))))
           
(5am:test merge-the-trees
  (let* ((left-tree (make-instance 'binary-tree))
         (right-tree (make-instance 'binary-tree))
         (the-vector (make-array 3 :adjustable t :fill-pointer 0)))
    (insert-left left-tree nil "left data")
    (insert-left right-tree nil "right data")
    (let ((tree (merge-trees left-tree right-tree "root data")))
      (in-order tree (add-to-vector the-vector))
      (5am:is (string= (elt the-vector 0) "left data"))
      (5am:is (string= (elt the-vector 1) "root data"))
      (5am:is (string= (elt the-vector 2) "right data")))))

