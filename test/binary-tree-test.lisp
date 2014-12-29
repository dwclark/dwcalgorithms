(in-package #:dwcalgorithms)

(5am:def-suite binary-tree-suite :description "Binary Tree Test Suite")
(5am:in-suite binary-tree-suite)

(5am:test create-node
  (let ((the-node (make-instance 'binary-node :data 1)))
    (5am:is (= 1 (data the-node)))
    (5am:is (null (right the-node)))
    (5am:is (null (left the-node)))
    (5am:is (not (null the-node)))))

(5am:test is-leaf
  (let ((the-node (make-instance 'binary-node)))
    (5am:is (leaf? the-node))
    (setf (right the-node) 1)
    (5am:is (not (leaf? the-node)))
    (setf (right the-node) 1)
    (setf (left the-node) 2)
    (5am:is (not (leaf? the-node)))))

(defun make-traversal-test-node ()
  (let ((the-node (make-instance 'binary-node :data "root data")))
    (setf (left the-node) (make-instance 'binary-node :data "left data"))
    (setf (right the-node) (make-instance 'binary-node :data "right data"))
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

(5am:test right-and-left-root
  (let* ((root-node (make-instance 'binary-node-with-parent :data 1)))
    (5am:is (null (right? root-node)))
    (5am:is (null (left? root-node)))))

(5am:test right-and-left-parent
  (let* ((the-tree (make-instance 'binary-tree-with-parent))
         (root-node (insert-left the-tree nil 2))
         (left-node (insert-left the-tree root-node 1))
         (right-node (insert-right the-tree root-node 3)))
    
    (5am:is (eq (parent right-node) root-node))
    (5am:is (eq (parent left-node) root-node))
    (5am:is (right? right-node))
    (5am:is (left? left-node))
    (5am:is (null (left? right-node)))
    (5am:is (null (right? left-node)))))

(5am:test merge-the-trees-with-parents
  (let* ((left-tree (make-instance 'binary-tree-with-parent))
         (left-root (insert-left left-tree nil "left data"))
         (right-tree (make-instance 'binary-tree-with-parent))
         (right-root (insert-left right-tree nil "right data")))
    (let ((tree (merge-trees left-tree right-tree "root data")))
      (5am:is (eq (parent right-root) (root tree)))
      (5am:is (eq (parent left-root) (root tree))))))

(5am:test correctly-rotates
  (let* ((tree (make-instance 'binary-tree-with-parent))
         (node (insert-left tree nil "stuff")))
    (5am:signals simple-error (right-rotate tree node))
    (5am:signals simple-error (left-rotate tree node))))

(5am:test right-rotation
  (let* ((tree (make-instance 'binary-tree-with-parent))
         (one (insert-right tree nil 1))
         (two (insert-right tree one 2))
         (before-vector (make-array 3 :adjustable t :fill-pointer 0))
         (after-vector (make-array 3 :adjustable t :fill-pointer 0)))

    (insert-right tree two 3)
    (in-order tree (add-to-vector before-vector))
    (left-rotate tree one)
    (in-order tree (add-to-vector after-vector))
    (5am:is (equalp before-vector after-vector))
    (5am:is (eq (root tree) two))))

(5am:test left-rotation
  (let* ((tree (make-instance 'binary-tree-with-parent))
         (three (insert-left tree nil 3))
         (two (insert-left tree three 2))
         (before-vector (make-array 3 :adjustable t :fill-pointer 0))
         (after-vector (make-array 3 :adjustable t :fill-pointer 0)))

    (insert-left tree two 1)
    (in-order tree (add-to-vector before-vector))
    (right-rotate tree three)
    (in-order tree (add-to-vector after-vector))
    (5am:is (equalp before-vector after-vector))
    (5am:is (eq (root tree) two))))

(5am:test compute-balance-factors-balanced
  (let* ((tree (make-instance 'avl-tree))
         (root (insert-left tree nil 2))
         (the-left (insert-left tree root 1))
         (the-right (insert-right tree root 3)))

    (loop 
       for node in (list root the-left the-right)
       do (5am:is (= 0 (balance-factor node))))))

(5am:test compute-balance-factors-left-unbalanced
  (let* ((tree (make-instance 'avl-tree))
         (three (insert-left tree nil -3))
         (two (insert-left tree three -2))
         (one (insert-left tree two -1))
         (zero (insert-left tree one 0)))

    (loop
       for node in (list three two one zero)
       do (5am:is (= (data node) (balance-factor node))))))

(5am:test compute-balance-factors-right-unbalanced
  (let* ((tree (make-instance 'avl-tree))
         (three (insert-right tree nil 3))
         (two (insert-right tree three 2))
         (one (insert-right tree two 1))
         (zero (insert-right tree one 0)))

    (loop
       for node in (list three two one zero)
       do (5am:is (= (data node) (balance-factor node))))))
    
(5am:test insert-binary-search-tree
  (let ((tree (make-instance 'binary-search-tree))
        (vec (make-array 6 :adjustable t :fill-pointer 0)))
    (loop
       for number across (vector 4 6 1 9 3 7)
       do (insert tree number))
    (in-order tree (add-to-vector vec))
    (5am:is (equalp (vector 1 3 4 6 7 9) vec))))
    
(5am:test insert-binary-search-tree-with-duplicates
  (let ((tree (make-instance 'binary-search-tree))
        (vec (make-array 6 :adjustable t :fill-pointer 0)))
    (loop
       for number across (vector 4 6 1 9 3 7 4 6 1 9 3 7)
       do (insert tree number))
    (in-order tree (add-to-vector vec))
    (5am:is (equalp (vector 1 3 4 6 7 9) vec))))

(5am:test search-binary-search-tree
  (let ((tree (make-instance 'binary-search-tree)))
    (loop
       for number across (vector 4 6 1 9 3 7)
       do (insert tree number))
    (5am:is (= 9 (search tree 9)))
    (5am:is (null (search tree 88)))))

(5am:test binary-search-tree-min-max
  (let ((tree (make-instance 'binary-search-tree)))
    (loop
       for number across (vector 4 6 1 9 3 7)
       do (insert tree number))
    (5am:is (= 1 (minimum tree)))
    (5am:is (= 9 (maximum tree)))))

(5am:test binary-search-tree-sucessor-predecessor
  (let ((tree (make-instance 'binary-search-tree)))
    (loop
       for number across (vector 4 6 1 9 3 7)
       do (insert tree number))
    
    (5am:is (= 4 (successor tree 3)))
    (5am:is (= 9 (successor tree 7)))
    (5am:is (null (successor tree 9)))

    (5am:is (= 3 (predecessor tree 4)))
    (5am:is (= 1 (predecessor tree 3)))
    (5am:is (null (predecessor tree 1)))))

(5am:test binary-search-tree-delete-single
  (let ((tree (make-instance 'binary-search-tree))
        (vec (make-array 6 :adjustable t :fill-pointer 0)))
    (insert tree 9)
    (in-order tree (add-to-vector vec))
    (5am:is (= 1 (length vec)))
    (delete tree 9)
    (setf (fill-pointer vec) 0)
    (in-order tree (add-to-vector vec))
    (5am:is (= 0 (length vec)))))

(defun add-list-to-tree (tree l)
  (loop for item in l do (insert tree item)))

(5am:test binary-search-tree-delete-leaf
  (let ((left-leaning-tree (make-instance 'binary-search-tree))
        (right-leaning-tree (make-instance 'binary-search-tree))
        (vec (make-array 6 :adjustable t :fill-pointer 0)))
    (add-list-to-tree left-leaning-tree (list 2 1))
    (add-list-to-tree right-leaning-tree (list 2 3))
    (delete left-leaning-tree 1)
    (delete right-leaning-tree 3)
    
    (in-order left-leaning-tree (add-to-vector vec))
    (5am:is (equalp vec (vector 2)))
    
    (setf (fill-pointer vec) 0)
    (in-order right-leaning-tree (add-to-vector vec))
    (5am:is (equalp vec (vector 2)))))

(5am:test binary-search-tree-delete-single-child-node
  (let ((tree (make-instance 'binary-search-tree))
        (vec (make-array 6 :adjustable t :fill-pointer 0)))
    (add-list-to-tree tree (list 5 8 10))
    
    (delete tree 8)
    (in-order tree (add-to-vector vec))
    (5am:is (equalp vec (vector 5 10)))))

(5am:test binary-search-tree-double-child-node
  (let ((tree (make-instance 'binary-search-tree))
        (vec (make-array 6 :adjustable t :fill-pointer 0)))
    (add-list-to-tree tree (list 5 3 8 2 4 7 10))
    
    (delete tree 3)
    (in-order tree (add-to-vector vec))
    (5am:is (equalp vec (vector 2 4 5 7 8 10)))

    (setf (fill-pointer vec) 0)
    (delete tree 8)
    (in-order tree (add-to-vector vec))
    (5am:is (equalp vec (vector 2 4 5 7 10)))))
