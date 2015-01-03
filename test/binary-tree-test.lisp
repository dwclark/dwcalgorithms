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
         (root-node (insert-left tree nil "root data")))
    (insert-left tree root-node "left data")
    (insert-right tree root-node "right data")
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
  (let* ((node (make-instance 'binary-node-with-parent :data 5)))
    (5am:signals simple-error (right-rotate-node node))
    (5am:signals simple-error (left-rotate-node node))))

(5am:test right-rotate-node
  (let* ((one (make-instance 'binary-node-with-parent :data 1))
         (two (make-instance 'binary-node-with-parent :data 2))
         (three (make-instance 'binary-node-with-parent :data 3))
         (before-vector (make-array 3 :adjustable t :fill-pointer 0))
         (after-vector (make-array 3 :adjustable t :fill-pointer 0)))
    
    (link-on right two one)
    (link-on right two three)
    (in-order one (add-to-vector before-vector))
    (left-rotate-node one)
    (in-order two (add-to-vector after-vector))
    (5am:is (equalp before-vector after-vector))))

(5am:test left-rotate-node
  (let* ((one (make-instance 'binary-node-with-parent :data 1))
         (two (make-instance 'binary-node-with-parent :data 2))
         (three (make-instance 'binary-node-with-parent :data 3))
         (before-vector (make-array 3 :adjustable t :fill-pointer 0))
         (after-vector (make-array 3 :adjustable t :fill-pointer 0)))

    (link-on left two three)
    (link-on left one two)
    (in-order three (add-to-vector before-vector))
    (right-rotate-node three)
    (in-order two (add-to-vector after-vector))
    (5am:is (equalp before-vector after-vector))))

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

(defun delete-list-from-tree (tree l)
  (loop for item in l do (delete tree item)))

(defun display-tree (node)
  (format t "Node Data: ~A, Node Parent: ~A~%"
          (data node) 
          (if (not (null (parent node))) 
              (data (parent node))
              nil)))

(5am:test binary-search-tree-delete-leaf
  (let ((left-leaning-tree (make-instance 'binary-search-tree))
        (right-leaning-tree (make-instance 'binary-search-tree))
        (vec (make-array 6 :adjustable t :fill-pointer 0)))
    (loop for i in (list 2 1) do (insert left-leaning-tree i))
    (loop for i in (list 2 3) do (insert right-leaning-tree i))
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
    (loop for i in (list 5 8 10) do (insert tree i))
    
    (delete tree 8)
    (in-order tree (add-to-vector vec))
    (5am:is (equalp vec (vector 5 10)))))

(5am:test binary-search-tree-double-child-node
  (let ((tree (make-instance 'binary-search-tree))
        (vec (make-array 6 :adjustable t :fill-pointer 0)))
    (loop for i in (list 5 3 8 2 4 7 10) do (insert tree i))
    
    (delete tree 3)
    (in-order tree (add-to-vector vec))
    (5am:is (equalp vec (vector 2 4 5 7 8 10)))

    (setf (fill-pointer vec) 0)
    (delete tree 8)
    (in-order tree (add-to-vector vec))
    (5am:is (equalp vec (vector 2 4 5 7 10)))))

(5am:test basic-binary-search-delete-three-nodes
  (let ((tree (make-instance 'binary-search-tree)))
    (loop for i in (list 2 3 1) do (insert tree i))
    (delete tree 3)
    (5am:is (= 2 (size tree)))
    (5am:is (= 2 (data (root tree))))
    
    (delete tree 2)
    (5am:is (= 1 (size tree)))
    (5am:is (= 1 (data (root tree))))

    (delete tree 1)
    (5am:is (= 0 (size tree)))
    (5am:is (null (root tree)))))

(5am:test binary-search-tree-size
  (let ((tree (make-instance 'binary-search-tree)))
    (loop for i in (list 5 2 7 9 1) do (insert tree i))
    (5am:is (= 5 (size tree)))

    (delete tree 1)
    (5am:is (= 4 (size tree)))
    
    (delete tree 2)
    (5am:is (= 3 (size tree)))

    (delete tree 5)
    (5am:is (= 2 (size tree)))
    (5am:is (= 7 (data (root tree))))

    (delete tree 7)
    (5am:is (= 1 (size tree)))
    (5am:is (= 9 (data (root tree))))

    (delete tree 9)
    (5am:is (= 0 (size tree)))
    (5am:is (null (root tree)))))

(5am:test basic-avl-rebalance
  (let ((tree (make-instance 'avl-tree)))
    (loop for i in (list 1 2 3) do (insert tree i))
    (5am:is (= 2 (height (root tree))))
    (loop for i in (list 4 5 6 7) do (insert tree i))
    (5am:is (= 3 (height (root tree))))
    (5am:is (= 4 (data (root tree))))))

(5am:test avl-lots-of-insertions
  (let ((tree (make-instance 'avl-tree)))
    (loop for i from 1 to 15 do (insert tree i))
    (5am:is (= 4 (height (root tree))))
    (5am:is (= 8 (data (root tree))))
    (5am:is (= 15 (data (right (right (right (root tree)))))))))

(5am:test basic-double-rotations
  (let ((tree (make-instance 'avl-tree)))
    (loop for i in (list 5 7 6) do (insert tree i))
    (5am:is (= 2 (height (root tree))))
    (5am:is (= 6 (data (root tree))))))

(5am:test avl-unique-delete-right-case
  (let ((tree (make-instance 'avl-tree)))
    (loop for i in (list 1 2 3 4 5 6 7) do (insert tree i))
    (loop for i in (list 7 6 5) do (delete tree i))
    (5am:is (= 4 (size tree)))
    (5am:is (= 2 (data (root tree))))
    (delete tree 2)
    (5am:is (= 3 (data (root tree))))
    (5am:is (= 3 (size tree)))
    (delete tree 3)
    (5am:is (= 2 (size tree)))
    (5am:is (= 4 (data (root tree))))
    (delete tree 1)
    (5am:is (= 1 (size tree)))
    (5am:is (= 4 (data (root tree))))
    (delete tree 4)
    (5am:is (= 0 (size tree)))
    (5am:is (null (root tree)))))

(5am:test avl-unique-delete-left-case
  (let ((tree (make-instance 'avl-tree))
        (the-vector (make-array 4 :adjustable t :fill-pointer 0)))
    (loop for i in (list 1 2 3 4 5 6 7) do (insert tree i))
    (loop for i in (list 2 1 3) do (delete tree i))
    (5am:is (= 3 (height (root tree))))
    (5am:is (= 6 (data (root tree))))
    (in-order tree (add-to-vector the-vector))
    (5am:is (equalp (vector 4 5 6 7) the-vector))))

(5am:test test-more-avl-deletions
  (let ((tree (make-instance 'avl-tree)))
    (loop for i in (list 10 5 20 3 7 15 25) do (insert tree i))
    (loop for i in (list 3 7 25 5) do (delete tree i))
    (5am:is (= 3 (size tree)))
    (5am:is (= 15 (data (root tree))))
    (5am:is (= 10 (data (left (root tree)))))
    (5am:is (= 20 (data (right (root tree)))))))

(5am:test test-all-basic-avl-insertons
  (labels
      ((test-now-balanced (tree)
         (5am:is (= 2 (data (root tree))))
         (5am:is (= 2 (height (root tree))))
         (5am:is (= 1 (data (left (root tree)))))
         (5am:is (= 3 (data (right (root tree)))))))
    (let ((tree (make-instance 'avl-tree)))
      ;;test right-right leaning
      (test-now-balanced (loop for i in (list 1 2 3) do (insert tree i) finally (return tree)))
      (clear tree)

      ;; test right-left leaning
      (test-now-balanced (loop for i in (list 1 3 2) do (insert tree i) finally (return tree)))
      (clear tree)

      ;;test left-left leaning
      (test-now-balanced (loop for i in (list 3 2 1) do (insert tree i) finally (return tree)))
      (clear tree)
      
      ;;test left-right leaning
      (test-now-balanced (loop for i in (list 3 1 2) do (insert tree i) finally (return tree))))))
