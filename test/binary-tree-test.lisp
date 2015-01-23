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

(defun add-to-vector-node (vec)
  (lambda (node)
    (vector-push-extend (data node) vec)))

(5am:test pre-order-traversal
  (let ((the-node (make-traversal-test-node))
        (the-vector (make-array 3 :adjustable t :fill-pointer 0)))
    (pre-order-node the-node (add-to-vector-node the-vector))
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
    (in-order-node one (add-to-vector-node before-vector))
    (left-rotate-node one)
    (in-order-node two (add-to-vector-node after-vector))
    (5am:is (equalp before-vector after-vector))))

(5am:test left-rotate-node
  (let* ((one (make-instance 'binary-node-with-parent :data 1))
         (two (make-instance 'binary-node-with-parent :data 2))
         (three (make-instance 'binary-node-with-parent :data 3))
         (before-vector (make-array 3 :adjustable t :fill-pointer 0))
         (after-vector (make-array 3 :adjustable t :fill-pointer 0)))

    (link-on left two three)
    (link-on left one two)
    (in-order-node three (add-to-vector-node before-vector))
    (right-rotate-node three)
    (in-order-node two (add-to-vector-node after-vector))
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

(5am:test insert-duplicates
  (let ((tree (make-instance 'avl-tree)))
    (insert tree 100)
    (insert tree 100)

    (5am:is (= 1 (size tree)))))

(defparameter *number-constraint-tests* 150)

(defun avl-balanced? (tree)
  (let ((results (make-array *number-constraint-tests* :adjustable t :fill-pointer 0)))
    (in-order-node (root tree) 
                   #'(lambda (node)
                       (if (>= (balance-factor node) 2)
                           (vector-push-extend 1 results)
                           (vector-push-extend 0 results))))
    (5am:is (= 0 (count 1 results)))))

(5am:test honors-avl-constraints
  (let ((tree (make-instance 'avl-tree))
        (random-numbers (random-num-array 150 10000)))
    (loop for i across random-numbers do (insert tree i))
    (avl-balanced? tree)
    (loop 
       for i across random-numbers
       do (progn
            (delete tree i)
            (avl-balanced? tree)))))

(5am:test red-violations?-test
  (let ((root (make-instance 'red-black-node :color :black)))
    (5am:is (not (red-violations? root))))

  (let* ((root (make-instance 'red-black-node :color :black)))
    (link-on left (make-instance 'red-black-node :color :red) root)
    (link-on right (make-instance 'red-black-node :color :red) root)
    (5am:is (not (red-violations? root))))
  
  (let* ((root (make-instance 'red-black-node :color :black))
         (first-left (link-on left (make-instance 'red-black-node :color :red) root)))
    (link-on right (make-instance 'red-black-node :color :red) root)
    (link-on right (make-instance 'red-black-node :color :red) first-left)
    (5am:is (red-violations? root))))

(5am:test black-violations?-test
  (let ((root (make-instance 'red-black-node :color :black)))
    (5am:is (not (black-violations? root))))

  (let* ((root (make-instance 'red-black-node :color :black)))
    (link-on left (make-instance 'red-black-node :color :black) root)
    (link-on right (make-instance 'red-black-node :color :black) root)
    (5am:is (not (black-violations? root))))

  (let* ((root (make-instance 'red-black-node :color :black))
         (first-left (link-on left (make-instance 'red-black-node :color :red) root)))
    (link-on right (make-instance 'red-black-node :color :red) root)
    (link-on right (make-instance 'red-black-node :color :red) first-left)
    (5am:is (not (black-violations? root))))

  (let* ((root (make-instance 'red-black-node :color :black))
         (first-left (link-on left (make-instance 'red-black-node :color :black) root)))
    (link-on right (make-instance 'red-black-node :color :black) root)
    (link-on right (make-instance 'red-black-node :color :black) first-left)
    (5am:is (black-violations? root))))

(5am:test first-three-red-black
  (let ((tree (make-instance 'red-black-tree)))
    (insert tree 10)
    (5am:is (not (red-black-violations? (root tree))))
    (insert tree 5)
    (insert tree 15)
    (5am:is (not (red-black-violations? (root tree))))))

(5am:test four-red-black
  (let ((on-left (make-instance 'red-black-tree))
        (on-right (make-instance 'red-black-tree)))
    (loop for i in (list 2 1 3 0) do (insert on-left i))
    (5am:is (not (red-black-violations? (root on-left))))
    (loop for i in (list 2 1 3 4) do (insert on-right i))
    (5am:is (not (red-black-violations? (root on-right))))))

(defun print-node-values (node)
  (format t "Node Data: ~A, Color: ~A, Parent: ~A~%" (data node) (color node)
          (if (not (null (parent node))) (data (parent node)) nil)))

(5am:test simple-red-black-right-rotation
  (let ((tree (make-instance 'red-black-tree)))
    (loop 
       for i in (list 1 0 3 2 5 4 6 7) 
       do (progn
            (insert tree i)
            (5am:is (not (red-black-violations? (root tree))))))))

(5am:test ascending-red-black-linked-list-insert
  (let ((tree (make-instance 'red-black-tree)))
    (loop for i from 1 to 100 do (insert tree i))
    (5am:is (not (red-black-violations? (root tree))))))

(5am:test descending-red-black-linked-list-insert
  (let ((tree (make-instance 'red-black-tree)))
    (loop for i from 1 to 100 do (insert tree i))
    (5am:is (not (red-black-violations? (root tree))))
    (5am:is (= 100 (size tree)))))

(5am:test random-red-black-insertions
  (let ((tree (make-instance 'red-black-tree))
        (random-numbers (random-num-array 200 10000)))
    (loop 
       for i across random-numbers
       do (progn
            (insert tree i)
            (5am:is (not (red-black-violations? (root tree))))))))
    

(5am:test red-black-deletion-of-black-node-with-red-child
  (let ((tree (make-instance 'red-black-tree)))
    (loop for i in (list 1 0 3 2 5 4 6 7) do (insert tree i))
    (delete tree 6)
    (5am:is (not (red-black-violations? (root tree))))))

(5am:test red-black-deletion-of-red-node-with-black-child
  (let ((tree (make-instance 'red-black-tree)))
    (loop for i in (list 1 0 3 2 5 4 6 7) do (insert tree i))
    (delete tree 1)
    (5am:is (not (red-black-violations? (root tree))))))

(5am:test red-black-deletion-of-black-leaf-node
  (let ((tree (make-instance 'red-black-tree)))
    (loop for i in (list 1 0 3 2 5 4 6 7) do (insert tree i))
    (delete tree 2)
    (5am:is (not (red-black-violations? (root tree))))))

(5am:test delete-case-5-right-leaning
  (let ((10-node (make-instance 'red-black-node :color :red :data 10))
        (5-node (make-instance 'red-black-node :color :black :data 5))
        (20-node (make-instance 'red-black-node :color :black :data 20))
        (15-node (make-instance 'red-black-node :color :red :data 15))
        (25-node (make-instance 'red-black-node :color :black :data 25))
        (13-node (make-instance 'red-black-node :color :black :data 13))
        (17-node (make-instance 'red-black-node :color :black :data 17))
        (tree (make-instance 'red-black-tree)))

    (setf (root tree) 10-node)
    (link-on left 5-node 10-node)
    (link-on right 20-node 10-node)
    (link-on left 15-node 20-node)
    (link-on right 25-node 20-node)
    (link-on left 13-node 15-node)
    (link-on right 17-node 15-node)
    
    (delete-case-5 tree 5-node)
    (5am:is (root? 15-node))
    (loop
       for node in (list 10-node 5-node 10-node 25-node 13-node 17-node)
       do (5am:is (black? node)))
    (5am:is (not (red-black-violations? 15-node)))))

(5am:test delete-case-5-left-leaning
  (let ((20-node (make-instance 'red-black-node :color :red :data 20))
        (30-node (make-instance 'red-black-node :color :black :data 30))
        (10-node (make-instance 'red-black-node :color :black :data 10))
        (5-node (make-instance 'red-black-node :color :black :data 5))
        (17-node (make-instance 'red-black-node :color :red :data 17))
        (12-node (make-instance 'red-black-node :color :black :data 12))
        (19-node (make-instance 'red-black-node :color :black :data 19))
        (tree (make-instance 'red-black-tree)))

    (setf (root tree) 20-node)
    (link-on left 10-node 20-node)
    (link-on right 30-node 20-node)
    (link-on left 5-node 10-node)
    (link-on right 17-node 10-node)
    (link-on left 12-node 17-node)
    (link-on right 19-node 17-node)

    (delete-case-5 tree 30-node)
    (5am:is (root? 17-node))
    (loop
       for node in (list 20-node 30-node 10-node 5-node 12-node 19-node)
       do (5am:is (black? node)))
    (5am:is (not (red-black-violations? 17-node)))))
        

(5am:test delete-case-6-right-leaning
  (let ((10-node (make-instance 'red-black-node :color :red :data 10))
        (5-node (make-instance 'red-black-node :color :black :data 5))
        (20-node (make-instance 'red-black-node :color :black :data 20))
        (25-node (make-instance 'red-black-node :color :red :data 25))
        (22-node (make-instance 'red-black-node :color :black :data 22))
        (27-node (make-instance 'red-black-node :color :black :data 27))
        (tree (make-instance 'red-black-tree)))

    (setf (root tree) 10-node)
    (link-on left 5-node 10-node)
    (link-on right 20-node 10-node)
    (link-on right 25-node 20-node)
    (link-on left 22-node 25-node)
    (link-on right 27-node 25-node)
    
    (delete-case-6 tree 5-node)
    (5am:is (root? 20-node))
    (5am:is (red? 20-node))
    (loop 
       for node in (list 10-node 5-node 25-node 22-node 27-node)
       do (5am:is (black? node)))))

(5am:test delete-case-6-left-leaning
  (let ((20-node (make-instance 'red-black-node :color :red :data 20))
        (17-node (make-instance 'red-black-node :color :black :data 17))
        (25-node (make-instance 'red-black-node :color :black :data 25))
        (10-node (make-instance 'red-black-node :color :red :data 10))
        (5-node (make-instance 'red-black-node :color :black :data 5))
        (15-node (make-instance 'red-black-node :color :black :data 20))
        (tree (make-instance 'red-black-tree)))

    (setf (root tree) 20-node)
    (link-on right 25-node 20-node)
    (link-on left 17-node 20-node)
    (link-on left 10-node 17-node)
    (link-on left 5-node 10-node)
    (link-on right 15-node 10-node)
    
    (delete-case-6 tree 25-node)
    (5am:is (root? 17-node))
    (5am:is (red? 17-node))
    (loop 
       for node in (list 20-node 25-node 10-node 5-node 15-node)
       do (5am:is (black? node)))))

(5am:test basic-red-black-deletes
  (let ((10-node (make-instance 'red-black-node :color :black :data 10))
        (5-node (make-instance 'red-black-node :color :red :data 5))
        (15-node (make-instance 'red-black-node :color :red :data 15))
        (3-node (make-instance 'red-black-node :color :black :data 3))
        (7-node (make-instance 'red-black-node :color :black :data 7))
        (13-node (make-instance 'red-black-node :color :black :data 13))
        (17-node (make-instance 'red-black-node :color :black :data 17))
        (tree (make-instance 'red-black-tree)))

    (setf (root tree) 10-node)
    (setf (size tree) 7)
    (link-on left 5-node 10-node)
    (link-on right 15-node 10-node)
    (link-on left 3-node 5-node)
    (link-on right 7-node 5-node)
    (link-on left 13-node 15-node)
    (link-on right 17-node 15-node)

    (5am:is (not (red-black-violations? (root tree))))
    (5am:is (= 7 (size tree)))
    (delete tree 7)
    (5am:is (not (red-black-violations? (root tree))))
    (5am:is (= 6 (size tree)))
    (delete tree 15)
    (5am:is (not (red-black-violations? (root tree))))
    (5am:is (= 5 (size tree)))
    (delete tree 13)
    (5am:is (not (red-black-violations? (root tree))))
    (5am:is (= 4 (size tree)))
    (delete tree 17)
    (5am:is (not (red-black-violations? (root tree))))
    (5am:is (= 3 (size tree)))
    (delete tree 10)
    (5am:is (not (red-black-violations? (root tree))))
    (5am:is (= 2 (size tree)))
    (delete tree 5)
    (5am:is (not (red-black-violations? (root tree))))
    (5am:is (= 1 (size tree)))
    (delete tree 3)
    (5am:is (not (red-black-violations? (root tree))))
    (5am:is (= 0 (size tree)))))

(5am:test test-random-deletes
  (let ((tree (make-instance 'red-black-tree))
        (vals (vector 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30
                      31 32 33 34 35 36 37 38 39 40)))
    (shuffle! vals)
    (loop for i across vals do (insert tree i))
    (shuffle! vals)
    (loop 
       for i across vals
       do (progn
            (delete tree i)
            (5am:is (not (red-black-violations? (root tree))))))))

(5am:test bad-deletion
  (let ((tree (make-instance 'red-black-tree))
        (8-node (make-instance 'red-black-node :color :black :data 8))
        (4-node (make-instance 'red-black-node :color :red :data 4))
        (10-node (make-instance 'red-black-node :color :black :data 10))
        (2-node (make-instance 'red-black-node :color :black :data 2))
        (6-node (make-instance 'red-black-node :color :black :data 6))
        (5-node (make-instance 'red-black-node :color :red :data 5))
        (7-node (make-instance 'red-black-node :color :red :data 7)))
    
    (setf (root tree) 8-node)
    (link-on left 4-node 8-node)
    (link-on right 10-node 8-node)
    (link-on left 2-node 4-node)
    (link-on right 6-node 4-node)
    (link-on left 5-node 6-node)
    (link-on right 7-node 6-node)
    (5am:is (not (red-black-violations? (root tree))))
    (delete tree 8)
    (5am:is (not (red-black-violations? (root tree))))))

(5am:test test-tree-map
  (let* ((elements (list (cons 1 "one") (cons 2 "two")
                         (cons 3 "tree") (cons 4 "four")))
         (map (make-instance 'tree-map :elements elements)))

    (5am:is (equal "four" ([] map 4)))
    (setf ([] map 5) "five")
    (5am:is (= 5 (size map)))
    (5am:is (equal "five" ([] map 5)))
    (setf ([] map 3) "iii")
    (5am:is (equal "iii" ([] map 3)))
    (5am:is (= 5 (size map)))))

(5am:test test-priority-violations?
  (let ((tree (make-instance 'treap))
        (the-root (make-instance 'treap-node :data 10 :priority 100))
        (the-left (make-instance 'treap-node :data 1 :priority 10))
        (the-right (make-instance 'treap-node :data 100 :priority 1)))
    (setf (root tree) the-root)
    (5am:is (not (priority-violations? tree)))
    (link-on left the-left the-root)
    (5am:is (not (priority-violations? tree)))
    (link-on right the-right the-root)
    (5am:is (not (priority-violations? tree)))
    (link-on left nil the-root)
    (5am:is (not (priority-violations? tree)))
    (link-on left the-left the-root)
    (setf (priority the-root) 0)
    (5am:is (priority-violations? tree))
    (setf (priority the-left) -1)
    (5am:is (priority-violations? tree))))

(defun print-node-priorities (node)
   (format t "Node Data: ~A, Parent Priority: ~A My Priority: ~A~%" 
           (data node)
           (if (not (null (parent node))) (priority (parent node)) nil)
           (priority node)))

(5am:test test-treap-inserts
  (let ((first (make-instance 'treap))
        (second (make-instance 'treap))
        (random-numbers (random-num-array 150 10000)))
    (loop for i from 1 to 20 do (insert first i))
    (5am:is (not (priority-violations? first)))
    (loop for i across random-numbers do (insert second i))
    (5am:is (not (priority-violations? second)))))

(5am:test test-treap-deletes
  (let ((first (make-instance 'treap))
        (second (make-instance 'treap))
        (random-numbers (random-num-array 150 10000))
        (first-vector (make-array 20 :adjustable t :fill-pointer 0)))

    (loop for i from 1 to 20 do (insert first i))
    (in-order first (add-to-vector first-vector))
    (5am:is (sorted? first-vector))
    (loop
       for i from 1 to 20
       do (progn
            (setf (fill-pointer first-vector) 0)
            (delete first i)
            (in-order first (add-to-vector first-vector))
            (5am:is (sorted? first-vector))
            (5am:is (not (priority-violations? first)))))
    (loop for i across random-numbers do (insert second i))
    (shuffle! random-numbers)
    (loop
       for i across random-numbers
       do (progn
            (delete second i)
            (5am:is (not (priority-violations? second)))))))
