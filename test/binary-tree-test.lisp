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

(defun has-red-violations (node)
  (let ((possible-violations (make-array 10 :adjustable t :fill-pointer 0)))
    (in-order-node node
                   #'(lambda (the-node)
                       (if (red-violation? the-node)
                           (vector-push-extend 1 possible-violations))))
    (> (count 1 possible-violations) 0)))

(defun black-path-lengths (top-level-node)
  (let ((path-lengths (make-array 10 :adjustable t :fill-pointer 0)))
    (labels ((counter (node total)
               (cond 
                 ((null node)
                  (vector-push-extend total path-lengths))
                 ((red? node)
                  (counter (left node) total)
                  (counter (right node) total))
                 ((not (red? node))
                  (counter (left node) (1+ total))
                  (counter (right node) (1+ total))))))
      (counter top-level-node 0))
    path-lengths))

(defun has-black-violations (node)
  (let ((lengths (black-path-lengths node)))
    (notevery #'(lambda (val) 
                  (= (aref lengths 0) val)) lengths)))
            
(defun has-red-black-violations (node)
  (or (has-red-violations node)
      (has-black-violations node)))

(5am:test test-has-red-violations
  (let ((root (make-instance 'red-black-node)))
    (5am:is (not (has-red-violations root))))

    (let ((root (make-instance 'red-black-node)))
      (setf (color root) :black)
      (5am:is (not (has-red-violations root))))

    (let ((root (make-instance 'red-black-node))
          (child (make-instance 'red-black-node)))
      (setf (color root) :black)
      (link-on left child root)
      (5am:is (not (has-red-violations root))))

    (let ((root (make-instance 'red-black-node))
          (child (make-instance 'red-black-node))
          (grand-child (make-instance 'red-black-node)))
      (setf (color root) :black)
      (link-on left child root)
      (link-on left grand-child child)
      (5am:is (has-red-violations root))))

(5am:test test-black-path-lengths
   (let ((root (make-instance 'red-black-node)))
     (setf (color root) :black)
     (5am:is (not (has-black-violations root))))
   
   (let ((zero (make-instance 'red-black-node :data 0))
         (one (make-instance 'red-black-node :data 1))
         (two (make-instance 'red-black-node :data 2))
         (three (make-instance 'red-black-node :data 3))
         (four (make-instance 'red-black-node :data 4))
         (five (make-instance 'red-black-node :data 5))
         (six (make-instance 'red-black-node :data 6)))
         
         (loop for i in (list zero one two five) do (setf (color i) :black))
         (link-on left zero one)
         (link-on right three one)
         (link-on left two three)
         (link-on right five three)
         (link-on left four five)
         (link-on right six five)
         (5am:is (not (has-black-violations one)))))
         

(5am:test first-three-red-black
  (let ((tree (make-instance 'red-black-tree)))
    (insert tree 10)
    (5am:is (not (has-red-black-violations (root tree))))
    (insert tree 5)
    (insert tree 15)
    (5am:is (not (has-red-black-violations (root tree))))))

(5am:test four-red-black
  (let ((on-left (make-instance 'red-black-tree))
        (on-right (make-instance 'red-black-tree)))
    (loop for i in (list 2 1 3 0) do (insert on-left i))
    (5am:is (not (has-red-black-violations (root on-left))))
    (loop for i in (list 2 1 3 4) do (insert on-right i))
    (5am:is (not (has-red-black-violations (root on-right))))))

(5am:test simple-red-black-right-rotation
  (let ((tree (make-instance 'red-black-tree)))
    (loop for i in (list 1 0 3 2 5 4 6 7) do (insert tree i))
    (5am:is (not (has-red-black-violations (root tree))))))

(5am:test red-black-insertions
  (let ((tree (make-instance 'red-black-tree))
        (random-numbers (random-num-array 200 10000)))
    (loop for i across random-numbers do (insert tree i))
    (5am:is (not (has-red-black-violations (root tree))))))
