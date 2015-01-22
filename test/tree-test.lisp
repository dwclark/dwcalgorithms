(in-package #:dwcalgorithms)

(5am:def-suite tree-suite :description "Tree Test Suite")
(5am:in-suite tree-suite)

(5am:test test-insert
  (let* ((lst nil)
	 (tree (make-instance 'search-tree))
	 (func (lambda (x) (cl:push x lst))))
    (insert tree 2)
    (insert tree 3)
    (insert tree 1)
    (inorder-walk tree func)
    (5am:is (equal (reverse lst) (list 1 2 3)))
    (insert tree 10)
    (insert tree 8)
    (insert tree 5)
    (setf lst nil)
    (inorder-walk tree func)
    (5am:is (equal (reverse lst) (list 1 2 3 5 8 10)))))

(5am:test test-search
  (let ((tree (make-instance 'search-tree :elements (shuffle! (vector 5 7 1 3 9 8 2 10)))))
    (5am:is (= 10 (search tree 10)))
    (5am:is (null (search tree 17)))
    (5am:is (= 1 (minimum tree)))
    (5am:is (= 10 (maximum tree)))
    (5am:is (= 9 (successor tree 8)))
    (5am:is (null (successor tree 10)))
    (5am:is (= 1 (predecessor tree 2)))
    (5am:is (= 7 (predecessor tree 8)))
    (5am:is (null (predecessor tree 1)))))

(5am:test test-delete
  (let ((tree (make-instance 'search-tree 
			     :elements (shuffle! (vector 10 8 12 14 7 15 5 19 3 21 22 2 1)))))
    (5am:is (= 3 (search tree 3)))
    (delete tree 3)
    (5am:is (null (search tree 3)))
    (5am:is (= 10 (search tree 10)))
    (delete tree 10)
    (5am:is (null (search tree 10)))))

(5am:test test-rotate
  (let* ((tree (make-instance 'search-tree :elements (list 10 5 15 12 17)))
	 (lst nil)
	 (func (lambda (x) (cl:push x lst))))
    (inorder-walk tree func)
    (5am:is (equal (list 5 10 12 15 17) (reverse lst)))
    (setf lst nil)
    (left-rotate tree (root tree))
    (inorder-walk tree func)
    (5am:is (equal (list 5 10 12 15 17) (reverse lst)))
    (5am:is (= 15 (data (root tree))))
    (right-rotate tree (root tree))
    (5am:is (= 10 (data (root tree))))))
    
(5am:test test-rb-tree-basic
  (let ((tree (make-instance 'rb-tree :elements (shuffle! (vector 1 2 3 4 5 6 7 8 9 10)))))
    (5am:is (= 9 (search tree 9)))
    (5am:is (= 8 (predecessor tree 9)))
    (delete tree 8)
    (5am:is (null (search tree 8)))))

(5am:test test-rb-insert
  (let ((to-insert (shuffle! (make-array 101 :initial-contents (loop for i from 0 upto 100 collect i))))
	(tree (make-instance 'rb-tree)))
    (loop for i across to-insert
       do (let* ((collector (make-array 101 :fill-pointer 0 :adjustable t))
		 (func (lambda (x) (vector-push-extend x collector))))
	    (insert tree i)
	    (inorder-walk tree func)
	    (5am:is (sorted? collector :cmp #'<=>)))))

  (let ((tree (make-instance 'rb-tree)))
    (loop for i from 0 upto 100
       do (let* ((collector (make-array 101 :fill-pointer 0 :adjustable t))
		 (func (lambda (x) (vector-push-extend x collector))))
	    (insert tree i)
	    (inorder-walk tree func)
	    (5am:is (sorted? collector :cmp #'<=>))))))

(5am:test test-rb-delete
  (let ((tree (make-instance 'rb-tree :elements (loop for i from 0 upto 100 collect i))))
    (loop for i in '(57 89 30 31 5 30 21 50 51 52)
       do (let* ((collector (make-array 101 :fill-pointer 0 :adjustable t))
		 (func (lambda (x) (vector-push-extend x collector))))
	    (delete tree i)
	    (inorder-walk tree func)
	    (5am:is (sorted? collector :cmp #'<=>))))))

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
