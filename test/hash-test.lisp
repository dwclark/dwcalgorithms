(in-package #:dwcalgorithms)

(5am:def-suite hash-suite :description "Hash Test Suite")
(5am:in-suite hash-suite)

(5am:test test-hash-search
  (let ((hs (make-instance 'hash-search :elements (list 1 2 3 4 5 6 7 8 9 10 10 10))))
    (5am:is (= 8 (first (search hs 8))))
    (5am:is (= 12 (size hs)))
    (5am:is (= 3 (length (search hs 10))))
    (delete hs 10)
    (5am:is (= 9 (size hs)))
    (reduce #'insert (list 10 10 10) :initial-value hs)
    (5am:is (= 12 (size hs)))
    (5am:is (null (search hs 25)))))

(5am:test test-hash-set
  (let ((hs (make-instance 'hash-set :elements (list "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "10" "10"))))
    (5am:is (equal "8" (search hs "8")))
    (5am:is (= 10 (size hs)))
    (5am:is (equal "10" (search hs "10")))
    (delete hs "10")
    (5am:is (= 9 (size hs)))
    (reduce #'insert (list "10" "10" "10") :initial-value hs)
    (5am:is (= 10 (size hs)))
    (5am:is (null (search hs "25")))))
    
(5am:test test-hash-map
  (let ((hmap (make-instance 'hash-map)))
    (5am:is (= 0 (size hmap)))
    (setf ([] hmap "one") 1)
    (setf ([] hmap "two") 2)
    (5am:is (= 2 (size hmap)))
    (5am:is (= 2 ([] hmap "two")))
    (reduce #'insert (list (cons "three" 3) (cons "four" 4)
			   (cons "five" 5) (cons "six" 6)) :initial-value hmap)
    (5am:is (= 6 (size hmap)))
    (5am:is (= 5 ([] hmap "five")))
    (delete hmap "four")
    (5am:is (= 5 (size hmap))))

  (let ((hmap (make-instance 'hash-map :elements (list (cons 1 "one") (cons 2 "two") (cons 3 "three")))))
    (5am:is (= 3 (size hmap)))
    (setf ([] hmap 4) "four")
    (setf ([] hmap 2) "twooo")
    (5am:is (= 4 (size hmap)))
    (5am:is (equal "twooo" ([] hmap 2)))))

(defclass person ()
  ((name :initarg :name :accessor name)
   (age :initarg :age :accessor age)))

(defgeneric person-equal (p1 p2)
  (:method ((p1 person) (p2 person))
    (and (= (age p1) (age p2))
	 (equal (name p1) (name p2)))))

(defgeneric person-hash (p)
  (:method ((p person))
    (sxhash (+ (loop for char across (name p) summing (char-code char))
	       (age p)))))

(5am:test test-multi-hash-map
  (let ((grades (make-instance 'multi-hash-map :hash-func #'person-hash :equal-func #'person-equal))
	(sally (make-instance 'person :age 25 :name "Sally"))
	(fred (make-instance 'person :age 23 :name "Fred"))
	(james (make-instance 'person :age 29 :name "James"))
	(mary (make-instance 'person :age 26 :name "Mary")))
    (setf ([] grades sally) (cons "English" #\A))
    (setf ([] grades fred) (cons "English" #\B))
    (setf ([] grades james) (cons "English" #\F))
    (setf ([] grades mary) (cons "English" #\D))
    (5am:is (= 4 (size grades)))
    (5am:is (char= #\B (cdr (first ([] grades fred)))))
    (setf ([] grades sally) (cons "History" #\F))
    (setf ([] grades fred) (cons "History" #\D))
    (setf ([] grades james) (cons "History" #\A))
    (setf ([] grades mary) (cons "History" #\B))
    (5am:is (= 8 (size grades)))
    (5am:is (= 2 (length ([] grades sally))))
    (delete grades james)
    (5am:is (= 6 (size grades)))))