(in-package #:dwcalgorithms)

(defmacro swap! (vec idx1 idx2)
  (once-only (idx1 idx2)
    (with-unique-names (tmp)
      `(let ((,tmp (aref ,vec ,idx1)))
         (setf (aref ,vec ,idx1) (aref ,vec ,idx2))
         (setf (aref ,vec ,idx2) ,tmp)))))

(defun shuffle! (vec &key (factor 2))
  (loop 
     with limit = (length vec) 
     for outer from 0 to factor
     do (loop for inner from 0 to limit
           do (swap! vec (random limit) (random limit))))
  vec)

(defun shuffle (vec &key (factor 2))
  (shuffle! (copy-array vec) :factor factor))

(defun shift-left! (ary)
  (if (= 0 (length ary))
      nil
      (progn
        (let ((first (aref ary 0)))
          (loop for i from 0 below (1- (length ary))
             do (setf (aref ary i) (aref ary (1+ i))))
          (setf (fill-pointer ary) (1- (fill-pointer ary)))
          first))))

(defmacro <? (func left right)
  (once-only (func left right)
    (if (symbolp func)
        `(= -1 (funcall ,func ,left ,right))
        `(= -1 (,func ,left ,right)))))

(defmacro >? (func left right)
  (once-only (func left right)
    `(= 1 (funcall ,func ,left ,right))))

(defmacro =? (func left right)
  (once-only (func left right)
    `(= 0 (funcall ,func ,left ,right))))

(defmacro <=? (func left right)
  (once-only (func left right)
    `(>= 0 (funcall ,func ,left ,right))))

(defmacro >=? (func left right)
  (once-only (func left right)
    `(<= 0 (funcall ,func ,left ,right))))
