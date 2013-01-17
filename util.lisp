(in-package #:dwcalgorithms)

(defmacro swap! (vec idx1 idx2)
  (once-only (idx1 idx2)
    (with-unique-names (tmp)
      `(let ((,tmp (aref ,vec ,idx1)))
	 (setf (aref ,vec ,idx1) (aref ,vec ,idx2))
	 (setf (aref ,vec ,idx2) ,tmp)))))

(defun shuffle! (vec &key (factor 2))
  (loop with limit = (length vec) 
     for outer from 0 to factor
     do (loop for inner from 0 to limit
	   do (swap! vec (random limit) (random limit))))
  vec)

(defun shuffle (vec &key (factor 2))
   (shuffle! (copy-array vec) :factor factor))
