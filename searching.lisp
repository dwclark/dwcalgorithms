(in-package #:dwcalgorithms)

(defun linear-search (vec test-for cmp)
  (loop for idx from 0 below (length vec)
     do (if (= 0 (funcall cmp test-for (aref vec idx)))
	    (return idx))
     finally (return nil)))

(defun binary-search (vec test-for cmp)
  (let* ((low 0)
	 (high (1- (length vec))))
    (loop with idx = nil
       while (<= low high)
       do (let* ((idx (ash (+ high low) -1))
		 (result (funcall cmp test-for (aref vec idx))))
	    (cond ((< result 0)
		   (setf high (1- idx)))
		  ((> result 0)
		   (setf low (1+ idx)))
		  (t (return-from binary-search idx))))))
  nil)
