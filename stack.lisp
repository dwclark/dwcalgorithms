(in-package #:dwcalgorithms)

(defclass stack ()
  ((top :initform nil)))

(defgeneric push (the-stack element))
(defgeneric pop (the-stack))
(defgeneric peek (the-stack))

(defmethod push ((the-stack stack) element)
  (with-slots (top) the-stack
    (setf top (cons element top))))

(defmethod pop ((the-stack stack))
  (with-slots (top) the-stack
    (if top
	(let ((ret (car top)))
	  (setf top (cdr top))
	  ret))))

(defmethod peek ((the-stack stack))
  (with-slots (top) the-stack
    (if top
	(car top))))
