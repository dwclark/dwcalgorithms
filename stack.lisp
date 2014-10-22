(in-package #:dwcalgorithms)

(defclass stack ()
  ((top :initform nil)))

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
