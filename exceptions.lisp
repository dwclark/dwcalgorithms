(in-package #:dwcalgorithms)

(define-condition unsupported-operation (error)
  ((message :reader message :initarg :message))
  (:report (lambda (condition stream)
             (format stream "~A." (message condition)))))
