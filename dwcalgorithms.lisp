(in-package #:dwcalgorithms)

(defgeneric <=> (first second))
(defgeneric <=>-ignore-case (first second))

(defgeneric search (collection value))
(defgeneric insert (collection value))
(defgeneric delete (collection value))
(defgeneric minimum (collection))
(defgeneric kth (collection k)) ;note 1 based!
(defgeneric median (collection))
(defgeneric maximum (collection))
(defgeneric successor (collection value))
(defgeneric predecessor (collection value))

(defgeneric push (the-stack element))
(defgeneric pop (the-stack))
(defgeneric peek (collection))

(defgeneric enqueue (the-queue element))
(defgeneric dequeue (the-queue))

(defgeneric [] (map key))
(defgeneric (setf []) (val map key))
