(in-package #:dwcalgorithms)

(defgeneric search (collection value))
(defgeneric insert (collection value))
(defgeneric delete (collection value))
(defgeneric minimum (collection))
(defgeneric kth (collection k)) ;note 1 based!
(defgeneric median (collection))
(defgeneric maximum (collection))
(defgeneric successor (collection value))
(defgeneric predecessor (collection value))

