;;;; dwcalgorithms.asd

(asdf:defsystem #:dwcalgorithms
  :serial t
  :description "Simple algorithm package"
  :author "David Clark <david@psionicwave.com>"
  :license "Specify license here"
  :depends-on (#:cl-utilities)
  :components ((:file "package")
               (:file "dwcalgorithms")
               (:file "util")
               (:file "sort")
               (:file "search")
               (:file "hash")
               (:file "binarytree")
               (:file "searchtree")
               (:file "rbtree")
               (:file "treemap")
               (:file "orderstatistics")
               (:file "stack")
               (:file "heap")
               (:file "queue")))

