(asdf:defsystem dwcalgorithms
  :serial t
  :description "Simple algorithm package"
  :author "David Clark <david@psionicwave.com>"
  :license "Apache 2 License"
  :depends-on (#:cl-utilities)
  :in-order-to ((asdf:test-op (asdf:test-op "dwcalgorithms/test")))
  :components ((:file "package")
               (:file "exceptions")
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

(defun run-all-tests ()
  (loop 
     for sym in (list (uiop:find-symbol* '#:sort-suite :dwcalgorithms)
                      (uiop:find-symbol* '#:hash-suite :dwcalgorithms)
                      (uiop:find-symbol* '#:heap-suite :dwcalgorithms)
                      (uiop:find-symbol* '#:queue-suite :dwcalgorithms)
                      (uiop:find-symbol* '#:search-suite :dwcalgorithms)
                      (uiop:find-symbol* '#:stack-suite :dwcalgorithms)
                      (uiop:find-symbol* '#:tree-suite :dwcalgorithms)
                      (uiop:find-symbol* '#:util-suite :dwcalgorithms)
                      (uiop:find-symbol* '#:binary-tree-suite :dwcalgorithms))
     do (uiop:symbol-call :fiveam '#:run! sym)))

(asdf:defsystem dwcalgorithms/test
  :serial t
  :description "Tests for dwcalgorithms"
  :author "David Clark <your.name@example.com>"
  :license "Apache 2 License"
  :depends-on (#:fiveam #:dwcalgorithms)
  :components ((:file "test/sort-test")
               (:file "test/search-test")
               (:file "test/hash-test")
               (:file "test/tree-test")
               (:file "test/stack-test")
               (:file "test/util-test")
               (:file "test/heap-test")
               (:file "test/queue-test")
               (:file "test/binary-tree-test"))
  :perform (asdf:test-op (o s)
                         (run-all-tests)))
