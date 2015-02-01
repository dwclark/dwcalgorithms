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
               (:file "stack" :depends-on ("dwcalgorithms"))
               (:file "heap" :depends-on ("dwcalgorithms" "util"))
               (:file "hash" :depends-on ("dwcalgorithms" "util"))
               (:file "binary-tree" :depends-on ("dwcalgorithms" "util"))
               (:file "binary-tree-with-parent" :depends-on ("binary-tree"))
               (:file "binary-search-tree" :depends-on ("binary-tree-with-parent"))
               (:file "avl-tree" :depends-on ("binary-search-tree"))
               (:file "red-black-tree" :depends-on ("binary-search-tree"))
               (:file "scapegoat-tree" :depends-on ("binary-search-tree"))
               (:file "queue" :depends-on ("heap"))
               (:file "sort" :depends-on ("heap"))
               (:file "search" :depends-on ("sort"))
               (:file "treap" :depends-on ("binary-search-tree"))
               (:file "tree-map" :depends-on ("red-black-tree"))
               (:file "order-statistic-tree" :depends-on ("red-black-tree" "avl-tree"))
               (:file "interval")))

(defun run-all-tests ()
  (loop 
     for sym in (list (uiop:find-symbol* '#:sort-suite :dwcalgorithms)
                      (uiop:find-symbol* '#:hash-suite :dwcalgorithms)
                      (uiop:find-symbol* '#:heap-suite :dwcalgorithms)
                      (uiop:find-symbol* '#:queue-suite :dwcalgorithms)
                      (uiop:find-symbol* '#:search-suite :dwcalgorithms)
                      (uiop:find-symbol* '#:stack-suite :dwcalgorithms)
                      (uiop:find-symbol* '#:util-suite :dwcalgorithms)
                      (uiop:find-symbol* '#:binary-tree-suite :dwcalgorithms)
                      (uiop:find-symbol* '#:interval-suite :dwcalgorithms))
     do (uiop:symbol-call :fiveam '#:run! sym)))

(asdf:defsystem dwcalgorithms/test
  :serial t
  :description "Tests for dwcalgorithms"
  :author "David Clark <your.name@example.com>"
  :license "Apache 2 License"
  :depends-on (#:fiveam #:dwcalgorithms)
  :perform (asdf:test-op (o s)
                         (run-all-tests))
  :components ((:module "test"
                        :components ((:file "testing-utils")
                                     (:file "sort-test" :depends-on ("testing-utils"))
                                     (:file "search-test")
                                     (:file "hash-test")
                                     (:file "stack-test")
                                     (:file "util-test")
                                     (:file "heap-test")
                                     (:file "queue-test")
                                     (:file "binary-tree-test" :depends-on ("testing-utils"))
                                     (:file "interval-test")))))
