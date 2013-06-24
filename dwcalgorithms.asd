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
	       (:file "tree")
	       (:file "stack")
	       (:file "heap")
	       (:file "queue")))

