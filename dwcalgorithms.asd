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
	       (:file "sorting")
	       (:file "searching")
	       (:file "hashing")
	       (:file "trees")))

