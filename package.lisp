;;;; package.lisp

(defpackage #:dwcalgorithms
  (:use #:cl #:cl-utilities)
  (:shadow #:search #:delete #:sort #:push #:pop)
  (:export #:sort #:comparable-array #:sorted-array #:ary #:sorted-array-set #:sorted-map
	   #:search #:insert #:delete #:minimum #:kth #:median #:maximum #:successor #:predecessor

	   #:swap! #:shuffle! #:shuffle #:<=> #:<=>-ignore-case #:<=>-reverse #:sorted?
	   #:partition! #:select-kth!
	   
	   #:build-heap! #:heapify! 

	   #:hash-search #:hash-set #:hash-map #:multi-hash-map #:size #:[]

	   #:binary-tree #:red-black-tree #:tree-map
	   #:root #:data #:inorder-walk #:left-rotate #:right-rotate #:height-to-value

	   ;stack operations
	   #:push #:pop #:peek #:stack

	   ))
