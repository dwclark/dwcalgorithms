;;;; package.lisp

(defpackage #:dwcalgorithms
  (:use #:cl #:cl-utilities)
  (:shadow #:search #:delete #:sort #:push #:pop)
  (:export #:sort #:comparable-array #:sorted-array #:ary #:sorted-array-set #:sorted-map
           #:search #:insert #:delete #:minimum #:kth #:median #:maximum #:successor #:predecessor
           
           #:swap! #:shuffle! #:shift-left! #:shuffle
           
           #:<=> #:<=>-ignore-case #:<=>-reverse #:sorted?
           #:<? #:>? #:=? #:<=? #:>=?
           #:partition! #:select-kth!
           
           #:build-heap! #:heapify! 
           
           #:hash-search #:hash-set #:hash-map #:multi-hash-map #:size #:[]
           
           #:binary-tree-node #:binary-tree #:right #:left #:leaf? #:nil? #:nil-node
           #:pre-order #:in-order #:post-order #:merge-trees
           #:insert-left #:insert-right #:remove-left #:remove-right #:clear
           #:search-tree #:red-black-tree #:tree-map
           #:root #:data #:inorder-walk #:left-rotate #:right-rotate #:height-to-value
           
           ;stack operations
           #:push #:pop #:peek #:stack
           
           ;queue operations
           #:enqueue #:dequeue #:vector-queue #:heap-queue
           
           ;heap operations
           #:heap #:parent-index #:left-index #:right-index #:parent-value 
           #:left-value #:right-value #:heap->sorted))
