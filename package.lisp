;;;; package.lisp

(defpackage #:dwcalgorithms
  (:use #:cl #:cl-utilities)
  (:shadow #:search #:delete #:sort #:push #:pop)
  (:export 
   ;sorting interface
   #:<=> #:<=>-ignore-case #:<=>-reverse #:sorted? #:sort

   ;sorted types
   #:comparable-array #:sorted-array #:sorted-array-set #:sorted-map
   
   ;searching interface
   #:search #:insert #:delete #:minimum #:kth #:median #:maximum #:successor #:predecessor
   
   ;hashing/map interface and types
   #:hash-search #:hash-set #:hash-map #:multi-hash-map #:tree-map #:size #:[]
   
   ;tree interface and types
   #:binary-tree #:binary-tree-with-parent #:binary-search-tree #:avl-tree #:red-black-tree #:search-tree
   #:pre-order #:in-order #:post-order #:merge-trees #:clear
   
   ;stack interface and type
   #:push #:pop #:peek #:stack
   
   ;queue interface and type
   #:enqueue #:dequeue #:vector-queue #:heap-queue
   
   ;heap operations
   #:heap #:parent-index #:left-index #:right-index #:parent-value 
   #:left-value #:right-value #:heap->sorted))
