(in-package #:dwcalgorithms)

(defclass bt-node ()
  ((data :initform nil :initarg :data :accessor data)
   (right :initarg :right :accessor right)
   (left :initarg :left :accessor left)
   (parent :initarg :parent :accessor parent)))

(defparameter *nil-bt-node* (make-instance 'bt-node))

(defmethod initialize-instance :after ((node bt-node) &key)
  (if (eq (type-of node) 'bt-node)
      (progn 
	(if (not (slot-boundp node 'right))
	    (setf (right node) *nil-bt-node*))
	
	(if (not (slot-boundp node 'left))
	    (setf (left node) *nil-bt-node*))
	
	(if (not (slot-boundp node 'parent))
	    (setf (parent node) *nil-bt-node*)))))

(defgeneric nil? (node)
  (:method ((node bt-node))
    (eq node *nil-bt-node*)))

(defclass binary-tree ()
  ((root :initform *nil-bt-node* :initarg :root :accessor root)
   (cmp :initform #'<=> :initarg :cmp :accessor cmp)))

(defmethod initialize-instance :after ((tree binary-tree) &key (elements nil))
  (if (not (null elements))
      (reduce (lambda (arg-tree element)
		(insert arg-tree element)
		arg-tree) elements :initial-value tree)))

(defgeneric new-node (tree value)
  (:method ((tree binary-tree) value)
    (make-instance 'bt-node :data value)))

(defgeneric nil-node (tree)
  (:method ((tree binary-tree))
    *nil-bt-node*))

(defgeneric insert (tree value)
  (:method ((tree binary-tree) value)
    (with-accessors ((root root) (cmp cmp)) tree
      (let ((new-node (new-node tree value))
	    (new-parent (nil-node tree))
	    (iter-node root))
	
	;;Find the insertion point.  iter-node tracks search for new
	;;insertion point.  parent-node tracks the parent of iter-node,
	;;which will eventually become the parent of new-node.  When iter-node
	;;is nil, we have found the insertion point and the parent of new-node.
	(loop while (not (nil? iter-node))
	   do (progn
		(setf new-parent iter-node)
		(if (< (funcall cmp (data new-node) (data iter-node)) 0)
		    (setf iter-node (left iter-node))
		    (setf iter-node (right iter-node)))))
	
	;;Do the actual insertion.
	;;Step #1: assign the parent of new-node, as found previously (may be nil)
	;;Step #2a: if the found parent-node is nil, then we have an empty
	;;tree, just assign new node as the tree root and we are finished
	;;Step #2b: if the parent-node is not nil, then we have a non empty
	;;tree and need to update either the left or right of parent-node
	;;to point to new-node
	(setf (parent new-node) new-parent)
	(if (nil? new-parent)
	    (setf root new-node)
	    (progn
	      (if (< (funcall cmp (data new-node) (data new-parent)) 0)
		  (setf (left new-parent) new-node)
		  (setf (right new-parent) new-node))))

	new-node))))

(defgeneric inorder-walk (tree func)
  (:method ((tree binary-tree) func)
    (labels ((do-walk (node)
	       (if (not (nil? node))
		   (progn (do-walk (left node))
			  (funcall func (data node))
			  (do-walk (right node))))))
      (do-walk (root tree)))))

(defgeneric search-node (tree node val)
  (:method ((tree binary-tree) (node bt-node) val)
    ;;Basic recursive search.
    (let ((cmp-res nil))

      ;;Termination case.  If the node is the nil node, then just
      ;;return it.  If the node's data matches val, return the node
      ;;otherwise, keep recursing.  Store comparison result in cmp-res
      ;;so that later comparison doesn't have to recompute it.
      (if (or (nil? node) 
	      (= 0 (setf cmp-res (funcall (cmp tree) val (data node)))))
	  (return-from search-node node))
      
      ;;Recursive search.  Use the cmp-res value calculated previously
      ;;to figure out where to go next, either left or right
      (if (< cmp-res 0)
	  (search-node tree (left node) val)
	  (search-node tree (right node) val)))))

(defmacro extract-data (&body body)
  (with-unique-names (node)
    `(let ((,node (progn ,@body)))
       (if (nil? ,node)
	   nil
	   (data ,node)))))

(defgeneric search-value (tree val)
  (:method ((tree binary-tree) val)
    (extract-data (search-node tree (root tree) val))))

(defgeneric min-node (tree node)
  (:method ((tree binary-tree) (node bt-node))
    ;;Loop until there are no left nodes to follow.  Once
    ;;(left tmp) is nil, we have found the left most node
    (loop with tmp = node 
       while (not (nil? (left tmp)))
       do (setf tmp (left tmp))
       finally (return tmp))))

(defgeneric min-value (tree)
  (:method ((tree binary-tree))
    (extract-data (min-node tree (root tree)))))

(defgeneric max-node (tree node)
  (:method ((tree binary-tree) (node bt-node))
    ;;Loop until there are no right nodes to follow.  Once
    ;;(right tmp) is nil, we have found the right most node
    (loop with tmp = node
       while (not (nil? (right tmp)))
       do (setf tmp (right tmp))
       finally (return tmp))))

(defgeneric max-value (tree)
  (:method ((tree binary-tree))
    (extract-data (max-node tree (root tree)))))

(defgeneric successor-node (tree node)
  (:method ((tree binary-tree) (node bt-node))
    ;;If the node to the right is not nil, then the next element
    ;;will simply be the minimum element in the right tree.
    (if (not (nil? (right node)))
	(return-from successor-node (min-node tree (right node))))

    ;;The basic idea is described in CLRS: We are looking for the
    ;;lowest ancestor of node whose left child is also an ancestor
    ;;of node.  We follow the parents up the tree, as long as we are
    ;;always traveling up the right side of the branch we are on.
    ;;When we come to a point where we would have to travel up the left side of
    ;;a branch, we have found what we are looking for.  The parent is
    ;;the ancestor whose left node is an ancestor of node.
    (loop with iter-node = node 
       with parent-node = (parent iter-node)
       while (and (not (nil? parent-node)) (eq iter-node (right parent-node)))
       do (progn
	    (setf iter-node parent-node)
	    (setf parent-node (parent parent-node)))
       finally (return parent-node))))

(defgeneric successor-value (tree val)
  (:method ((tree binary-tree) val)
    (extract-data (successor-node tree (search-node tree (root tree) val)))))

(defgeneric predecessor-node (tree node)
  (:method ((tree binary-tree) (node bt-node))
    ;;Symmetric to successor-node, but with max-node in place
    ;;of min-node and left and rights swapped.  See successor-node
    ;;for more details
    (if (not (nil? (left node)))
	(return-from predecessor-node (max-node tree (left node))))

    (loop with iter-node = node
       with parent-node = (parent iter-node)
       while (and (not (nil? parent-node)) (eq iter-node (left parent-node)))
       do (progn
	    (setf iter-node parent-node)
	    (setf parent-node (parent parent-node)))
       finally (return parent-node))))

(defgeneric predecessor-value (tree val)
  (:method ((tree binary-tree) val)
    (extract-data (predecessor-node tree (search-node tree (root tree) val)))))

(defgeneric delete-node (tree node)
  (:method ((tree binary-tree) (node bt-node))
    (let ((delete-node node)
	  (splice-node (nil-node tree))
	  (child-node (nil-node tree)))
      
      ;;Determine node to splice out.  If there is at most one child
      ;;node, then the node we are deleting out is the node we are splicing.
      ;;Otherwise the node to splice out is the successor of the node
      ;;to delete
      (if (or (nil? (left delete-node)) (nil? (right delete-node)))
	  (setf splice-node delete-node)
	  (setf splice-node (successor-node tree delete-node)))
      
      ;;Find the children of splice-node
      (if (not (nil? (left splice-node)))
	  (setf child-node (left splice-node))
	  (setf child-node (right splice-node)))
      
      ;;Set parent of child-node.  Note, can avoid testing for null
      ;;because we are using a sentinel for nil
      (setf (parent child-node) (parent splice-node))
      
      ;;1) Handle special case of child-node now being the only node in the tree
      ;;2) Link child-node into proper position (left/right) of parent node
      (if (nil? (parent splice-node))
	  (setf (root tree) child-node)
	  (progn
	    (if (eq splice-node (left (parent splice-node)))
		(setf (left (parent splice-node)) child-node)
		(setf (right (parent splice-node)) child-node))))
      
      ;;move the spliced node's data into delete-node's data
      (if (not (eq splice-node delete-node))
	  (setf (data delete-node) (data splice-node)))
      
      (values splice-node child-node))))

(defgeneric delete-value (tree val)
  (:method ((tree binary-tree) val)
    (let ((node (search-node tree (root tree) val)))
      (if (not (nil? node))
	  (extract-data (delete-node tree node))))))

(defgeneric left-rotate (tree x)
  (:method ((tree binary-tree) (x bt-node))
    (let ((y (right x)))
      (setf (right x) (left y))
      (if (not (nil? (left y)))
	  (setf (parent (left y)) x))
      (setf (parent y) (parent x))
      (if (nil? (parent x))
	  (setf (root tree) y)
	  (if (eq x (left (parent x)))
	      (setf (left (parent x)) y)
	      (setf (right (parent x)) y)))
      (setf (left y) x)
      (setf (parent x) y))))

(defgeneric right-rotate (tree x)
  (:method ((tree binary-tree) (x bt-node))
    (let ((y (left x)))
      (setf (left x) (right y))
      (if (not (nil? (right y)))
	  (setf (parent (right y)) x))
      (setf (parent y) (parent x))
      (if (nil? (parent x))
	  (setf (root tree) y)
	  (if (eq x (right (parent x)))
	      (setf (right (parent x)) y)
	      (setf (left (parent x)) y)))
      (setf (right y) x)
      (setf (parent x) y))))

(defgeneric height-to-value (tree val)
  (:method ((tree binary-tree) val)
    (loop with node = (root tree)
       with height = 1
       while (not (nil? node))
       do (let ((res (funcall (cmp tree) (data node) val)))
	    (cond ((= 0 res) 
		   (return-from height-to-value height))
		  
		  ((= -1 res) 
		   (incf height)
		   (setf node (left node)))
		  
		  (t 
		   (incf height)
		   (setf node (right node)))))
       finally (return height))))
		
(defclass red-black-node (bt-node)
  ((color :initarg :color :initform 'black :accessor color)))
  
(defparameter *nil-red-black-node* (make-instance 'red-black-node))

(defmethod initialize-instance :after ((node red-black-node) &key)
  (if (eq (type-of node) 'red-black-node)
      (progn 
	(if (not (slot-boundp node 'right))
	    (setf (right node) *nil-red-black-node*))
	
	(if (not (slot-boundp node 'left))
	    (setf (left node) *nil-red-black-node*))
	
	(if (not (slot-boundp node 'parent))
	    (setf (parent node) *nil-red-black-node*)))))

(defmethod nil? ((node red-black-node))
  (eq *nil-red-black-node* node))

(defclass red-black-tree (binary-tree)
  ((root :initform *nil-red-black-node* :initarg :root :accessor root)))

(defmethod new-node ((tree red-black-tree) value)
  (make-instance 'red-black-node :data value))

(defmethod nil-node ((tree red-black-tree))
  *nil-red-black-node*)

(defmethod insert ((tree red-black-tree) value)
  (let ((new-node (call-next-method)))
    (setf (color new-node) 'red)
    (insert-fixup tree new-node)))

(defgeneric insert-fixup (tree node)
  (:method ((tree red-black-tree) (the-node red-black-node))
    (loop with node = the-node 
       while (eq 'red (color (parent node)))
       do (if (eq (parent node) (left (parent (parent node))))
	      (let ((tmp (right (parent (parent node)))))
		(if (eq 'red (color tmp))
		    (progn
		      (setf (color (parent node)) 'black)
		      (setf (color tmp) 'black)
		      (setf (color (parent (parent node))) 'red)
		      (setf node (parent (parent node))))
		    (progn
		      (if (eq node (right (parent node)))
			  (progn
			    (setf node (parent node))
			    (left-rotate tree node)))
		      (progn
			(setf (color (parent node)) 'black)
			(setf (color (parent (parent node))) 'red)
			(right-rotate tree (parent (parent node)))))))

	      (let ((tmp (left (parent (parent node)))))
		(if (eq 'red (color tmp))
		    (progn
		      (setf (color (parent node)) 'black)
		      (setf (color tmp) 'black)
		      (setf (color (parent (parent node))) 'red)
		      (setf node (parent (parent node))))
		    (progn
		      (if (eq node (left (parent node)))
			  (progn
			    (setf node (parent node))
			    (right-rotate tree node)))
		      (progn
			(setf (color (parent node)) 'black)
			(setf (color (parent (parent node))) 'red)
			(left-rotate tree (parent (parent node)))))))))
    (setf (color (root tree)) 'black)))

(defmethod delete-node ((tree red-black-tree) (node red-black-node))
  (multiple-value-bind (splice-node child-node) (call-next-method)
    (if (eq 'black (color splice-node))
	(delete-fixup tree child-node))
    (values splice-node child-node)))

(defgeneric delete-fixup (tree node)
  (:method ((tree red-black-tree) (the-node red-black-node))
    (loop with node = the-node
       while (and (not (eq node (root tree)))
		  (eq (color node) 'black))
       do (if (eq node (left (parent node)))
	      ;;The main idea here is to convert cases 1-3 -> case 4
	      ;;and then handle case 4
	      ;;first if clause, node is on the left
	      (let ((sib (right (parent node))))
		(if (eq (color sib) 'red)
		    (progn  ;;case 1
		      (setf (color sib) 'black)
		      (setf (color (parent node)) 'red)
		      (left-rotate tree (parent node))
		      (setf sib (right (parent node)))))

		(if (and (eq (color (left sib)) 'black)
			 (eq (color (right sib)) 'black))
		    (progn  ;;case 2
		      (setf (color sib) 'red)
		      (setf node (parent node)))
		    (progn
		      (if (eq (color (right sib)) 'black)
			  (progn ;;case 3
			    (setf (color (left sib)) 'black)
			    (setf (color sib) 'red)
			    (right-rotate tree sib)
			    (setf sib (right (parent node)))))
		      ;;case 4
		      (setf (color sib) (color (parent node)))
		      (setf (color (parent node)) 'black)
		      (setf (color (right sib)) 'black)
		      (left-rotate tree (parent node))
		      (setf node (root tree)))))

	      ;;The main idea here is to convert cases 1-3 -> case 4
	      ;;and then handle case 4
	      ;;symetric else, node is on the right
	      (let ((sib (left (parent node))))
		(if (eq (color sib) 'red)
		    (progn ;;case 1
		      (setf (color sib) 'black)
		      (setf (color (parent node)) 'red)
		      (right-rotate tree (parent node))
		      (setf sib (left (parent node)))))

		(if (and (eq (color (right sib)) 'black)
			 (eq (color (left sib)) 'black))
		    (progn ;;case 2
		      (setf (color sib) 'red)
		      (setf node (parent node)))
		    (progn
		      (if (eq (color (left sib)) 'black)
			  (progn ;;case 3
			    (setf (color (right sib)) 'black)
			    (setf (color sib) 'red)
			    (left-rotate tree sib)
			    (setf sib (left (parent node)))))
		      ;;case 4
		      (setf (color sib) (color (parent node)))
		      (setf (color (parent node)) 'black)
		      (setf (color (left sib)) 'black)
		      (right-rotate tree (parent node))
		      (setf node (root tree))))))

	 finally (setf (color node) 'black))))
