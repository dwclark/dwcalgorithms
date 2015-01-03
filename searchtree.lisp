(in-package #:dwcalgorithms)

(defclass search-node ()
  ((data :initform nil :initarg :data :accessor data)
   (right :initarg :right :accessor right)
   (left :initarg :left :accessor left)
   (parent :initarg :parent :accessor parent)))

(defparameter *nil-search-node* (make-instance 'search-node))

(defmethod initialize-instance :after ((node search-node) &key)
  (if (eq (type-of node) 'search-node)
      (progn 
	(if (not (slot-boundp node 'right))
	    (setf (right node) *nil-search-node*))
	
	(if (not (slot-boundp node 'left))
	    (setf (left node) *nil-search-node*))
	
	(if (not (slot-boundp node 'parent))
	    (setf (parent node) *nil-search-node*)))))

(defmethod nil? ((node search-node))
  (eq node *nil-search-node*))

(defclass search-tree ()
  ((root :initform *nil-search-node* :initarg :root :accessor root)
   (cmp :initform #'<=> :initarg :cmp :reader cmp)))

(defun load-elements (tree elements)
  (if (not (null elements))
      (reduce (lambda (arg-tree element)
                (insert arg-tree element)
                arg-tree) 
              elements :initial-value tree)))

(defmethod initialize-instance :after ((tree search-tree) &key (elements nil))
  (if (or (eq (type-of tree) 'search-tree)
	  (eq (type-of tree) 'rb-tree))
      (load-elements tree elements)))

(defgeneric new-node (tree value))

(defmethod new-node ((tree search-tree) value)
  (make-instance 'search-node :data value))

(defgeneric nil-node (tree)
  (:method ((tree search-tree))
    *nil-search-node*))

(defgeneric insert (tree value))

(defmethod insert ((tree search-tree) value)
    (with-accessors ((root root) (cmp cmp)) tree
      (let ((new-node (new-node tree value))
            (new-parent (nil-node tree))
            (iter-node root))
        
        ;;Find the insertion point.  iter-node tracks search for new
        ;;insertion point.  parent-node tracks the parent of iter-node,
        ;;which will eventually become the parent of new-node.  When iter-node
        ;;is nil, we have found the insertion point and the parent of new-node.
        (loop 
           while (not (nil? iter-node))
           do (progn
                (setf new-parent iter-node)
                (if (<? cmp value (data iter-node))
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
              (if (<? cmp value (data new-parent))
                  (setf (left new-parent) new-node)
                  (setf (right new-parent) new-node))))
        
        new-node)))

(defgeneric inorder-walk (tree func))

(defmethod inorder-walk ((tree search-tree) func)
  (labels 
      ((do-walk (node)
         (if (not (nil? node))
             (progn (do-walk (left node))
                    (funcall func (data node))
                    (do-walk (right node))))))
    (do-walk (root tree))))

(defgeneric search-node (tree node val))

(defmethod search-node ((tree search-tree) (node search-node) val)
  (if (nil? node)
      (return-from search-node node))

  (case (funcall (cmp tree) val (data node))
    ((-1) (search-node tree (left node) val))
    ((0) node)
    ((1) (search-node tree (right node) val))))

(defmacro extract-data (&body body)
  (with-unique-names (node)
    `(let ((,node (progn ,@body)))
       (if (nil? ,node)
           nil
           (data ,node)))))

(defmethod search ((tree search-tree) val)
  (extract-data (search-node tree (root tree) val)))

(defgeneric min-node (tree node))

(defmethod min-node ((tree search-tree) (node search-node))
  ;;Loop until there are no left nodes to follow.  Once
  ;;(left tmp) is nil, we have found the left most node
  (loop 
     with tmp = node 
     while (not (nil? (left tmp)))
     do (setf tmp (left tmp))
     finally (return tmp)))

(defmethod minimum ((tree search-tree))
  (extract-data (min-node tree (root tree))))

(defgeneric max-node (tree node))

(defmethod max-node ((tree search-tree) (node search-node))
  ;;Loop until there are no right nodes to follow.  Once
  ;;(right tmp) is nil, we have found the right most node
  (loop 
     with tmp = node
     while (not (nil? (right tmp)))
     do (setf tmp (right tmp))
     finally (return tmp)))

(defmethod maximum ((tree search-tree))
  (extract-data (max-node tree (root tree))))

(defgeneric successor-node (tree node))

(defmethod successor-node ((tree search-tree) (node search-node))
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
  (loop 
     with iter-node = node 
     with parent-node = (parent iter-node)
     while (and (not (nil? parent-node)) (eq iter-node (right parent-node)))
     do (progn
          (setf iter-node parent-node)
          (setf parent-node (parent parent-node)))
     finally (return parent-node)))

(defmethod successor ((tree search-tree) val)
  (extract-data (successor-node tree (search-node tree (root tree) val))))

(defgeneric predecessor-node (tree node))

(defmethod predecessor-node ((tree search-tree) (node search-node))
  ;;Symmetric to successor-node, but with max-node in place
  ;;of min-node and left and rights swapped.  See successor-node
  ;;for more details
  (if (not (nil? (left node)))
      (return-from predecessor-node (max-node tree (left node))))

  (loop 
     with iter-node = node
     with parent-node = (parent iter-node)
     while (and (not (nil? parent-node)) (eq iter-node (left parent-node)))
     do (progn
          (setf iter-node parent-node)
          (setf parent-node (parent parent-node)))
     finally (return parent-node)))

(defmethod predecessor ((tree search-tree) val)
  (extract-data (predecessor-node tree (search-node tree (root tree) val))))

(defgeneric delete-node (tree node))

(defmethod delete-node ((tree search-tree) (node search-node))
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
      
      (values splice-node child-node)))

(defmethod delete ((tree search-tree) val)
  (let ((node (search-node tree (root tree) val)))
    (if (not (nil? node))
        (extract-data (delete-node tree node)))))

(defgeneric left-rotate (tree x))

(defmethod left-rotate ((tree search-tree) (x search-node))
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
    (setf (parent x) y)))

(defgeneric right-rotate (tree x))

(defmethod right-rotate ((tree search-tree) (x search-node))
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
    (setf (parent x) y)))

(defgeneric height-to-value (tree val))

(defmethod height-to-value ((tree search-tree) val)
  (loop 
     with node = (root tree)
     with height = 1
     while (not (nil? node))
     do (let ((res (funcall (cmp tree) (data node) val)))
          (cond 
            ((= 0 res) 
             (return-from height-to-value height))
		  
            ((= -1 res)
             (incf height)
             (setf node (left node)))
		  
            (t 
             (incf height)
             (setf node (right node)))))
     finally (return height)))
