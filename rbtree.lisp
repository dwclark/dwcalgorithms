(in-package #:dwcalgorithms)

(defclass red-black-node (search-node)
  ((color :initarg :color :initform 'black :accessor color)))
  
(defparameter *nil-red-black-node* (make-instance 'red-black-node))

(defun init-rb-node (node nil-node)
  (if (not (slot-boundp node 'right))
      (setf (right node) nil-node))
  
  (if (not (slot-boundp node 'left))
      (setf (left node) nil-node))
  
  (if (not (slot-boundp node 'parent))
      (setf (parent node) nil-node)))

(defmethod initialize-instance :after ((node red-black-node) &key)
  (if (eq (type-of node) 'red-black-node)
      (init-rb-node node *nil-red-black-node*)))

(defmethod nil? ((node red-black-node))
  (eq *nil-red-black-node* node))

(defclass red-black-tree (search-tree)
  ((root :initform *nil-red-black-node* :initarg :root :accessor root)))

(defmethod new-node ((tree red-black-tree) value)
  (make-instance 'red-black-node :data value))

(defmethod nil-node ((tree red-black-tree))
  *nil-red-black-node*)

(defmethod insert ((tree red-black-tree) value)
  (let ((new-node (call-next-method)))
    (setf (color new-node) 'red)
    (insert-fixup tree new-node)))

(defgeneric insert-fixup (tree node))

(defmethod insert-fixup((tree red-black-tree) (the-node red-black-node))
  (loop 
     with node = the-node 
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

  (setf (color (root tree)) 'black))

(defmethod delete-node ((tree red-black-tree) (node red-black-node))
  (multiple-value-bind (splice-node child-node) (call-next-method)
    (if (eq 'black (color splice-node))
        (delete-fixup tree child-node splice-node))
    
    (values splice-node child-node)))

(defgeneric delete-fixup (tree node splice-node))

(defmethod delete-fixup ((tree red-black-tree) (the-node red-black-node) (splice-node red-black-node))
  (loop 
     with node = the-node
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
       
	 finally (setf (color node) 'black)))
