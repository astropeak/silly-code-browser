;; data structure of the tree:
;; (root (child_1 (cchild_1)) (child_2) )
;; If `tree' is a tree, then (car tree) is the root element, (cdr tree) is the list of it's children, which are all trees.

;; TODO: make some tree functions from destructive to non-destructive. Then the head node will not needed.

(defconst tree-head-element ">ROOT_OF_TREE<")

(defun tree-head-element-p (elt)
  "Judge whether `elt' is a tree head element"
  (and (stringp elt)
       (string= elt tree-head-element)))


(defun tree-get-element-and-parent (tree elt &optional predicate parent)
  "Find in a tree an element and its parent subtree. The car is the element subtree, the cdr is its parent subtree. nil means not exist, both for the element or its partent. the `parent' parameter is not given, then `tree' is the root. The parameter will not given in most cases. `predicate' is the compare function who takes two args,elm1 and elm2, if nil, eq will be used"
  (or predicate (setq predicate 'eq))

  (and tree
       (if (funcall predicate (car tree) elt)
	   (cons tree parent)
	 
	 ;; TODO: write a macro: dolist-if. Its form is (dolist-if (var list condition) BODY... ). Same as dolist except check condition is true before every iteration.
	 (let ((child-list (cdr tree))
	       (rst))
	   (while (and child-list (not rst))
	     (setq rst (tree-get-element-and-parent (car child-list) elt predicate tree))
	     (setq child-list (cdr child-list)))
	   rst))))

(defun tree-get-element (tree elt)
  "Get this element(a pointer to it, modify this element will modify the tree) of the given `elt' in `tree'"
  (car 
   (car (tree-get-element-and-parent tree elt 'equal)))) ;this element tree

(defun tree-get-parent (tree elt)
  "Get the parent element of the given `elt' in `tree'"
  (car 
   (cdr (tree-get-element-and-parent tree elt 'equal)))) ;the parent tree

(defun tree-get-children (tree elt)
  "Get the child elements of the given `elt' in `tree'"
  (mapcar 'car 
	  (cdr						   ;the list of children of this element
	   (car (tree-get-element-and-parent tree elt 'equal)))))


(defun tree-add-element (tree parent-elt elt)
  "Add `elt' as a child of `parent-elt' element, in `tree'."
  (let ((parent-subtree 
	 (car (tree-get-element-and-parent tree parent-elt 'equal))))
    (and parent-subtree
	 (listp parent-subtree)
	 (nconc parent-subtree (list (list elt))))))

;; TODO: This function may not needed anymore.
(defun tree-add-child (parent elt)
  "Add `elt' as a child of `parent', which is a tree. Return the new created child tree"
  (and (listp parent)
       (let ((rst (list elt)))
	 (nconc parent (list rst))
	 rst)))

(defun tree-delete-element (tree elt)
  "Delete `elt' from tree. Children of `elt' will become child of `elt''s parent. Return the parent element of `elt'(maybe the head element), if `elt' not exist in `tree', return nil, if `elt' is head element, return head element"
  (if (tree-head-element-p elt)
      ;; Head element can't be deleted.
      (progn 
	(message "Error: head element can't be deleted")
	elt)

    (let* ((rst (tree-get-element-and-parent tree elt 'equal))
	   (subtree (car rst))
	   (parent-tree (cdr rst)))
      
      (message "subtree=%s, parent-tree=%s"
	       subtree parent-tree)

      ;; parent of `elt' will always not be nil, because we have a head node
      ;; Add all children of `elt' to its parent
      (dolist (e (cdr subtree))
	(nconc parent-tree (list e))
	(message "e=%s, parent-tree=%s" e parent-tree)
	)

      (delete subtree parent-tree)
      (message "subtree=%s, parent-tree=%s"
	       subtree parent-tree)
      
      (car parent-tree))))

(defun tree-decendent-p (tree elt1 elt2)
  "Check if elt1 is a decendent of elt2 in the tree"
  )

(defun tree-move-subtree (tree elt parent-elt)
  "Move the subtree whose root is `elt' to the tree whose root is `parent-elt'. BUG: if we move a element subtree to a child of it, there will be wrong. There will be a ring. So we must ensure parent is not a decendent of element before moving."
  (let* ((rst (tree-get-element-and-parent tree elt 'equal))
	 (subtree (car rst))
	 (parent-subtree-old (cdr rst))
	 (parent-subtree-new (car (tree-get-element-and-parent tree parent-elt 'equal))))
    (when (and subtree parent-subtree-new)
      (if (eq parent-subtree-new parent-subtree-old)
	  (message "'%s' already the child of '%s', no needed to move" elt parent-elt)
	(nconc parent-subtree-new (list subtree))
	(delete subtree parent-subtree-old)))))

(defun tree-create ()
  (cons tree-head-element nil))

(defun tree-print (tree &optional fn fn-header depth)
  "Print the tree in current buffer. `fn' is a function to convert the element to its printed string. `depth' is the orignal depth of print. `fn-header' is a function construct the header of each printed line"
  (or fn (setq fn (lambda (e) e)))
  (or depth (setq depth 0))
  (or fn-header (setq fn-header (lambda (d) (make-string d ?*))))
  
  (and tree
       (progn 
	 (insert (format "%s%s"
			 (funcall fn-header depth)
			 (funcall fn (car tree))))
	 
	 (let ((child-list (cdr tree)))
	   (while child-list
	     (tree-print (car child-list) fn fn-header (+ depth 1))
	     (setq child-list (cdr child-list)))))))


;; (tree-print (nth 1 scb-jump-history) (lambda (e)
;; 				       (format "%s:%d"
;; 					       (scb-bookmark-fname e)
;; 					       (scb-bookmark-linum e))))

;; (setq tr (tree-create))
;; (setq cur tr)
;; (setq cur (tree-add-child cur 81))
;; (tree-get-parent tr 8)

;; (setq cur (cdr (tree-get-element-and-parent tr 8)))
(provide 'tree)
