;; Test tree
(setq tree (tree-create))

(tree-add-element tree tree-head-element "A")
(tree-add-element tree tree-head-element "B")
(tree-add-element tree tree-head-element "C")
(tree-add-element tree "A" "AA")
(tree-add-element tree "A" "AB")
(tree-add-element tree "AB" "AAA")

;; currently tree will be:
;;(">ROOT_OF_TREE<" ("A" ("AA") ("AB" ("AAA"))) ("B") ("C"))

;;(tree-move-subtree tree "AB" ">ROOT_OF_TREE<")
;; Problem: if we move a element under a child of it, there will be wrong. There will be a ring. So we must ensure parent is not a decendent of element before moving.
;;(tree-move-subtree tree "A" "AB")





(equal
   (make-scb-bookmark 
    :fname "AAA"
    :linum 30
    :content "CCCCCC"
    :time '(1 2 3))

   (make-scb-bookmark 
    :fname "AAA"
    :linum 30
    :content "CCCCCC"
    :time '(1 2 3))
   )
(tree-get-element-and-parent  scb-jump-history scb-jump-current 'equal)