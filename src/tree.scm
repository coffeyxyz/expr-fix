;;;; tree.scm - Binary tree functions.

(declare (unit tree))

(import (chicken format)
        (chicken port))

(define-record-type tree
  (%make-tree root left right)
  tree?
  (root tree-root tree-root-set!)
  (left tree-left tree-left-set!)
  (right tree-right tree-right-set!))

(define (make-tree #!optional root left right)
  (%make-tree root left right))

;; Get the string representation of a traversal of a binary tree.
(define (traverse fix tree)
  (define (preorder tree)
    (when tree
      (format #t "~A " (tree-root tree))
      (preorder (tree-left tree))
      (preorder (tree-right tree))))

  (define (inorder tree)
    (when tree
      (inorder (tree-left tree))
      (format #t "~A " (tree-root tree))
      (inorder (tree-right tree))))

  (define (postorder tree)
    (when tree
      (postorder (tree-left tree))
      (postorder (tree-right tree))
      (format #t "~A " (tree-root tree))))

  (let* ((str (with-output-to-string
                (lambda ()
                  (case fix
                    ((prefix) (preorder tree))
                    ((infix) (inorder tree))
                    ((postfix) (postorder tree))))))
         (len (string-length str)))
    (substring str 0 (- len 1))))
