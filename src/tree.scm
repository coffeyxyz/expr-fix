;;;; tree.scm - Binary tree functions.

(declare (unit tree))

;; Get the root of a binary tree.
(define (root tree)
  (cond ((null? tree) '())
        ((list? tree) (car tree))
        (else tree)))

;; Get the left branch of a binary tree.
(define (left tree)
  (if (or (not (list? tree))
          (null? tree)
          (null? (cdr tree)))
      '()
      (cadr tree)))

;; Get the right branch of a binary tree.
(define (right tree)
  (if (or (not (list? tree))
          (null? tree)
          (null? (cdr tree))
          (null? (cddr tree)))
      '()
      (caddr tree)))

;; Get the string representation of a traversal of a binary tree.
(define (traverse order tree)
  (define (preorder tree)
    (unless (null? tree)
      (format #t "~A " (root tree))
      (preorder (left tree))
      (preorder (right tree))))
  (define (inorder tree)
    (unless (null? tree)
      (inorder (left tree))
      (format #t "~A " (root tree))
      (inorder (right tree))))
  (define (postorder tree)
    (unless (null? tree)
      (postorder (left tree))
      (postorder (right tree))
      (format #t "~A " (root tree))))
  (let* ((str (with-output-to-string
                (lambda ()
                  (case order
                    ((preorder) (preorder tree))
                    ((inorder) (inorder tree))
                    ((postorder) (postorder tree))))))
         (len (string-length str)))
    (if (> len 0)
        (substring str 0 (- len 1))
        str)))
