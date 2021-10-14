(import (chicken format)
        (chicken io)
        (chicken port)
        (chicken process-context)
        (chicken string))

;;; binary-tree ----------------------------------------------------------------

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

;;; lexer ----------------------------------------------------------------------

;; TODO Throw an error when STRING->TOKEN encounters an invalid number string.
;; TODO Add support for tokens not separated by spaces.

(define operator-characters '(#\+ #\- #\* #\/ #\( #\)))

;; Convert a string into a token.
(define (string->token str)
  (if (and (= (string-length str) 1)
           (member (string-ref str 0) operator-characters))
      (list 'operator (string-ref str 0))
      (list 'number (string->number str))))

;; Get the type of a token.
(define (token-type token)
  (car token))

;; Get the value of a token.
(define (token-value token)
  (cadr token))

;; Determine if a token is of the type: operator.
(define (token-operator? token)
  (eq? 'operator (token-type token)))

;; Determine if a token is of the type: number.
(define (token-number? token)
  (eq? 'number (token-type token)))

;; Get a list of the tokens contained within an expression string.
(define (lex-expr expr)
  (map string->token (string-split expr)))

;;; main -----------------------------------------------------------------------

(define (main args)
  (define input-fix)
  (define output-fix)

  (let ((len (length args)))
    (if (not (= len 2))
        (begin (format #t "expr-fix: Invalid argument count: ~A~%~
                           Usage: expr-fix INPUT_FIX OUTPUT_FIX~%"
                       len)
               (exit 1))
        (begin (set! input-fix (car args))
               (set! output-fix (cadr args)))))

  (format #t "~A -> ~A~%" input-fix output-fix)

  (let repl ((i 0))
    (format #t "~A> " i)
    (let ((line (read-line)))
      (format #t "-> ~A~%" line))
    (repl (+ i 1))))

(main (command-line-arguments))
