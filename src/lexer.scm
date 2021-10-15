;;;; lexer.scm - Mathematical expression lexer.

(declare (unit lexer))

(import (chicken string))

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
