;; xpr-fix.scm - xpr-fix in CHICKEN Scheme
;;
;; GRAMMARS

#|

For all grammars, the following symbols are defined:

    N -> ['+', '-']? ['0' - '9']+
    O -> '+' | '-' | '*' | '/'

PREFIX
    E -> N | O ' ' E ' ' E

POSTFIX

INFIX

|#

;;
;; Copyright (C) 2022 Robert Coffey

(import (chicken format)
        (chicken io))

;; token -----------------------------------------------------------------------
;;
;; token types: 'n (number), 'o (operation), 'p (parenthesis)

(define-record-type token
  (%make-token t v)
  token?
  (t token-t)  ; type: symbol
  (v token-v)) ; value: number

(define-record-printer (token tok out)
  (fprintf out "#<token ~A ~A>" (token-t tok) (token-v tok)))

(define (make-token t #!optional (v (void)))
  (cond ((not (symbol? t)) (error 'make-token "type must be a symbol" t))
        ((not (memq t '(n o p))) (error 'make-token "invalid token type" t))
        (else (%make-token t v))))

;; lexer -----------------------------------------------------------------------

(define (lex-num word)
  (define num (string->number word))
  (if num (make-token 'n num) #f))

(define (lex-op word)
  (if (and (= (string-length word) 1)
           (memv (string-ref word 0) '(#\+ #\- #\* #\/)))
      (make-token 'o (string->symbol word))
      #f))

(define (lex-line line)
  (define (lex-words words)
    (if (null? words)
        '()
        (let* ((word (car words))
               (num (lex-num word)))
          (if num
              (cons num (lex-words (cdr words)))
              (let ((op (lex-op word)))
                (if op
                    (cons op (lex-words (cdr words)))
                    (error 'lexer "invalid token" word)))))))
  (lex-words (string-split line)))

;; parser ----------------------------------------------------------------------

(define (parse-num tok)
  (if (match? tok 'n)
      (token-v tok)
      (error 'parser "failed to parse number" tok)))

(define (parse-op tok)
  (if (match? tok 'o)
      (token-v tok)
      (error 'parser "failed to parse operator" tok)))

(define token-curr)
(define token-rest)

(define (match? tok type)
  (eq? (token-t tok) type))

(define (next!)
  (if (null? token-rest)
      (error 'parser "invalid expression")
      (begin (set! token-curr (car token-rest))
             (set! token-rest (cdr token-rest)))))

(define (parse-prefix line)
  (define (parse-expr!)
    (next!)
    (cond ((match? token-curr 'n) (parse-num token-curr))
          ((match? token-curr 'o) (list (parse-op token-curr)
                                        (parse-expr!)
                                        (parse-expr!)))
          (else (error 'parser "failed to parse expression"))))
  (set! token-rest (lex-line line))
  (let ((tree (parse-expr!)))
    (if (null? token-rest)
        tree
        (error 'parser "invalid expression"))))

;; main ------------------------------------------------------------------------

;;(define (main)
;;  )
