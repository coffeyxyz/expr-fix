;; xpr-fix.scm - xpr-fix in CHICKEN Scheme
;; Copyright (C) 2022 Robert Coffey
;; Released under the MIT license.

;; Grammars --------------------------------------------------------------------
;;
;;     N -> ['+', '-']? ['0' - '9']+
;;     O -> '+' | '-' | '*' | '/'
;;
;; Prefix
;;
;;     E -> N | O ' ' E ' ' E
;;
;; Postfix
;;
;;     I tried to find an LL grammar... just going to use a stack automaton.
;;
;; Infix
;;
;;------------------------------------------------------------------------------

(import (chicken format)
        (chicken io))

;; token -----------------------------------------------------------------------
;;
;; token types: 'n (number), 'o (operation), 'p (parenthesis)

(define-record-type token
  (%make-token t v)
  token?
  (t tok-t)  ; type: symbol
  (v tok-v)) ; value: number

(define-record-printer (token tok out)
  (fprintf out "#<token ~A ~A>" (tok-t tok) (tok-v tok)))

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
                    (error 'lex-line "invalid token" word)))))))
  (lex-words (string-split line)))

;; parser ----------------------------------------------------------------------

(define (parse-num tok)
  (if (match? tok 'n)
      (tok-v tok)
      (error 'parse-num "failed to parse number" tok)))

(define (parse-op tok)
  (if (match? tok 'o)
      (tok-v tok)
      (error 'parse-op "failed to parse operator" tok)))

(define tok-curr)
(define tok-rest)

(define (match? tok type)
  (eq? (tok-t tok) type))

(define (next!)
  (if (null? tok-rest)
      #f
      (begin (set! tok-curr (car tok-rest))
             (set! tok-rest (cdr tok-rest))
             #t)))

(define (parse-prefix line)
  (define (parse-expr!)
    (unless (next!) (error 'parse-expr! "missing tokens"))
    (cond ((match? tok-curr 'n) (parse-num tok-curr))
          ((match? tok-curr 'o) (list (parse-op tok-curr)
                                      (parse-expr!)
                                      (parse-expr!)))
          (else (error 'parse-expr! "invalid token" tok-curr))))
  (set! tok-rest (lex-line line))
  (let ((tree (parse-expr!)))
    (if (null? tok-rest)
        tree
        (error 'parse-prefix "unparsed tokens remain"))))

(define (parse-postfix line)
  (define stk '())
  (define (parse-expr!)
    (when (next!)
      (cond ((match? tok-curr 'n) (set! stk (cons (parse-num tok-curr) stk)))
            ((match? tok-curr 'o)
             (if (or (null? stk) (null? (cdr stk)))
                 (error 'parse-expr! "missing arguments" (tok-v tok-curr))
                 (set! stk (cons (list (tok-v tok-curr) (cadr stk) (car stk))
                                 (cddr stk)))))
            (else (error 'parse-expr! "invalid token" tok-curr)))
      (parse-expr!)))
  (set! tok-rest (lex-line line))
  (parse-expr!)
  (cond ((null? stk) (error 'parse-postfix "missing tokens"))
        ((not (null? (cdr stk))) (error 'parse-postfix "too many tokens"))
        (else (car stk))))

;; main ------------------------------------------------------------------------

;;(define (main)
;;  )
