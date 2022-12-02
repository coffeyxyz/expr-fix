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
        (chicken io)
        (chicken string))

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

(define tok-head)
(define tok-tail)

(define (init! tok-lst)
  (set! tok-tail tok-lst)
  (consume!))

(define (consume!)
  (if (null? tok-tail)
      (set! tok-head #f)
      (begin (set! tok-head (car tok-tail))
             (set! tok-tail (cdr tok-tail)))))

(define (next) tok-head)
(define (done?) (not (next)))
(define (match? type) (eq? (tok-t (next)) type))

(define (expect! type)
  (if (match? type)
      (consume!)
      (error 'expect!
             (printf "token type mismatch\n  expected: ~A\n  received: ~A\n"
                     type (tok-t (next))))))

(define (parse-num!)
  (if (match? 'n)
      (let ((tok (next)))
        (consume!)
        (tok-v tok))
      (error 'parse-num! "failed to parse number" (next))))

(define (parse-op!)
  (if (match? 'o)
      (let ((tok (next)))
        (consume!)
        (tok-v tok))
      (error 'parse-op! "failed to parse operator" (next))))

;; Recursive descent parser for the following grammar:
;;
;;     E -> N | O E E
;;
(define (parse-prefix line)
  (define (parse-expr!)
    (cond ((done?) (error 'parse-expr! "missing tokens"))
          ((match? 'n) (parse-num!))
          ((match? 'o) (list (parse-op!)
                             (parse-expr!)
                             (parse-expr!)))
          (else (error 'parse-expr! "invalid token" (next)))))
  (init! (lex-line line))
  (let ((tree (parse-expr!)))
    (if (done?)
        tree
        (error 'parse-prefix "too many tokens"))))

;; Stack automaton parser for the following grammar:
;;
;;     E -> N | E E O
;;
(define (parse-postfix line)
  (define stk '())
  (define (parse-expr!)
    (unless (done?)
      (cond ((match? 'n) (set! stk (cons (parse-num!) stk)))
            ((match? 'o)
             (if (or (null? stk) (null? (cdr stk)))
                 (error 'parse-expr! "missing arguments" (tok-v (next)))
                 (set! stk (cons (list (parse-op!)
                                       (cadr stk)
                                       (car stk))
                                 (cddr stk)))))
            (else (error 'parse-expr! "invalid token" (next))))
      (parse-expr!)))
  (init! (lex-line line))
  (parse-expr!)
  (cond ((null? stk) (error 'parse-postfix "missing tokens"))
        ((not (null? (cdr stk))) (error 'parse-postfix "too many tokens"))
        (else (car stk))))

;; main ------------------------------------------------------------------------

;;(define (main)
;;  )
