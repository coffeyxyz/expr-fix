;;;; parser.scm - Mathematical expression parser.

(declare (unit parser))

;; Convert a list of tokens into a parse tree.
(define (parse-expr fix tokens)
  (define (prefix tokens)
    (make-tree (let ((token (car tokens)))
                 (set! (car tokens) #f)
                 (token-value token))
               (if (token-operator? (cadr tokens))
                   (prefix (cdr tokens))
                   (let ((token (cadr tokens)))
                     (set! (cadr tokens) #f)
                     (make-tree (token-value token))))
               (let loop ((i 2))
                 (if (list-ref tokens i)
                     (if (token-operator? (list-ref tokens i))
                         (prefix (list-tail tokens i))
                         (let ((token (list-ref tokens i)))
                           (set! (list-ref tokens i) #f)
                           (make-tree (token-value token))))
                     (loop (+ i 1))))))

  ;;(define (infix tokens)
  ;;  )

  ;;(define (postfix tokens)
  ;;  )

  (if (and (= (length tokens) 1)
           (eq? (token-type (car tokens)) 'number))
      (make-tree (token-value (car tokens)))
      (case fix
        ((prefix) (prefix tokens))
        ;;((infix) (infix tokens))
        ;;((postfix) (postfix tokens))
        )))
