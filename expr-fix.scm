(import (chicken format)
        (chicken process-context))

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

  (format #t "~A -> ~A~%" input-fix output-fix))

(main (command-line-arguments))
