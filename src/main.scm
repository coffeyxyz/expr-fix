;;;; main.scm - Main function and REPL.

(import (chicken format)
        (chicken io)
        (chicken port)
        (chicken process-context)
        (chicken string))

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
