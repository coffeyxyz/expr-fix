;;;; main.scm - Main function and REPL.

(declare (uses lexer)
         (uses parser)
         (uses tree))

(import (chicken format)
        (chicken io)
        (chicken process-context))

(define (main args)
  (define input-fix)
  (define output-fix)

  (let ((len (length args)))
    (if (not (= len 2))
        (begin (format #t "xpr-fix: Invalid argument count: ~A~%~
                           Usage: xpr-fix INPUT_FIX OUTPUT_FIX~%"
                       len)
               (exit 1))
        (begin (set! input-fix (string->symbol (car args)))
               (set! output-fix (string->symbol (cadr args))))))

  (format #t "~A -> ~A~%" input-fix output-fix)

  (let repl ((i 0))
    (format #t "~A> " i)
    (let ((line (read-line)))
      (when (eof-object? line)
        (newline)
        (exit))
      (format #t "-> ~A~%"
              (traverse output-fix
                        (parse-xpr input-fix
                                   (lex-xpr line)))))
    (repl (+ i 1))))

(main (command-line-arguments))
