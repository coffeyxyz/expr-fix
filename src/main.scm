;;;; main.scm - Main function and REPL.

(declare (uses lexer)
         (uses parser)
         (uses tree))

(import (chicken format)
        (chicken process-context))

(define (parse-fix-arg arg)
  (cond ((or (string-ci=? arg "pre")
             (string-ci=? arg "prefix"))
         'prefix)
        ((or (string-ci=? arg "in")
             (string-ci=? arg "infix"))
         'infix)
        ((or (string-ci=? arg "post")
             (string-ci=? arg "postfix"))
         'postfix)
        (else (format #t "xpr-fix: Invalid fix argument: ~A~%" arg)
              (exit 1))))

(define (main args)
  (unless (= (length args) 3)
    (format #t "xpr-fix: Invalid argument count: ~A~%~
                Usage: xpr-fix INPUT_FIX OUTPUT_FIX EXPRESSION~%"
            (length args))
    (exit 1))
  (format #t "~A~%"
          (traverse (parse-fix-arg (cadr args))
                    (parse-xpr (parse-fix-arg (car args))
                               (lex-xpr (caddr args))))))

(main (command-line-arguments))
