;;;; stack.scm - Stack data type.

;; Make a stack containing ELEMENTS.
(define (make-stack . elements)
  elements)

;; Check if STACK is empty.
(define (stack-empty? stack)
  (null? stack))

;; Push ELEMENT onto STACK and return STACK.
(define (stack-push stack element)
  (cons element stack))

;; Pop an element from STACK and return STACK.
(define (stack-pop stack)
  (if (stack-empty? stack)
      (error "stack-pop: Stack is empty.")
      (cdr stack)))

;; Return the top element of STACK.
(define (stack-top stack)
  (if (stack-empty? stack)
      (error "stack-top: Stack is empty.")
      (car stack)))
