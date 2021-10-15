;;;; stack.scm - Stack data type.

(declare (unit stack))

;; Make a stack containing ELEMENTS.
(define (make-stack elements)
  (if (list? elements)
      elements
      (error "make-stack: Bad argument type: ELEMENTS must be a list")))

;; Check if a stack is empty.
(define stack-empty? null?)

;; Get the length of a stack.
(define stack-length length)

;; Push ELEMENT onto STACK and return STACK.
(define (stack-push stack element)
  (cons element stack))

;; Pop an element from STACK and return STACK.
(define (stack-pop stack)
  (if (stack-empty? stack)
      (error "stack-pop: STACK is empty")
      (cdr stack)))

;; Pop N elements from STACK and return STACK.
(define (stack-pop-n stack n)
  (if (> n (stack-length stack))
      (error "stack-pop-n: STACK contains less than N elements")
      (list-tail stack n)))

;; Return the top element of STACK.
(define (stack-top stack)
  (if (stack-empty? stack)
      (error "stack-top: STACK is empty")
      (car stack)))

;; Return the Nth element from the top of the stack.
(define (stack-top-n stack n)
  (if (< n (stack-length stack))
      (list-ref stack n)
      (error "stack-top-n: STACK contains less than N+1 elements")))
