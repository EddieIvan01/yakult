(library (util/stack)
  (export
    Stack
    stack-top
    stack-empty?
    stack-push!
    stack-pop!)
  (import
    (chezscheme))

  (define Stack list)

  (define (stack-top stack)
    (if (null? (cdr stack))
      (car stack)
      (stack-top (cdr stack))))

  (define (stack-empty? stack)
    (null? stack))

  (define-syntax stack-push!
    (lambda (x)
      (syntax-case x ()
        ((stack-push! stack obj)
        (syntax (set! stack 
            (append stack (list obj))))))))

  (define-syntax stack-pop!
    (lambda (x)
      (syntax-case x ()
        ((stack-pop! stack)
        (syntax (let ((top (stack-top stack)))
                  (define (remove-last lst ret)
                    (if (null? (cdr lst))
                      ret
                      (remove-last 
                        (cdr lst) (append ret (list (car lst))))))
                  (set! stack (remove-last stack '()))
                  top)))))))