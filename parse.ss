#!chezscheme
(library (parse)
  (export
    parse)

  (import
    (chezscheme)
    (token)
    (util/halt)
    (util/hash)
    (util/stack)
    (ast))
  
  
  (define parse-define
    (lambda (flow ptr)
      (define name (token::ident-value
                     (list-ref flow (+ 1 ptr))))
      (define ptr-output (parse-expr flow (+ ptr 3)))
      (if (not (string=? 
                 (token::symbol-value
                 (list-ref flow (+ ptr 2))) "="))
          (halt "Invalid define syntax"))
      (cons (car ptr-output)
            (make-ast::define name (cdr ptr-output)))))


  (define parse-assign
    (lambda (flow ptr)
      (define name (token::ident-value
                     (list-ref flow ptr)))
      (define ptr-output (parse-expr flow (+ ptr 2)))
      (if (not (string=? 
                 (token::symbol-value
                 (list-ref flow (+ ptr 1))) "="))
          (halt "Invalid define syntax, expect `=`"))
      (cons (car ptr-output)
            (make-ast::assign name (cdr ptr-output)))))


  (define parse-if
    (lambda (flow ptr)
      (define condition 'null)
      (define body '())

      (if (not (string=?
                 (token::symbol-value
                 (list-ref flow (+ ptr 1))) "("))
          (halt "Invalid if syntax, expect `(`"))

      (let ((temp (parse-expr flow (+ ptr 2))))
        (set! ptr (car temp))
        (set! condition (cdr temp)))
      
      (let ((token (list-ref flow ptr)))
        (if (and
              (token::symbol? token)
              (string=? "{" (token::symbol-value token)))

          (let loop ((inner-ptr (+ 1 ptr))
                     (buffer '()))
            (define token (list-ref flow inner-ptr))
            (if (and
                  (token::symbol? token)
                  (string=? "}" (token::symbol-value token)))
              (begin
                (set! body buffer)
                (set! ptr inner-ptr))
              (let ((temp (next flow ptr)))
                (loop (car temp) (append buffer `(,(cdr temp))))))
            )

          (let ((temp (parse-expr flow ptr)))
            (set! ptr (car temp))
            (set! body `(,(cdr temp))))))
        
        (cons ptr (make-ast::if condition body))
      ))


  (define parse-switch
    (lambda (flow ptr)
      ('TODO)))


  (define parse-cond
    (lambda (flow ptr)
      ('TODO)))


  (define parse-while
    (lambda (flow ptr)
      ('TODO)))


  (define parse-func
    (lambda (flow ptr)
      ('TODO)))


  (define parse-class
    (lambda (flow ptr)
      ('TODO)))


  (define parse-list
    (lambda (flow ptr)
      (let loop ((inner-ptr (+ 1 ptr))
                 (buffer '()))
        (define token (list-ref flow inner-ptr))
        (cond 
          ((and
              (token::symbol? token)
              (string=? "]" (token::symbol-value token)))
             (cons (+ 2 inner-ptr) (make-ast::list (car buffer))))
          ((and
              (token::symbol? token)
              (or
                (string=? "#[" (token::symbol-value token))
                (string=? "<<" (token::symbol-value token))
                (string=? "#{" (token::symbol-value token))))
             (let ((temp (parse-expr flow inner-ptr)))
               (loop (car temp)
                     (append buffer `(,(cdr temp))))))
          ((or
             (token::number? token)
             (token::string? token)
             (token::char? token)
             (token::boolean? token)
             (token::ident? token))
            (let ((temp (parse-expr flow inner-ptr)))
              (loop (car temp)
                    (append buffer `(,(cdr temp))))))
          (else (halt "Invalid syntax in list literal")))
        )))
  

  (define parse-vec
    (lambda (flow ptr)
      (let loop ((inner-ptr (+ 1 ptr))
                 (buffer '()))
        (define token (list-ref flow inner-ptr))
        (cond
          ((and
              (token::symbol? token)
              (string=? ">>" (token::symbol-value token)))
             (cons inner-ptr (make-ast::vec (list->vector buffer))))
          ((and
              (token::symbol? token)
              (or
                (string=? "#[" (token::symbol-value token))
                (string=? "<<" (token::symbol-value token))
                (string=? "#{" (token::symbol-value token))))
             (let ((temp (parse-expr flow inner-ptr)))
               (loop (car temp)
                     (append buffer `(,(cdr temp))))))
          ((or
             (token::number? token)
             (token::string? token)
             (token::char? token)
             (token::boolean? token)
             (token::ident? token))
            (let ((temp (parse-expr flow inner-ptr)))
              (loop (car temp)
                    (append buffer `(,(cdr temp))))))
          (else (halt "Invalid syntax in vector literal"))))))


  (define parse-hashtable
    (lambda (flow ptr)
      (define buffer (hashtable))
      (let loop ((inner-ptr (+ 1 ptr)))
        (define token (list-ref flow inner-ptr))
        (cond
          ((and
              (token::symbol? token)
              (string=? "}" (token::symbol-value token)))
             (cons inner-ptr (make-ast::hashtable buffer)))
          ((and
              (token::symbol? token)
              (or
                (string=? "#[" (token::symbol-value token))
                (string=? "<<" (token::symbol-value token))
                (string=? "#{" (token::symbol-value token))))
             (let ((k (parse-expr flow inner-ptr)))
               (if (not (and 
                          (token::symbol? (list-ref flow (car k))) 
                          (string=? "=>" (token::symbol-value (car k)))))
                 (halt "Invalid syntax in hashtable literal, expect `=>`"))
               (let ((v (parse-expr flow (+ 1 (car k)))))
                 (hashtable-set! buffer (cdr k) (cdr v))
                 (loop (car v)))))

          ((or
             (token::number? token)
             (token::string? token)
             (token::char? token)
             (token::boolean? token)
             (token::ident? token))

            (let ((k (parse-expr flow inner-ptr)))
              (if (not (and 
                         (token::symbol? (list-ref flow (car k))) 
                         (string=? "=>" (token::symbol-value (car k)))))
                (halt "Invalid syntax in hashtable literal, expect `=>`"))
              (let ((v (parse-expr flow (+ 1 (car k)))))
                (hashtable-set! buffer (cdr k) (cdr v))
                (loop (car v)))))

          (else (halt "Invalid syntax in list literal"))))))
  

  (define priority
    (lambda (op)
      (if (token::symbol? op)
        (set! op (token::symbol-value op)))
        (case op
          (("++" "!") 8)
          (("*" "/" "%") 7)
          (("+" "-") 6)
          (("<" ">" "<=" ">=") 5)
          ("&" 4)
          ("^" 3)
          ("|" 2)
          ("&&" 1)
          ("||" 0))))


  (define reverse-polish
    (lambda (eles)
      (define num-stack (Stack))
      (define op-stack (Stack))
      (let loop ((tokens eles))
        (if (null? tokens)
          (let loop ()
            (if (stack-empty? op-stack)
              num-stack
              (begin
                (stack-push! num-stack (stack-pop! op-stack))
                (loop))))

          (let ((token (car tokens)))
          (cond
            ((token::symbol? token)
              (case (token::symbol-value token)
                (("+" "-" "*" "/" "%"
                  "**" "==" "!=" "<" ">"
                  "<=" ">=" "^" "&" "|"
                  "&&" "||")
                 (if (stack-empty? op-stack)
                   (stack-push! op-stack 
                     (token::symbol-value token))
                   (case (stack-top op-stack)
                     ("(" (stack-push! op-stack
                            (token::symbol-value token)))

                     (else
                       (cond
                         ((= 
                           (priority (stack-top op-stack))
                           (priority (token::symbol-value token)))
                           (stack-push! op-stack (token::symbol-value token)))
                         ((<
                           (priority (stack-top op-stack))
                           (priority (token::symbol-value token)))
                           (stack-push! op-stack (token::symbol-value token)))
                         ((> 
                           (priority (stack-top op-stack))
                           (priority (token::symbol-value token)))
                           (stack-push! num-stack (stack-pop! op-stack))
                           (stack-push! op-stack (token::symbol-value token)))
                         )))))
                
                ("(" (stack-push! op-stack (token::symbol-value token)))
                (")" (let loop ()
                            (if (stack-empty? op-stack) 'end)
                            (case (stack-top op-stack)
                              ("(" (stack-pop! op-stack))
                              (else
                                (stack-push! num-stack
                                             (stack-pop! op-stack))
                                (loop)))))))

            ((token::ident? token)
              (case (token::symbol-value (cadr tokens))
                ("[" 'TODO)
                ("(" 'TODO)
                ("." 'TODO)
                (else (stack-push! num-stack token))))

            ((or
              (token::number? token)
              (token::string? token)
              (token::char? token)
              (token::boolean? token)) (stack-push! num-stack token))
              
            (else (stack-push! num-stack token)))
            (loop (cdr tokens))))        
      )))

  
  (define parse-re-polish
    (lambda (exprs)
      (define s (Stack))
      (let loop ((exprs exprs))
        (if (null? exprs)
          s
          (let ((exp (car exprs))
                (fn 'null))
            (if (string? exp)
              (begin
                (case exp
                  ("+" (set! fn make-ast::+))
                  ("-" (set! fn make-ast::-))
                  ("*" (set! fn make-ast::*))
                  ("/" (set! fn make-ast::/))
                  ("%" (set! fn make-ast::%))
                  ("**" (set! fn make-ast::**))
                  ("==" (set! fn make-ast::==))
                  ("!=" (set! fn make-ast::!=))
                  ("<" (set! fn make-ast::<))
                  (">" (set! fn make-ast::>))
                  ("<=" (set! fn make-ast::<=))
                  (">=" (set! fn make-ast::>=))
                  ("^" (set! fn make-ast::^))
                  ("&" (set! fn make-ast::&))
                  ("|" (set! fn make-ast::\|))
                  ("||" (set! fn make-ast::\|\|))
                  ("&&" (set! fn make-ast::&&)))
                  
                (let ((right (stack-pop! s)))
                  (stack-push! s (fn (stack-pop! s) right)))
                (loop (cdr exprs)))

              (begin
                (stack-push! s exp)
                (loop (cdr exprs)))))))))


  ;; stop while reading:
  ;; `;` `,` `]` `}` `>>`
  (define parse-expr
    (lambda (flow ptr)
      (define result 'null)
      (define token (list-ref flow ptr))
      (cond
        ((and
           (token::symbol? token)
           (string=? "," (token::symbol-value token)))
          'TODO)
        
        ((token::symbol? token)
           (case (token::symbol-value token)
             ("#[" (let ((temp (parse-list flow ptr)))
                     (set! ptr (car temp))
                     (set! result (cdr temp))))
             ("<<" (let ((temp (parse-vec flow ptr)))
                     (set! ptr (car temp))
                     (set! result (cdr temp))))
             ("#{" (let ((temp (parse-hashtable flow ptr)))
                     (set! ptr (car temp))
                     (set! result (cdr temp))))))

        ((token::ident? token)
           (let loop ((inner-ptr (+ 1 ptr))
                      (tmp token)
                      (attr #f))
             (define token (list-ref flow inner-ptr))
             (cond
               ((token::ident? token)
                  (case attr
                    (#t (loop (+ 1 inner-ptr) 
                              (make-ast::attr tmp token)
                              #f))
                    (#f (halt "Invalid syntax"))))
               ((and
                   (token::symbol? token)
                   (string=? "." (token::symbol-value token)))
                (loop (+ 1 inner-ptr) tmp #t))
               (else (set! ptr (+ 1 inner-ptr))
                     (set! result tmp)))))

        ((or
           (token::boolean? token)
           (token::char? token)
           (token::string? token)
           (token::number? token)
           (and
             (token::symbol? token)
             (string=? "(" (token::symbol-value token))))
          (let loop ((inner-ptr ptr)
                     (buffer '()))
            (define token (list-ref flow inner-ptr))
            (cond
              ((or
                 (token::boolean? token)
                 (token::char? token)
                 (token::string? token)
                 (token::number? token)) (loop 
                                           (+ 1 inner-ptr) 
                                           (append buffer `(,token))))
              ((token::symbol? token)
                (case (token::symbol-value token)
                  (("+" "-" "*" "/" "%"
                  "**" "==" "!=" "<" ">"
                  "<=" ">=" "^" "&" "|"
                  "&&" "||" "(" ")" "."
                  "[") (loop
                         (+ 1 inner-ptr)
                         (append buffer `(,token))))
                  
                  (("]" "}" ">>")
                     (set! ptr inner-ptr)
                     (set! result buffer))
                  (else (halt "Invalid syntax in expression"))))
              
              ((token::ident? token)
                 (cond
                   ((token::symbol? (list-ref flow (+ 1 inner-ptr)))
                     (case (token::symbol-value (list-ref flow (+ 1 inner-ptr)))
                       ("." (let inner-loop ((iinner-ptr (+ 2 inner-ptr))
                                       (tmp token))
                              (define token (list-ref flow iinner-ptr))
                              (cond 
                                ((token::ident? token) 
                                  (inner-loop (+ 1 iinner-ptr) 
                                        (make-ast::attr (token::ident-value tmp) 
                                                        (token::ident-value token))))
                                ((and
                                   (token::symbol? token)
                                   (string=? (token::symbol-value token) "."))
                                  (inner-loop (+ 1 inner-ptr) tmp))
                                (else (loop (+ 1 inner-ptr) (append buffer `(,tmp)))))))
                       ("(" 'TODO)
                       ("[" 'TODO)))

                   (else (loop (+ 1 inner-ptr) (append buffer `(,token))))))

              ((token::endline? token)
                (set! result (parse-re-polish (reverse-polish buffer)))
                (set! ptr (+ 1 inner-ptr))))))
        (else (halt "Invalid expression")))
        (cons ptr result)))
  

  (define next
    (lambda (flow ptr)
      (define token (list-ref flow ptr))
      (cond
        ((token::keyword? token)
          (case (token::keyword-value token)
            ('let (parse-define flow ptr))
            ('if (parse-if flow ptr))
            ('while (parse-while flow ptr))
            ('switch (parse-switch flow ptr))
            ('cond (parse-cond flow ptr))
            ('fn (parse-func flow ptr))
            ('class (parse-class flow ptr))
            ('import 'TODO)))

        ((token::symbol? token)
          (parse-expr flow ptr))
        ((token::ident? token)
          ((case (token::symbol-value 
                  (list-ref flow (+ 1 ptr)))
              ("=" (parse-assign flow ptr))
              (else (parse-expr flow ptr)))))
        ((token::eof? token)
          'end)
        (else (halt "Invalid syntax")))))

  (define parse
    (lambda (flow)
      (define temp '())
      (let loop ((ptr 0)
                 (output '()))
        (define token (list-ref flow ptr))
        (define block (next flow ptr))
        (if (eq? block 'end)
           output
           (loop (car block) (append output `(,(cdr block))))))))
)


