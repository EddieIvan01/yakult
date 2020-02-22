(library (lexer)
  (export 
    scan)
  (import 
    (chezscheme)
    (token)
    (util/halt))

  (define-syntax skip-comment-until-newline
    (syntax-rules ()
      ((_ code ptr)
        (let loop ((inner-ptr ptr))
          (define c (string-ref code inner-ptr))
          (if (char=? c #\newline)
            (set! ptr (+ 1 inner-ptr))
            (loop (+ inner-ptr 1)))))))


  (define-syntax skip-comment
    (syntax-rules ()
      ((_ code ptr)
        (let loop ((inner-ptr ptr)
                   (flag #f))
          (define c (string-ref code inner-ptr))
          (cond ((and flag (char=? c #\/)) 
                  (set! ptr inner-ptr)
                  (skip-comment-until-newline code ptr))
                ((char=? c #\/) (loop (+ inner-ptr 1) #t))
                (else (loop (+ inner-ptr 1) #f)))))))


  (define-syntax skip-blank
    (syntax-rules ()
      ((_ code ptr)
       (let loop ((inner-ptr ptr))
         (define c (string-ref code inner-ptr))
         (if (or (char=? c #\space)
                 (char=? c #\tab)
                 (char=? c #\newline)
                 (char=? c #\return))
             (loop (+ inner-ptr 1))
             (set! ptr inner-ptr))))))


  (define char-number?
    (lambda (c)
      (define ascii (char->integer c))
      (or (char=? c #\.) (and (< ascii 58) (> ascii 47)))))
  

  (define line-n
    (lambda (tokens)
      (let loop ((tokens tokens)
                 (n 1))
        (cond ((null? tokens) n)
              ((token::endline? (car tokens)) (loop (cdr tokens) (+ n 1)))
              (else (loop (cdr tokens) n))))))
  

  (define scan-number
    (lambda (code ptr)
      (let loop ((inner-ptr ptr)
                 (buffer '()))
        (define c (string-ref code inner-ptr))
        (if (char-number? c)
            (loop (+ inner-ptr 1) (append buffer `(,c)))
            (cons inner-ptr
                  (make-token::number 
                    (string->number (list->string buffer))))))))
  

  (define scan-char
    (lambda (code ptr)
      (if (and 
            (char=? #\' (string-ref code ptr)) 
            (char=? #\' (string-ref code (+ 2 ptr))))
         (cons (+ ptr 3) 
               (make-token::char (string-ref code (+ ptr 1))))
         (halt "Invalid char literal"))))
        

  (define scan-str
    (lambda (code ptr)
      (let loop ((inner-ptr (+ ptr 1))
                 (buffer '()))
        (define c (string-ref code inner-ptr))
        (case c
          (#\newline (halt "Invalid string literal"))
          (#\" (cons (+ 1 inner-ptr)
                     (make-token::string
                       (list->string buffer))))
          (else (loop (+ inner-ptr 1) (append buffer `(,c))))))))
  

  (define scan-bool
    (lambda (code ptr)
      (define c (string-ref code (+ 1 ptr)))
      (case c 
         (#\t (cons (+ ptr 2) (make-token::boolean #t)))
         (#\f (cons (+ ptr 2) (make-token::boolean #f)))
         (else (halt "Invalid boolean literal")))))
  

  ;; + - * / % ** ++
  ;; == != < > <= >=
  ;; =
  ;; ^ & |
  ;; && || !
  ;; [ ] { } < >
  ;; . , =>
  ;; #[ #{ << >>
  (define scan-symbol
    (lambda (code ptr)
      (define c (string-ref code ptr))
      (case c
        ((#\- #\/ #\% #\^ #\{ #\} #\[ #\] #\( #\) #\. #\,)
          (cons
            (+ 1 ptr)
            (make-token::symbol (list->string `(,c)))))

        (#\+ (if (char=? #\+ (string-ref code (+ ptr 1)))
               (cons (+ 2 ptr)
                     (make-token::symbol "++"))
               (cons (+ 1 ptr)
                     (make-token::symbol "+"))))

        (#\* (if (char=? #\* (string-ref code (+ ptr 1)))
                (cons (+ 2 ptr)
                      (make-token::symbol "**"))
                (cons (+ 1 ptr)
                      (make-token::symbol "*"))))

        (#\= (case (string-ref code (+ ptr 1))
              (#\= (cons (+ 2 ptr)
                     (make-token::symbol "==")))
              (#\> (cons (+ 2 ptr)
                     (make-token::symbol "=>")))
              (else (cons (+ 1 ptr)
                      (make-token::symbol "=")))))

        (#\= (if (char=? #\= (string-ref code (+ ptr 1)))
                (cons (+ 2 ptr)
                      (make-token::symbol "=="))
                (cons (+ 1 ptr)
                      (make-token::symbol "="))))

        (#\< (case (string-ref code (+ ptr 1))
               (#\= (cons (+ 2 ptr)
                      (make-token::symbol "<=")))
               (#\< (cons (+ 2 ptr)
                      (make-token::symbol "<<")))
               (else (cons
                       (+ 1 ptr)
                       (make-token::symbol "<")))))

        (#\> (case (string-ref code (+ ptr 1))
               (#\= (cons (+ 2 ptr)
                      (make-token::symbol ">=")))
               (#\> (cons (+ 2 ptr)
                      (make-token::symbol ">>")))
               (else (cons
                       (+ 1 ptr)
                       (make-token::symbol ">")))))

        (#\& (if (char=? #\& (string-ref code (+ ptr 1)))
                (cons (+ 2 ptr)
                      (make-token::symbol "&&"))
                (cons (+ 1 ptr)
                      (make-token::symbol "&"))))

        (#\| (if (char=? #\| (string-ref code (+ ptr 1)))
                (cons (+ 2 ptr)
                      (make-token::symbol "||"))
                (cons (+ 1 ptr)
                      (make-token::symbol "|"))))

        (#\# (case (string-ref code (+ ptr 1))
              (#\[ (cons (+ 2 ptr)
                     (make-token::symbol "#[")))
              (#\{ (cons (+ 2 ptr)
                     (make-token::symbol "#{")))
              ((#\t #\f) 
                (cons (+ 2 ptr)
                  (make-token::symbol 
                    (string->symbol (substring code ptr (+ ptr 2))))))
              (else (halt "Invalid syntax"))))
        (else (halt "Invalid syntax")))))
  

  (define keywords
    '(let if while break continue switch
      case cond fn return class method attr
      require import))
  
 
  (define scan-keyword-or-ident
    (lambda (code ptr)
      (define value 'null)
      (let loop ((inner-ptr ptr)
                 (buffer '()))
        (define c (string-ref code inner-ptr))
        (cond 
          ((or (alpha? c) (digit? c))
             (loop (+ inner-ptr 1) (append buffer `(,c))))

          ((or (char=? c #\?) (char=? c #\!))
             (set! ptr (+ 1 inner-ptr))
             (set! buffer (append buffer `(,c)))
             (set! value (string->symbol (list->string buffer))))

          (else 
            (set! value (string->symbol (list->string buffer)))
            (set! ptr inner-ptr))))

      (if (member value keywords)
        (cons ptr (make-token::keyword value))
        (cons ptr (make-token::ident value)))))
  

  (define alpha?
    (lambda (c)
      (define ascii (char->integer c))
      (or (and (< ascii 91) (> ascii 64)) 
          (and (< ascii 123) (> ascii 96))
          (char=? c #\_))))
  

  (define digit?
    (lambda (c)
      (define ascii (char->integer c))
      (and (< ascii 58) (> ascii 47))))


  (define scan 
    (lambda (code)
      (define len (string-length code))
      (define temp '())
      (let loop ((ptr 0)
                 (output '()))
        (if (= ptr len)
          (append output `(,(make-token::eof)))

          (begin
            (case (string-ref code ptr)
              (#\/ (if (char=? (string-ref code (+ ptr 1)) #\/)
                     (begin (skip-comment code ptr) 
                            (set! temp (cons ptr 'nop)))))

              ((#\space #\tab #\newline #\return)
                 (skip-blank code ptr) (set! temp (cons ptr 'nop)))

              ((#\0 #\1 #\2 #\3 #\4 
                #\5 #\6 #\7 #\8 #\9) (set! temp (scan-number code ptr)))

              (#\' (set! temp (scan-char code ptr)))
              (#\" (set! temp (scan-str code ptr)))
              (#\; (set! temp `(,(+ ptr 1) . ,(make-token::endline))))

              (else
                (if (alpha? (string-ref code ptr))
                  (set! temp (scan-keyword-or-ident code ptr))
                  (set! temp (scan-symbol code ptr)))))

            (loop (car temp) 
                  (append output
                    (case (cdr temp)
                      ('nop '())
                      (else `(,(cdr temp)))))))))))
)
