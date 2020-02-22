(library (token)
  (export
    make-token::number
    token::number?
    token::number-value

    make-token::char
    token::char?
    token::char-value

    make-token::string
    token::string?
    token::string-value

    make-token::boolean
    token::boolean?
    token::boolean-value

    make-token::symbol
    token::symbol?
    token::symbol-value

    make-token::keyword
    token::keyword?
    token::keyword-value

    make-token::ident
    token::ident?
    token::ident-value
    
    make-token::endline
    token::endline?

    make-token::eof
    token::eof?
    
    token-value)

  (import
    (chezscheme))

  (define-record-type token::number 
    (fields value))

  (define-record-type token::char
    (fields value))

  (define-record-type token::string 
    (fields value))

  (define-record-type token::boolean
    (fields value))

  (define-record-type token::symbol
    (fields value))

  (define-record-type token::keyword
    (fields value))

  (define-record-type token::ident
    (fields value))
      
  (define-record-type token::endline)
  (define-record-type token::eof)
  
  (define token-value
    (lambda (token)
      (cond
        ((token::number? token) (token::number-value token))
        ((token::char? token) (token::char-value token))
        ((token::string? token) (token::string-value token))
        ((token::boolean? token) (token::boolean-value token))
        ((token::symbol? token) (token::symbol-value token))
        ((token::keyword? token) (token::keyword-value token))
        ((token::ident? token) (token::ident-value token)))))

  )

