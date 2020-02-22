#!chezscheme
(library (ast)
  (export
    export
    
    make-ast::define
    ast::define?
    ast::define-name
    ast::define-value

    make-ast::assign
    ast::assign?
    ast::assign-name
    ast::assign-value

    make-ast::if
    ast::if?
    ast::if-condition
    ast::if-body

    make-ast::switch
    ast::switch?
    ast::switch-value
    ast::switch-cases

    make-ast::cond
    ast::cond?
    ast::cond-cases

    make-ast::while
    ast::while?
    ast::while-condition
    ast::while-body

    make-ast::subs
    ast::subs?
    ast::subs-name
    ast::subs-index

    make-ast::func
    ast::func?
    ast::func-name
    ast::func-body

    make-ast::invoke
    ast::invoke?
    ast::invoke-func
    ast::invoke-args

    make-ast::class
    ast::class?
    ast::class-name
    ast::class-attr
    ast::class-method

    make-ast::attr
    ast::attr?
    ast::attr-name
    ast::attr-field

    make-ast::list
    ast::list?
    ast::list-value

    make-ast::vec
    ast::vec?
    ast::vec-value

    make-ast::hashtable
    ast::hashtable?
    ast::hashtable-value

    make-ast::+
    ast::+?
    ast::+-left
    ast::+-right

    make-ast::-
    ast::-?
    ast::--left
    ast::--right

    make-ast::*
    ast::*?
    ast::*-left
    ast::*-right

    make-ast::/
    ast::/?
    ast::/-left
    ast::/-right

    make-ast::%
    ast::%?
    ast::%-left
    ast::%-right

    make-ast::**
    ast::**?
    ast::**-left
    ast::**-right

    make-ast::==
    ast::==?
    ast::==-left
    ast::==-right

    make-ast::<
    ast::<?
    ast::<-left
    ast::<-right

    make-ast::>
    ast::>?
    ast::>-left
    ast::>-right

    make-ast::<=
    ast::<=?
    ast::<=-left
    ast::<=-right

    make-ast::>=
    ast::>=?
    ast::>=-left
    ast::>=-right

    make-ast::^
    ast::^?
    ast::^-left
    ast::^-right

    make-ast::&
    ast::&?
    ast::&-left
    ast::&-right

    make-ast::\|
    ast::\|?
    ast::\|-left
    ast::\|-right

    make-ast::&&
    ast::&&?
    ast::&&-left
    ast::&&-right

    make-ast::\|\|
    ast::\|\|?
    ast::\|\|-left
    ast::\|\|-right

    make-ast::!=
    ast::!=?
    ast::!=-left
    ast::!=-right

    make-ast::!
    ast::!?
    ast::!-right

    make-ast::++
    ast::++?
    ast::++-left
    )
  (import
      (chezscheme))
      
  (define-record-type ast::define
    (fields name value))

  (define-record-type ast::assign
    (fields name value))

  (define-record-type ast::if
    (fields condition body))

  (define-record-type ast::switch
    (fields value cases))
  
  (define-record-type ast::cond
    (fields cases))

  (define-record-type ast::while
    (fields condition body))

  (define-record-type ast::subs
    (fields name index))

  (define-record-type ast::func
    (fields name body))

  (define-record-type ast::invoke
    (fields func args))

  (define-record-type ast::class
    (fields name attr method))

  (define-record-type ast::attr
    (fields name field))

  (define-record-type ast::list
    (fields value))

  (define-record-type ast::vec
    (fields value))

  (define-record-type ast::hashtable
    (fields value))

  ;; + - * / % ** ++
  ;; == != < > <= >=
  ;; ^ & |
  ;; && || !
  (define-record-type ast::+
    (fields left right))
  
  (define-record-type ast::-
    (fields left right))

  (define-record-type ast::*
    (fields left right))

  (define-record-type ast::/
    (fields left right))

  (define-record-type ast::%
    (fields left right))

  (define-record-type ast::**
    (fields left right))

  (define-record-type ast::==
    (fields left right))

  (define-record-type ast::!=
    (fields left right))

  (define-record-type ast::<
    (fields left right))

  (define-record-type ast::>
    (fields left right))

  (define-record-type ast::<=
    (fields left right))

  (define-record-type ast::>=
    (fields left right))

  (define-record-type ast::&
    (fields left right))

  (define-record-type ast::\|
    (fields left right))

  (define-record-type ast::^
    (fields left right))

  (define-record-type ast::&&
    (fields left right))

  (define-record-type ast::\|\|
    (fields left right))

  (define-record-type ast::++
    (fields left))

  (define-record-type ast::!
    (fields right))
)