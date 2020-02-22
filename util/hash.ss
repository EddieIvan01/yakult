(library (util/hash)
  (export
    hashtable)
  (import
    (chezscheme))
  
  #|
  (define _equal?
    (lambda (a b)
      ((cond
        ((type-xx? a) 
          (and
            (type-xx? b)
            (equal? (type-xx-x a) (type-xx-x b))))))))
  |#

  (define hashtable
    (lambda ()
      (make-hashtable equal-hash equal?))))

