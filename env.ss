(library (env)
  (export
    export)
  (import
    (chezscheme)
    (util/halt))

  (define-record-type env (fields vars ref))
  
  (define-syntax new-env
    (syntax-rules ()
      ((_ all-env)
       (define env (make-env (make-eq-hashtable) 'null))
       (set! all-env (append all-env `(,env)))
       env)))

  (define-syntax ext-env
    (syntax-rules ()
      ((_ ref-env all-env)
       (define env (make-env (make-eq-hashtable) ref))
       (set! all-env (append all-env `(,env))))))

  (define-syntax env-set!
    (syntax-rules ()
      ((_ env-index name val all-env)
       (let ((value (hashtable-ref 
                      (env-vars 
                        (list-ref all-env env-index)) 
                      name 'null)))
         (cond
           ((eq? value 'null)
            (let ((ref (env-ref (list-ref all-env env-index))))
              (cond
                ((eq? 'null ref) (halt "Undefined"))
                (else (let loop ((env (list-ref all-env ref)))
                  (define h (env-vars env))
                  (define value (hashtable-ref h name 'null))
                  (case value
                    ('null (loop (list-ref all-env (env-ref env))))
                    (else
                      (hashtable-set! h name val))))))))
           (else (hashtable-set! 
                   (env-vars 
                     (list-ref all-env env-index) 
                     name val))))))))

  (define env-lookup
    (lambda (env-index name all-env)
      (define env (list-ref all-env env-index))
      (let ((value (hashtable-ref 
                     (env-vars env) 
                     name 'null)))
        (cond
          ((eq? value 'null)
           (let ((ref (env-ref env)))
             (cond
               ((eq? 'null ref) 'null)
               (else (env-lookup ref name all-env)))))
          (else value)))))
)