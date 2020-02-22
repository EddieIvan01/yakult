(library (util/halt)
  (export
    halt)
  (import
    (chezscheme))
  (define halt
    (lambda (msg)
      (printf "Exception occured: ~a" msg)
      (exit))))