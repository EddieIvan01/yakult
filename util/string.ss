(define string-split
  (lambda (str sep)
    (define end (string-length str))
    (let loop ([ptr (- end 1)]
               [end end]
               [output '()])
      (if (= ptr -1)
          (cons (substring str 0 end) output)
          (if (char=? sep (string-ref str ptr))
              (loop (- ptr 1) ptr (cons (substring str (+ 1 ptr) end) output))
              (loop (- ptr 1) end output))))))