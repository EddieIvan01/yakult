(import (parse))
(import (lexer))
(import (ast))

(define code "let a = 2 * (1 + 3);")

(define t (scan code))
(printf "~s" t)

(define a (parse t))
(printf "~s" a)
