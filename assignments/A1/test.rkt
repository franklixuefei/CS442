(define (abc n)
  (if (eq? n 0) #t
      (if (eq? n 1) #f
          5)))

(caddr '(1 2 3))

(cadddr '(1 2 3 4))

(define lst '(6 2 0 4 8))

(map (lambda(x) (set-cdr! lst (map (lambda(y) (+ y 1)) (cdr lst)))(set! lst (cdr lst)) (+ x 1)) lst)

(define n 0)

(define countme (lambda() (set! n (+ n 1)) n))