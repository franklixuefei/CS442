(define foldl 
  (lambda(f i l) 
    (if (null? l) i
        (foldl f (f (car l) i) (cdr l)))))

(define foldr
  (lambda(f i l) 
    (if (null? l) i
        (f (car l) (foldr f i (cdr l))))))

(define l '(1 2 3 4 5))


#(foldr (lambda(x y) (set! x 2)) '() l)

#(set-car! l 2);

(define (kill3 l) (set-cdr! (cdr l) (cdr (cdr (cdr l)))))

(define lst '(6 2 0 4 8))

(map (lambda(x) (set-cdr! lst (map (lambda(y) (+ y 1)) (cdr lst)))(set! lst (cdr lst)) (+ x 1)) lst)
