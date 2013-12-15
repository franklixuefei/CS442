(define foldl 
  (lambda(f i l) 
    (if (null? l) i
        (foldl f (f (car l) i) (cdr l)))))

(define foldr
  (lambda(f i l) 
    (if (null? l) i
        (f (car l) (foldr f i (cdr l))))))

(define L '(-1 -1 2 -1 -1 -1))

(foldr + 0 L)

(foldr (lambda(x y) (+ y 1)) 0 L)

(foldl (lambda(x y) (if (>= x y) x y)) (car L) L)

(define p (lambda(x) (if(>= x 0) #t #f)))

(foldr (lambda(x y) (if (eq? y #t) #t (if (p x) #t #f))) #f L)

(define f (lambda(x) (+ 1 x)))

(foldr (lambda(x y)(cons (f x) y)) '() L)

(define M '(1 2 3 4 5))

(foldr cons M L)