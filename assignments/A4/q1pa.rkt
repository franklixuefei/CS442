(define lazymap (lambda (f stream)
  (if (null? stream) '()
      (cons (f (car stream)) (delay (lazymap f (force (cdr stream))))))))

(define ones (cons 1 (delay ones)))

(define scar car)

(define scdr (lambda(x) (force (cdr x))))