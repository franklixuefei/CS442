;; Booleans
(define ctrue (lambda (x) (lambda (y) x)))
(define cfalse (lambda (x) (lambda (y) y)))

;; Lists
(define ccons (lambda (h) (lambda (t) (lambda (s) ((s h) t)))))

(define ccar (lambda (p) (p ctrue)))
(define ccdr (lambda (p) (p cfalse)))

(define cnull? (lambda (p) (p (lambda (x) (lambda (y) cfalse)))))
(define cnil (lambda (x) ctrue))

;; Church numerals
(define c0 (lambda (f) (lambda (x) x)))
(define c1 (lambda (f) (lambda (x) (f x))))
(define c2 (lambda (f) (lambda (x) (f (f x)))))
(define c3 (lambda (f) (lambda (x) (f (f (f x))))))

(define sum (lambda (m) (lambda (n) (lambda (f) (lambda (x) ((m f) ((n f) x)))))))
(define prod (lambda (m) (lambda (n) (lambda (f) (m (n f))))))
(define expo (lambda (m) (lambda (n) (n m))))

(define pred (lambda (n)
  (ccdr ((n (lambda (p)
             ((ccons ((sum (ccar p)) c1)) (ccar p))))
           ((ccons c0) c0)))))

(define minus (lambda (m) (lambda (n) ((n pred) m))))

;; Convert Church numeral to Scheme number
(define c->n (lambda (n) ((n (lambda (x) (+ x 1))) 0)))