;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subst.scm
;; Written for CS 442/642
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some functions for manipulating lambda expressions.
;;
;; (make-abs <var> <expr>) creates the encoding for an abstraction
;; (var-of <abs>) return the variable of an abstraction
;; (body-of <abs>) return the body of an abstraction
;;
;; (make-app <rator> <rand>) creates the encoding for an application
;; (rator-of <app>) return the function of an application
;; (rand-of <app>) return the argument of an application
;;
;; (abs? <expr>) predicate to identify abstractions
;; (app? <expr>) predicate to identify applications
;; (var? <expr>) predicate to identify variables
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-abs (lambda (var body) (list 'fun var body)))
(define make-app (lambda (rator rand) (list rator rand)))

(define abs? (lambda (expr) 
  (and (list? expr) (= (length expr) 3) (eqv? 'fun (car expr)))))
(define app? (lambda (expr)
  (and (list? expr) (= (length expr) 2))))
(define var? symbol?)

(define var-of cadr)
(define body-of caddr)

(define rator-of car)
(define rand-of cadr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Substitution 
;; - Implements Dynamic Binding
;;
;; [e/x]y = y  (if x != y)
;; [e/x]x = e
;; [e/x](e1 e2) = ([e/x]e1 [e/x]e2)
;; [e/x](fun y e1) (fun y [e/x]e1)  (if x != y)
;; [e/x](fun x e1) = (fun x e1)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define free-var? (lambda(var expr elim-lst)
    (cond 
      ((abs? expr)
         (free-var? var (body-of expr) (cons (var-of expr) elim-lst))
      )
      ((app? expr)      ;; [e/x](e1 e2) = ([e/x]e1 [e/x]e2)
         (or (free-var? var (rand-of expr) elim-lst) (free-var? var (rator-of expr) elim-lst))
      )
      ((var? expr) ;; base case
         (if (and (eqv? var expr) (not (in-lst? var elim-lst)))
          #t
          #f
         )
      )
      (else #f)    ;; Error! Just return the expr
    )
))

(define in-lst? (lambda(elem lst)
    (if (null? lst) #f
        (if (eqv? (car lst) elem) #t
            (in-lst? elem (cdr lst))))
))

(define n 0)

(define new_sym (lambda()
    (set! n (+ 1 n))
    (string->symbol (string-append (symbol->string 'new_var_) (number->string n)))
))

(define subst (lambda (e x expr)  
    (cond 
      ((abs? expr)
         (if (eqv? x (var-of expr))
             expr        ;; [e/x](fun x e1) = (fun x e1)
             (if (not (free-var? (var-of expr) e '())) ;;
                 (make-abs   ;; [e/x](fun y e1) = (fun y [e/x]e1)
                  (var-of expr) (subst e x (body-of expr))
                 )
                 (let (
                       (new_var (new_sym))
                      ) ;; example of using let
                   (make-abs
                    new_var (subst e x (subst new_var (var-of expr) (body-of expr)))
                   )
                 )
             )
         )
      )
      ((app? expr)      ;; [e/x](e1 e2) = ([e/x]e1 [e/x]e2)
         (make-app (subst e x (rator-of expr))
                   (subst e x (rand-of expr))
         )
      )
      ((var? expr)
         (if (eqv? x expr)
             e       ;; [e/x]x = e
             expr    ;; [e/x]y = y
         )
      )
      (else expr)    ;; Error! Just return the expr
    )
))

(define reduce (lambda(lst)
    (cond
      ((var? lst) lst)
      ((abs? lst) 
       (make-abs (var-of lst) (reduce (body-of lst)))
      )
      ((app? lst)
       (if (var? (rator-of lst))
           (make-app (rator-of lst) (reduce (rand-of lst)))
           (if (abs? (rator-of lst))
               (reduce (subst (cadr lst) (var-of (car lst)) (reduce (body-of (car lst)))))
               (let ((rator-lst (reduce (rator-of lst)))) ;; backtrack; only add reduce at front when finally rator is an abstraction.
                 (if (abs? rator-lst) ;; backtrack
                     (reduce (make-app rator-lst (reduce (rand-of lst)))) ;; rator-of lst is app 
                     (make-app rator-lst (reduce (rand-of lst))) ;; rator-of lst is app 
                 )
               )
           )
       )
      )
      (else lst) ;; error
    )
))

(define parse-lambda (lambda(lst)
    (cond
      ((symbol? lst) lst)
      (else
       (cond
         ((= (length lst) 1) (parse-lambda (car lst)))
         ((= (length lst) 2) (list (parse-lambda (car lst)) (parse-lambda (cadr lst))))
         (else
          (cond
            ((eqv? (car lst) 'fun) (cons 'fun (cons (cadr lst) (list (parse-lambda (cddr lst))))))
            (else
             (cond
               ((eqv? (cadr lst) 'fun)
                (let ((rederred-list (cons (car lst) (list (parse-lambda (cdr lst))))))
                  (parse-lambda (cons (list (car rederred-list) (cadr rederred-list))(cddr rederred-list)))
                )
               )
               (else (parse-lambda (cons (list (parse-lambda (car lst)) (parse-lambda (cadr lst)))(cddr lst)))) 
             )
            )
          )
         )
       )
      )
    )
))

(define (interpret E) (reduce (parse-lambda E)))


;; Fibonacci test
(define PICK2ND '(fun x fun y x))
(define PLUS '(fun m fun n fun f fun x m f (n f x)))
(define c0 '(fun f fun x x))
(define c1 '(fun f fun x f x))
(define c2 '(fun f fun x f (f x)))
(define c6 '(fun f fun x f (f (f (f (f (f x)))))))
(define c10 '(fun f fun x f (f (f (f (f (f (f (f (f (f x)))))))))))

(define fib `(fun n (n (fun f fun a fun b f b (,PLUS a b)) ,PICK2ND ,c0 ,c1)))

(interpret `(,fib ,c6))

