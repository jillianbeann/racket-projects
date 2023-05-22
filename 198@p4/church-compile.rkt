#lang racket

;; Assignment 4: A church-compiler for Scheme, to Lambda-calculus

(provide church-compile
         ; provided conversions:
         church->nat
         church->bool
         church->listof)


;; Input language:
;
; e ::= (letrec ([x (lambda (x ...) e)]) e)
;
;  Z combinator = (lambda  (f) ((lambda  (u) (u u))  (lambda  (x) (f (lambda  (y) ((x x) y)) ))
; (Z (lambda  (foo) (lambda  (x) (if .... else foo()...))))  
;
;     | (let ([x e] ...) e)  
;     | (let* ([x e] ...) e) make into  nested lets and churchify that 
;     | (lambda (x ...) e)
;     | (e e ...)    
;     | x  
;     | (and e ...) | (or e ...) compile into ifs, churchify that
;     | (if e e e)
;     | (prim e) | (prim e e)
;     | datum
; datum ::= nat | (quote ()) | #t | #f 
; nat ::= 0 | 1 | 2 | ... 
; x is a symbol
; prim is a primitive operation in list prims
; The following are *extra credit*: -, =, sub1  
(define prims '(+ * - = add1 sub1 cons car cdr null? not zero?))

; This input language has semantics identical to Scheme / Racket, except:
;   + You will not be provided code that yields any kind of error in Racket
;   + You do not need to treat non-boolean values as #t at if, and, or forms
;   + primitive operations are either strictly unary (add1 sub1 null? zero? not car cdr), 
;                                           or binary (+ - * = cons)
;   + There will be no variadic functions or applications---but any fixed arity is allowed

;; Output language:

; e ::= (lambda (x) e)
;     | (e e)
;     | x
;
; also as interpreted by Racket
(define plus
  (lambda (n)
    (lambda (k)
      (lambda (f) (lambda (x) ((k f) ((n f) x)))))))


;; Using the following decoding functions:

; A church-encoded nat is a function taking an f, and x, returning (f^n x)
(define (church->nat c-nat)
  ((c-nat add1) 0))

; A church-encoded bool is a function taking a true-thunk and false-thunk,
;   returning (true-thunk) when true, and (false-thunk) when false
(define (church->bool c-bool)
  ((c-bool (lambda (_) #t)) (lambda (_) #f)))

; A church-encoded cons-cell is a function taking a when-cons callback, and a when-null callback (thunk),
;   returning when-cons applied on the car and cdr elements
; A church-encoded cons-cell is a function taking a when-cons callback, and a when-null callback (thunk),
;   returning the when-null thunk, applied on a dummy value (arbitrary value that will be thrown away)
(define ((church->listof T) c-lst)
  ; when it's a pair, convert the element with T, and the tail with (church->listof T)
  ((c-lst (lambda (a) (lambda (b) (cons (T a) ((church->listof T) b)))))
   ; when it's null, return Racket's null
   (lambda (_) '())))

(define (expr? e)
  (match e
    [`(let ([,(? symbol?) ,(? expr?)] ...) ,(? expr?)) #t]
    [`(let* ([ ,es ,eb] ...) ,bod) #t]
    [`(begin ,(? expr?) ,(? expr?) ...) #t]
    [`(if ,(? expr?) ,(? expr?) ,(? expr?)) #t]
    [`(set! ,(? symbol?) ,(? expr?)) #t]
    [`(lambda (,(? symbol?) ...) ,(? expr?)) #t]
    [`(fact ,e) #t]
    [(or (? symbol?) (? number?) (? boolean?)) #t]
    [`(,(? expr?) ,(? expr?) ...) #t]
    [''() #t]
    [_ #f]))

;; Output language -- lambda calculus
(define (lambda? e)
  (match e
    [(? symbol? x) #t]
    [`(lambda (,(? symbol? x)) ,(? lambda? e-body)) #t]
    [`(,(? lambda? e0) ,(? lambda? e1)) #t]
    [_ #f]))


(define (build-stack f x n)
  (if (equal? n 0)
      x
      (let ([n-minus-1-syntax (build-stack f x (sub1 n))])
        `(,f ,n-minus-1-syntax))))

(define (succ n)
  (define name-f 'f)
  (define name-x 'x)
  `(lambda (,name-f) (lambda (,name-x)  ,(build-stack  name-f name-x n))))
;; Write your church-compiling code below:
(define Y '((lambda  (y) (lambda  (F) (F (lambda  (x) (((y y) F) x))))) 
            (lambda  (y) (lambda  (F) (F (lambda  (x) (((y y) F) x)))))))

; churchify recursively walks the AST and converts each expression in the input language (defined above)
;   to an equivalent (when converted back via each church->XYZ) expression in the output language (defined above)
(define (churchify e)
  (-> expr? lambda?)
  (define (curry expr)
    (define (h f xs)
      (if (= (length xs) 1)
          `(,f ,(churchify (first xs))) ;; call churchify here
          `(,(h f (rest xs)) ,(churchify (first xs)))))
    (match expr
      [`(,f) (h (churchify f) '((lambda (x) x)))]
      [`(,f ,xs ...) (h (churchify f) (reverse xs))]))
  (match e
     [(? symbol? e) e]
    ;; lambda whtout a (lambda  ()) remember to cover this case, 0 arg func -> 1 arg func
     [`(lambda (,(? symbol? xs) ...) ,(? expr? e-body))
     (define (h xs)
       (match xs
         ['() `(lambda (_) ,(churchify e-body))]
         [`(,x) `(lambda (,x) ,(churchify e-body))]
         [`(,hd . ,tl) `(lambda (,hd) ,(h tl))]))
     (h xs)]
    
    [`(let ([,xs ,es] ...) ,e-body) (churchify `((lambda (,@xs) ,e-body) ,@es))]
    [`(letrec  ([,f ,e]) ,e-body) `(let ((,f (,Y (lambda (,f) ,(churchify e))))) ,(churchify e-body))]
    [`(let* () ,eb) (churchify eb)]
    [`(let* (,b0 ,bs ...) ,e-body) (churchify `(let (,b0) (let* ,bs ,e-body)))]
    [#t (churchify '(lambda  (t f) (t)))] 
    [#f (churchify '(lambda  (t f) (f)))]
    
    [`(and) (churchify #t)]
    [`(and ,e) (churchify e)]
    [`(and ,e0 ,e1)
     (churchify `(if ,e0 ,e1 #f))]
    [`(if ,e-guard ,e-true ,e-false)
     (churchify `(,e-guard (lambda (_) ,e-true) (lambda (_) ,e-false)))]

    [`(or ,x ,y ,es ...) `(or (or ,x ,y) ,es)]
    [`(or ,x ,y) (churchify `(if ,y ,y ,(churchify x)))]
    [`(or ,x) (churchify x)]
    
    [`(+ ,x ,y) `((+ ,(churchify x)) ,(churchify y))]
    [`(* ,x ,y) `((* ,(churchify x)) ,(churchify y))]
    [`(- ,x ,y) `((- ,(churchify x)) ,(churchify y))]
    [`(= ,x ,y) 'todoeq]
    [''() '(lambda (c) (lambda (n) (n (lambda (x) x))))]

    [(? integer? e) (succ e)]
    [`(,f ,es ...) (curry e)]
    [_ e]))

; Takes a whole program in the input language, and converts it into an equivalent program in lambda-calc
(define (church-compile program)
  ; Define primitive operations and needed helpers using a top-level let form?
  (define todo `(lambda (x) x))
  (churchify
   `(let ([+ (lambda (n) (lambda (k) (lambda (f) (lambda (x) ((k f) ((n f) x))))))]
          [- (lambda (n) (lambda (m) ((m (lambda (n) (lambda (f) (lambda (x) (((n (lambda (g) (lambda (h) (h (g f))))) (lambda (u) x)) (lambda (u) u)))))) n)))]
          [and (lambda (x) (lambda (y) ((x y) x)))]
          [fact (((lambda  (y) (lambda  (F) (F (lambda  (x) (((y y) F) x))))) 
            (lambda  (y) (lambda  (F) (F (lambda  (x) (((y y) F) x)))))) (lambda (f) (lambda (n) ((((lambda (n) ((n (lambda  (x) (lambda (t) (lambda (f) (f (lambda (x) x))))))
                                                     (lambda (t) (lambda (f) (t (lambda (x) x)))))) n) (lambda (f) (lambda (x) (f x))))
                                                   (((lambda (m) (lambda (n) (lambda (f) (lambda (x) ((m (n f)) x))))) n)
                                                  (f (lambda (n) (lambda (f) (lambda (x) (((n (lambda (g) (lambda (h) (h (g f))))) (lambda (u) x)) (lambda (u) u))))) n))))))]
          [not (lambda (a) (lambda (x y) (a y x)))]
          [or (lambda (x) (lambda (y) ((x x) y)))]
          [add1 (lambda (n) (lambda (f) (lambda (x) (f ((n f) x)))))]
          [fib (,Y (lambda  (fib)
     (lambda  (x)
       ((((lambda  (c) (lambda  (x) (lambda  (y) ((c x) y)))) (((lambda  (m) (lambda  (n) ((lambda (n) ((n (lambda  (x) (lambda (t) (lambda (f) (f (lambda (x) x)))))) (lambda (t) (lambda (f) (t (lambda (x) x))))))
                                                             (((lambda (n) (lambda (m) ((m (lambda (n) (lambda (f) (lambda (x) (((n (lambda (g) (lambda (h) (h (g f))))) (lambda (u) x)) (lambda (u) u)))))) n)))
                                                               m) n)))) x) (lambda (f) (lambda (x) (f x)))))
         (lambda () x))
        (lambda () (fib ((lambda (n) (lambda (m) ((m (lambda (n) (lambda (f) (lambda (x) (((n (lambda (g) (lambda (h) (h (g f))))) (lambda (u) x)) (lambda (u) u)))))) n)))
                   x (lambda (f) (lambda (x) (f (f x)))))))))))]
          [cons (lambda (a b when-cons when-null) (when-cons a b))]
          [null? (lambda (lst) (lst (lambda (a b) (lambda (t) (lambda (f) (f (lambda (x) x))))) (lambda () (lambda (t) (lambda (f) (t (lambda (x) x)))))))]
          [cdr (lambda (cons-cell) (cons-cell (lambda (car-value cdr-value) cdr-value) (lambda () (lambda (x) x))))]
          [car (lambda (cons-cell) (cons-cell (lambda (car-value cdr-value) car-value) (lambda () (lambda (x) x))))]
          [* (lambda (m) (lambda (n) (lambda (f) (lambda (x) ((m (n f)) x)))))]
          [zero? (lambda (n) ((n (lambda  (x) (lambda (t) (lambda (f) (f (lambda (x) x)))))) (lambda (t) (lambda (f) (t (lambda (x) x))))))])
      ,program)))

;;[zero (lambda (f) (lambda  (x) x))]
;;pred (lambda (n) (lambda (f) (lambda (x) (((n (lambda (g) (lambda (h) (h (g f))))) (lambda (u) x)) (lambda (u) u)))))
