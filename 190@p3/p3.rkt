;; P3 -- Metacircular Interpreter for Scheme
;; 
;; Starter Code
#lang racket

(provide run)

;;
;; Language
;;

;; Scheme expressions
;
; e ::= (lambda (x ...) e)
;     | (let ([x e] ...) e)
;     | (begin e e ...)
;     | (if e e e)
;     | (set! x e)
;     | (op ...)
;     | (e ...)
;     | x | n | b
;
; op ::= + | - | * | / | equal? | add1 | sub1 | car | cdr
;      | cons | empty? | append
(define (expr? e)
  (match e
    [`(let ([,(? symbol?) ,(? expr?)] ...) ,(? expr?)) #t]
    [`(begin ,(? expr?) ,(? expr?) ...) #t]
    [`(if ,(? expr?) ,(? expr?) ,(? expr?)) #t]
    [`(set! ,(? symbol?) ,(? expr?)) #t]
    [`(lambda (,(? symbol?) ...) ,(? expr?)) #t]
    [(or (? symbol?) (? number?) (? boolean?)) #t]
    [`(,(? expr?) ,(? expr?) ...) #t]
    [_ #f]))

(define (builtin-op? op) (member op '(+ - * / equal? add1 sub1 car cdr cons empty? append)))

(define ops-tbl (hash '+ + '- - '* * '/ / 'equal? equal? 'add1 add1 'sub1 sub1))

;; helper function to apply a scheme operator to a list of numbers /
;; booleans
;; (apply-builtin-op '+ '(1 2)) --> 3
;; (apply-builtin-op 'equal? '(#t #t)) --> #t
;; (apply-builtin-op 'add1 '(5)) --> 6
(define (apply-builtin-op op lst)
  (apply (hash-ref ops-tbl op) lst))

;;
;; Interpreter values
;;

;; !! reverse the list before you perform foldl to retain its order
;; !! evaluate expressions from left to right

;; Your interpreter will manifest three sorts of values: numbers,
;; booleans, and closures. The closure case is worth noting: when you
;; produce closures, they must be in precisely this format.
(define (value? v)
  (match v
    [(? number? v) #t]
    [(? boolean? v) #t]
    [`(closure ,(? expr? e) ,(? environment?)) #t]
    [_ #f]))

;; Environments are hashes from symbol? to address?

;; Addresses will just be symbols, which we will generate using gensym
(define address? symbol?)

;; See the README and video for why it's symbol? -> address?, rather
;; than (say) symbol? -> value?
(define (environment? env)
  (and (hash? env) (andmap symbol? (hash-keys env)) (andmap address? (hash-values env))))

;; Stores (/ heaps / etc...) are maps from addresses to values. 
(define (store? sto)
  (and (hash? sto) (andmap address? (hash-keys sto)) (andmap value? (hash-values sto))))

;; Your interpreter must produce a tagged result, `(result ,v ,store+)
;; of a final value and new (possibly updated) store.
(define (eval-result? r)
  (match r
    [`(result ,(? value? v) ,(? store? s)) #t]
    [_ #f]))

;; 
;; Allocation
;; 

;; Allocate a fresh address (to be put in the store) for some named
;; variable.
(define (allocate var) (gensym var))

;;
;; YOUR CODE HERE
;; 

;; (interp e env sto) 
;; (-> expr? environment? store? eval-result?)
;;
;; You will build out the implementation of a big-step, metacircular
;; interpreter.
(define/contract (interp e env store)
  (-> expr? environment? store? eval-result?)
  (match e
    ;; You may wish to handle only one variable to start
    [`(let ([,xs ,es] ...) ,ebody)
     (interp `((lambda ,xs ,ebody) ,@es) env store)]

    [`(begin ,e0 ,es ...)
     (match-define `(result ,val ,store-update) (interp e0 env store))
     (if (>= (length es) 2)
         (interp `(begin ,@es) env store-update)
         (interp (last es)     env store-update))]
    
    ;; Hint: use hash-ref for both env *and* store
    [(? symbol? x) `(result ,(hash-ref store (hash-ref env x))  ,store)]
    ;; These two are done for you: since the store doesn't change we
    ;; simply pass it back
    [(? number? n) `(result ,n ,store)]
    [(? boolean? b) `(result ,b ,store)]
    ;; Evaluate to a closure, don't forget to store the environment
 ;;   [`(lambda (,(? symbol? xs) ...) ,(? expr?)) `(result (closure ,e ,env) ,store)]
    [`(lambda (,xs ...) ,ebody) `(result (closure ,e ,env) ,store)]
    ;; Evaluate guard (ec) and then either evaluate et or ef
    [`(if ,ec ,et ,ef)
       (let ([guard-result (interp ec env store)])
       (match guard-result
         [`(result ,result-value ,store+)
          (if (equal? result-value 0)
              (interp et env store+)
              (interp ef env store+))]))]
    ;; Evaluate e, then set x's address in the store, return
    ;; updated store
    [`(set! ,x ,e)
     (match-define `(result ,v ,store+) (interp e env store))
     `(result ,(hash-ref store (hash-ref env x) v) ,(hash-set store+ (hash-ref env x) v))]
    ;; Evaluate each of the arguments to a value, then call 
    ;; apply-builtin-op to get the final result turn es to vs and apply the
    ;; turn es into vs use apply builtin ops 
    [`(,(? builtin-op? op) ,es ...)
     'todo]
;     (match-define `(result ,vs ,store-new) (interp es env store))
;     (if (> (length es) 0)
;         (interp es env store)
;        `(result ,(apply-builtin-op op vs) ,store-new))]
;     (match-define (cons vs vs-store) 
;     (foldl (lambda (e acc)
;            (match-define (cons vs cur-store) acc)
;            (match-define `(result ,v ,store-v) (interp e env cur-store))
;              (cons (cons v vs) store-v))
;              (cons '() store-f) es))
;     `(result ,(apply-builtin-op op vs) ,store)]
    
    ;; See the lecture on closures for how to handle this one
    [`(,ef ,es ...)
     (match-define `(result ,vf ,store-f) (interp ef env store))
     (match-define `(closure (lambda (,xs ...) ,ebody) ,lam-env)  vf)
     (match-define (cons vs vs-store)
     (foldl (lambda (e acc) (match-define (cons vs cur-store) acc)
            (match-define `(result ,v ,store-v) (interp e env cur-store))
              (cons (cons v vs) store-v))
              (cons '() store-f) es))
     (match-define (cons final-env final-store)
       (foldl (lambda (x v acc) (match-define (cons cur-env cur-store) acc)
                               (define addr (allocate x))
                               (cons (hash-set cur-env x addr) (hash-set cur-store addr v)))
                               (cons lam-env vs-store)
                                xs
                                vs))
     (interp ebody final-env final-store)])) 

(define (run e)
  (match-define `(result ,v ,_) (interp e (hash) (hash)))
  v)
