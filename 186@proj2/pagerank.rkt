#lang racket

;; Project 2: Implementing PageRank (see README.md and video)
;;
;; PageRank is a popular graph algorithm used for information
;; retrieval and was first popularized as an algorithm powering
;; the Google search engine. Details of the PageRank algorithm will be
;; discussed in class. Here, you will implement several functions that
;; implement the PageRank algorithm in Racket.
;;
;; Hints: 
;; 
;; - For this assignment, you may assume that no graph will include
;; any "self-links" (pages that link to themselves) and that each page
;; will link to at least one other page.
;;
;; - you can use the code in `testing-facilities.rkt` to help generate
;; test input graphs for the project. The test suite was generated
;; using those functions.
;;
;; - You may want to define "helper functions" to break up complicated
;; function definitions.

(provide graph?
         pagerank?
         num-pages
         num-links
         get-backlinks
         mk-initial-pagerank
         step-pagerank
         iterate-pagerank-until
         rank-pages)

;; This program accepts graphs as input. Graphs are represented as a
;; list of links, where each link is a list `(,src ,dst) that signals
;; page src links to page dst.
;; (-> any? boolean?)
(define (graph? glst)
  (and (list? glst)
       (andmap
        (lambda (element)
          (match element
                 [`(,(? symbol? src) ,(? symbol? dst)) #t]
                 [else #f]))
        glst)))

;; Our implementation takes input graphs and turns them into
;; PageRanks. A PageRank is a Racket hash-map that maps pages (each 
;; represented as a Racket symbol) to their corresponding weights,
;; where those weights must sum to 1 (over the whole map).
;; A PageRank encodes a discrete probability distribution over pages.
;;
;; The test graphs for this assignment adhere to several constraints:
;; + There are no "terminal" nodes. All nodes link to at least one
;; other node.
;; + There are no "self-edges," i.e., there will never be an edge `(n0
;; n0).
;; + To maintain consistenty with the last two facts, each graph will
;; have at least two nodes.
;; + There will be no "repeat" edges. I.e., if `(n0 n1) appears once
;; in the graph, it will not appear a second time.
;;
;; (-> any? boolean?)
(define (pagerank? pr)
  (and (hash? pr)
       (andmap symbol? (hash-keys pr))
       (andmap rational? (hash-values pr))
       ;; All the values in the PageRank must sum to 1. I.e., the
       ;; PageRank forms a probability distribution.
       (= 1 (foldl + 0 (hash-values pr)))))

;; Takes some input graph and computes the number of pages in the
;; graph. For example, the graph '((n0 n1) (n1 n2)) has 3 pages, n0,
;; n1, and n2.
;;
;; (-> graph? nonnegative-integer?)
(define (unwrap graph)
  (cond
    [(empty? graph) '()]
    [(cons (first (first graph)) (cons (second (first graph)) (unwrap (rest graph))))]))


(define (num-pages graph)
  (define unwrapped (unwrap graph))
  (count symbol? (remove-duplicates unwrapped)))

;; Takes some input graph and computes the number of links emanating
;; from page. For example, (num-links '((n0 n1) (n1 n0) (n0 n2)) 'n0)
;; should return 2, as 'n0 links to 'n1 and 'n2.
;;
;; (-> graph? symbol? nonnegative-integer?)
(define (num-links graph page)
  (cond
    [(empty? graph) 0]
    [(equal? (first (first graph)) page) (+ 1 (num-links (rest graph) page))] 
    [(num-links (rest graph) page)]))     

;; Calculates a set of pages that link to page within graph. For
;; example,
;;(get-backlinks '((n0 n1) (n1 n2) (n0 n2)) 'n2) should
;; return (set 'n0 'n1).
;; 
;; (-> graph? symbol? (set/c symbol?))

(define (backlinks-help graph page)
  (cond
    [(empty? graph) '()]
    [(equal? (second (first graph)) page)
     (cons (first (first graph)) (backlinks-help (rest graph) page))]
    [(backlinks-help (rest graph) page)]))

(define (get-backlinks graph page)
  (define mylst (backlinks-help graph page))
  (list->set mylst))

;; Generate an initial pagerank for the input graph g. The returned
;; PageRank must satisfy pagerank?, and each value of the hash must be
;; equal to (/ 1 N), where N is the number of pages in the given
;; graph.
;; (-> graph? pagerank?)
(define (pglist graph count)
    (if (equal? count 0)
        '()
        (cons (/ 1 (num-pages graph)) (pglist graph (- count 1)))))
    

(define (mk-initial-pagerank graph)
  (define pgcount (num-pages graph))
  (define mypages (remove-duplicates (flatten graph)))
  ;; get list of pagecount
  (define listpgs (pglist graph pgcount))
  (foldl (lambda (k v h) (hash-set h k v)) (hash) mypages listpgs))


;; Perform one step of PageRank on the specified graph. Return a new
;; PageRank with updated values after running the PageRank
;; calculation. The next iteration's PageRank is calculated as
;;
;; NextPageRank(page-i) = (1 - d) / N + d * S
;;
;; Where:
;;  + d is a specified "dampening factor." in range [0,1]; e.g., 0.85
;;  + N is the number of pages in the graph
;;  + S is the sum of P(page-j) for all page-j.
;;  + P(page-j) is CurrentPageRank(page-j)/NumLinks(page-j)
;;  + NumLinks(page-j) is the number of outbound links of page-j
;;  (i.e., the number of pages to which page-j has links).
;;
;; (-> pagerank? rational? graph? pagerank?)
;; (step-help '(n0) '(1/2) '((n0 n1) (n1 n0)) #hash((n0 . 1/3) (n1 . 1/3)))
;; (step-help '(n0) '(1/2) '((n0 n1) (n1 n0) (n1 n2) (n2 n0)) #hash((n0 . 1/3) (n1 . 1/3)(n2 . 1/3)))

(define (step-help key graph pr)
  (if (= (length key) 0)
      '()
   (cons (foldl + 0 (foldl (lambda (link l) (cons (/ (hash-ref pr link) (num-links graph link)) l))
          '() (set->list (get-backlinks graph (car key))))) (step-help (rest key) graph pr))))

(define (step-pagerank pr d graph)
  (define numpg (num-pages graph))
  (define keys (hash-keys pr))
  (define values (hash-values pr))
  (define backlink-sums (foldr (lambda (sums l) (cons (* d sums) l)) '() (step-help keys graph pr)))
  (define updated (foldr (lambda (final l) (cons (+ final (/ (- 1 d) numpg)) l)) '() backlink-sums))
  (foldl (lambda (k v h) (hash-set h k v)) (hash) keys updated))

;; Iterate PageRank until the largest change in any page's rank is
;; smaller than a specified delta.
;;
;; To explain the reasoning behind this function: the PageRank step
;; function is constructed so that it converges to some "final" result
;; via a long series of steps. In practice, PageRank is iterated some
;; large number of times. Because our computers use finite
;; approximations, we often only want to iterate an equation until it
;; reaches some delta within true convergence. This function allows us
;; to do that for PageRanks.
;;
;; (-> pagerank? rational? graph? rational? pagerank?)
(define (iterate-pagerank-until pr d graph delta)
  (define thisit (hash-values pr))
  (define nextit (hash-values (step-pagerank pr d graph)))
  (if (empty? (filter positive? (foldr (lambda (x l) (cons (- x delta) l)) '()
                                       (foldr (lambda (x y l) (cons (- x y) l)) '() nextit thisit))))
      (step-pagerank pr d graph)
      (iterate-pagerank-until (step-pagerank pr d graph) d graph delta)))


;; Given a PageRank, returns the list of pages it contains in ranked
;; order (from least-popular to most-popular) as a list. You may
;; assume that the none of the pages in the pagerank have the same
;; value (i.e., there will be no ambiguity in ranking)
;;
;; (-> pagerank? (listof symbol?))
;;(sort '((n1 0) (n2 20) (n3 10) (n4 15))
     ;;   #:key second <)

(define (rank-pages pr)
  (define keys (hash-keys pr))
  (define vals (hash-values pr))
  (define myList (sort (foldl (lambda (x y l) (cons (cons x (cons y '())) l)) '() keys vals) #:key second <))
  (define newKeys (foldr (lambda (x l) (cons (first x) l)) '() myList))
  newKeys)
