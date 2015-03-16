#lang typed/racket
(require typed/racket/unit)
;; Supplies canned moves and board-rating functions for the state
;; explorer.

(require ;racket/unit
         "sig.rkt"
         "plays-3x3.rkt")

(provide heuristics-unit@)

(define-unit heuristics-unit@
    (import config^ model^ explore^)
    (export heuristics^)
    
    (: make-3x3-canned-moves (-> (case-> (-> Board Color (Pairof Bytes XForm))
                                         (-> Bytes False (Pairof Bytes XForm)))
                                 (HashTable Any (Listof (Pairof Real Any)))
                                 (-> Board Color Any XForm (Listof Any))))
    (define (make-3x3-canned-moves canonicalize init-memory)
      ;; Add known good plays to init-memory. These plays define
      ;; a perfect red player.
      (for-each (lambda ([play : (Pairof (Vectorof Integer) (Pairof Integer (Listof (Option Integer))))])
                  (let ([key+xform : (Pairof Bytes XForm) (canonicalize (list->bytes (vector->list (car play))) #f)])
                    ((inst hash-set! Any (Listof (Pairof Real Any))) init-memory
                               (car key+xform)
                               (let-values ([(from-i from-j)
                                             (if (list-ref play 2)
                                                 (unapply-xform (cdr key+xform) (assert ((inst list-ref (Option Integer)) (cdr play) 1)) #;(list-ref play 2))
                                                 (values #f #f))]
                                            [(to-i to-j)
                                             (unapply-xform (cdr key+xform) (assert ((inst list-ref (Option Integer)) (cdr play) 2)) #;(list-ref play 3))])
                                 (list
                                  (cons +inf.0
                                        (plan
                                         (cadr play) ;(list-ref play 1)
                                         from-i from-j to-i to-j
                                         (cdr key+xform)
                                         (assert ((inst list-ref (Option Integer)) (cdr play) 3)) #;(list-ref play 4))))))))
                3x3-plays)
      (lambda ([board : Board] [me : Color] [k : Any] [xform : XForm])
        null))
    
    (: make-3x3-no-canned-moves (-> (case-> (-> Board Color (Pairof Bytes XForm))
                                            (-> Bytes False (Pairof Bytes XForm)))
                                    (HashTable Any (Listof (Pairof Real Any)))
                                    (-> Board Color Any XForm (Listof (Pairof Real Plan)))))
    (define (make-3x3-no-canned-moves canonicalize init-memory) 
      (lambda (board me k xform)
        null))
    (: make-3x3-rate-board (-> Any (-> Board Color (Option Integer) (Option Integer) Real)))
    (define (make-3x3-rate-board canon)
      (lambda (board me to-i to-j)
        (+ (random)
           ;; Occupying the middle cell seems good
           (rate-cell board me 1 1))))
    
    (: make-4x4-canned-moves (-> (case-> (-> Board Color (Pairof Bytes XForm))
                                         (-> Bytes False (Pairof Bytes XForm)))
                                 (HashTable Any (Listof (Pairof Real Any)))
                                 (-> Board Color Any XForm (Listof (Pairof Real Plan)))))
    (define (make-4x4-canned-moves canon init-memory) 
      (lambda (board me k xform)
        null))
    (: make-4x4-rate-board (-> Any (-> Board Color (Option Integer) (Option Integer) Real)))
    (define (make-4x4-rate-board canon)
      (lambda (board me to-i to-j)
        (+ (random) 
           (if (and (top-color? board to-i to-j (other me))
                    (3-in-a-row? board (assert to-i) (assert to-j) (other me)))
               -10
               0)
           ;; Controlling the middle cells seems good
           (rate-cell board me 1 1)
           (rate-cell board me 1 2)
           (rate-cell board me 2 1)
           (rate-cell board me 2 2))))
    
    (: rate-cell (-> Board Color (Option Integer) (Option Integer) Integer))
    (define (rate-cell board me i j)
      (let ([l (board-ref board (assert i) (assert j))])
        (if (pair? l)
            (if (eq? (piece-color (car l)) me)
                2
                -2)
            0)))
    (: top-color? (-> Board (Option Integer) (Option Integer) Color Boolean))
    (define (top-color? board i j c)
      (let ([l (board-ref board (assert i) (assert j))])
        (and (pair? l)
             (eq? (piece-color (car l)) c)))))
