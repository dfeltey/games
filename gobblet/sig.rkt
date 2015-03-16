#lang typed/racket
(require typed/racket/unit)

(define-struct plan-external ([size : Integer] 
                              [from-i : (Option Integer)]
                              [from-j : (Option Integer)]
                              [to-i : Integer] 
                              [to-j : Integer]
                              [xform : XForm]
                              ;; NOTE: turns is not exposed in the actual explore unit
                              ;; but has to be here because struct definitions don't work in units right now
                              [turns : Integer]) #:mutable)

(define-struct piece-external ([size : Integer] 
                               [color : Color] 
                               [gobble-table : (HashTable (Listof Piece) (Listof Piece))]))  

(define-type Color (U 'red 'yellow))
(define-type Piece piece-external)
(define-type Plan plan-external)
(define-type Board (Vectorof (Vectorof (Listof Piece))))
(define-type XForm (Vectorof Integer))
(define-type Play-Rest (List Piece (Option Integer) (Option Integer) Integer Integer Integer))
(define-type Play (Pairof Real Play-Rest))


(provide config^ 
         heuristics^ explore^
         model^ restart^
         (struct-out plan-external)
         (struct-out piece-external)
         Board
         Piece
         Color
         XForm
         Plan
         Play
         Play-Rest)

(define-signature config^
  ([BOARD-SIZE : Natural]))

(define-signature heuristics^
  ([make-3x3-rate-board : (-> Any (-> Board Color (Option Integer) (Option Integer) Real))]
   [make-3x3-canned-moves : (-> (case-> (-> Board Color (Pairof Bytes XForm))
                                        (-> Bytes False (Pairof Bytes XForm)))
                                (HashTable Any (Listof (Pairof Real Any)))
                                (-> Board Color Any XForm (Listof Any)))]
   [make-3x3-no-canned-moves : (-> (case-> (-> Board Color (Pairof Bytes XForm))
                                           (-> Bytes False (Pairof Bytes XForm)))
                                   (HashTable Any (Listof (Pairof Real Any)))
                                   (-> Board Color Any XForm (Listof (Pairof Real Plan))))]
   [make-4x4-rate-board : (-> Any (-> Board Color (Option Integer) (Option Integer) Real))]
   [make-4x4-canned-moves : (-> (case-> (-> Board Color (Pairof Bytes XForm))
                                        (-> Bytes False (Pairof Bytes XForm)))
                                (HashTable Any (Listof (Pairof Real Any)))
                                (-> Board Color Any XForm (Listof (Pairof Real Plan))))]))


(define-signature explore^
  ([make-search : (-> (-> (case-> (-> Board Color (Pairof Bytes XForm))
                                  (-> Bytes False (Pairof Bytes XForm))) 
                          (-> Board Color (Option Integer) (Option Integer) Real))
                      (-> (case-> (-> Board Color (Pairof Bytes XForm))
                                  (-> Bytes False (Pairof Bytes XForm))) 
                          (HashTable Any (Listof (Pairof Real Any)))
                          (-> Board Color Any XForm (Listof (Pairof Real Plan)))) 
                      (-> Nonnegative-Real Real Integer Color Board (Listof Board) Play-Rest))]
   [apply-play : (-> Board Play-Rest Board)] ; a play is (list piece from-i from-j to-i to-j)
   #;(struct plan (size from-i from-j to-i to-j xform))
   ;; NOTE: For typed/racket implementation lifting the struct outside of the signature
   ;; is required
   [plan : (-> Integer (Option Integer) (Option Integer) Integer Integer XForm Integer Plan)]
   [plan? : (-> Any Boolean : Plan)] ;; I want to be able to retain the filter here...
   [plan-size : (-> Plan Integer)]
   [plan-from-i : (-> Plan (Option Integer))]
   [plan-from-j : (-> Plan (Option Integer))]
   [plan-to-i : (-> Plan Integer)]
   [plan-to-j : (-> Plan Integer)]
   [plan-xform : (-> Plan XForm)]
   ))

(define-signature model^
  ([move : (All (a b) (-> Board Piece (Option Integer) (Option Integer) Integer Integer
                        (-> Board a)
                        (-> b)
                        (U a b)))]
   [winner? : (-> Board Color Boolean)] 
   [3-in-a-row? : (-> Board Integer Integer Color Boolean)]
   [red-pieces : (Listof Piece)]
   [yellow-pieces : (Listof Piece)]
   [piece-color  : (-> Piece Color)]
   [piece-size : (-> Piece Integer)]
   [empty-board : Board]
   [board-ref : (-> Board Integer Integer (Listof Piece))]
   [fold-board : (All (a) (-> (-> Integer Integer a a) a a))]
   [fold-rowcol  : (All (a) (-> (-> Integer a a) a a))]
   [other : (-> Color Color)]
   [available-off-board : (-> Board Color (Listof (Listof Integer)))]
   [compact-board : (-> Board Color Bytes)]
   [make-canonicalize : (-> (case-> (-> Board Color (Pairof Bytes XForm))
                                    (-> Bytes False (Pairof Bytes XForm))))]
   [apply-xform : (-> XForm Integer Integer Integer)]
   [unapply-xform : (-> XForm Integer (Values Integer Integer))]
   [board->string : (-> Integer Board String)]))

(define-signature restart^
  ([new-game : Any]
   [show-gobblet-help : Any]))


