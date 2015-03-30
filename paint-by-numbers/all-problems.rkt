#lang typed/racket
(require typed/racket/unit
         racket/include
         "problem.rkt"
         (for-syntax racket))

(define-signature paint-by-numbers:all-problems^ ([problemss : (Listof (Listof Problem))] [set-names : (Listof String)]))
(define-signature paint-by-numbers:problem-set^ ([problems : (Listof Problem)] [set-name : String]))
(define-signature paint-by-numbers:problem^ 
  (;(struct problem (name rows cols solution) #:omit-constructor))
   [problem-name : (-> Problem String)]
   [problem-rows : (-> Problem (Listof (Listof Integer)))]
   [problem-cols : (-> Problem (Listof (Listof Integer)))]
   [problem-solution : (-> Problem (Option (U (Listof String) (Vectorof (Vectorof (U 'on 'off 'unknown))))))]))

(define-syntax (mk-units stx)
  (syntax-case stx ()
    [(_)
     (with-syntax
         ([(unit-names ...)
           (let* ([prob-dir (collection-file-path "problems" "games" "paint-by-numbers")]
                  [files (call-with-input-file (build-path prob-dir "directory") read)])
             (for/list ([file files] #:when (file-exists? (build-path prob-dir file)))
               (define path-spec (string-append "problems" "/" file))
               path-spec))])
       #'(list (include unit-names) ...))]))

(define units ;(mk-units))
  (list
   (include "problems/games")
   (include "problems/misc")
   (include "problems/h1-30")
   (include "problems/h31-60")
   (include "problems/h61-90")
   (include "problems/h91-120")
   (include "problems/h121-138")
   (include "problems/k15x15")
   (include "problems/k15x20")
   (include "problems/k15x25")
   (include "problems/k20x15")
   (include "problems/k20x20")
   (include "problems/k20x25")
   (include "problems/k20x30")
   (include "problems/k25x15")
   (include "problems/k25x20")
   (include "problems/k25x25")
   (include "problems/k25x30")
   (include "problems/k25x35")
   (include "problems/k30x20")
   (include "problems/k30x25")
   (include "problems/k30x30")
   (include "problems/k30x35")
   (include "problems/k30x40")
   (include "problems/k35x25")
   (include "problems/k35x30")
   (include "problems/k35x35")
   (include "problems/k35x40")
   (include "problems/k40x30")
   (include "problems/k40x35")
   (include "problems/k40x40")))

(: empty-unit (Unit (import paint-by-numbers:problem^)
                    (export paint-by-numbers:all-problems^)
                    Void))
(define empty-unit
  (unit 
    (import paint-by-numbers:problem^)
    (export paint-by-numbers:all-problems^)
    (define problemss null)
    (define set-names null)))

(: combine-units (-> (Unit (import paint-by-numbers:problem^)
                           (export paint-by-numbers:problem-set^))
                     (Unit (import paint-by-numbers:problem^)
                           (export paint-by-numbers:all-problems^))
                     (Unit (import paint-by-numbers:problem^)
                           (export paint-by-numbers:all-problems^))))
(define (combine-units new-unit sofar)
  (compound-unit
    (import [p : paint-by-numbers:problem^])
    (export combine)
    (link [((new : paint-by-numbers:problem-set^)) new-unit p]
          [((old : paint-by-numbers:all-problems^)) sofar p]
          [((combine : paint-by-numbers:all-problems^))
           (unit 
             (import (prefix old: paint-by-numbers:all-problems^)
                     (prefix new: paint-by-numbers:problem-set^)
                     paint-by-numbers:problem^)
             (export paint-by-numbers:all-problems^)
             
             (: expand-problem (-> Problem Problem))
             (define (expand-problem pbm)
               (make-problem (problem-name pbm)
                             (problem-rows pbm)
                             (problem-cols pbm)
                             (expand-solution (assert (problem-solution pbm) (lambda (arg) (not (vector? arg)))))))
             
             (define problemss 
               (if (null? new:problems)
                   old:problemss
                   (cons (map expand-problem new:problems) old:problemss)))
             (define set-names
               (if (null? new:problems)
                   old:set-names
                   (cons new:set-name old:set-names)))) new old p])))

;; expand-solution : (union #f (listof string[row])) -> 
;;                   (union #f (vectorof (vectorof (union 'on 'off 'unknown))))
(: expand-solution (-> (Option (Listof String)) (Option (Vectorof (Vectorof (U 'on 'off 'unknown))))))
(define (expand-solution sol)
  (and sol (apply vector (map expand-row sol))))
;; expand-row : string -> (vectorof (union 'on 'off 'unknown))
(: expand-row (-> String (Vectorof (U 'on 'off 'unknown))))
(define (expand-row str)
  (list->vector (map expand-char (string->list str))))
;; expand-char : char -> (union 'on 'off 'unknown)
(: expand-char (-> Char (U 'on 'off 'unknown)))
(define (expand-char c)
  (case c
    [(#\x) 'on]
    [(#\space) 'off]
    [(#\U) 'unknown]
    ; this line to make typed racket do better with types it should never be reached
    [else (error "got bad character")]))

(provide-signature-elements paint-by-numbers:all-problems^)

(define-values/invoke-unit 
  (foldr combine-units empty-unit units)
  (import paint-by-numbers:problem^)
  (export paint-by-numbers:all-problems^))

