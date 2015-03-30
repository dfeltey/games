#lang typed/racket

(provide (struct-out problem) Problem)
(define-struct problem ([name : String] 
                        [rows : (Listof (Listof Integer))] 
                        [cols : (Listof (Listof Integer))]
                        [solution : (Option (U (Listof String) (Vectorof (Vectorof (U 'on 'off 'unknown)))))])
  #:mutable)
(define-type Problem problem)