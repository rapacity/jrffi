#lang racket
(require rackunit "../require.rkt" 
         (java java/util/Arrays java/lang/Integer) 
         (java java/util/Arrays java/lang/Integer #:rktfy))

(provide (java java/util/Arrays java/lang/Integer))