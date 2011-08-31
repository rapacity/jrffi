#lang setup/infotab

(define name "jrffi")
(define blurb (list "A Java FFI for racket"))
(define categories `(devtools))
(define version "0.01")
(define required-core-version "5.1")
(define primary-file "main.rkt")
(define pre-install-collection "make.rkt")
(define compile-omit-files '("tests" "make.rkt"))
(define scribblings '(("scribblings/auto.scrbl")))
