#lang racket/base

(require rackunit/gui "auto.rkt" "manual.rkt" "vector.rkt")

(test/gui auto-tests manual-tests vector-tests)

(read-line)