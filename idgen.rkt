#lang racket/base

(require "utils.rkt"
         "tree.rkt")

(provide generate-id
         restart-id-generator-at!
         restart-id-generator!-and-get-resumer
         configure-id-generator-with-tree!)

(define generate-id
  (λ () 'if-you-see-this-something-horrible-has-happened-with-id-generation))

(define (get-generator initial-value)
  (define value initial-value)
  (λ ()
    (set! value (+ 1 value))
    value))

(define (restart-id-generator-at! n)
  (set! generate-id (get-generator n)))

(define (restart-id-generator!-and-get-resumer n)
  (let ((x (- (generate-id) 1)))
    (restart-id-generator-at! n)
    (λ ()
      (restart-id-generator-at! x))))

(define (configure-id-generator-with-tree! tree)
  (restart-id-generator-at! (max-id-in-tree-rooted-at tree)))
