#lang racket/base

(require racket/list
         racket/match
         "utils.rkt")

(provide spec+tokens->bindings-table)

#| The point is to be able to match stuff like this, and extract parts of it out:
("make" (maybe (test number? integer? x) 0)
        (test string? txt)
        (maybe (test number? size) 1))
|#

(define-namespace-anchor namespace-anchor)
(define ns (namespace-anchor->namespace namespace-anchor))

(define (parse-with-defaults-spec defaults-spec tokens)
  (let ((parsed (parse-with-spec (last defaults-spec) tokens)))
    (cons (car parsed)
          (append (map (λ (xy-list) (cons (car xy-list)
                                          (cadr xy-list)))
                       (pair-adjacents (butlast (cdr defaults-spec))))
                  (cdr parsed)))))

(define (parse-with-maybe-spec maybe-spec tokens)
  (if (null? tokens)
      (cons tokens '())
      (let ((parsed (parse-with-spec (cadr maybe-spec) tokens)))
        (if parsed
            parsed
            (cons tokens
                  '())))))

(define (parse-with-test-spec test-spec tokens)
  (let* ((test (eval (cadr test-spec)
                     ns)) ; hacky eval grrr
         (spec-symbol (last test-spec))
         (token (car tokens))
         (parsed (parse-with-spec spec-symbol tokens)))
    (cond ((or (not parsed)
               (not (test token))) #f)
          (#t (parse-with-spec spec-symbol
                               (cons (test token)
                                     (cdr tokens)))))))

(define (parse-with-seq-spec seqspec tokens (acc '()))
  (if (null? seqspec)
      (if (null? tokens)
          (cons tokens
                acc)
          #f)
      (let ((next-parsed (parse-with-spec (car seqspec) tokens)))
        (if (not next-parsed)
            #f
            (let ((remaining-tokens (car next-parsed))
                  (new-bindings (cdr next-parsed)))
              (parse-with-seq-spec (cdr seqspec)
                                   remaining-tokens
                                   (append acc new-bindings)))))))

(define (parse-with-spec spec tokens)
  ; returns a pair: (remaining-tokens . list-of-binding-pairs)
  (if (and (list? spec)
           (not (null? spec)))
      (match (first spec)
        ('maybe         (parse-with-maybe-spec spec tokens))
        ('test          (parse-with-test-spec spec tokens))
        ('with-defaults (parse-with-defaults-spec spec tokens))
        (_              (parse-with-seq-spec spec tokens)))
      (cond
        ((and (null? spec) (null? tokens))
         (cons (list)
               (list)))
        ((and (not (null? tokens))
              (symbol? spec))
         (cons (cdr tokens)
               (list (cons spec
                           (car tokens)))))
        ((and (not (null? tokens))
              (equal? spec (car tokens)))
         (cons (cdr tokens)
               (list)))
        (#t #f))))

(define (bindings-list-with-spec spec tokens)
  (let ((parsed (parse-with-spec spec tokens)))
    (if parsed
        (cdr parsed)
        #f)))

(define (spec+tokens->bindings-table spec tokens)
  (let ((parsed (bindings-list-with-spec spec tokens)))
    (if parsed        
        (foldl (λ (next-binding table)
                 (hash-set* table (car next-binding) (cdr next-binding)))
               (hash)
               parsed)
        #f)))
