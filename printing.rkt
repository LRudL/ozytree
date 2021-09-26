#lang racket/base

(require threading
         racket/function
         "utils.rkt"
         "tree-structs.rkt")

(provide tree-printed-lines
         print-tree)

; ---- PRINTING FOR THE OUTPUT REPORTS: ----

(define (entry-print entry)
  (string-append (task-entry-text entry)
                 " (size: "
                 (number->string (task-entry-size entry))
                 ")"))

(define indent "   ")

(define (tree-printed-lines node)
  (cons (string-append (if (equal? 'incomplete (task-state node))
                           "> Task "
                           " task ")
                       (number->string (task-id node))
                       ": " (entry-print (task-info node)))
        (map (curry string-append indent)
             (mapcat tree-printed-lines (task-children node)))))

(define (print-tree node)
  (map displayln (tree-printed-lines node)))

