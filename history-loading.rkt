#lang racket/base

(require "history.rkt"
         "cli.rkt"
         "tree-structs.rkt")

(provide commits-from-file
         base-tree)

(define (commits-from-file)
  (history-file->commits interpret-cmd))

(define base-tree
  (task 0 (task-entry "root" 'incomplete 0)
        (list)))

