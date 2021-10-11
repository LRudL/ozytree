#lang racket/base

(require racket/match
         racket/set
         "tree-structs.rkt"
         "history.rkt"
         "action-structs.rkt"
         "commands.rkt")

(provide add-commands-to-history-file
         apply-actions-to-tree
         apply-commits-to-tree
         history-functions-table)

(define (add-commands-to-history-file commands)
  (append-commits->history-file
   (list (commands->commit commands))))

(define (apply-actions-to-tree tree actions)
  (foldl (λ (act tree*)
           (if (set-member? tree-modifying-commands
                            (action-name act))
               ((action-fn act) tree*)
               tree*))
         tree
         actions))

(define (apply-commits-to-tree tree commits)
  (foldl (λ (commit tree*)
           (apply-actions-to-tree tree* (commit-commands commit)))
         tree
         commits))

(define history-functions-table
  (hash 'apply-actions-to-tree apply-actions-to-tree
        'apply-commits-to-tree apply-commits-to-tree
        'add-commands-to-history-file add-commands-to-history-file))
