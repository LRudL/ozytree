#lang racket/base

(require racket/match
         racket/set
         "utils.rkt"
         "idgen.rkt"
         "tree-structs.rkt"
         "history.rkt"
         "action-structs.rkt"
         "commands.rkt"
         "cli.rkt")

(provide add-commands-to-history-file
         apply-actions-to-tree
         apply-commits-to-tree
         history-functions-table)

(define (add-commands-to-history-file commands tree)
  (append-commits->history-file
   (list (commands->commit commands))
   tree))

(define (apply-actions-to-tree tree actions)
  (foldl (λ (act tree*)
           (if (set-member? tree-modifying-commands
                            (action-name act))
               ((action-fn act) tree*)
               tree*))
         tree
         actions))

(define (apply-commits-to-tree tree commits)
  (configure-id-generator-with-tree! tree)
  (foldl (λ (commit tree*)
           (let ((next-tree (apply-actions-to-tree
                             tree*
                             (map interpret-cmd
                                  (commit-commands commit)))))
             (configure-id-generator-with-tree! next-tree)
             next-tree))
         tree
         (tail-cons commits (commands->commit (list)))))

(define (history-file->tree)
  (apply-commits-to-tree base-tree
                         (history-file->commits)))

(define history-functions-table
  (hash 'apply-actions-to-tree apply-actions-to-tree
        'apply-commits-to-tree apply-commits-to-tree
        'add-commands-to-history-file add-commands-to-history-file
        'commands->commit commands->commit
        'history-file->tree history-file->tree))
