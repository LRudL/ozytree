#lang racket/base

(require racket/match
         "tree-structs.rkt"
         "history.rkt"
         "action-structs.rkt")

(provide add-commands-to-history-file
         apply-actions-to-tree
         apply-commits-to-tree)

(define (add-commands-to-history-file commands)
  (append-commits->history-file
   (list (commands->commit commands))))

(define (apply-actions-to-tree tree actions)
  (if (null? actions)
      tree
      (let ((act (car actions)))
        
        (match (action-name act)
          ((or 'make 'delete 'move 'set 'mark 'unmark 'set-size 'set-text) ; TO-DO: CHANGE!!!
           (apply-actions-to-tree
            ((action-fn act) tree)
            (cdr actions)))
          (_ (apply-actions-to-tree
              tree (cdr actions)))))))

(define (apply-commits-to-tree tree commits)
  (foldl (λ (commit tree*)
           (apply-actions-to-tree tree* (commit-commands commit)))
         tree
         commits))
