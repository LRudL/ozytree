#lang racket/base

(require racket/match
         racket/function
         threading
         "tree-structs.rkt"
         "printing.rkt")

(provide task
         task-entry
         task-state
         put-node-under
         remove-node-by-id
         set-node-entry
         get-node
         get-node-entry
         move-node
         set-node-info-prop
         max-node-id)


(define (put-node-under tree node parent-node-id)
  (struct-copy task tree
               [children 
                (if (equal? parent-node-id (task-id tree))
                    (cons node (task-children tree))
                    (map (curryr put-node-under node parent-node-id)
                         (task-children tree)))]))

(define (remove-node-by-id tree node-id)
  (struct-copy task tree
               [children
                (~>> (task-children tree)
                     (map (curryr remove-node-by-id node-id))
                     (filter (λ (subnode)
                               (not (equal? (task-id subnode) node-id)))))]))

(define (set-node-entry tree node-id task-entry-val)
  (if (equal? node-id (task-id tree))
      (struct-copy task tree
                   [info task-entry-val])
      (struct-copy task tree
                   [children
                    (map (curryr set-node-entry node-id task-entry-val)
                         (task-children tree))])))

(define (get-node tree node-id)
  (cond ((equal? node-id (task-id tree)) tree)
        ((null? (task-children tree)) #f)
        (#t (foldl (λ (node acc)
                     (or acc (get-node node node-id)))
                   #f
                   (task-children tree)))))

(define (get-node-entry tree node-id)
  (task-info (get-node tree node-id)))

(define (set-node-info-prop tree node-id info-prop new-val)
  (let ((new-node-info
         ; horror repetitive code ahead, FIX
         (match info-prop
           ('text (struct-copy task-entry
                               (get-node-entry tree node-id)
                               [text new-val]))
           ('state (struct-copy task-entry
                                (get-node-entry tree node-id)
                                [state new-val]))
           ('size (struct-copy task-entry
                               (get-node-entry tree node-id)
                               [size new-val]))
           (_ (raise
               (format "ERROR: set-node-info-prop: ~a is not a task entry property"
                       info-prop))))))
    (set-node-entry tree node-id new-node-info)))

(define (move-node tree node-id new-parent-id)
  (let ((node (get-node tree node-id)))
    (~> tree
        (remove-node-by-id node-id)
        (put-node-under node new-parent-id))))

(define (max-node-id tree)
  (let ((child-max (apply max
                          (cons 1
                                (map max-node-id
                                     (task-children tree))))))
    (max child-max
         (task-id tree))))

(define test
  (task 0 (task-entry "root" 'incomplete 1)
        (list
         (task 1 (task-entry "a" 'incomplete 1)
               (list
                (task 2 (task-entry "a/b" 'complete 3)
                      (list))))
         (task 3 (task-entry "b" 'incomplete 10)
               (list)))))
