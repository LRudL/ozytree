#lang racket/base

(require racket/match
         racket/function
         threading
         "utils.rkt"
         "tree-structs.rkt")

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
         max-id-in-tree-rooted-at
         size-of-tree-rooted-at
         ; printing:
         tree-printed-lines
         print-tree)


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

(define (node-reduce node-fn aggregating-fn tree)
  (let ((child-vals (map (curry node-reduce node-fn aggregating-fn)
                         (task-children tree))))
    (apply aggregating-fn
           (cons (node-fn tree)
                 child-vals))))

(define (size-of-tree-rooted-at tree)
  (node-reduce (compose task-entry-size task-info)
               +
               tree))

(define (max-id-in-tree-rooted-at tree)
  (node-reduce task-id
               max
               tree))

(define test ; a test tree for testing during development
  (task 0 (task-entry "root" 'incomplete 1)
        (list
         (task 1 (task-entry "a" 'incomplete 1)
               (list
                (task 2 (task-entry "a/b" 'complete 3)
                      (list))))
         (task 3 (task-entry "b" 'incomplete 10)
               (list)))))



;; TREE PRINTING FUNCTIONS

(define (entry-print entry entire-size (colors? #f))
  (string-append (if colors? "\x1b[34m\x1b[3m" "")
                 (task-entry-text entry)
                 (if colors? "\x1b[0m" "")
                 " (own size: "
                 (if colors? "\x1b[31m" "")
                 (number->string (task-entry-size entry))
                 (if colors? "\x1b[0m" "")
                 " / total size: "
                 (if colors? "\x1b[35m" "")
                 (number->string entire-size)
                 (if colors? "\x1b[0m" "")
                 ")"))

(define indent "   ")

(define (tree-printed-lines node (colors? #f))
  (cons (string-append (if (equal? 'incomplete (task-state node))
                           "> Task "
                           (string-append
                            (if colors? "\x1b[2m" "")
                            " task "
                            (if colors? "\x1b[0m" "")))
                       (if colors? "\x1b[1m" "")
                       (number->string (task-id node))
                       (if colors? "\x1b[0m" "")
                       ": " (entry-print (task-info node)
                                         (size-of-tree-rooted-at node)
                                         colors?))
        (map (curry string-append indent)
             (mapcat (curryr tree-printed-lines colors?) (task-children node)))))

(define (print-tree node (colors? #f))
  (map displayln (tree-printed-lines node colors?)))
