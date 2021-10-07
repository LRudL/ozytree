#lang racket/base

(require racket/list
         racket/match
         racket/set
         racket/function
         "utils.rkt"
         "tree.rkt"
         "action-structs.rkt"
         "history-management.rkt"
         "command-parse.rkt"
         (for-syntax racket/base
                     racket/list
                     racket/function
                     "command-parse.rkt"
                     "tree.rkt"))

(provide all-command-names
         tree-modifying-commands
         command-list-modifying-commands
         history-modifying-commands
         viewing-commands
         command-help-texts
         command-interpreter-table
         command-name-and-parser-pairs)

(define tree-modifying-commands (mutable-set))
(define command-list-modifying-commands (mutable-set))
(define history-modifying-commands (mutable-set))
(define viewing-commands (mutable-set))

(define command-help-texts (make-hash))

(define command-interpreter-table (make-hash))

(define command-name-and-parser-pairs (list))

(define-syntax create-command
  (λ (stx)
    (let* ((forms (cdr (syntax->datum stx)))
           (type (first forms))
           (type-set (string->symbol
                      (string-append (symbol->string type) "s")))
           (name (second forms))
           (spec (third forms))
           (help-start (fourth forms))
           (cmdλ (fifth forms)))
      (let ((tokens->bindings-table-attempt
             (curry spec+tokens->bindings-table spec))
            (interpreter-name
             (string->symbol (string-append "interpret-" (symbol->string name)))))        
        (datum->syntax
         stx
         `(begin
            (set-add! ,type-set ',name)            
            (hash-set! command-help-texts ',name
                       ,(apply string-append (append (list help-start "\n"
                                                           "Arguments:\n")
                                                     (map (λ (s)
                                                            (format "~s\n" s))
                                                          (cdr spec))
                                                     (list "\n"))))
            (define
              ,(list interpreter-name
                     'bindings)
              (list ',name
                    bindings   
                    (let ((bindings-lookup
                           (λ (v)
                             (hash-ref bindings v))))
                      (curry ,cmdλ bindings-lookup))))            
            (hash-set! command-interpreter-table ',name ,interpreter-name)
            (set! command-name-and-parser-pairs
                  (cons ,(list cons `',name
                               (curry spec+tokens->bindings-table spec))
                        command-name-and-parser-pairs))))))))


(create-command tree-modifying-command make
                ("make" new-node-id
                        (with-defaults parent-id 0
                          (maybe (test string->number parent-id)))
                        text
                        (with-defaults size 1
                          (maybe (test string->number size))))
                "Create a new task. (NOTE: the first argument, new-node-id, is auto-generated)"
                (λ (bindings-lookup tree)
                  (put-node-under tree
                                  (task (bindings-lookup 'new-node-id)
                                        (task-entry (bindings-lookup 'text)
                                                    'incomplete
                                                    (bindings-lookup 'size))
                                        '())
                                  (bindings-lookup 'parent-id))))

(create-command tree-modifying-command delete
                ("delete" (test string->number node-id))
                "Delete a task by its ID"
                (λ (bindings-lookup tree)
                  (remove-node-by-id tree
                                     (bindings-lookup 'node-id))))

(create-command tree-modifying-command move
                ("move" (test string->number node-id)
                        (maybe "under")
                        (test string->number new-parent-node-id))
                "Move a task from underneath one node to underneath another node."
                (λ (bindings-lookup tree)
                  (move-node tree
                             (bindings-lookup 'node-id)
                             (bindings-lookup 'new-parent-node-id))))

(create-command tree-modifying-command set
                ("set" (test string->number node-id)
                       (test (λ (prop)
                               (match prop
                                 ("text" 'text)
                                 ("size" 'size)
                                 (_ #f)))
                             property)
                       new-val)
                "Set the text or size property of a task to something different."
                (λ (bindings-lookup tree)
                  (set-node-info-prop tree
                                      (bindings-lookup 'node-id)
                                      (bindings-lookup 'property)
                                      (bindings-lookup 'new-val))))

(create-command tree-modifying-command mark
                ("mark" (test string->number node-id))
                "Mark a task node as completed."
                (λ (bindings-lookup tree)
                  (set-node-info-prop tree
                                      (bindings-lookup 'node-id)
                                      'state 'complete)))

(create-command tree-modifying-command unmark
                ("unmark" (test string->number node-id))
                "Mark a task node as incomplete."
                (λ (bindings-lookup tree)
                  (set-node-info-prop tree
                                      (bindings-lookup 'node-id)
                                      'state 'incomplete)))

(create-command command-list-modifying-command reset
                ("reset")
                "Reset all changes you have made after the last commit."
                (λ (bindings-lookup commands) '()))

(create-command command-list-modifying-command undo
                ("undo"
                 (with-defaults number-to-undo 1
                   (maybe (test string->number number-to-undo))))
                "Undo changes you have made since the last commit (supply number to undo more than one)."
                (λ (bindings-lookup commands)
                  (drop commands
                        (min (bindings-lookup 'number-to-undo)
                             (length commands)))))

(create-command viewing-command view
                ("view")
                "View the committed state of the task tree."
                (λ (bindings-lookup tree commands)
                  (displayln "---------TREE---------")
                  (print-tree tree)
                  (displayln "----------------------")))

(create-command viewing-command preview
                ("preview")
                "View the state of the task tree if uncommitted changes are applied."
                (λ (bindings-lookup tree commands)
                  (displayln "----TREE (PREVIEW)----")
                  (print-tree (apply-actions-to-tree tree commands))
                  (displayln "----------------------")))

(create-command viewing-command list
                ("list")
                "List the (active, non-undone/reset) commands you have entered since the last commit."
                (λ (bindings-lookup tree commands)
                  (if (null? commands)
                      (displayln "No uncommitted actions.")
                      (begin
                        (displayln "All uncommitted actions:")
                        (map (λ (s)
                               (displayln (string-append " - " s)))
                             (map action-cmd commands))))))

(create-command history-modifying-command commit
                ("commit")
                "Commits commands you have entered to your history file."
                (λ (bindings-lookup tree commands)
                  (add-commands-to-history-file commands)))

(set! command-name-and-parser-pairs
      ; right now this does nothing;
      ; in the future if there are two different commands that match,
      ; this ensures that the first one in this file that matches
      ; will be the one it matches to
      ; (note how this list is built in the macro, i.e. reverse order)
      (reverse command-name-and-parser-pairs))

(define all-command-names (mutable-set))
(set-union! all-command-names
            tree-modifying-commands
            command-list-modifying-commands
            viewing-commands
            history-modifying-commands)
