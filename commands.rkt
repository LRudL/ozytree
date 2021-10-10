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
         "settings.rkt"
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
         settings-commands
         command-help-texts
         command-interpreter-table
         command-name-and-parser-pairs)

(define tree-modifying-commands (mutable-set))
(define command-list-modifying-commands (mutable-set))
(define history-modifying-commands (mutable-set))
(define viewing-commands (mutable-set))
(define settings-commands (mutable-set))

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
                                                            (format "- ~s\n" s))
                                                          (cdr spec))
                                                     (list "\n"))))
            (define
              ,(list interpreter-name
                     'bindings)
              ; This function is called to create the last three elements
              ; of the list that is applied to the action struct (see action-structs.rkt)
              (list ',name
                    bindings
                    (let ((bindings-lookup
                           (λ (v)
                             (hash-ref bindings v))))
                      ;(curry ,cmdλ bindings-lookup)
                      (λ args
                        ; Instead of currying (like the commented-out line), we do this,
                        ; because otherwise if bindings-lookup is the only argument that
                        ; cmdλ takes, the above also calls the procedure and makes it run
                        ; (since it has received a sufficient number of arguments).
                        ; Could be made to work with this, but then the structure and behaviour
                        ; of the main command loop in main.rkt would be messier.
                        (apply ,cmdλ
                               (append (list bindings-lookup)
                                       args))))))
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
                "Create a new task. (NOTE: the first argument, new-node-id, is auto-generated and should not be supplied)"
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

(define-syntax create-set-command
  (λ (stx)
    (let* ((forms (syntax->datum stx))
           (normal-args (cdr (take forms 4)))
           (set-type (fifth forms)))
      (datum->syntax
       stx
       (append (list 'create-command 'tree-modifying-command)
               normal-args
               (list `(λ (bindings-lookup tree)
                        (set-node-info-prop tree
                                            (bindings-lookup 'node-id)
                                            ,set-type
                                            (bindings-lookup 'new-val)))))))))

(create-set-command set-size
                    ("set-size" (test string->number node-id)
                                (test string->number new-val))
                    "Set the size of a task to something different."
                    'size)

(create-set-command set-text
                    ("set-text" (test string->number node-id)
                                new-val)
                    "Set the text description of a task to something different."
                    'text)

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
                  (take commands
                        (max (- (length commands)
                                (bindings-lookup 'number-to-undo))
                             0))))

(create-command viewing-command view
                ("view")
                "View the committed state of the task tree."
                (λ (bindings-lookup displayer tree commands)
                  (displayer "(Note: the view command excludes uncommitted changes.)"
                             "(Use the preview command to include uncommitted changes.)"
                             "---------TREE---------")
                  (print-tree-with-settings (tree-display-settings-table) tree #t)
                  (displayer "----------------------")))

(create-command viewing-command preview
                ("preview")
                "View the state of the task tree if uncommitted changes are applied."
                (λ (bindings-lookup displayer tree commands)
                  (displayer "(Note: the preview command includes uncommitted changes)"
                             "(Use the view command to view committed changed.)"
                             "(Use the commit command to commit changes.)"
                             "----TREE (PREVIEW)----")
                  (print-tree-with-settings (tree-display-settings-table)
                                            (apply-actions-to-tree tree commands)
                                            #t)
                  (displayer "----------------------")))

(create-command viewing-command list
                ("list")
                "List the (active, non-undone/reset) commands you have entered since the last commit."
                (λ (bindings-lookup displayer tree commands)
                  (if (null? commands)
                      (displayer "No uncommitted actions.")
                      (begin
                        (displayer "All uncommitted actions:")
                        (map (λ (s)
                               (displayer (string-append " - " s)))
                             (map action-cmd commands))))))

(create-command history-modifying-command commit
                ("commit")
                "Commits commands you have entered to your history file."
                (λ (bindings-lookup displayer tree commands)
                  (add-commands-to-history-file commands)
                  (displayer "COMMITTED all actions; all actions are now saved.")
                  (hash 'new-commands '()
                        'new-tree (apply-actions-to-tree tree commands))))

(create-command viewing-command help
                ("help" (with-defaults command-name "all"
                          (maybe command-name)))
                "If no args: view list of commands; else, view help info for command specified in first argument."
                (λ (bindings-lookup tree commands)
                  (let ((cmd-name (bindings-lookup 'command-name)))
                    (if (set-member? all-command-names (string->symbol cmd-name))
                        (begin
                          (displayln (format "HELP FOR: ~a" cmd-name))
                          (display (hash-ref command-help-texts
                                             (string->symbol cmd-name))))
                        (begin
                          (displayln "THE AVAILABLE COMMANDS ARE:")
                          (map (λ (cmd-name)
                                 (displayln (format " - ~a" cmd-name)))
                               (set->list all-command-names))
                          (displayln "Type 'help' followed by the name of a command for help on that command."))))))

(create-command settings-command sort
                ("sort" (with-defaults type "invert"
                          (maybe (test (λ (t)
                                         (match t
                                           ("id" "id")
                                           ("size" "size")
                                           (_ #f)))
                                       type))))
                "If no args: invert current sort order. First arg: new sort order type: id/size."
                (λ (bindings-lookup)
                  (let ((type (bindings-lookup 'type)))
                    (if (equal? type "invert")
                        (begin
                          (invert-sort-order)
                          (displayln "Inverted sort order."))
                        (begin
                          (displayln (format "Set sort order to ~a" type))
                          (set-sort-order-type type))))))

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
            history-modifying-commands
            settings-commands)
