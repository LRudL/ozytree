#lang racket/base
(require racket/file
         racket/path
         racket/list
         racket/string
         racket/match
         racket/set
         threading
         json
         "utils.rkt"
         "cli.rkt"
         "setup.rkt"
         "tree.rkt"
         "history-management.rkt"
         "history-loading.rkt"
         "commands.rkt"
         (for-syntax racket/base
                     racket/list))

(define (add-act act action-list)
  (append action-list
          (list act)))

(define (command-loop tree commands)
  (let ((cmd (read-line)))
    (cond ((equal? cmd "q")
           (displayln "Exited Ozytree."))
          ((equal? cmd "")
           (displayln "")
           (command-loop tree commands))
          (#t
           (let ((act (interpret-cmd cmd)))
             (set-match (action-name act)
                        ((set 'generic-invalid-command-error)
                         (begin
                           (displayln "ERROR: something is wrong with your command")
                           (displayln "       Type 'help' to see command list.")
                           (displayln "       Type 'help' followed by a command name to see help for that command.")
                           (command-loop tree commands)))
                        (tree-modifying-commands
                         (begin
                           (displayln (format "Added ~a action to list"
                                              (action-name act)))
                           (command-loop tree
                                         (add-act act commands))))
                        (command-list-modifying-commands
                         (begin
                           (displayln (format "Changing action list: ~a"
                                              (action-name act)))
                           (command-loop tree
                                         ((action-fn act) commands))))
                        (viewing-commands
                         (begin
                           (cond ((equal? (action-name act) 'view)
                                  (displayln "(Note: the view command excludes uncommitted changes.)")
                                  (displayln "(Use the preview command to include uncommitted changes.)"))
                                 ((equal? (action-name act) 'preview)
                                  (displayln "(Note: the preview command includes uncommitted changes)")
                                  (displayln "(Use the view command to view committed changed.)")
                                  (displayln "(Use the commit command to commit changes.)")))
                           ((action-fn act) tree commands)
                           (command-loop tree commands)))
                        (history-modifying-commands
                         (begin
                           ((action-fn act) tree commands)
                           (displayln "COMMITTED all actions; no unsaved actions left.")
                           (command-loop (apply-actions-to-tree tree commands)
                                         '())))
                        (#t (raise "Error: something very strange happened in command-loop"))))))))

(define (start-command-loop)
  (let ((commits (commits-from-file)))
    (displayln (format "Loaded ~a commits from your history file"
                       (length commits)))
    (let ((current-tree-state
           (apply-commits-to-tree base-tree
                                  commits)))
      (configure-id-generator-with-tree current-tree-state)
      (displayln "CURRENT TREE STATE:")
      (print-tree current-tree-state #t)
      (displayln "Command prompt started (q + enter to quit)")
      (command-loop current-tree-state
                    '()))))

(run-setup)
(configure-id-generator-with-tree base-tree)
(start-command-loop)
