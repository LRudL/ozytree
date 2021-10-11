#lang racket/base
(require racket/file
         racket/path
         racket/list
         racket/string
         racket/match
         racket/set
         racket/date
         threading
         "utils.rkt"
         "cli.rkt"
         "setup.rkt"
         "settings.rkt"
         "tree.rkt"
         "history-management.rkt"
         "history.rkt"
         "commands.rkt"
         (for-syntax racket/base
                     racket/list))

(define (add-act act action-list)
  (append action-list
          (list act)))

(define (displayer . args)
  (if (> (length args) 1)
      (map displayer args)
      (displayln (car args))))

(define (act-match loop tree commands act)
  (set-match (action-name act)
             ((set 'generic-invalid-command-error)
              (begin
                (displayer "ERROR: something is wrong with your command"
                           "       Type 'help' to see command list."
                           "       Type 'help' followed by a command name to see help for that command.")
                (loop tree commands)))
             (tree-modifying-commands              
              (loop tree
                    (add-act act commands)))
             (command-list-modifying-commands              
              (loop tree
                    ((action-fn act) commands)))
             (viewing-commands
              (begin
                ((action-fn act) displayer
                                 history-functions-table
                                 tree
                                 commands)
                (loop tree commands)))
             (history-modifying-commands
              (let* ((res ((action-fn act) displayer
                                           history-functions-table
                                           tree
                                           commands))
                     (new-commands (hash-ref res 'new-commands))
                     (new-tree (hash-ref res 'new-tree)))
                (loop new-tree new-commands)))
             (settings-commands
              (begin
                ((action-fn act))
                (loop tree commands)))
             (enter-history-mode-commands
              (start-history-manager-loop
               (λ () ; return function
                 (configure-id-generator-with-tree tree)
                 (loop tree commands))))
             (#t (raise "Error: something very strange happened in the command loop."))))

(define (command-loop tree commands)
  (let ((cmd (read-line)))
    (cond ((equal? cmd "q")
           (displayln "Exited Ozytree."))
          ((equal? cmd "")
           (displayln "")
           (command-loop tree commands))
          (#t
           (with-handlers ((exn:fail?
                            (λ (e)
                              (displayln "\x1b[31m AN ERROR OCCURRED:")
                              (display e)
                              (displayln " \x1b[0m\nCommand loop successfully recovered.")
                              (command-loop tree commands))))
             (act-match command-loop tree commands (interpret-cmd cmd)))))))

(define (history-viewer-printout history current-commit future)
  (clear-terminal-screen)
  (displayer "\x1b[1m Ozytree History Browser \x1b[0m"
             (format "Commit \x1b[1m\x1b[32m~a of ~a\x1b[0m, committed at \x1b[1m\x1b[32m~a\x1b[0m"
                     (+ 1 (length history))
                     (+ (length history) (length future) 1)
                     (date->string (commit-time current-commit) #t))
             "~~~~~~~~"
             "AVAILABLE COMMANDS:"
             "b = back / f or nothing = forwards"
             "rollback = irreversibly revert to this point in history"
             "done = exit history view"
             "~~~~~~~~"
             "The state of your task tree at this time:")
  (configure-id-generator-with-tree base-tree)
  (print-tree-with-settings (tree-display-settings-table)
                            (apply-commits-to-tree base-tree
                                                   (reverse
                                                    (cons current-commit
                                                          history)))
                            #t))

(define (history-manager-loop command-loop-resumer history current future)
  (history-viewer-printout history current future)
  (let ((cmd (read-line)))
    (cond ((equal? cmd "done")
           (displayln "\x1b[1m Exited history viewer. \x1b[0m")           
           (command-loop-resumer))
          ((or (equal? cmd "")
               (equal? cmd "f"))
           (if (null? future)
               (history-manager-loop command-loop-resumer history current future)
               (history-manager-loop command-loop-resumer
                                     (cons current history)
                                     (car future)
                                     (cdr future))))
          ((equal? cmd "b")
           (if (null? history)               
               (history-manager-loop command-loop-resumer history current future)
               (history-manager-loop command-loop-resumer
                                     (cdr history)
                                     (car history)
                                     (cons current future))))
          ((equal? cmd "rollback")
           (overwrite-history-file-with-commits
            (reverse (cons current history)))
           (start-history-manager-loop command-loop-resumer))
          (#t (begin
                (displayln "\x1b[1m Invalid command in history loop, exiting \x1b[0m")
                (command-loop-resumer))))))

(define (start-history-manager-loop command-loop-resumer)
  (configure-id-generator-with-tree base-tree)
  (let ((commit-history-reversed (reverse (history-file->commits))))
    (history-manager-loop command-loop-resumer
                          (cdr commit-history-reversed)
                          (car commit-history-reversed)
                          '())))

(define (start-command-loop)
  (let ((commits (history-file->commits)))
    (displayln (format "Loaded ~a commits from your history file"
                       (length commits)))
    (let ((current-tree-state
           (apply-commits-to-tree base-tree
                                  commits)))
      (configure-id-generator-with-tree current-tree-state)
      (displayln "CURRENT TREE STATE:")
      (print-tree current-tree-state #t)
      (displayln "Command prompt started (q + enter to quit, help + enter for help)")
      (command-loop current-tree-state
                    '()))))

(run-setup)
(configure-id-generator-with-tree base-tree)
(start-command-loop)
