#lang racket/base
(require racket/file
         racket/path
         racket/list
         racket/string
         racket/match
         racket/set
         threading
         "utils.rkt"
         "cli.rkt"
         "setup.rkt"
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
                              (displayln "\x1b[31m AN ERROR OCCURRED: \x1b[0m")
                              (display e)
                              (displayln "\nCommand loop successfully recovered.")
                              (command-loop tree commands))))
             (act-match command-loop tree commands (interpret-cmd cmd)))))))

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
