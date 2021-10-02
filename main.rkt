#lang racket/base
(require racket/file
         racket/path
         racket/list
         racket/string
         racket/match
         threading
         json
         "cli.rkt"
         "setup.rkt"
         "tree.rkt"
         "history-management.rkt"
         "history-loading.rkt"
         (for-syntax racket/base
                     racket/list))

(define (add-act act action-list)
  (append action-list
          (list act)))

(define (command-loop tree commands)
  (let ((cmd (read-line)))
    (if (equal? cmd "q")
        'exited
        (let ((act (interpret-cmd cmd)))
          (match (action-name act)
            ('invalid-prefix-error
             (begin
               (displayln (format "ERROR: ~a is not a valid command prefix"
                                (car (tokenise cmd))))
               (command-loop tree commands)))
            ('invalid-arity-error
             (begin
               (displayln (format "ERROR: wrong number of parameters"))
               (command-loop tree commands)))
            ((or 'make 'delete 'move 'set 'mark 'unmark)
             (begin
               (displayln (format "Added ~a action to list"
                                (action-name act)))
               (command-loop tree
                             (add-act act commands))))
            ((or 'reset 'undo)
             (begin
               (displayln (format "Changing action list: ~a"
                                (action-name act)))
               (command-loop tree
                             ((action-fn act) commands))))
            ((or 'view 'preview 'list)
             (begin
               (cond ((equal? (action-name act) 'view)
                      (displayln "Note: the view command excludes uncommitted changes, use the preview command to include those"))
                     ((equal? (action-name act) 'preview)
                      (displayln "Note: the preview command includes uncommitted changes, use the view command to view committed changed and commit to commit")))
               ((action-fn act) tree commands)
               (command-loop tree commands)))
            ('commit
             ((action-fn act) tree commands)
             (displayln "COMMITTED all actions; no unsaved actions left.")
             (command-loop (apply-actions-to-tree tree commands)
                           '()))
            (_ (raise "Error: something very strange happened in command-loop")))))))

(define (start-command-loop)
  (let ((commits (commits-from-file)))
    (displayln (format "Loaded ~a commits from your history file"
                       (length commits)))
    (let ((current-tree-state
           (apply-commits-to-tree base-tree
                                  commits)))
      (configure-id-generator-with-tree current-tree-state)
      (displayln "CURRENT TREE STATE:")
      (print-tree current-tree-state)
      (displayln "Command prompt started (q + enter to quit)")
      (command-loop current-tree-state
                    '()))))

(run-setup)
(configure-id-generator-with-tree base-tree)
(start-command-loop)
