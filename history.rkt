#lang racket/base

(require racket/date
         racket/string
         racket/function
         racket/file
         racket/list
         threading
         "utils.rkt"
         "idgen.rkt"
         "settings.rkt"
         "action-structs.rkt"
         "tree-structs.rkt"
         "tree.rkt"
         "cli.rkt")

(provide commands->commit
         history-file->commits
         append-commits->history-file
         overwrite-history-file-with-commits
         base-tree
         (struct-out commit))

(define (action->line act)
  (action-cmd act)) ; i.e. the raw command as entered by the user

(date-display-format 'iso-8601)

(define (commit-time->string date)
  (string-append (date->string date #t)
                 " " (number->string (date->seconds date))))

(define (string->commit-time s)
  (~> s
      (string-split " ")
      (cddddr)
      (car) ; this is processing the text defined in commit->commit-header-line
      (string->number)
      (seconds->date)))

(struct commit (time commands) #:transparent)

(define (commands->commit commands)
  (commit (current-date)
          (if (and (not (null? commands))
                   (not (string? (car commands))))
              (map action->line commands)
              commands)))

(define commit-prefix ">>>>>>>>")

(define (commit->commit-header-line c)
  (string-append commit-prefix
                 " COMMIT ON "
                 (commit-time->string (commit-time c))))

(define (commit->lines max-previous-id c)
  (cons (commit->commit-header-line c)
        (map (λ (cmd-string)
               (if (and (prefix? "make" cmd-string)
                        (not (prefix? "->"
                                      ; prevent rollback-related overwrite from adding more IDs
                                      (last (string-split cmd-string)))))
                   (string-append cmd-string " ->"
                                  (number->string
                                   (hash-ref (action-props (interpret-cmd cmd-string))
                                             'new-node-id)))
                   cmd-string))
             (commit-commands c))))

(define (commit-from-header header commands)
  (commit (string->commit-time header)
          ;(map command-interpreter commands)
          commands ; ^ don't want to interpret already, since that assigns IDs to nodes
          ))

(define (history-file->commits)
  (~>> (history-file-path)
       (file->lines)
       ((labelled-segments-from-list-maker
         (curry prefix? commit-prefix)
         commit-from-header))))

(define (commits->lines commits max-previous-id)
  (mapcat (curry commit->lines max-previous-id) commits))

(define (append-commits->file commits file-path on-top-of-tree)
  (let* ((tree-max-id (max-id-in-tree-rooted-at on-top-of-tree))
         (idgen-resumer (restart-id-generator!-and-get-resumer
                         tree-max-id)))
    (display-lines-to-file (commits->lines commits tree-max-id)
                           file-path
                           #:exists 'append)
    (idgen-resumer)))

(define (append-commits->history-file commits on-top-of-tree)
  (append-commits->file commits
                        (history-file-path)
                        on-top-of-tree))

(define (overwrite-history-file-with-commits commits on-top-of-tree)
  (let* ((tree-max-id (max-id-in-tree-rooted-at on-top-of-tree))
         (idgen-resumer (restart-id-generator!-and-get-resumer
                         tree-max-id)))
    (display-lines-to-file (commits->lines commits tree-max-id)
                           (history-file-path)
                           #:exists 'truncate/replace)
    (idgen-resumer)))

(define base-tree
  (task 0 (task-entry "root" 'incomplete 0)
        (list)))
