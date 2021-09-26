#lang racket/base

(require racket/date
         racket/string
         racket/function
         racket/file
         threading
         "utils.rkt"
         "settings.rkt"
         "action-structs.rkt")

(provide commands->commit
         history-file->commits
         append-commits->history-file
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

(struct commit (time commands))

(define (commands->commit commands)
  (commit (current-date)
          commands))

(define commit-prefix ">>>>>>>>")

(define (commit->commit-header-line c)
  (string-append commit-prefix
                 " COMMIT ON "
                 (commit-time->string (commit-time c))))

(define (commit->lines c)
  (cons (commit->commit-header-line c)
        (map action->line
             (commit-commands c))))

(define (commit-from-header command-interpreter header commands)
  (commit (string->commit-time header)
          (map command-interpreter commands)))

(define (history-file->commits command-interpreter)
  (~>> (history-file-path)
       (file->lines)
       ((labelled-segments-from-list-maker
         (curry prefix? commit-prefix)
         (curry commit-from-header command-interpreter)))))

(define (commits->lines commits)
  (mapcat commit->lines commits))

(define (append-commits->file commits file-path)
  (display-lines-to-file (commits->lines commits)
                         file-path
                         #:exists 'append))

(define (append-commits->history-file commits)
  (append-commits->file commits (history-file-path)))

