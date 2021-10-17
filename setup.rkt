#lang racket/base
(require racket/file
         racket/path
         racket/list
         racket/string
         threading
         json
         "utils.rkt"
         "settings.rkt"
         (for-syntax racket/base
                     racket/list))

(provide run-setup
         clear-terminal-screen)

(define-syntax do-and-inform
  (λ (stx)
    (let* ((forms (syntax->datum stx))
           (start-msg (second forms))
           (exec-exprs
            (take (cddr forms)
                  (- (length forms) 3)))
           (inform-fn (last forms)))
      (datum->syntax
       stx
       `(with-handlers
          ((exn:fail? (λ (e)
                        (displayln (string-append "FAILED AT: " ,start-msg))
                        (raise e))))
          (let ((result ,(cons 'begin
                               (if (= (length exec-exprs) 0)
                                   '(list 'no-op)
                                   exec-exprs))))
            (displayln (format "~a ... ~a" ,start-msg (,inform-fn result)))))))))

(define-syntax do-and-confirm
  (λ (stx)
    (datum->syntax stx
                   (cons 'do-and-inform
                         (append (cdr (syntax->datum stx))
                                 '((λ (x) "done")))))))

(define (prompt q (default ""))
    (displayln q)
    (let ((in (read-line)))
      (if (equal? in "")
          default
          (begin
            (displayln (string-append "Value read: " in))))))

(define default-data-dir-path (string-append current-path "data/"))
(define default-report-dir-path (string-append current-path "ozytree-reports/"))

(define (prompt-user-for-config)
  (hash 'history-location
        (prompt (string-append "Enter workfiles/history storage location; "
                               "press enter for " default-data-dir-path)
                default-data-dir-path)
        'output-location (prompt (string-append "Enter location where reports will be output to; "
                                                "press enter for " default-report-dir-path)
                                 default-report-dir-path)
        'sort-order-type "size"
        'sort-order-inversion #f))

(define (create-dir-if-not-existing path)
  (if (directory-exists? path)
      #t
      (make-directory path)))

(define (clear-terminal-screen)
  (display "x1b[2J") ; clear screen
  (display "\x1b[1;1f") ; move cursor to top-left
  (display "\x1b[0J") ; clear screen below cursor (needed for some reason??)
)

(define (run-setup)
  (clear-terminal-screen)
  (displayln "\x1b[1m === OZYTREE (v0.5.2) === \x1b[0m")
  (displayln "(WARNING: this is an unstable pre-alpha that will destroy your precious tasks and brick your computer)")
  (do-and-inform "Finding directory location: "
                 current-path
                 (λ (path) path))
  
  (define config-file-port null)

  (if (not (file-exists? config-file-path))
      (begin
        (do-and-confirm "Config file does not exist, opening"
                        (set! config-file-port (open-output-file config-file-path)))
        (do-and-confirm (string-append "Writing new config file to: " config-file-path)
                        (write-json (prompt-user-for-config)
                                    config-file-port))
        (close-output-port config-file-port))
      (do-and-inform "config.json exists; finding config file parameters"
                     (refresh-configuration)
                     (cons (data-directory-path)
                           (output-directory-path))
                     (λ (c) (string-append "\n   data location: " (car c)
                                           "\n   output location: " (cdr c)))))
  ; Calling this settings.rkt function to reload settings because above config.json changed:
  (refresh-configuration)
  (if (directory-exists? (data-directory-path))
      (do-and-confirm "Checking that data directory exists")
      (do-and-confirm "Data directory does not exist, creating"
                      (make-directory (data-directory-path))))

  (if (directory-exists? (output-directory-path))
      (do-and-confirm (string-append "Checking that the following output directory exists: "
                                     (output-directory-path)))
      (do-and-confirm (string-append "Creating the following output directory: "
                                     (output-directory-path))
                      (make-directory (output-directory-path))))

  (if (file-exists? (history-file-path))
      (do-and-inform "Found history.txt file."
                     (history-file-path)
                     (λ (p) (string-append "The path to your history.txt file is: " p)))
      (do-and-confirm (string-append "history.txt not found; creating at: " (history-file-path))
                      (let ((historytxt (open-output-file (history-file-path))))
                        (display "" historytxt)
                        (close-output-port historytxt))))
  (displayln "---------------"))
