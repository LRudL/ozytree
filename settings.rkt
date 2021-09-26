#lang racket/base

(require racket/file
         racket/path
         json
         "utils.rkt")

(provide
 ; variables:
 current-path
 config-file-path
 ; functions:
 data-directory-path
 output-directory-path
 history-file-path
 temp-file-path
 refresh-configuration ; <- use this if config file is changed and want changes to take effect
 )

(define current-path (path->string (current-directory)))

(define config-file-name "config.json")

(define config-file-path
  ; note that this is always the same - to enforce this, not a function unlike paths below
  (string-append current-path config-file-name))

(define (read-json-file path)
  (define port (open-input-file path))
  (let ((json (read-json port)))
    (close-input-port port)
    json))

(define (read-config-from-file)
  (read-json-file config-file-path))

(define config 'run-refresh-before-config-exists)

(define (refresh-configuration)
  (set! config (read-config-from-file)))

; GET FILE LOCATIONS:
(define (data-directory-path)
  (hash-ref config 'history-location))

(define (output-directory-path)
  (hash-ref config 'output-location))

(define (history-file-path)
  (string-append (data-directory-path)
                 "history.txt"))

(define (temp-file-path)
  (string-append (data-directory-path)
                 "temp.txt"))

; GET FILE CONTENTS:
(define (get-history-file-lines)
  (file->lines (history-file-path)))

(define (get-temp-file-lines)
  (if (file-exists? temp-file-path)
      (file->lines (temp-file-path))
      '()))
