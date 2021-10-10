#lang racket/base

(require racket/file
         racket/path
         racket/set
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
 sort-order-type
 sort-order-inversion
 set-sort-order-type
 invert-sort-order
 tree-display-settings-table
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

(define (write-config-to-file)
  (if (jsexpr? config)
      (let ((port (open-output-file config-file-path
                                    #:exists 'truncate/replace)))
        (write-json config port)
        (close-output-port port))
      (raise "ERROR: config file has reached an un-JSON-able state.")))

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

; GET/SET SORT ORDER STUFF:
(define (sort-order-type)
  (hash-ref config 'sort-order-type "size"))

(define (sort-order-inversion)
  (hash-ref config 'sort-order-inversion #f))

(define (set-sort-order-type type)
  (set! config
        (hash-set config 'sort-order-type type))
  (write-config-to-file))

(define (invert-sort-order)
  (set! config
        (hash-set config 'sort-order-inversion
                  (not
                   (hash-ref config 'sort-order-inversion #f))))
  (write-config-to-file))

; TREE DISPLAY SETTINGS:
(define (tree-display-settings-table)
  (hash 'sort-order-type (sort-order-type)
        'sort-order-inverted? (sort-order-inversion)))
