#lang racket/base
(require racket/match
         racket/function
         threading
         "utils.rkt"
         "idgen.rkt"
         "tree.rkt"
         "action-structs.rkt"
         "commands.rkt"
         (for-syntax racket/base))

(provide tokenise
         get-interpreter
         (struct-out action)
         interpret-cmd)

(define (tokeniser-acc word-list in-string? current-word)
  (cons in-string?
        (cons current-word
              word-list)))

(define (tokenise s)
  (~>> (string->list s)
       (foldr (λ (char acc)
                (let ((word-list (cddr acc))
                      (in-string? (car acc))
                      (current-word (cadr acc)))
                  (cond ((equal? char #\")
                          (tokeniser-acc (cons current-word word-list)
                                        (not in-string?)
                                        '()))
                        ((and (equal? char #\space)
                              (not in-string?))
                         (tokeniser-acc (cons current-word word-list)
                                        in-string?
                                        '()))
                        (#t (tokeniser-acc word-list
                                           in-string?
                                           (cons char current-word))))))
              (tokeniser-acc '() #f '()))
       (cdr) ; <- includes current-word, exploiting tokeniser-acc definition 
       ; space/quote interaction, plus above, can lead to empty "words", so:
       (filter (compose not null?))
       (map list->string)))

(define (generic-error tokens)
  (list 'generic-invalid-command-error
        (cdr tokens)
        (λ (tree) tree)))

(define (get-cmd-name-and-parser s)
  (define (finder s pairs)
    (if (null? pairs)
        'no-interpreter
        (let ((cmd-name (caar pairs))
              (cmd-parser (cdar pairs)))
          (if (cmd-parser s)
              (car pairs)
              (finder s (cdr pairs))))))
  (finder s command-name-and-parser-pairs))

(define (get-cmd-interpreter cmd-name)
  (hash-ref command-interpreter-table cmd-name
            (λ ()
              (raise
               "Error: tried to get and run command interpreter that does not exist"))))

(define (get-interpreter s)
  (get-cmd-interpreter (car (get-cmd-name-and-parser s))))

(define (interpret-cmd s)
  (apply
   action
   (append (list s)
           (let* ((tokens-raw (tokenise s))
                  (tokens (if (equal? (car tokens-raw) "make")
                              ; Hacky: make takes as an extra argument the
                              ; id, which is generated rather than user-supplied.
                              ; Needs to be done already at this stage
                              ; because next we compute the name.parser pair;
                              ; this requires make commands in their "full" form.
                              (cons "make"
                                    (cons (generate-id)
                                          (cdr tokens-raw)))
                              tokens-raw))
                  (name.parser (get-cmd-name-and-parser tokens)))
             (if (not (pair? name.parser))
                 (generic-error tokens)
                 (let* ((cmd-name (car name.parser))
                        (parser (cdr name.parser))
                        (interpreter (get-cmd-interpreter cmd-name)))
                   (if (not (procedure? interpreter))
                       (generic-error tokens)
                       (let ((bindings (parser tokens)))
                         (if bindings
                             (interpreter bindings)
                             (generic-error tokens))))))))))
