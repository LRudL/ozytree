#lang racket/base
(require racket/match
         racket/function
         threading
         "utils.rkt"
         "tree.rkt"
         "action-structs.rkt"
         "printing.rkt"
         "history-management.rkt"
         (for-syntax racket/base))

(provide tokenise
         get-interpreter
         get-interpreter-arity
         (struct-out action)
         configure-id-generator-with-tree
         interpret-cmd)

; To enable hacky eval magic later on:
(define-namespace-anchor namespace-anchor)
(define ns (namespace-anchor->namespace namespace-anchor))

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

; make 0 "blah blah solve physics" 1
; delete 0
; set 0 text "new text"
; set 0 size 3
; mark 0
; unmark 0
; move 3 under 4
; swap 2 4

; delete lists like above: reset

; undo
; undo 3

; show tree as in saved files: view
; show tree including uncommitted: preview
; append to file: commit


(define generate-id
  (λ () 'if-you-see-this-something-horrible-has-happened))

(define (get-generator initial-value)
  (define value initial-value)
  (λ ()
    (set! value (+ 1 value))
    value))

(define (configure-id-generator-with-tree tree)
  (set! generate-id (get-generator (max-node-id tree))))

(define-syntax args-str->num
  (λ (stx)
    (let* ((forms (syntax->datum stx))
           (fn (cadr forms))
           (arg-nums (cddr forms)))
      (datum->syntax stx
                     (append
                      `(convert-numbered-args ,fn
                                              string->number)
                      arg-nums)))))

(define (interpret-make* new-node-id parent-id task-text size)
  (list 'make
        (list parent-id
              task-text
              size)        
        (λ (tree)
          (put-node-under tree
                          (task new-node-id
                                (task-entry task-text
                                            'incomplete
                                            size)
                                '())
                          parent-id))))

(define interpret-make ; note: new-node-id is already a number, taken directly from generate-id
  (args-str->num interpret-make* 1 3))

(define (interpret-delete* node-id)
  (list 'delete
        (list node-id)
        (λ (tree)
          (remove-node-by-id tree node-id))))

(define interpret-delete
  (args-str->num interpret-delete* 0))

(define (interpret-move* node-id _under_ new-parent-node-id)
  (list 'move
        (list node-id new-parent-node-id)
        (λ (tree)
          (move-node tree node-id new-parent-node-id))))

(define interpret-move
  (args-str->num interpret-move* 0 2))

(define (interpret-set* node-id prop new-val)
  (list 'set
        (list node-id prop new-val)
        (λ (tree)
          (set-node-info-prop tree node-id prop new-val))))

(define interpret-set
  (converters-on-numbered-args
   interpret-set*
   0 string->number
   1 string->symbol))

(define (interpret-mark* node-id)
  (list 'mark
        (list node-id)
        (λ (tree)
          (set-node-info-prop tree
                              node-id
                              'state
                              'complete))))

(define interpret-mark
  (args-str->num interpret-mark* 0))

(define (interpret-unmark* node-id)
  (list 'unmark
        (list node-id)
        (λ (tree)
          (set-node-info-prop tree
                              node-id
                              'state
                              'incomplete))))

(define interpret-unmark
  (args-str->num interpret-unmark* 0))

(define (interpret-reset*)
  (list 'reset
        (list)
        (λ (commands) '())))

(define interpret-reset interpret-reset*)

(define (interpret-undo*)
  (list 'undo
        (list)
        (λ (commands)
          (if (null? commands)
              (begin
                (displayln "No uncommitted commands to undo.")
                '())
              (cdr commands)))))

(define interpret-undo interpret-undo*)

(define (interpret-view*)
  (list 'view
        (list)
        (λ (tree commands)
          (displayln "---------TREE---------")
          (print-tree tree)
          (displayln "----------------------"))))

(define interpret-view interpret-view*)

(define (interpret-preview*)
  (list 'preview
        (list)
        (λ (tree commands)
          (displayln "----TREE (PREVIEW)----")
          (print-tree (apply-actions-to-tree tree commands))
          (displayln "----------------------"))))

(define interpret-preview interpret-preview*)

(define (interpret-commit*)
  (list 'commit
        (list)
        (λ (tree commands)
          (add-commands-to-history-file commands))))

(define interpret-commit interpret-commit*)

(define (interpret-list*)
  (list 'list
        (list)
        (λ (tree commands)
          (if (null? commands)
              (displayln "No uncommitted actions.")
              (begin
                (displayln "All uncommitted actions:")
                (map (λ (s)
                       (displayln (string-append " - " s)))
                     (map action-cmd commands)))))))

(define interpret-list interpret-list*)

(define (get-interpreter s)
  (match (car (tokenise s))
    ("make"    interpret-make)
    ("delete"  interpret-delete)
    ("move"    interpret-move)
    ("set"     interpret-set)
    ("mark"    interpret-mark)
    ("unmark"  interpret-unmark)
    ("reset"   interpret-reset)
    ("view"    interpret-view)
    ("preview" interpret-preview)
    ("undo"    interpret-undo)
    ("commit"  interpret-commit)
    ("list"    interpret-list)
    (_         'no-interpreter)))

(define (valid-command-prefix? s)
  (not (equal? (get-interpreter s) 'no-interpreter)))

(define (get-interpreter-arity s)
  (if (valid-command-prefix? s)
      (eval (list
             'procedure-arity
             (string->symbol (string-append "interpret-" s "*")))
            ns)
      (raise (string-append
              "get-interpreter-arity: Cannot get arity for invalid command prefix: "
              s))))

(define (arity-error tokens)
  (list 'invalid-arity-error
        (cdr tokens)
        (λ (tree) tree)))

(define (interpret-cmd s)
  (apply action
         (append (list s)                 
                 (let ((tokens (tokenise s)))
                   (let ((interpreter (get-interpreter (car tokens))))
                     (cond ((equal? interpreter 'no-interpreter)
                            (list 'invalid-prefix-error
                               (cdr tokens)
                               (λ (tree) tree)))
                           ((equal? (car tokens) "make")
                            ; uglyish: we have an exception for make, because
                            ; need to call generate-id when interpreting,
                            ; but not e.g. when preview leads to applying it as part of
                            ; going through the actions list (with apply-actions-to-tree)
                            (if (= (- (procedure-arity interpret-make*) 1)
                                   (length (cdr tokens)))
                                (apply interpret-make (cons (generate-id)
                                                            (cdr tokens)))
                                (arity-error tokens)))
                           ((= (get-interpreter-arity (car tokens))
                               (length (cdr tokens)))
                            (apply interpreter (cdr tokens)))
                           (#t (arity-error tokens))))))))
