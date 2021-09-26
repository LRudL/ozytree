#lang racket/base

(require racket/match
         racket/list
         racket/function
         racket/set
         threading
         (for-syntax racket/base))

(provide
 dbprint
 ; debug printer; println-s a value and then returns it
 intersperse
 ; takes lists and returns the list you get from taking an element
 ; from one, then the next, and so on, looping around after reaching
 ; the end of the lists, and taking no more elements from lists
 ; after they are empty
 sliding-window
 ; takes a window size, lst, and (optional, default 1) step size
 ; to a list of sublists of lst
 pair-adjacents
 ; pairs adjacent elements in a list; equivalent to:
 ; sliding-window with window size 2 and step size 2
 list-with-exceptions
 ; takes a list, a set of list indices, and a function that will be applied
 ; to the exceptional list indices to generate a replacement for whatever
 ; is currently in the list at that point
 mapi
 ; maps with index: works like map, except function is of the form
 ; (index, x0, x1, x2 ...) -> val
 ; NOTE: stops at shortest list
 mapcat
 ; map and then concatenate the resulting lists
 set!ret
 ; like set!, but also returns the value
 split-list
 ; (fn that is #t if list should be broken there, list) -> list
 split-list-at
 ; same as split-list but takes an item instead of a function
 repeat
 ; takes a number n and a thing and returns that thing n times 
 labelled-segments-from-list-maker
 ; takes a function to determine whether a list item is a label,
 ; does split-list with that, and then uses second arg to combine
 ; labels with the segments outputted by split-list; returns a function
 ; from list -> list
 list-prefix?
 ; test whether first arg (a list) is a prefix of the 2nd arg (also a list)
 prefix?
 ; generic version of the above, working for strings too
 input-converter
 ; used for making functions more generic by pre-processing
 ; inputs; specifically:
 ; takes a list of lists of pairs of the form (t1? . t1->t2)
 ; and a function f; returns a function that is like f but for
 ; the nth arg, for the first  t1? in the nth list of pairs is true
 ; for the value of the nth arg, calls t1->t2 before passing the arg
 ; to f; if no t1? matches, passes arg as is
 convert-numbered-args
 ; takes a function f, another function to convert some of the arguments,
 ; and any number of argument indices; will return a function that is like
 ; f except the converter function is applied to the specified argument
 ; positions before the arguments are passed in to f
 converters-on-numbered-args
 ; like the above, but instead of taking a single conversion
 ; function and a list of argument indices, takes
 ; an even-numbered list of args after the first arg (f)
 ; of the form:
 ; arg-index converter-for-that-index ...
 )

(define (dbprint x)
  (println x)
  x)

(define-syntax-rule (set!ret x v)
  (begin
    (set! x v)
    x))

(define (intersperse . lists)
  (let ((non-empty-lists
         (filter (compose not null?) lists)))
    (if (null? non-empty-lists)
        '()
        (append (map car non-empty-lists)
                (apply intersperse (map cdr non-empty-lists))))))

(define (sliding-window-helper
         window-size l llength step)
  (let ((to-take (min window-size llength)))
    (cons (take l to-take)
          (if (= to-take llength)
              '()
              (sliding-window-helper window-size
                                     (drop l to-take)
                                     (- llength to-take)
                                     step)))))

(define (sliding-window window-size l (step 1))
  (sliding-window-helper window-size l (length l) step))

(define (pair-adjacents lst)
  (sliding-window 2 lst 2))

(define (list-with-exceptions lst exceptions-set exception-generator)
  (~>> lst
       (mapi (λ (i x)
               (if (set-member? exceptions-set i)
                   (exception-generator i)
                   x)))))

(define (mapcat fn . lists)
  (apply append
         (apply map (cons fn lists))))

(define (mapi-helper n max fn lists)
  (if (= n max)
      '()
      (cons (apply fn
                   (cons n
                         (map car lists)))
            (mapi-helper (+ n 1)
                         max
                         fn
                         (map cdr lists)))))

(define (mapi fn-from-index-and-el . lists)
  (mapi-helper  0
                (apply min (map length lists))
                fn-from-index-and-el
                lists))

(define (split-list fn l)
  (foldr (λ (next acc)
           (if (fn next)
               (cons '() acc)
               (cons (cons next (car acc))
                     (cdr acc))))
         '(())
         l))

(define (split-list-at item l)
  (split-list (λ (el)
                (equal? el item))
              l))

(define (repeat n x)
  (if (= n 0)
      '()
      (cons x (repeat (- n 1) x))))

(define (labelled-segments-from-list-maker label? seg-maker)
  (λ (l)
    (let ((labels (filter label? l))
          (segmented-list (cdr (split-list label? l))))
      (if (equal? (length labels)
                  (length segmented-list))
          (map seg-maker ; (seg-label, seg-items) -> seg-object
               labels
               segmented-list)
          (raise
           (format
            (string-append "Error in labelled-segments-from-list-maker: "
                           "must have one label for each segment; "
                           "instead labels are ~a and segments are ~a. "
                           "Note that it is assumed labels precede "
                           "each segment and therefore the list starts "
                           "with a label.")
            labels
            segmented-list))))))

(define (list-prefix?* pre l)
  ; this already exists in racket/list
  ; however, need definite-arity version for input-converter to work
  (if (null? pre)
      #t
      (if (equal? (car pre) (car l))
          (prefix? (cdr pre) (cdr l))
          #f)))

(define (apply-test-convert-pairs pairs arg)
  (match pairs
    ['() arg]
    [(cons (cons is-a-t1? t1->t2) rest-pairs)
     (if (is-a-t1? arg)
         (t1->t2 arg)
         (apply-test-convert-pairs rest-pairs arg))]))

(define (input-converter test-convert-pairs-lists fn)
  (λ args
    (apply fn
           (map (λ (test-convert-pairs arg)
                  (apply-test-convert-pairs test-convert-pairs arg))
                test-convert-pairs-lists
                args))))

(define (convert-numbered-args fn converter . arg-nums)
  ; TODO: this is a special case of
  ; converters-on-numbered-args, write in terms of that
  (let ((arity (procedure-arity fn)))
    (if (number? arity)
        (input-converter
         (list-with-exceptions (repeat arity '())
                               (list->set arg-nums)
                               (λ (i)
                                 (list (cons (λ (_) #t)
                                             converter))))
         fn)
        (raise
         "Tried to use convert-numbered-args with fn without explicit function arity"))))

(define (converters-on-numbered-args fn . args)
  (let* ((argnum-converter-pairs
          (pair-adjacents args))
         (argnum-set (list->set
                      (map car
                           argnum-converter-pairs)))
         (converter-table
          (apply hash args)))
    (let ((arity (procedure-arity fn)))
      (if (number? arity)
          (input-converter
           (list-with-exceptions
            (repeat arity '())
            argnum-set
            (λ (i)
              (list
               (cons (λ (_) #t)
                     (hash-ref converter-table
                               i)))))
           fn)
          (raise
           "Tried to use convert-numbered-args with fn without explicit function arity")))))

(define prefix?
  (input-converter (repeat 2 ; because list-prefix takes 2 args
                           (list (cons string?
                                       string->list)))
                   list-prefix?))

