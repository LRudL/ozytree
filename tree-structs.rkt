#lang racket/base

(provide (struct-out task)
         (struct-out task-entry)
         task-state)

(struct task (id info children) #:transparent)

(struct task-entry (text state size) #:transparent)

(define task-state (compose task-entry-state task-info))
