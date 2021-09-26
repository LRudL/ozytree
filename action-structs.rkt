#lang racket/base

(provide (struct-out action))

(struct action (cmd name props fn) #:transparent)
