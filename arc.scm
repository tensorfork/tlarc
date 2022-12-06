#lang racket/load

(compile-allow-set!-undefined #t)
(compile-enforce-module-constants #f)

(define-runtime-path ac-scm "ac.scm")


(load ac-scm)
(require 'ac)

(define-runtime-path brackets-scm "brackets.scm")
(load brackets-scm)
(require 'brackets)

(use-arc-readtable)

(define-runtime-path arc-arc "arc.arc")
(aload arc-arc)
(define-runtime-path libs-arc "libs.arc")
(aload libs-arc) 

