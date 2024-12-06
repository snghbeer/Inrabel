#lang racket
(require "NMBS/NMBS.rkt")

(define ipv4-adress "192.168.172.1") ;ip-address of machine running Infrabel
(define nmbs (nmbs-adt ipv4-adress))
(nmbs 'start)