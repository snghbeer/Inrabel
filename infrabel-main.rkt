#lang racket

(require "INFRABEL/INFRABEL.rkt")

;LOOP scenarios:
;scenario 1: start D2,prev D1, destination D9

;scenario 2: start D2, prev D1, destination D5
;or start D2, prev D3, destination D5

;scenario 3: start D3, prev D2, dest D7 or
;start D3, prev D4, D7

;HARDWARE scenarios:
;setup-hardware (prev position doesnt matter)
;add 2 locos:

;scenario 1:
;start 1-3, destination 2-7  
;start 1-1, destination 2-5
;confirm beide locos chronologisch

;scenario 2:
;start 1-2, destination 2-6  
;start 1-3, destination 2-2
;confirm beide locos chronologisch

;STRAIGHT scenario:
;start D1, destination D5  
;start D1, destination D7

(define infrabel (infrabel-adt))