#lang racket
(require rackunit
         "../NMBS/NMBS.rkt"
         rackunit/gui
         ;rackunit/text-ui
         )

;complex scenarios:
;1-1 2-7
;1-3 2-5
;1-5 2-1

(define ipv4-adress "192.168.1.50") ;ip-address of machine running Infrabel

;;BELANGRIJK;;
;eerst moet infrabel-main.rkt runnen ../infrabel-main.rkt
;dan run deze test-file

(define 1-1 13)
(define 1-2 14)
(define 1-3 15)
(define 1-4 17)
(define 1-5 18)
(define 1-6 19)
(define 1-7 21)
(define 1-8 22)
(define 2-1 29)
(define 2-2 30)
(define 2-3 31)
(define 2-4 32)
(define 2-5 33)
(define 2-6 34)
(define 2-7 35)
(define 2-8 36)

;Run nmbs
(define nmbs (nmbs-adt ipv4-adress))
(nmbs 'start)

;Initialize hardware mode
((nmbs 'send) (vector "Mode" "Setup-hardware"))
(sleep 1)

(define path-tests
  (test-suite
   "Path finding tests" ;taking start position and destination
   (test-case
    "Simple scenario path-finding"
    (check-equal? ((nmbs 'newp) 1-4 2-4) '("17" "18" "32"))
    (check-equal? ((nmbs 'newp) 1-1 1-5) '("13" "12" "32" "18"))
    (check-equal? ((nmbs 'newp) 2-1 2-5) '("29" "2" "3" "8" "33"))
    )
   (test-case
    "Complex scenario path-finding"
    (check-equal? ((nmbs 'newp) 1-7 2-7) '("21" "19" "6" "5" "7" "2" "3" "8" "4" "35"))
    (check-equal? ((nmbs 'newp) 1-4 2-2) '("17" "18" "20" "5" "7" "2" "3" "30"))
    (check-equal? ((nmbs 'newp) 1-3 2-5) '("15" "32" "20" "5" "7" "2" "3" "8" "33"))
    )
   (test-case
    "TCP-test"
    (check-true (string=? (nmbs 'tcp) "none"))
    ((nmbs 'send) "TCP-test")
    (sleep 1)
    (check-true (string=? (nmbs 'tcp) "succes")))
   ))

;(run-tests path-tests)
(test/gui path-tests)