#lang scheme

;TDA fecha
;Dirigido a sistema
;Representación: listas

;Constructor
(define tiempo current-seconds)
(define tiempo2 (seconds->date(tiempo)))

;Selector
(define dia (date-day tiempo2))
(define mes (date-month tiempo2))
(define año (date-year tiempo2))

;Constructor
(define fecha (list dia mes año)) ; Crea lista con 3 valores, día, mes y año


(provide tiempo tiempo2 fecha dia mes año)