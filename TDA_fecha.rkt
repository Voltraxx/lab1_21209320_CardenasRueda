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

;Pertenencia
;Dominio: fecha (lista)
;Recorrido: Booleano (True / False)
(define fecha? (lambda (fecha)
                 (and (list? fecha) (= 3 (length fecha)))))


(provide tiempo tiempo2 fecha fecha? dia mes año)