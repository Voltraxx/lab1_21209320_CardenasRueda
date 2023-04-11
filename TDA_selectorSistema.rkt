#lang scheme

;TDA_selectorSistema
;Dirigido a sistema

;Selectores
(define sistema_nombre car)
(define sistema_drives cadr)
(define sistema_usuarios caddr)
(define sistema_log cadddr)

(provide sistema_nombre sistema_drives sistema_usuarios sistema_log)