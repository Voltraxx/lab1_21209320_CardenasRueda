#lang scheme

;TDA_selectorSistema
;Dirigido a sistema

;Selectores
(define sistema_nombre car)
(define sistema_drives cadr)
(define sistema_usuarios caddr)
(define sistema_log cadddr)
(define sistema_folders (lambda (lista)
                          (list-ref lista 4)))

(provide sistema_nombre sistema_drives sistema_usuarios sistema_log sistema_folders)