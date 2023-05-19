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
(define sistema_files (lambda (lista)
                        (list-ref lista 5)))
(define sistema_papeleraC (lambda (lista) ; papeleraC refiere a la papelera hecha para carpetas
                            (list-ref lista 6)))
(define sistema_papeleraA (lambda (lista) ; papeleraA refiere a la papelera hecha para archivos
                            (list-ref lista 7)))
(define file list) ; simplementa define la palabra "file" como si se usase "list"

(provide sistema_nombre sistema_drives sistema_usuarios sistema_log sistema_folders sistema_files sistema_papeleraC sistema_papeleraA file)