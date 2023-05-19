#lang scheme

;TDA_selectorDatos
;Dirigido a folders y files

; Selectores

; Crea lista con las carpetas del nivel actual
; Dominio: Sistema
; Recorrido: Lista
(define carpetas_nivel (lambda (sistema)
                         (filter (lambda (elemento) (string=? (cadr (list-ref sistema 3)) (cadr (caddr elemento)))) (list-ref sistema 4))))

; Crea lista con los archivos del nivel actual
; Dominio: Sistema
; Recorrido: Lista
(define archivos_nivel (lambda (sistema)
                         (filter (lambda (elemento) (string=? (cadr (list-ref sistema 3)) (cadr (caddr elemento)))) (list-ref sistema 5))))

; Dado el nombre de una carpeta "x", crea una lista de "n" carpetas con todos los "m" subdirectorios borrados que están dentro de la carpeta "x"
; Dominio: Sistema X comando (nombre de la carpeta a borrar)
; Recorrido: Lista
(define subdir_borrados (lambda (sistema comando)
                         (filter (lambda (elemento) (not (string-contains? (cadr (caddr elemento)) comando))) (list-ref sistema 4))))

; Crea una lista con los "m" subdirectorios borrados dentro de una carpeta "x" que fue borrada
; Dominio: Sistema X comando (nombre de la carpeta a borrar)
; Recorrido: Lista
(define subdir_borrar (lambda (sistema comando)
                        (filter (lambda (elemento) (string-contains? (cadr (caddr elemento)) comando)) (list-ref sistema 4))))

; Dado el nombre de una carpeta "x", crea una lista de "n" archivos con todos los "m" sub-archivos borrados que están dentro de la carpeta "x"
; Dominio: Sistema X comando (nombre de la carpeta a borrar)
; Recorrido: Lista
(define archivos_borrados (lambda (sistema comando)
                            (filter (lambda (elemento) (not (string-contains? (cadr (caddr elemento)) comando))) (list-ref sistema 5))))

; Crea una lista con los "m" archivos borrados dentro de una carpeta "x" que fue borrada
; Dominio: Sistema X comando (nombre de la carpeta a borrar)
; Recorrido: Lista
(define archivos_borrar (lambda (sistema comando)
                          (filter (lambda (elemento) (string-contains? (cadr (caddr elemento)) comando)) (list-ref sistema 5))))

(provide carpetas_nivel archivos_nivel subdir_borrados subdir_borrar archivos_borrados archivos_borrar)