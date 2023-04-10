#lang scheme

(define system (lambda (nombre)      ; Crea una lista del sistema
          (list nombre null null null null null null)))

(define run (lambda (sistema funcion) ; Hace que la función que se pone como 2do valor de entrada se aplique al sistema
              (funcion sistema)))

(define add-drive (lambda (sistema)  ; Añade un drive al sistema dado como entrada
                    (lambda (letra nombre capacidad)
                      (if (member letra (map car (cadr sistema))) ; Se busca la letra del drive mediante member, si member retorna un valor distinto a null, no se aplica la adición del drive
                          sistema
                          (list (car sistema)  ; Toma simplemente el nombre del sistema
                                (cons (list letra nombre capacidad)(cadr sistema))  ; Añade en la 2da posición los componentes del drive
                                (caddr sistema)))))) ; Si el sistema no ha añadido nada, lista vacía, si ya ha sido agregado algo en la 3ra posición, se mantiene.

(define register (lambda (sistema) ; añade un usuario al sistema
                   (lambda (nombre)
                     (if (member nombre (caddr sistema)) ; Evita guardar dos usuarios con el mismo nombre
                         sistema
                         (list (car sistema)
                               (cadr sistema) ; mantiene los drives añadidos anteriormente
                               (cons nombre (caddr sistema))))))) ; agrega el nonbre de usuario al sistema (3ra posición)


                  





; ¡No se han implementado los TDA tal cual (en vez de llamar a "car", llamar a "drive-sistema" o algo por el estilo)!


















; CÓDIGOS DE PRUEBA
(define S0 (system "newSystem")) ; sistema

(define S1 ((run S0 add-drive) #\C "SO" 1000)) ; add drive
(define S2 ((run S1 add-drive) #\C "SO1" 3000))
(define S3 ((run S2 add-drive) #\D "Util" 2000))

(define S4 ((run S3 register) "user1")) ; register users
(define S5 ((run S4 register) "user1"))
(define S6 ((run S5 register) "user2"))
