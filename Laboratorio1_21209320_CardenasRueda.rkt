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
                                (caddr sistema) 
                                (cadddr sistema))))))

(define register (lambda (sistema) ; añade un usuario al sistema
                   (lambda (nombre)
                     (if (member nombre (caddr sistema)) ; Evita guardar dos usuarios con el mismo nombre
                         sistema
                         (list (car sistema)
                               (cadr sistema) ; mantiene los drives añadidos anteriormente
                               (cons nombre (caddr sistema)) ; agrega el nombre de usuario al sistema (3ra posición)
                               (cadddr sistema)))))) 

(define login (lambda (sistema) ; se tomará la 4ta posición de sistema para constatar si hay un usuario logueado o no
                (lambda (user)
                  (if (eq? null (cadddr sistema)) ; si está vacío, añade el nombre del usuario logueado
                      (list (car sistema)
                            (cadr sistema)
                            (caddr sistema)
                             user)
                      sistema)))) ; si está un usuario logueado, no hace nada

(define logout (lambda (sistema)
                 (if (eq? (cadddr sistema) null) ; si no hay ningún usuario logueado, no hace nada
                     sistema
                     (list (car sistema) ; si hay un usuario logueado, borra la 4ta posición la cual indica qué usuario está logueado
                           (cadr sistema)
                           (caddr sistema)
                           null))))
                         
(define switch-drive (lambda (sistema) 
                       (lambda (letra)
                         (if (and (member letra (map car (cadr sistema))) (not (eq? null (cadddr sistema)))) ; Dirige al drive si y solo si hay usuario logueado y el drive existe
                             (list (car sistema)
                                   letra
                                   (cadddr sistema))
                             sistema)))) ; si no se cumplen las condiciones devuelve el sistema de entrada
                             
                      
                
                  





; ¡No se han implementado los TDA tal cual (en vez de llamar a "car", llamar a "drive-sistema" o algo por el estilo)!


















; CÓDIGOS DE PRUEBA
(define S0 (system "newSystem")) ; sistema

(define S1 ((run S0 add-drive) #\C "SO" 1000)) ; add drive
(define S2 ((run S1 add-drive) #\C "SO1" 3000))
(define S3 ((run S2 add-drive) #\D "Util" 2000))

(define S4 ((run S3 register) "user1")) ; register users
(define S5 ((run S4 register) "user1"))
(define S6 ((run S5 register) "user2"))

(define S7 ((run S6 login) "user1")) ; login
(define S8 ((run S7 login) "user2"))

(define S9 (run S8 logout)) ; logout
(define S10 ((run S9 login) "user2"))

(define S11 ((run S10 switch-drive) #\K)) ; fijar drive
(define S12 ((run S11 switch-drive) #\C))
