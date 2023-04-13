#lang scheme

(require "TDA_fecha.rkt")
(require "TDA_selectorSistema.rkt")

; Función sistema
; Constructor
; Crea una lista para simular un sistema, conteniendo el nombre, sus drives, sus usuarios y el usuario logueado
; Dominio: nombre(string)
; Recorrido: sistema(lista)
(define system (lambda (nombre)
                 (list (list nombre fecha) null null null))) ; deja registro de la fecha junto al nombre del sistema
; ¡NO SEGURO SI LA FECHA SE REGISTRARÁ ASÍ EN SU FORMA FINAL!

; Función run
; Permite que una función se ejecute, como lo puede ser add-drive, register, etc.
; Dominio: sistema X función
; Recorrido: sistema (con la función ya aplicada)
(define run (lambda (sistema funcion)
              (funcion sistema)))

; Función add-drive
; Añade una unidad (ej: #/C) al sistema (2da posición) siempre que no se haya agregado una anteriormente con la misma letra
; Dominio: sistema X (letra, nombre y capacidad de la unidad)
; Recorrido: sistema
(define add-drive (lambda (sistema)  
                    (lambda (letra nombre capacidad)
                      (if (member letra (map car (sistema_drives sistema))) ; Se busca la letra del drive mediante member, si member retorna un valor distinto a null, no se aplica la adición del drive
                          sistema
                          (list (sistema_nombre sistema)  ; Toma simplemente el nombre del sistema
                                (cons (list letra nombre capacidad)(sistema_drives sistema))  ; Añade en la 2da posición los componentes del drive
                                (sistema_usuarios sistema) 
                                (sistema_log sistema))))))

; Función register
; añade un usuario al sistema, siempre que no se haya creado con anterioridad
; Dominio: sistema X nombre del usuario
; Recorrido: sistema
(define register (lambda (sistema) 
                   (lambda (nombre)
                     (if (member nombre (sistema_usuarios sistema)) ; Evita guardar dos usuarios con el mismo nombre
                         sistema
                         (list (sistema_nombre sistema)
                               (sistema_drives sistema) ; mantiene los drives añadidos anteriormente
                               (cons nombre (sistema_usuarios sistema)) ; agrega el nombre de usuario al sistema (3ra posición)
                               (sistema_log sistema))))))

; Función login
; Permite iniciar sesión a un usuario existente
; Dominio: sistema X usuario
; Recorrido: sistema
(define login (lambda (sistema) ; se tomará la 4ta posición de sistema para constatar si hay un usuario logueado o no
                (lambda (user)
                  (if (eq? null (sistema_log sistema)) ; si está vacío, añade el nombre del usuario logueado
                      (list (sistema_nombre sistema)
                            (sistema_drives sistema)
                            (sistema_usuarios sistema)
                             user)
                      sistema)))) ; si está un usuario logueado, no hace nada

; Función logout
; Permite salir de un usuario actualmente logueado
; Dominio: Sistema
; Recorrido: Sistema
(define logout (lambda (sistema)
                 (if (eq? (sistema_log sistema) null) ; si no hay ningún usuario logueado, no hace nada
                     sistema
                     (list (sistema_nombre sistema) ; si hay un usuario logueado, borra la 4ta posición la cual indica qué usuario está logueado
                           (sistema_drives sistema)
                           (sistema_usuarios sistema)
                           null))))

; Función switch-drive
; Cambia al drive en el cual se desea trabajar siempre y cuando este exista, y haya un usuario actualmente logueado
; Dominio: sistema X drive (letra)
; Recorrido: sistema
(define switch-drive (lambda (sistema) 
                       (lambda (letra)
                         (if (and (member letra (map car (sistema_drives sistema))) (not (eq? null (sistema_log sistema)))) ; Dirige al drive si y solo si hay usuario logueado y el drive existe
                             (list (sistema_nombre sistema)
                                   (sistema_drives sistema) 
                                   (sistema_usuarios sistema)
                                   (list (sistema_log sistema)
                                         (string-append (string-append (string letra) ":") "/"))) ; crea una lista añadiendo el path al usuario logueado
                             sistema)))) ; si no se cumplen las condiciones devuelve el sistema de entrada

; Funcion make directory
; Crea una carpeta en el path actual, la carpeta tiene información como su nombre, fecha creación, usuario quien la creo, path y atributo de seguridad (lo crea como vacío)
(define md (lambda (sistema) 
             (lambda (nombre)
               (let ((archivo (list nombre fecha (sistema_log sistema) null)))
                 (if (and (member nombre (map car sistema)) (eq? (cadr (list-ref archivo 2)) (cadr (list-ref sistema 3)))) ; si el nombre existe en el mismo nivel (path actual) no se crea la carpeta
                     sistema
                     (append sistema (list archivo)))))))




         
         
         
  

















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

(define S13 ((run S12 md) "folder1")) ; make directory
(define S14 ((run S13 md) "folder2"))
(define S15 ((run S14 md) "folder2"))