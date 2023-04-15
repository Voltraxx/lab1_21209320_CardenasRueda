#lang scheme

(require "TDA_fecha.rkt")
(require "TDA_selectorSistema.rkt")


; Función sistema
; Constructor
; Crea una lista para simular un sistema, conteniendo el nombre, sus drives, sus usuarios y el usuario logueado
; Dominio: nombre(string)
; Recorrido: sistema(lista)
(define system (lambda (nombre)
                 (list (list nombre fecha) null null (list null null) null null null null))) ; deja registro de la fecha junto al nombre del sistema
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
                                (sistema_log sistema)
                                (sistema_folders sistema)))))) ; Se mantienen estos espacios para próximas funciones

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
                               (sistema_log sistema)
                               (sistema_folders sistema))))))

; Función login
; Permite iniciar sesión a un usuario existente
; Dominio: sistema X usuario
; Recorrido: sistema
(define login (lambda (sistema) ; se tomará la 4ta posición de sistema para constatar si hay un usuario logueado o no
                (lambda (user)
                  (if (eq? null (car (sistema_log sistema))) ; si está vacío, añade el nombre del usuario logueado
                      (list (sistema_nombre sistema)
                            (sistema_drives sistema)
                            (sistema_usuarios sistema)
                            (list user null)
                            (sistema_folders sistema))
                      sistema)))) ; si está un usuario logueado, no hace nada

; Función logout
; Permite salir de un usuario actualmente logueado
; Dominio: Sistema
; Recorrido: Sistema
(define logout (lambda (sistema)
                 (if (eq? (sistema_log sistema) null) ; si no hay ningún usuario logueado, no hace nada
                     sistema
                     (list (sistema_nombre sistema) ; si hay un usuario logueado, borra el primer elemento en la 4ta posición la cual indica qué usuario está logueado
                           (sistema_drives sistema)
                           (sistema_usuarios sistema)
                           (list null (cadr (sistema_log sistema)))
                           (sistema_folders sistema)))))

; Función switch-drive
; Cambia al drive en el cual se desea trabajar siempre y cuando este exista, y haya un usuario actualmente logueado
; Dominio: sistema X drive (letra)
; Recorrido: sistema
(define switch-drive (lambda (sistema) 
                       (lambda (letra)
                         (if (and (member letra (map car (sistema_drives sistema))) (not (eq? null (car (sistema_log sistema))))) ; Dirige al drive si y solo si hay usuario logueado y el drive existe
                             (list (list (car (sistema_nombre sistema)) (cadr (sistema_nombre sistema)) (string-append (string letra) ":/")) ; guarda la letra del drive que se está trabajando
                                   (sistema_drives sistema) 
                                   (sistema_usuarios sistema)
                                   (list (car (sistema_log sistema))
                                             (string-append (string-append (string letra) ":") "/"))
                                   (sistema_folders sistema)) ; crea una lista añadiendo el path actual al usuario logueado
                             sistema)))) ; si no se cumplen las condiciones devuelve el sistema de entrada

; Función make directory
; Crea una carpeta en el path actual, la carpeta tiene información como su nombre, fecha creación, usuario quien la creo, path y atributo de seguridad (lo crea como vacío)
; Dominio: sistema X nombre del directorio
; Recorrido: sistema
(define md (lambda (sistema) 
             (lambda (nombre)
               (let ((archivo (list nombre fecha (sistema_log sistema) null))) ; archivo contiene el nombre, fecha de creación, el usuario y la ruta en donde fue creado, y un null para atributos de seguridad
                 (if (eq? (list-ref sistema 4) null)
                     (let ((archivo_esp (cons (list nombre fecha (sistema_log sistema) null) '())))
                     (append (take sistema 4) (list archivo_esp))) ; Para cuando se agrega el primer archivo del sistema, no habrá problemas
                     (if (and (member nombre (map car (list-ref sistema 4))) (eq? (cadr (list-ref archivo 2)) (cadr (list-ref sistema 3)))) ; si el nombre existe en el mismo nivel (path actual) no se crea la carpeta
                         sistema
                         (list (sistema_nombre sistema)
                               (sistema_drives sistema)
                               (sistema_usuarios sistema)
                               (sistema_log sistema)
                               (append (sistema_folders sistema) (list archivo)))))))))

; Función change directory
; Permite ubicarse dentro del drive en las distintas carpetas. "/" ubica en la base del drive, ".." retrocede una ubicación y en caso de cualquier otro string,
; solo irá a esa ubicación si dicha carpeta existe INMEDIATAMENTE VISIBLE, no se ha implementado una forma en la cual se pueda acceder de forma Ex: ("folder1/folder12")
; Dominio: sistema X comando ("/", "..", "folder" [existente])
; Recorrido: sistema
(define cd (lambda (sistema)
             (lambda (comando)
               (if (eq? comando "/")
                   (list (sistema_nombre sistema)
                         (sistema_drives sistema)
                         (sistema_usuarios sistema)
                         (list (car (sistema_log sistema)) (caddr (sistema_nombre sistema))) ; como la 3ra posición en sistema_nombre es el drive actual, se reemplaza el path actual por este, volviendo a la raíz
                         (sistema_folders sistema))
                   (if (eq? comando "..")
                       (if (string=? (caddr (sistema_nombre sistema)) (cadr (sistema_log sistema))) 
                           sistema ; si el drive actual es igual al path actual, no se puede devolver más por lo que no se hace nada
                           (list (sistema_nombre sistema)
                                 (sistema_drives sistema)
                                 (sistema_usuarios sistema)
                                 (list (car (sistema_log sistema))
                                       (string-append (string-join (reverse (cdr (reverse (string-split (cadr (sistema_log sistema)) "/")))) "/") "/")) ; como al separarlo se crea un espacio vacío "", se borra al hacerle un reverse y cdr. luego se aplica otro reverse y se junta todo
                                 (sistema_folders sistema)))
                       (if (not (member (cadr (sistema_log sistema)) (map cadr (map caddr (sistema_folders sistema)))))
                           sistema ; si no existe un folder con la ruta actual, no se puede hacer nada por lo que se devuelve el mismo sistema de entrada
                           (let ((filtro (filter (lambda (sublist) (string=? (cadr (sistema_log sistema)) (cadr (caddr sublist))))(sistema_folders sistema)))) ; crea una lista con los folder que estén actualmente visibles
                             (let ((nombres (map car filtro))) ; crea una lista con los nombres de los folder actualmente visibles
                               (if (member comando nombres)
                                 (begin ; si el nombre está registrado en nombres, puede acceder al archivo ya que existe
                                   (list (sistema_nombre sistema)
                                         (sistema_drives sistema)
                                         (sistema_usuarios sistema)
                                         (list (car (sistema_log sistema))
                                               (string-append (cadr (sistema_log sistema)) (string-append comando "/"))) ; actualiza el path al entrar a una carpeta
                                         (sistema_folders sistema)))
                                 sistema))))))))) ; si no estaba el nombre, no se puede acceder a una carpeta inexistente, por lo que no se hace nada
                       
                       

                   
                   


                   
                   
                  
                     
               


         
         
         
  

















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
(define S16 ((run S15 md) "folder3"))

(define S17 ((run S16 cd) "folder2")) ; change directory

(define S18 ((run S17 md) "folder21")) ; distintas combinaciones entre cd y md
(define S19 ((run S18 md) "folder21"))
(define S20 ((run S19 cd) "folder21"))
(define S21 ((run S20 cd) "folder22"))
(define S22 ((run S21 cd) ".."))
(define S23 ((run S22 cd) "folder21"))
(define S24 ((run S23 md) "folder211"))
(define S25 ((run S24 cd) "folder211"))
(define S26 ((run S24 cd) "/"))
(define S27 ((run S26 switch-drive) #\D))
(define S28 ((run S27 md) "folder5"))
(define S29 ((run S28 cd) "folder5"))