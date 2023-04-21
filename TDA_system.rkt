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
                                (sistema_folders sistema)
                                (sistema_files sistema)))))) ; Se mantienen estos espacios para próximas funciones

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
                               (sistema_folders sistema)
                               (sistema_files sistema))))))

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
                            (sistema_folders sistema)
                            (sistema_files sistema))
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
                           (sistema_folders sistema)
                           (sistema_files sistema)))))

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
                                   (sistema_folders sistema)
                                   (sistema_files sistema)) ; crea una lista añadiendo el path actual al usuario logueado
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
                     (append (take sistema 4) (list archivo_esp) (list (sistema_files sistema)))) ; Para cuando se agrega el primer archivo del sistema, no habrá problemas
                     (if (and (member nombre (map car (sistema_folders sistema))) (eq? (cadr (list-ref archivo 2)) (cadr (list-ref sistema 3)))) ; si el nombre existe en el mismo nivel (path actual) no se crea la carpeta
                         sistema
                         (list (sistema_nombre sistema)
                               (sistema_drives sistema)
                               (sistema_usuarios sistema)
                               (sistema_log sistema)
                               (append (sistema_folders sistema) (list archivo)) ; añade la carpeta a la lista de carpetas
                               (sistema_files sistema))))))))

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
                         (sistema_folders sistema)
                         (sistema_files sistema))
                   (if (eq? comando "..")
                       (if (string=? (caddr (sistema_nombre sistema)) (cadr (sistema_log sistema))) 
                           sistema ; si el drive actual es igual al path actual, no se puede devolver más por lo que no se hace nada
                           (list (sistema_nombre sistema)
                                 (sistema_drives sistema)
                                 (sistema_usuarios sistema)
                                 (list (car (sistema_log sistema))
                                       (string-append (string-join (reverse (cdr (reverse (string-split (cadr (sistema_log sistema)) "/")))) "/") "/")) ; como al separarlo se crea un espacio vacío "", se borra al hacerle un reverse y cdr. luego se aplica otro reverse y se junta todo
                                 (sistema_folders sistema)
                                 (sistema_files sistema)))
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
                                         (sistema_folders sistema)
                                         (sistema_files sistema)))
                                 sistema))))))))) ; si no estaba el nombre, no se puede acceder a una carpeta inexistente, por lo que no se hace nada

; Función add-file
; Agrega un archivo al sistema (dentro de la lista de su 6to elemento) representado como una lista, poseyendo nombre,  fecha creación, usuario y ruta, formato, contenido, y posibles atributos de seguridad
; Dominio: sistema X archivo (lista)
; Recorrido: sistema
(define add-file (lambda (sistema)
                   (lambda (archivo)
                     (let ((datos (append (take archivo 1) (list fecha) (list (sistema_log sistema)) (cdr archivo)))) ; primero toma el nombre del archivo, luego la fecha, usuario y path de creación, y luego añade los datos de formato, contenido, y atributos de seguridad
                       (if (eq? (sistema_files sistema) null) ; este es el caso donde no se han agregado archivos anteriormente
                           (list (sistema_nombre sistema)
                                 (sistema_drives sistema)
                                 (sistema_usuarios sistema)
                                 (sistema_log sistema)
                                 (sistema_folders sistema)
                                 (append (sistema_files sistema) (list datos)))
                           (if (member (car datos) (map car (sistema_files sistema))) ; este caso se cumple si se han agregado archivos anteriormente, si es así, revisa si no se ha creado un archivo con el mismo nombre
                               sistema
                               (list (sistema_nombre sistema)
                                     (sistema_drives sistema)
                                     (sistema_usuarios sistema)
                                     (sistema_log sistema)
                                     (sistema_folders sistema)
                                     (append (sistema_files sistema) (list datos))))))))) ; si no hay otro archivo existente con el mismo nombre y en el mismo path, añade dicho archivo al sistema
                       
; en la llamada a la función se ha implementado "file" definiéndola como una lista

; Función delete
; Permite eliminar un archivo SOLAMENTE SI DICHO ARCHIVO EXISTE. Se debe ingresar el nombre del archivo junto a su extensión (implementación simple)
; Dominio: Sistema X comando (string del nombre del archivo)
; Recorrido: Sistema
(define del (lambda (sistema)
              (lambda (comando)
                (if (member comando (map car (sistema_files sistema))) ; revisa si el archivo existe
                    (let ((new_list_files (filter (lambda (elemento) (not (eq? (car elemento) comando))) (sistema_files sistema)))) ; crea una lista sin el archivo a eliminar
                      (list (sistema_nombre sistema)
                            (sistema_drives sistema)
                            (sistema_usuarios sistema)
                            (sistema_log sistema)
                            (sistema_folders sistema)
                            new_list_files)) ; agrega la lista de archivos sin el que se quiere eliminar
                    (if (member comando (map car (sistema_folders sistema)))
                        (let ((list_folders_elim (filter (lambda (elemento) (not (string-contains? (cadr (caddr elemento)) comando))) (sistema_folders sistema)))) ; crea lista con las carpetas que no estén dentro de la carpeta a eliminar
                          (let ((list_files_elim_f (filter (lambda (elemento) (not (string-contains? (cadr (caddr elemento)) comando))) (sistema_files sistema)))) ; crea lista con los archivos que no estén dentro de la carpeta a eliminar
                            (let ((list_folders_elim_f (filter (lambda (elemento) (not (eq? (car elemento) comando))) list_folders_elim))) ; a la lista con las subcarpetas eliminadas se le elimina la carpeta principal a eliminar
                              (list (sistema_nombre sistema)
                                    (sistema_drives sistema)
                                    (sistema_usuarios sistema)
                                    (sistema_log sistema)
                                    list_folders_elim_f ; se reemplaza por una lista sin la carpeta eliminada y sus subcarpetas
                                    list_files_elim_f)))) ; se reemplaza por una lista sin los archivos de la carpeta eliminada
                        sistema)))))

; Función remove directory
; Elimina una carpeta (directorio) siempre y cuando este se encuentre vacío (no posee archivos NI otras carpetas dentro)
; Dominio: Sistema X nombre (o path)
; Recorrido: Sistema
(define rd (lambda (sistema)
             (lambda (nombre)
               (if (and (and (member nombre (map car (sistema_folders sistema))) ; revisa si el archivo existe
                             (eq? null (filter (lambda (elemento) (string-contains? (cadr (caddr elemento)) nombre)) (sistema_files sistema)))) ; se crea una lista con los archivos dentro de la carpeta a eliminar. si es nula se procede, sino, no se elimina la carpeta
                        (eq? null (filter (lambda (elemento) (string-contains? (cadr (caddr elemento)) nombre)) (sistema_folders sistema)))) ; al igual que lo anterior, crea una lista con los directorios dentro de la carpeta a eliminar. sigue las mismas condiciones que el anterior
                   (let ((new_list (filter (lambda (elemento) (not (eq? (car elemento) nombre))) (sistema_folders sistema)))) ; crea una lista de directorios sin el directorio a eliminar
                     (list (sistema_nombre sistema)
                           (sistema_drives sistema)
                           (sistema_usuarios sistema)
                           (sistema_log sistema)
                           new_list ; agrega la lista de carpetas sin la que se quiere eliminar
                           (sistema_files sistema)))
                     sistema))))

; Función copy
; Permite copiar un archivo o carpeta, y pegarlo en una dirección destino. Como no se especifica en el enunciado del TDA, esta función funciona SI Y SOLO SI SE ENCUENTRA EN LA MISMA DIRECCIÓN DEL ARCHIVO A COPIAR
; ej: si quiere copiar un archivo "ex1.txt" que está en la dirección "C:/", usted tambien debe estar ubicado en la dirección "C:/"
; aclaración: se pide que al ingresar la ruta destino se escriba el drive en mayúsculas, ya que en el script sale en minúsculas.
; Dominio: Sistema X nombre del dato X dirección de destino
; Recorrido: Sistema
(define copy (lambda (sistema)
               (lambda (dato destino)
                 (let ((carpeta (filter (lambda (elemento) (and (string=? dato (car elemento)) (string=? (cadr (sistema_log sistema)) (cadr (caddr elemento))))) (sistema_folders sistema)))) ; genera una lista con la carpeta a copiar. si no existe, entrega una lista vacía (null)        
                   (let ((archivo (filter (lambda (elemento) (and (string=? dato (car elemento)) (eq? (cadr (sistema_log sistema)) (cadr (caddr elemento))))) (sistema_files sistema))))  ; genera una lista con el archivo a copiar. si no existe, entrega una lista vacía (null)
                     (if (eq? carpeta null) ; pregunta si la carpeta a copiar existe
                         (if (eq? archivo null) ; pregunta si el archivo a copiar existe
                             sistema ; si no se ha copiado nada por no existir (en la ruta actualmente visible) no se modifica nada
                             (let ((string_mod (string-append destino (string-join (cdr (string-split (cadr (caddr (car archivo))) "/")) "/") "/"))) ; si se ha copiado un archivo, guarda la ruta destino
                               (let ((nuevo_archivo (list (car (car archivo)) (cadr (car archivo)) (list (car (caddr (car archivo))) string_mod) (cdr (cdddr (car archivo)))))) ; aplica la ruta actualizada al archivo
                               (list (sistema_nombre sistema)
                                     (sistema_drives sistema)
                                     (sistema_usuarios sistema)
                                     (sistema_log sistema)
                                     (sistema_folders sistema)
                                     (append (sistema_files sistema) (list nuevo_archivo)))))) ; añade el archivo ya pegado en la dirección destino
                         (let ((carpetas_int (filter (lambda (elemento) (string-contains? (cadr (caddr elemento)) (string-append (cadr (caddr (car carpeta))) dato "/"))) (sistema_folders sistema)))) ; si se ha copiado una carpeta, crea una lista con todas las carpetas que esta contiene              
                           (let ((archivos_int (filter (lambda (elemento) (string-contains? (cadr (caddr elemento)) (string-append (cadr (caddr (car carpeta))) dato "/"))) (sistema_files sistema)))) ; crea una lista con todos los archivos que la carpeta copiada contiene
                             (let ((carpetas_copy (map (lambda (folder) (list (car folder) (cadr folder) (list (car (caddr folder)) (string-append destino (string-join (cdr (string-split (cadr (caddr folder))))))) (cdr (cdddr folder))))carpetas_int))) ; a la copia de carpetas, le actualiza la dirección a la dirección destino
                               (let ((archivos_copy (map (lambda (file) (list (car file) (cadr file) (list (car (caddr file)) (string-append destino (string-join (cdr (string-split (cadr (caddr file))))))) (cdr (cdddr file))))archivos_int))) ; a la copia de archivos, le actualiza la dirección a la dirección destino
                                 (let ((carpeta_copia_principal (list (car (car carpeta)) (cadr (car carpeta)) (list (car (caddr (car carpeta))) (string-append destino (string-join (cdr (string-split (cadr (caddr (car carpeta)))))))) (cdr (cdddr (car carpeta)))))) ; actualiza la dirección a la copia de la carpeta que se quiere copiar (la que se ha dado como entrada)
                                 (list (sistema_nombre sistema)
                                       (sistema_drives sistema)
                                       (sistema_usuarios sistema)
                                       (sistema_log sistema)
                                       (append (sistema_folders sistema) (list carpeta_copia_principal) carpetas_copy) ; añade la carpeta copiada junto a sus carpetas contenidas con su nueva dirección
                                       (append (sistema_files sistema) archivos_copy))))))))))))) ; añade los archivos que estaban contenidos dentro de la carpeta copiada con su nueva dirección



                  
                     
               


         
         
         
  

















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
                     
(define S32 ((run S29 add-file) (file "foo1.txt" "txt" "hello world 1"))) ; add file
(define S33 ((run S32 add-file) (file "foo2.txt" "txt" "hello world 2")))
(define S34 ((run S33 add-file) (file "foo3.docx" "docx" "hello world 3")))
(define S35 ((run S34 add-file) (file "goo4.docx" "docx" "hello world 4" #\h #\r)))

(define S38 ((run S35 del) "goo4.docx")) ; del
(define S39 ((run S35 cd) ".."))
(define S40 ((run S35 del) "folder1"))

(define S41 ((run S38 rd) "folder1")) ; remove directory

(define S46 ((run S41 copy) "foo1.txt" "C:/folder3/")) ; copy