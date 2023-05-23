#lang scheme

(require "TDAfecha_21209320_CardenasRueda")
(require "TDAselectorSistema_21209320_CardenasRueda")
(require "TDAselectorDatos_21209320_CardenasRueda")

; Función sistema
; Constructor
; Crea una lista para simular un sistema, conteniendo el nombre, sus drives, sus usuarios y el usuario logueado
; Dominio: nombre(string)
; Recorrido: sistema(lista)
(define system (lambda (nombre)
                 (list (list nombre fecha) null null (list null null) null null null null))) ; deja registro de la fecha junto al nombre del sistema

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
                                (sistema_files sistema)
                                (sistema_papeleraC sistema)
                                (sistema_papeleraA sistema)))))) ; Se mantienen estos espacios para próximas funciones

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
                               (sistema_files sistema)
                               (sistema_papeleraC sistema)
                               (sistema_papeleraA sistema))))))

; Función login
; Permite iniciar sesión a un usuario existente
; Dominio: sistema X usuario
; Recorrido: sistema
(define login (lambda (sistema) ; se tomará la 4ta posición de sistema para constatar si hay un usuario logueado o no
                (lambda (user)
                  (if (and (eq? null (car (sistema_log sistema))) (member user (sistema_usuarios sistema))) ; si está vacío y existe el usuario, añade el nombre del usuario logueado
                      (list (sistema_nombre sistema)
                            (sistema_drives sistema)
                            (sistema_usuarios sistema)
                            (list user null)
                            (sistema_folders sistema)
                            (sistema_files sistema)
                            (sistema_papeleraC sistema)
                            (sistema_papeleraA sistema))
                      sistema)))) ; si está un usuario logueado o no existe el usuario, no hace nada

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
                           (sistema_files sistema)
                           (sistema_papeleraC sistema)
                           (sistema_papeleraA sistema)))))

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
                                   (sistema_files sistema) ; crea una lista añadiendo el path actual al usuario logueado
                                   (sistema_papeleraC sistema)
                                   (sistema_papeleraA sistema))
                             sistema)))) ; si no se cumplen las condiciones devuelve el sistema de entrada

; Función make directory
; Crea una carpeta en el path actual, la carpeta tiene información como su nombre, fecha creación, usuario quien la creo, path y atributo de seguridad (lo crea como vacío)
; Dominio: sistema X nombre del directorio
; Recorrido: sistema
(define md (lambda (sistema) 
             (lambda (nombre)
               (let ((archivo (list nombre fecha (sistema_log sistema) null))) ; archivo contiene el nombre, fecha de creación, el usuario y la ruta en donde fue creado, y un null para atributos de seguridad
                 (if (eq? (sistema_folders sistema) null)
                     (let ((archivo_esp (cons (list nombre fecha (sistema_log sistema) null) '())))
                       (append (take sistema 4) (list archivo_esp) (list (sistema_files sistema)) (list (sistema_papeleraC sistema)) (list (sistema_papeleraA sistema)))) ; Para cuando se agrega el primer archivo del sistema, no habrá problemas
                     (if (member nombre (map car (carpetas_nivel sistema))) ; si el nombre existe en el mismo nivel (path actual) no se crea la carpeta
                         sistema
                         (list (sistema_nombre sistema)
                               (sistema_drives sistema)
                               (sistema_usuarios sistema)
                               (sistema_log sistema)
                               (append (sistema_folders sistema) (list archivo)) ; añade la carpeta a la lista de carpetas
                               (sistema_files sistema)
                               (sistema_papeleraC sistema)
                               (sistema_papeleraA sistema))))))))

; Función change directory
; Permite ubicarse dentro del drive en las distintas carpetas. "/" ubica en la base del drive, ".." retrocede una ubicación y en caso de cualquier otro string,
; solo irá a esa ubicación si dicha carpeta existe INMEDIATAMENTE VISIBLE, no se ha implementado una forma en la cual se pueda acceder de forma Ex: ("folder1/folder12")
; Dominio: sistema X comando ("/", "..", "folder" [existente])
; Recorrido: sistema
(define cd (lambda (sistema)
             (lambda (comando)
               (if (string=? comando "/")
                   (list (sistema_nombre sistema)
                         (sistema_drives sistema)
                         (sistema_usuarios sistema)
                         (list (car (sistema_log sistema)) (caddr (sistema_nombre sistema))) ; como la 3ra posición en sistema_nombre es el drive actual, se reemplaza el path actual por este, volviendo a la raíz
                         (sistema_folders sistema)
                         (sistema_files sistema)
                         (sistema_papeleraC sistema)
                         (sistema_papeleraA sistema))
                   (if (string=? comando "..")
                       (if (string=? (caddr (sistema_nombre sistema)) (cadr (sistema_log sistema))) 
                           sistema ; si el drive actual es igual al path actual, no se puede devolver más por lo que no se hace nada
                           (list (sistema_nombre sistema)
                                 (sistema_drives sistema)
                                 (sistema_usuarios sistema)
                                 (list (car (sistema_log sistema))
                                       (string-append (string-join (reverse (cdr (reverse (string-split (cadr (sistema_log sistema)) "/")))) "/") "/")) ; como al separarlo se crea un espacio vacío "", se borra al hacerle un reverse y cdr. luego se aplica otro reverse y se junta todo
                                 (sistema_folders sistema)
                                 (sistema_files sistema)
                                 (sistema_papeleraC sistema)
                                 (sistema_papeleraA sistema)))
                       (if (not (member (cadr (sistema_log sistema)) (map cadr (map caddr (sistema_folders sistema)))))
                           sistema ; si no existe un folder con la ruta actual, no se puede hacer nada por lo que se devuelve el mismo sistema de entrada
                             (let ((nombres (map car (carpetas_nivel sistema)))) ; crea una lista con los nombres de los folder actualmente visibles
                               (if (member comando nombres)
                                 (begin ; si el nombre está registrado en nombres, puede acceder al archivo ya que existe
                                   (list (sistema_nombre sistema)
                                         (sistema_drives sistema)
                                         (sistema_usuarios sistema)
                                         (list (car (sistema_log sistema))
                                               (string-append (cadr (sistema_log sistema)) (string-append comando "/"))) ; actualiza el path al entrar a una carpeta
                                         (sistema_folders sistema)
                                         (sistema_files sistema)
                                         (sistema_papeleraC sistema)
                                         (sistema_papeleraA sistema)))
                                 sistema)))))))) ; si no estaba el nombre, no se puede acceder a una carpeta inexistente, por lo que no se hace nada

; Función add-file
; Agrega un archivo al sistema (dentro de la lista de su 6to elemento) representado como una lista, poseyendo nombre,  fecha creación, usuario y ruta, formato, contenido, y posibles atributos de seguridad
; Dominio: sistema X archivo (lista)
; Recorrido: sistema
(define add-file (lambda (sistema)
                   (lambda (archivo)
                     (let ((datos (append (take archivo 1) (list fecha) (list (sistema_log sistema)) (cdr archivo)))) ; primero toma el nombre del archivo, luego le agrega la fecha, usuario y path de creación, y luego añade los datos de formato, contenido, y atributos de seguridad
                       (if (eq? (sistema_files sistema) null) ; este es el caso donde no se han agregado archivos anteriormente
                           (list (sistema_nombre sistema)
                                 (sistema_drives sistema)
                                 (sistema_usuarios sistema)
                                 (sistema_log sistema)
                                 (sistema_folders sistema)
                                 (append (sistema_files sistema) (list datos))
                                 (sistema_papeleraC sistema)
                                 (sistema_papeleraA sistema))
                           (if (member (car datos) (map car (archivos_nivel sistema))) ; este caso se cumple si se han agregado archivos anteriormente, si es así, revisa si no se ha creado un archivo con el mismo nombre en el mismo nivel
                               sistema
                               (list (sistema_nombre sistema)
                                     (sistema_drives sistema)
                                     (sistema_usuarios sistema)
                                     (sistema_log sistema)
                                     (sistema_folders sistema)
                                     (append (sistema_files sistema) (list datos)) ; si no hay otro archivo existente con el mismo nombre y en el mismo path, añade dicho archivo al sistema
                                     (sistema_papeleraC sistema)
                                     (sistema_papeleraA sistema))))))))
                       
; en la llamada a la función se ha implementado "file" definiéndola como una lista

; Función delete
; Permite eliminar un archivo (o carpeta) SOLAMENTE SI DICHO ARCHIVO EXISTE. Se debe ingresar el nombre del archivo junto a su extensión (implementación simple)
; Además, como no se especifica, esta función elimina todos los archivos/carpetas con dicho nombre en el sistema
; Dominio: Sistema X comando (string del nombre del archivo)
; Recorrido: Sistema
(define del (lambda (sistema)
              (lambda (comando)
                (if (member comando (map car (sistema_files sistema))) ; revisa si el archivo existe
                    (let ((new_list_files (filter (lambda (elemento) (not (eq? (car elemento) comando))) (sistema_files sistema)))) ; crea una lista sin el archivo a eliminar (elimina todos los archivos con el nombre indicado)
                     (let ((archivo_borrar (filter (lambda (elemento) (eq? (car elemento) comando)) (sistema_files sistema)))) ; crea una lista con el archivo a eliminar
                      (list (sistema_nombre sistema)
                            (sistema_drives sistema)
                            (sistema_usuarios sistema)
                            (sistema_log sistema)
                            (sistema_folders sistema)
                            new_list_files ; agrega la lista de archivos sin el que se quiere eliminar
                            (sistema_papeleraC sistema)
                            (append (sistema_papeleraA sistema) archivo_borrar)))) ; añade el archivo borrado a la papelera
                    (if (member comando (map car (sistema_folders sistema)))
                        (let ((list_folders_elim_f (filter (lambda (elemento) (not (eq? (car elemento) comando))) (subdir_borrados sistema comando)))) ; a la lista con las subcarpetas eliminadas se le elimina la carpeta principal a eliminar
                          (let ((carpeta_principal_borrar (filter (lambda (elemento) (eq? (car elemento) comando)) (subdir_borrados sistema comando)))) ; crea una lista con tod
                              (list (sistema_nombre sistema)
                                    (sistema_drives sistema)
                                    (sistema_usuarios sistema)
                                    (sistema_log sistema)
                                    list_folders_elim_f ; se reemplaza por una lista sin la carpeta eliminada y sus subcarpetas
                                    (archivos_borrados sistema comando) ; se reemplaza por una lista sin los archivos de la carpeta eliminada
                                    (append (sistema_papeleraC sistema) carpeta_principal_borrar (subdir_borrar sistema comando)) ; añade la carpeta borrada y sus subdirectorios a la papelera
                                    (append (sistema_papeleraA sistema) (archivos_borrar sistema comando))))) ; añade los archivos eliminados a la papelera
                        sistema)))))

; Función remove directory
; Elimina una carpeta (directorio) siempre y cuando este se encuentre vacío (no posee archivos NI otras carpetas dentro). No agrega la carpeta a la papelera ya que se está borrando una carpeta vacía.
; Dominio: Sistema X nombre (o path)
; Recorrido: Sistema
(define rd (lambda (sistema)
             (lambda (nombre)
               (if (not (string-contains? nombre "/"))
                   (if (and (and (member nombre (map car (sistema_folders sistema))) ; revisa si la carpeta existe
                                 (eq? null (archivos_borrar sistema nombre))) ; revisa si hay archivos dentro de la carpeta; si es nula se procede, sino, no se elimina la carpeta
                            (eq? null (subdir_borrar sistema nombre))) ; al igual que lo anterior, revisa si hay subdirectorios dentro de la carpeta. Sigue las mismas condiciones que el anterior
                       (let ((new_list (filter (lambda (elemento) (not (string=? (car elemento) nombre))) (sistema_folders sistema)))) ; crea una lista de directorios sin el directorio a eliminar
                         (list (sistema_nombre sistema)
                               (sistema_drives sistema)
                               (sistema_usuarios sistema)
                               (sistema_log sistema)
                               new_list ; agrega la lista de carpetas sin la que se quiere eliminar
                               (sistema_files sistema)
                               (sistema_papeleraC sistema)
                               (sistema_papeleraA sistema)))
                       sistema)
                   (let ((carpeta (car (reverse (string-split nombre "/"))))) ; Si se entregó un path, extrae el nombre de la carpeta que se desea borrar
                     (if (and (and (member carpeta (map car (sistema_folders sistema))) ; revisa si la carpeta existe
                                   (eq? null (archivos_borrar sistema nombre))) ; revisa si la carpeta contiene archivos
                              (eq? null (subdir_borrar sistema nombre))) ; revisa si la carpeta contiene sub-carpetas
                         (let ((new_list (filter (lambda (elemento) (not (string=? (car elemento) carpeta))) (sistema_folders sistema))))
                           (list (sistema_nombre sistema)
                                 (sistema_drives sistema)
                                 (sistema_usuarios sistema)
                                 (sistema_log sistema)
                                 new_list ; agrega la lista de carpetas sin la que se quiere eliminar
                                 (sistema_files sistema)
                                 (sistema_papeleraC sistema)
                                 (sistema_papeleraA sistema)))
                         sistema))))))
                           

                    
; Función copy
; Permite copiar un archivo o carpeta, y pegarlo en una dirección destino. Como no se especifica en el enunciado del TDA, esta función funciona SI Y SOLO SI SE ENCUENTRA EN LA MISMA DIRECCIÓN DEL ARCHIVO A COPIAR. Esto para que no se copien 2 carpetas de distintos niveles
; ej: si quiere copiar un archivo "ex1.txt" que está en la dirección "C:/", usted tambien debe estar ubicado en la dirección "C:/"
; Dominio: Sistema X nombre del dato X dirección de destino
; Recorrido: Sistema
(define copy (lambda (sistema)
               (lambda (dato destino)
                (let ((destino (string-append (string-upcase (substring destino 0 1)) (substring destino 1)))) ; Hace que el primer elemento de la ruta (drive) sea tomado como mayúscula, por lo que no es case-sensitive
                 (let ((carpeta (filter (lambda (elemento) (and (string=? dato (car elemento)) (string=? (cadr (sistema_log sistema)) (cadr (caddr elemento))))) (sistema_folders sistema)))); genera una lista con la carpeta a copiar. si no existe, entrega una lista vacía (null)        
                   (let ((archivo (filter (lambda (elemento) (and (string=? dato (car elemento)) (eq? (cadr (sistema_log sistema)) (cadr (caddr elemento))))) (sistema_files sistema))))  ; genera una lista con el archivo a copiar. si no existe, entrega una lista vacía (null)
                     (if (or (eq? carpeta null) (not (eq? null (filter (lambda (elemento) (and (string=? dato (car elemento)) (string=? (cadr (caddr elemento)) destino))) (sistema_folders sistema))))) ; pregunta si la carpeta a copiar existe y si no existe una carpeta con el mismo nombre en la dirección destino
                         (if (or (eq? archivo null) (not (eq? null (filter (lambda (elemento) (and (string=? dato (car elemento)) (string=? (cadr (caddr elemento)) destino))) (sistema_files sistema))))) ; pregunta si el archivo a copiar existe y si no existe un archivo con el mismo nombre en la dirección destino
                             sistema ; si no se ha copiado nada por no existir (en la ruta actualmente visible) no se modifica nada
                             (let ((nuevo_archivo (append (list (car (car archivo))) (list (cadr (car archivo))) (list (list (car (caddr (car archivo))) destino)) (cdr (cddr (car archivo)))))) ; aplica la ruta actualizada al archivo
                               (list (sistema_nombre sistema)
                                     (sistema_drives sistema)
                                     (sistema_usuarios sistema)
                                     (sistema_log sistema)
                                     (sistema_folders sistema)
                                     (append (sistema_files sistema) (list nuevo_archivo)) ; añade el archivo ya pegado en la dirección destino
                                     (sistema_papeleraC sistema)
                                     (sistema_papeleraA sistema))))
                         (let ((carpetas_int (filter (lambda (elemento) (string-contains? (cadr (caddr elemento)) (string-append (cadr (caddr (car carpeta))) dato "/"))) (sistema_folders sistema)))) ; si se ha copiado una carpeta, crea una lista con todas las carpetas que esta contiene              
                           (let ((archivos_int (filter (lambda (elemento) (string-contains? (cadr (caddr elemento)) (string-append (cadr (caddr (car carpeta))) dato "/"))) (sistema_files sistema)))) ; crea una lista con todos los archivos que la carpeta copiada contiene
                             (let ((carpetas_copy (map (lambda (folder) (list (car folder) (cadr folder) (list (car (caddr folder)) (string-append destino (string-join (cdr (string-split (cadr (caddr folder))))))) (cdr (cdddr folder))))carpetas_int))) ; a la copia de carpetas, le actualiza la dirección a la dirección destino
                               (let ((archivos_copy (map (lambda (file) (append (list (car file)) (list (cadr file)) (list (list (car (caddr file)) (string-append destino (string-join (cdr (string-split (cadr (caddr file)))))))) (cdr (cddr file)))) archivos_int))) ; a la copia de archivos, le actualiza la dirección a la dirección destino
                                 (let ((carpeta_copia_principal (list (car (car carpeta)) (cadr (car carpeta)) (list (car (caddr (car carpeta))) destino) (cdr (cdddr (car carpeta)))))) ; actualiza la dirección a la copia de la carpeta que se quiere copiar (la que se ha dado como entrada)
                                   (list (sistema_nombre sistema)
                                         (sistema_drives sistema)
                                         (sistema_usuarios sistema)
                                         (sistema_log sistema)
                                         (append (sistema_folders sistema) (list carpeta_copia_principal) carpetas_copy) ; añade la carpeta copiada junto a sus carpetas contenidas con su nueva dirección
                                         (append (sistema_files sistema) archivos_copy) ; añade los archivos que estaban contenidos dentro de la carpeta copiada con su nueva dirección
                                         (sistema_papeleraC sistema)
                                         (sistema_papeleraA sistema))))))))))))))

; Función move
; Mueve una carpeta o archivo a una dirección destino. Trabaja de igual forma que la función copy, pero a excepción de que esta función borra los archivos en la ruta origen. En caso de mover una carpeta, lo mueve junto a sus subdirectorios y subarchivos
; Dominio: Sistema X dato (archivo o carpeta a mover) X destino (dirección de destino)
; Recorrido: Sistema
(define move (lambda (sistema)
                 (lambda (dato destino)
                   (let ((destino (string-append (string-upcase (substring destino 0 1)) (substring destino 1)))) ; Hace que el primer elemento de la ruta (drive) sea tomado como mayúscula, por lo que no es case-sensitive
                     (let ((carpeta (filter (lambda (elemento) (and (string=? dato (car elemento)) (string=? (cadr (sistema_log sistema)) (cadr (caddr elemento))))) (sistema_folders sistema)))) ; genera una lista con la carpeta a mover. si no existe, entrega una lista vacía (null)        
                       (let ((archivo (filter (lambda (elemento) (and (string=? dato (car elemento)) (eq? (cadr (sistema_log sistema)) (cadr (caddr elemento))))) (sistema_files sistema)))) ; genera una lista con el archivo a mover. si no existe, entrega una lista vacía (null)
                         (if (or (eq? carpeta null) (not (eq? null (filter (lambda (elemento) (and (string=? dato (car elemento)) (string=? (cadr (caddr elemento)) destino))) (sistema_folders sistema))))) ; pregunta si la carpeta a copiar existe y si no existe una carpeta con el mismo nombre en la dirección destino
                           (if (or (eq? archivo null) (not (eq? null (filter (lambda (elemento) (and (string=? dato (car elemento)) (string=? (cadr (caddr elemento)) destino))) (sistema_files sistema))))) ; pregunta si el archivo a copiar existe y si no existe un archivo con el mismo nombre en la dirección destino
                             sistema ; si no se ha movido nada por no existir (en la ruta actualmente visible) no se modifica nada
                             (let ((nuevo_archivo (append (list (car (car archivo))) (list (cadr (car archivo))) (list (list (car (caddr (car archivo))) destino)) (cdr (cddr (car archivo)))))) ; aplica la ruta actualizada al archivo
                               (let ((files_archivo_borrado (filter (lambda (elemento) (or (not (eq? (car elemento) dato)) (not (eq? (cadr (sistema_log sistema)) (cadr (caddr elemento)))))) (sistema_files sistema)))) ; recrea la lista de archivos anterior, pero con el archivo a mover eliminado de la ruta actual
                                   (list (sistema_nombre sistema)
                                         (sistema_drives sistema)
                                         (sistema_usuarios sistema)
                                         (sistema_log sistema)
                                         (sistema_folders sistema)
                                         (append files_archivo_borrado (list nuevo_archivo)) ; añade el archivo a una lista donde este se eliminó en su ruta de origen
                                         (sistema_papeleraC sistema)
                                         (sistema_papeleraA sistema)))))
                         (let ((carpetas_int (filter (lambda (elemento) (string-contains? (cadr (caddr elemento)) (string-append (cadr (caddr (car carpeta))) dato "/"))) (sistema_folders sistema)))) ; crea una lista con todas las carpetas que la carpeta a mover contiene              
                           (let ((archivos_int (filter (lambda (elemento) (string-contains? (cadr (caddr elemento)) (string-append (cadr (caddr (car carpeta))) dato "/"))) (sistema_files sistema)))) ; crea una lista con todos los archivos que la carpeta a mover contiene
                             (let ((carpetas_copy (map (lambda (folder) (list (car folder) (cadr folder) (list (car (caddr folder)) (string-append destino (string-join (cdr (string-split (cadr (caddr folder)) "/")) "/") "/")) (cdr (cdddr folder)))) carpetas_int))) ; a la copia de carpetas, le actualiza la dirección a la dirección destino
                               (let ((archivos_copy (map (lambda (file) (append (list (car file)) (list (cadr file)) (list (list (car (caddr file)) (string-append destino (string-join (cdr (string-split (cadr (caddr file)) "/")) "/") "/"))) (cdr (cddr file)))) archivos_int))) ; a la copia de archivos, le actualiza la dirección a la dirección destino
                                 (let ((carpeta_copia_principal (list (car (car carpeta)) (cadr (car carpeta)) (list (car (caddr (car carpeta))) (string-append destino (string-join (cdr (string-split (cadr (caddr (car carpeta)))))))) (cdr (cdddr (car carpeta)))))) ; actualiza la dirección a la carpeta principal a mover (la que se ha dado como entrada)
                                   (let ((folders_carpetas_borrar (filter (lambda (elemento) (and (not (eq? (car elemento) dato)) (not (string-contains? (cadr (caddr elemento)) dato)))) (sistema_folders sistema)))) ; crea una lista sin las carpetas a mover
                                     (let ((files_archivos_borrar (filter (lambda (elemento) (not (string-contains? (cadr (caddr elemento)) dato))) (sistema_files sistema)))) ; crea una lista sin los archivos a mover
                                       (list (sistema_nombre sistema)
                                             (sistema_drives sistema)
                                             (sistema_usuarios sistema)
                                             (sistema_log sistema)
                                             (append folders_carpetas_borrar carpetas_copy (list carpeta_copia_principal)) ; añade la carpeta movida junto a sus carpetas contenidas con su nueva dirección, y las elimina de su ruta origen
                                             (append files_archivos_borrar archivos_copy) ; añade los archivos que estaban contenidos dentro de la carpeta movida con su nueva dirección, y las elimina de su ruta origen
                                             (sistema_papeleraC sistema)
                                             (sistema_papeleraA sistema))))))))))))))))

; Función rename
; Renombra un archivo o carpeta QUE ESTÉ ACTUALMENTE VISIBLE, además, si se modifica el nombre de una carpeta se modifica la ruta de las subcarpetas y subarchivos
; Dominio: Sistema X dato a modificar X nuevo nombre
; Recorrido: Sistema
(define ren (lambda (sistema)
              (lambda (nombre_or nombre_nu)
                (let ((carpeta_existe (filter (lambda (elemento) (and (string=? (car elemento) nombre_or) (string=? (cadr (caddr elemento)) (cadr (sistema_log sistema))))) (sistema_folders sistema)))) ; crea lista para ver si la carpeta ya existe en el nivel actual
                  (let ((archivo_existe (filter (lambda (elemento) (and (string=? (car elemento) nombre_or) (string=? (cadr (caddr elemento)) (cadr (sistema_log sistema))))) (sistema_files sistema)))) ; crea lista para ver si el archivo ya existe en el nivel actual
                    (let ((unicidad_folders (filter (lambda (elemento) (and (string=? (car elemento) nombre_nu) (eq? (cadr (caddr elemento)) (cadr (sistema_log sistema))))) (sistema_folders sistema)))) ; crea lista con las carpetas nombradas con el nuevo nombre en el nivel actual
                      (let ((unicidad_files (filter (lambda (elemento) (and (string=? (car elemento) nombre_nu) (eq? (cadr (caddr elemento)) (cadr (sistema_log sistema))))) (sistema_files sistema)))) ; crea lista con los archivos nombrados con el nuevo nombre en el nivel actual
                        (if (or (eq? null carpeta_existe) (not (eq? null unicidad_folders)))
                            (if (or (eq? null archivo_existe) (not (eq? null unicidad_files)))
                                sistema
                                (let ((borrar_archivo (filter (lambda (elemento) (or (not (eq? (car elemento) nombre_or)) (not (eq? (cadr (sistema_log sistema)) (cadr (caddr elemento)))))) (sistema_files sistema)))) ; crea una lista sin el archivo al que se le quiere cambiar el nombre
                                  (let ((nuevo_archivo (append (list nombre_nu) (cdr (car archivo_existe))))) ; crea el archivo a modificar con su nuevo nombre
                                    (list (sistema_nombre sistema)
                                          (sistema_drives sistema)
                                          (sistema_usuarios sistema)
                                          (sistema_log sistema)
                                          (sistema_folders sistema)
                                          (append borrar_archivo (list nuevo_archivo))
                                          (sistema_papeleraC sistema)
                                          (sistema_papeleraA sistema)))))
                            (let ((carpetas_copy (map (lambda (folder) (list (car folder) (cadr folder) (list (car (caddr folder)) (string-replace (cadr (caddr folder)) (string-append nombre_or "/") (string-append nombre_nu "/"))) (cdr (cdddr folder)))) (subdir_borrar sistema nombre_or)))) ; a la copia de carpetas, le actualiza la dirección con el nuevo nombre del folder que las contiene
                                 (let ((archivos_copy (map (lambda (file) (append (list (car file)) (list (cadr file)) (list (list (car (caddr file)) (string-replace (cadr (caddr file)) (string-append nombre_or "/") (string-append nombre_nu "/")))) (cdr (cdddr file)))) (archivos_borrar sistema nombre_or)))) ; a la copia de archivos, le actualiza la dirección con el nuevo nombre del folder que las contiene
                                    (let ((folders_carpetas_borrar (filter (lambda (elemento) (or (not (string=? (cadr (sistema_log sistema)) (cadr (caddr elemento)))) (not (eq? (car elemento) nombre_or)))) (subdir_borrados sistema nombre_or)))) ; crea lista sin las carpetas que están siendo modificadas 
                                       (let ((nueva_carpeta (append (list nombre_nu) (cdr (car carpeta_existe))))) ; crea una carpeta con el nuevo nombre
                                          (list (sistema_nombre sistema)
                                                (sistema_drives sistema)
                                                (sistema_usuarios sistema)
                                                (sistema_log sistema)
                                                (append folders_carpetas_borrar carpetas_copy (list nueva_carpeta)) ; agrega la lista de carpetas con todo modificado
                                                (append (archivos_borrados sistema nombre_or) archivos_copy) ; agrega la lista de archivos con todo modificado
                                                (sistema_papeleraC sistema)
                                                (sistema_papeleraA sistema))))))))))))))
            
; Función dir
; Permite mostrar por pantalla los elementos del directorio actual y los elemenetos de subdirectorios, dependiendo del comando.
; Requisito de implementación: Deben aplicarse paréntesis extra a la hora de llamar a la función para el caso de no recibir argumentos. Ex: (display (run K dir)) -----> (display ((run K dir)))
; Dominio: Sistema X comandos (null, "/s", "/a", "/s /a", "/a /s" y "/?")
; Recorrido: String (separando cada elemento por " --- ")
(define dir (lambda (sistema)
              (lambda args ; permite entregar N cantidad de argumentos, pudiendo variar
                (let ((car_arch (append (sistema_folders sistema) (sistema_files sistema)))) ; crea una lista que junta los archivos y carpetas del sistema 
                  (define ciclo (lambda (car_arch (str "")) ; se crea función local, donde "str """ indica un string con valor inicial ""
                                  (if (eq? car_arch null) ; caso de bordes en donde no hay archivos o carpetas para analizar
                                      str ; retorna el string final armado
                                      (if (eq? args null)
                                          (if (and (string=? (cadr (sistema_log sistema)) (cadr (caddr (car car_arch)))) (not (member #\h (car car_arch)))) ; pregunta para ver si el archivo o carpeta se encuentra en el directorio actual, y si además este está visible
                                              (begin (ciclo (cdr car_arch) (string-append str (car (car car_arch)) " --- "))) ; vuelve a llamar a la función pero sin el primer elemento y con el string añadido al string inicial
                                              (ciclo (cdr car_arch) str)) ; en caso de no encontrarse visible o no estar en el directorio actual, no hace nada y vuelve a llamar a la función sin el elemento
                                          (if (string=? (car args) "/s")
                                              (if (and (or (string=? (cadr (sistema_log sistema)) (cadr (caddr (car car_arch))))
                                                           (string-contains? (cadr (caddr (car car_arch))) (cadr (sistema_log sistema)))) ; en este caso además de preguntar por los elementos directamente visibles, pregunta además por los subelementos
                                                       (not (member #\h (car car_arch))))
                                                  (begin (ciclo (cdr car_arch) (string-append str (car (car car_arch)) " --- "))) ; vuelve a llamar a la función, sin el elemento recién analizado pero agregándoselo al string
                                                  (ciclo (cdr car_arch) str))
                                              (if (string=? (car args) "/a")
                                                  (if (string=? (cadr (sistema_log sistema)) (cadr (caddr (car car_arch)))) ; en este caso simplemente pregunta si está en el mismo directorio, no importa si está oculto
                                                      (begin (ciclo (cdr car_arch) (string-append str (car (car car_arch)) " --- "))) ; vuelve a llamar a la función, sin el elemento recién analizado pero agregándoselo al string
                                                      (ciclo (cdr car_arch) str))
                                                  (if (or (string=? (car args) "/s /a") (eq? (car args) "/a /s")) ; toma el caso en el que se entreguen al revés los comandos
                                                      (if (or (string=? (cadr (sistema_log sistema)) (cadr (caddr (car car_arch)))) ; similar a "/s" pero ya no importa si el elemento está oculto o no
                                                              (string-contains? (cadr (caddr (car car_arch))) (cadr (sistema_log sistema))))
                                                          (begin (ciclo (cdr car_arch) (string-append str (car (car car_arch)) " --- "))) ; vuelve a llamar a la función, sin el elemento recién analizado pero agregándoselo al string
                                                          (ciclo (cdr car_arch) str))
                                                      (if (string=? (car args) "/?")
                                                          (begin (display "Comandos disponibles:") (newline)
                                                                 (display "- null: Si no se entregan argumentos a la función, se mostrarán los elementos del directorio actual a EXCEPCIÓN de los elementos de carácter oculto (h)") (newline)
                                                                 (display "- /s: Este comando permite mostrar los elementos tanto del directorio actual, como de los subdirectorios de este a EXCEPCIÓN de los elementos de carácter oculto (h)") (newline)
                                                                 (display "- /a: Este comando permite mostrar todos los elementos del directorio actual, INCLUYENDO los elementos de carácter oculto (h)") (newline)
                                                                 (display "- '/s /a' o '/a /s': Permite mostrar todos los elementos tanto del directorio actual como de los subdirectorios de este, INCLUYENDO los elementos de carácter oculto") (newline) (newline)
                                                                 "Esos son todos los comandos disponibles, para que cada comando surta efecto (a excepción de 'null') debe agregar doble comillas a los comandos.") ; No se añade display para que la llamada a la función mediante "display" no imprima un null
                                                          "No es un comando válido. Intente el comando /? (añadiéndole doble comillas)"))))))))
                  (ciclo car_arch))))) ; se llama a ciclo para ejecutar la función local  

; Función format
; Permite formatear una unidad. Esto borra todo el contenido de dicha unidad y permite cambiarle el nombre a la unidad formateada. Si el directorio en donde se está posicionado está en el drive a formatear, redirige el path a la raiz del drive
; Dominio: Sistema X letra (unidad) X nombre (unidad)
; Recorrido: Sistema
(define format (lambda (sistema)
                 (lambda (letra nombre)
                   (if (and (char? letra) (member letra (map car (sistema_drives sistema)))) ; Pregunta si lo dado es un caracter, y si el drive dado se encuentra registrado en el sistema
                       (let ((letra_s (string letra))) ; Crea "letra_s" que almacena la letra en versión string
                         (let ((reserva_folders (filter (lambda (elemento) (not (string-contains? (cadr (caddr elemento)) (string-append letra_s ":/")))) (sistema_folders sistema)))) ; Crea una lista con las carpetas que no pertenecen al drive a formatear
                           (let ((reserva_files (filter (lambda (elemento) (not (string-contains? (cadr (caddr elemento)) (string-append letra_s ":/")))) (sistema_files sistema)))) ; Crea una lista con los archivos que no pertenecen al drive a formatear
                             (let ((drive_a_borrar (filter (lambda (elemento) (char=? (car elemento) letra)) (sistema_drives sistema)))) ; Crea una lista con el drive a formatear
                               (let ((borrar_drive (filter (lambda (elemento) (not (char=? (car elemento) letra))) (sistema_drives sistema)))) ; Crea una nueva lista con el drive a formatear eliminado
                                 (if (string-contains? (caddr (sistema_nombre sistema)) letra_s) ; Revisa si se está usando actualmente la unidad a formatear
                                     (list (sistema_nombre sistema) 
                                           (append borrar_drive (cons (list letra nombre (caddr (car drive_a_borrar))) '())) ; registra el drive con su nuevo nombre a los drives del sistema
                                           (sistema_usuarios sistema)
                                           (list (car (sistema_log sistema)) (string-append letra_s ":/")) ; actualiza el path a la raiz de la unidad ya que se estaba posicionado dentro de ella
                                           reserva_folders
                                           reserva_files
                                           (sistema_papeleraC sistema)
                                           (sistema_papeleraA sistema))
                                     (list (sistema_nombre sistema)
                                           (append borrar_drive (cons (list letra nombre (caddr (car drive_a_borrar))) '())) ; registra el drive con su nuevo nombre a los drives del sistema
                                           (sistema_usuarios sistema)
                                           (sistema_log sistema)
                                           reserva_folders
                                           reserva_files
                                           (sistema_papeleraC sistema)
                                           (sistema_papeleraA sistema))))))))
                       (if (and (string? letra) (member (string-ref letra 0) (map car (sistema_drives sistema))))
                           (let ((reserva_folders (filter (lambda (elemento) (not (string-contains? (cadr (caddr elemento)) (string-append letra ":/")))) (sistema_folders sistema)))) ; Crea una lista con las carpetas que no pertenecen al drive a formatear
                             (let ((reserva_files (filter (lambda (elemento) (not (string-contains? (cadr (caddr elemento)) (string-append letra ":/")))) (sistema_files sistema)))) ; Crea una lista con los archivos que no pertenecen al drive a formatear
                               (let ((drive_a_borrar (filter (lambda (elemento) (string=? (string (car elemento)) letra)) (sistema_drives sistema)))) ; Crea una lista con el drive a formatear
                                 (let ((borrar_drive (filter (lambda (elemento) (not (string=? (string (car elemento)) letra))) (sistema_drives sistema)))) ; Crea una nueva lista con el drive a formatear eliminado
                                   (if (string-contains? (caddr (sistema_nombre sistema)) letra) ; Revisa si se está usando actualmente la unidad a formatear
                                     (list (sistema_nombre sistema)
                                           (append borrar_drive (cons (list letra nombre (caddr (car drive_a_borrar))) '())) ; registra el drive con su nuevo nombre a los drives del sistema
                                           (sistema_usuarios sistema)
                                           (list (car (sistema_log sistema)) (string-append letra ":/")) ; actualiza el path a la raiz de la unidad ya que se estaba posicionado dentro de ella
                                           reserva_folders
                                           reserva_files
                                           (sistema_papeleraC sistema)
                                           (sistema_papeleraA sistema))
                                     (list (sistema_nombre sistema)
                                           (append borrar_drive (cons (list letra nombre (caddr (car drive_a_borrar))) '())) ; registra el drive con su nuevo nombre a los drives del sistema
                                           (sistema_usuarios sistema)
                                           (sistema_log sistema)
                                           reserva_folders
                                           reserva_files
                                           (sistema_papeleraC sistema)
                                           (sistema_papeleraA sistema)))))))
                           sistema)))))
                                   


(provide run system add-drive register login logout switch-drive md cd add-file del rd copy move ren dir format)