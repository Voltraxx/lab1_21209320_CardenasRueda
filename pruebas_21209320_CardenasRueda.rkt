#lang scheme

(require "TDAsystem_21209320_CardenasRueda.rkt")
(require "TDAfecha_21209320_CardenasRueda.rkt")
(require "TDAselectorSistema_21209320_CardenasRueda.rkt")
(require "TDAselectorDatos_21209320_CardenasRueda.rkt")

; SCRIPT DE PRUEBAS LABORATORIO 1
; Se le ha puesto como comentario a algunas evaluaciones del script debido a que no se han implementado sus funciones.
; El script de pruebas propio está colocado siguiente al script de pruebas entregado por coordinación

; ADVERTENCIA CON EL SCRIPT DE PRUEBAS
; Debido a las evaluaciones S27 y S30, el script sufre problemas ya que en S27 nos cambiamos al drive "D", y en S30 deberíamos ubicarnos en "C:/folder1/", pero como la función
; "cd" está implementada en su forma básica, no se traslada al drive "C", por lo que muchas de las funciones siguientes no cumplen su cometido.

;creando un sistema
(define S0 (system "newSystem"))

;añadiendo unidades. Incluye caso S2 que intenta añadir unidad con una letra que ya existe
(define S1 ((run S0 add-drive) #\C "SO" 1000))
(define S2 ((run S1 add-drive) #\C "SO1" 3000))
(define S3 ((run S2 add-drive) #\D "Util" 2000))

;añadiendo usuarios. Incluye caso S6 que intenta registrar usuario duplicado
(define S4 ((run S3 register) "user1"))
(define S5 ((run S4 register) "user1"))
(define S6 ((run S5 register) "user2"))


;iniciando sesión con usuarios. Incluye caso S8 que intenta iniciar sesión con user2 sin antes haber salido con user1
(define S7 ((run S6 login) "user1"))
(define S8 ((run S7 login) "user2"))

;cerrando sesión user1 e iniciando con user2
(define S9 (run S8 logout))
(define S10 ((run S9 login) "user2"))

;cambios de unidad, incluyendo unidad inexistente K
(define S11 ((run S10 switch-drive) #\K))
(define S12 ((run S11 switch-drive) #\C))

;añadiendo carpetas. Incluye casos de carpetas duplicadas.
(define S13 ((run S12 md) "folder1"))
(define S14 ((run S13 md) "folder2"))
(define S15 ((run S14 md) "folder2"))
(define S16 ((run S15 md) "folder3"))

;ingresa a carpeta folder2
(define S17 ((run S16 cd) "folder2"))

;crea subcarpeta folder21 dentro de folder2 (incluye caso S19 de carpeta con nombre duplicado)
(define S18 ((run S17 md) "folder21"))
(define S19 ((run S18 md) "folder21"))

;ingresa a subcarpeta e intenta ingresar a subcarpeta inexistente S221
(define S20 ((run S19 cd) "folder21"))
(define S21 ((run S20 cd) "folder22"))


;vuelve a carpeta anterior
(define S22 ((run S21 cd) ".."))

;vuelve a ingresar folder21
(define S23 ((run S22 cd) "folder21"))


;crea subcarpeta folder211 e ingresa
(define S24 ((run S23 md) "folder211"))
(define S25 ((run S24 cd) "folder211"))


;vuelve a la raíz de la unidad c:/
(define S26 ((run S24 cd) "/"))


;se cambia de unidad
(define S27 ((run S26 switch-drive) #\D))


;crea carpeta e ingresa a carpeta
(define S28 ((run S27 md) "folder5"))
(define S29 ((run S28 cd) "folder5"))


;se cambia de carpeta en base a la ruta especificada
(define S30 ((run S29 cd) "C:/folder1/")) ; En este ejemplo del script, el programa no hace nada ya que se ha implementado la forma base de la función "cd"


;formateando drive D:
(define S31 ((run S30 format) #\D "newD"))

;añadiendo archivos
(define S32 ((run S31 add-file) (file "foo1.txt" "txt" "hello world 1")))
(define S33 ((run S32 add-file) (file "foo2.txt" "txt" "hello world 2")))
(define S34 ((run S33 add-file) (file "foo3.docx" "docx" "hello world 3")))
(define S35 ((run S34 add-file) (file "goo4.docx" "docx" "hello world 4" #\h #\r))) ;con atributos de seguridad oculto (h) y de solo lectura (r)
; Estos archivos fueron creados en "D:/" ya que la evaluación de S30 no desplazó la dirección a "folder1", al no estar completamente implementada

;eliminando archivos
(define S36 ((run S35 del) "*.txt")) ; No hace nada ya que se implementó la forma base de la función del
(define S37 ((run S35 del) "f*.docx")) ; Tampoco funciona
(define S38 ((run S35 del) "goo4.docx")) ; Borra el archivo goo4 y lo manda  a la papelera
(define S39 ((run S35 cd) ".."))
(define S40 ((run S39 del) "folder1")) ; Borra la carpeta "folder1" (vacía ya que anteriormente no se pudo evaluar la función cd evaluada con "C:/folder1")


;borrando una carpeta
(define S41 ((run S39 rd) "folder1")) ;no debería borrarla, pues tiene archivos ------> Si la borra, ya que como se explicó en S40, al no implementarse la función cd para aceptar paths como entrada, nunca accedió a "folder1" para crear los archivos
(define S42 ((run S41 cd) "folder1")) ;al borrarse la carpeta, y no estar en el drive "C:/" no ha accedido a la carpeta
(define S43 ((run S42 del) "*.*")) ;no se ha implementado esto en la función del
(define S44 ((run S43 cd) "..")) ; se mantiene en el disco "D:/"
(define S45 ((run S44 rd) "folder1")) ; ya se borró la carpeta en S41

;copiando carpetas y archivos
(define S46 ((run S35 copy) "foo1.txt" "c:/folder3/"))
(define S47 ((run S46 cd) "..")) ; como se está en la raiz de D, no hace nada
(define S48 ((run S47 copy) "folder1" "d:/")) ; La carpeta no está directamente visible por lo que no se hace nada

;moviendo carpetas y archivos
(define S49 ((run S48 move) "folder3" "d:/")) ; como no se está en la misma dirección que folder3, no lo mueve
(define S50 ((run S49 cd) "folder1")) ; no está ne el nivel actual
(define S51 ((run S50 move) "foo3.docx" "d:/folder3/")) ; Como folder3 no está en el drive "D", no la mueve

;renombrando carpetas y archivos
(define S52 ((run S51 ren) "foo1.txt" "newFoo1.txt"))
(define S53 ((run S52 ren) "foo2.txt" "newFoo1.txt")) ;no debería efectuar cambios pues ya existe archivo con este nombre
(define S54 ((run S53 cd) ".."))
(define S55 ((run S54 ren) "folder1" "newFolder1")) ;no cambia su nombre ya que la carpeta no está en la misma dirección que el usuario

;listando la información
;(display (run S16 dir)) ; ---------> Esta evaluación junto a la de abajo se han dejado comentadas debido a que como se indica en el archivo "leeme.txt" adjunto a la entrega, se deben de agregar paréntesis extra en la evaluación de la función
;(display (run S55 dir))
(display ((run S55 dir) "/s")) ;muestra carpetas y subcarpetas de la unidad C
(display ((run S55 dir) "/s /a")) ;muestra todo el contenido de carpetas y subcarpetas de la unidad C incluyendo archivo oculto goo4.docx

;; A partir de aquí, las funciones del script no se pueden evaluar ya que éstas no se han implementado.


;encriptando archivos y carpetas
;(define S56 ((run S55 encrypt) plus-one minus-one "1234" "newFolder1"))
;(define S57 ((run S56 switch-drive) #\\#D))
;(define S58 ((run S57 cd) "folder3"))
;(define S59 ((run S58 encrypt) plus-one minus-one "4321" "foo3.docx"))

;desencriptando archivos y carpetas
;(define S60 ((run S59 decrypt) "1234" "foo3.docx")) ;no logra desencriptar por clave incorrecta
;(define S61 ((run S60 decrypt) "4321" "foo3.docx"))
;(define S62 ((run S61 switch-drive) #\\#C))
;(define S63 ((run S62 decrypt) "1234" "newFolder1"))

;;buscando contenido
;(define S64 ((run S63 cd) "newFolder1"))
;(display ((run S64 grep) "hello" "newFoo1.txt"))
;(display ((run S64 grep) "hello" "*.*"))

;viendo la papelera
;(display (run S45 viewTrash))

;restaurando
;(define S65 ((run S45 restore) "folder1"))




; SCRIPT DE PRUEBAS: EJEMPLOS PROPIOS
; Algunos ejemplos añadidos se han puesto como comentarios ya que son ejemplos que a consciencia no funcionan

; Función system
(define Sistema1 (system "Mi_primer_sistema")) 
(define Sistema2 (system "22222")) 
;(define Sistema3 (system "MiPrimer" "Sistema")) ; Manda error ya que se entregan 2 valores de entrada
; .-.-.-.

; Función add-drive
(define AD1 ((run Sistema1 add-drive) #\C "Primera_unidad" 10000000)) 
(define AD2 ((run AD1 add-drive) #\E "Primera_unidad" 10000000)) ; La crea de igual manera ya que no hay reestricción de unicidad en nombre y capacidad
(define AD3 ((run AD2 add-drive) #\F "Segunda_unidad" 10000000)) 
; .-.-.-.

; Función register
(define R1 ((run AD3 register) "Primer_usuario")) 
;(define R2 ((run R1 register) "Segundo" "usuario")) ; No funciona ya que la cantidad de entradas debe ser una
(define R3 ((run R1 register) "Segundo_usuario"))
; .-.-.-.

; Función login
(define Li1 ((run R3 login) "Este_usuario_no_existe")) ; No se loguea ya que el usuario "Este usuario no existe" no se ha registrado previamente
(define Li2 ((run Li1 login) "Primer_usuario")) 
(define Li3 ((run Li2 login) "Primer_usuario")) ; No hace nada ya que el usuario que se queire loguear se encuentra actualmente logueado
; .-.-.-.

; Función logout
(define Lo1 (run Li3 logout)) 
(define Lo2 (run Lo1 logout)) ; No hace nada ya que no hay ningún usuario actualmente logueado
;(define Lo3 ((run Lo2 logout) "Primer usuario")) ; No funciona ya que la función logout no recibe entradas además del sistema
; .-.-.-.

; Función switch-drive
(define SD1 ((run Lo2 switch-drive) #\F)) ; No hace nada ya que no hay un usuario actualmente logueado
(define Lo3 ((run Lo2 login) "Primer_usuario")) ; loguea al usuario "Primer usuario"
(define SD2 ((run Lo3 switch-drive) #\F)) ; Como ahora esta "Primer usuario" logueado, si se puede cambiar drive (en este caso como no había ninguno, se "ingresa" en lugar de cambiar
(define SD3 ((run Lo3 switch-drive) #\E)) ; Si hay un usuario logueado, puede cambiar de unidad directamente 
; .-.-.-.

; Función md
(define MD1 ((run SD3 md) "carpeta1")) 
(define MD2 ((run MD1 md) "datos_personales")) 
(define MD3 ((run MD2 md) "mis_codigos")) 
;(define MD4 ((run MD3 md) "esta_es_una_carpeta_vacia" "?")) ; No crea una carpeta ya que solo soporte una entrada
(define MD4 ((run MD3 md) "esta_es_una_carpeta_vacia")) ; Con este ejemplo, ya van 4 directorios creados en el sistema "MiPrimerSistema" 
; .-.-.-.

; Función cd
(define CD1 ((run MD4 cd) "carpeta2")) ; No hace nada ya que no existe la carpeta llamada "carpeta2"
(define CD2 ((run CD1 cd) "mis_codigos")) 
(define CD3 ((run CD2 cd) "carpeta1")) ;  No ingresa ya que en el nivel actual no existe la carpeta llamada "carpeta2" (y no hay ninguna carpeta actualmente)
; .-.-.-.

; Creando carpetas en "mis codigos"
(define MD5 ((run CD3 md) "python")) 
(define MD6 ((run MD5 md) "scheme")) 
(define MD7 ((run MD6 md) "C#")) 
(define MD8 ((run MD7 md) "JavaScript")) ; Se han creado 4 subcarpetas dentro de "mis codigos" con nombres de distintos lenguajes de programación
(define CD4 ((run MD8 cd) ".."))
; .-.-.-.

; Función format
(define F1 ((run CD4 format) #\E "Nueva_unidad_E"))
;(define F2 ((run CD4 format) #\E "Nueva_unidad_E" "10000")) ; No evalúa correctamente ya que la función soporta solo 2 entradas
(define SD4 ((run CD4 switch-drive) #\F)) ; Cambia a unidad F
(define F3 ((run SD4 format) #\E "Nueva_unidad_E")) ; formatea la unidad E desde la unidad F
(define F4 ((run F3 format) #\F "Nueva_unidad_F")) ; formatea la unidad F sin tener contenido (le cambia el nombre)
; .-.-.-.

; Función add-file
(define CD5 ((run CD4 cd) "mis_codigos"))
(define CD6 ((run CD5 cd) "python"))
(define AF1 ((run CD6 add-file) (file "Hola_mundo.py" "py" "print('Hola mundo')"))) ; añade este archivo a la carpeta "python"
(define CD7 ((run AF1 cd) "/"))
(define AF2 ((run CD7 add-file) (file "Lista_de_cosas_por_hacer.txt" "txt" "1.- Terminar el laboratorio de paradigmas 2.- Dormir" #\h)))
(define AF3 ((run AF2 add-file) (file "Documento_vacio.docx" "docx"))) ; En este caso no se le agrega contenido, pero bien también se le puede agregar un elemento "" a file
; .-.-.-.

; Función del
(define DEL1 ((run AF3 del) "Documento_vacio.docx"))
(define DEL2 ((run DEL1 del) "python")) ; Borra la carpeta python junto a su archivo "Hola mundo.py"
(define DEL3 ((run DEL1 del) "Archivo_desconocido.txt")) ; No hace nada ya que el archivo no existe
; .-.-.-.

; Función rd
(define RD1 ((run DEL3 rd) "mis_codigos")) ; No la borra ya que tiene archivos dentro
(define RD2 ((run RD1 rd) "Hola_mundo.py")) ; No la borra ya que rd es solo para archivos
(define RD3 ((run RD2 rd) "E:/esta_es_una_carpeta_vacia/")) ; Borra la carpeta ya que está vacía
(define RD4 ((run RD2 rd) "esta_es_una_carpeta_vacia")) ; También borra la carpeta al evaluar la función solo con el nombre
; .-.-.-.

; Función copy
(define CP1 ((run RD4 copy) "carpeta1" "e:/C#/")) ; pega en la carpeta C# la carpeta vacía "carpeta1"
(define CP2 ((run CP1 copy) "scheme" "E:/")) ; como no se está en la misma dirección que la carpeta, no hace nada
(define CP3 ((run RD4 copy) "carpeta1" "E:/C#/")) ; también copia elementos si se da el drive en mayúsculas
(define CP4 ((run CP3 copy) "Lista_de_cosas_por_hacer.txt" "E:/carpeta1/")) ; Copia un archivo a carpeta1
; .-.-.-.

; Función move
(define M1 ((run CP4 move) "carpeta1" "e:/C#/")) ; No mueve la carpeta ya que existe una carpeta del mismo nombre en la dirección destino
(define M2 ((run M1 move) "datos_personales" "e:/carpeta1/"))
(define M3 ((run M2 move) "JavaScript" "E:/")) ; No la mueve ya que la carpet no está en la misma dirección que el usuario
(define M4 ((run M3 move) "Lista_de_cosas_por_hacer.txt" "e:/mis_codigos/")) ; Mueve un archivo a la carpeta "mis_codigos"
; .-.-.-.

; Función ren
(define REN1 ((run M4 ren) "carpeta1" "mi_propiedad"))
(define REN2 ((run REN1 ren) "mis_codigos" "codigos"))
;(define REN3 ((run REN1 ren) "mis_codigos" "codigos" "codigos")) ; No funciona ya que solo acepta 2 argumentos
; .-.-.-.

; Función dir
(newline)
(display ((run REN2 dir) "/?"))
(newline)
(newline)
(display ((run REN2 dir)))
(define CD8 ((run REN2 cd) "codigos"))
(newline)
(display ((run CD8 dir) "/s"))
(newline)
(display ((run CD8 dir) "/a"))
; .-.-.-.

; Las funciones "encrypt", "decrypt", "plus-one", "minus-one", "grep" y "view-trash" NO HAN SIDO IMPLEMENTADAS, por lo que no se han hecho ejemplos para dichas funciones.