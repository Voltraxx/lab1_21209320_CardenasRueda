#lang scheme

(define system (lambda (nombre)      ; Crea una lista del sistema
          (list nombre null null null null)))

(define run (lambda (sistema funcion) ; Hace que la función que se pone como 2do valor de entrada se aplique al sistema
              (funcion sistema)))

(define add-drive (lambda (sistema)  ; Añade un drive al sistema dado como entrada
                    (lambda (letra nombre capacidad)
                     (list (car sistema)  ; Toma simplemente el nombre del sistema
                           (cons (list letra nombre capacidad)(cadr sistema))  ; Añade en la 2da posición los componentes del drive
                           (caddr sistema))))) ; Si el sistema no ha añadido nada, lista vacía, si ya ha sido agregado algo en la 3ra posición, se mantiene.


; ¡AÚN NO SE IMPLEMENTAN LOS CASOS EN QUE LA LETRA ES REPETIDA (posiblemente un if antes de "list (car sistema)")!