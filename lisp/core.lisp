;; codigos con los requerimientos en fase 1

;; ========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura. Recibe el estado actual y el siguente, devuelve la accion de cambiar si la transicion es valida.
;; ESTRATEGIA: seleccion condicional
;; IMPACTO: no destructiva
;; ========================================================
(defun transicion (color-actual cambiar-a)
    (if (validar-transiciones-p color-actual cambiar-a)
        (list color-actual  (format nil "cambiar-a-~a" cambiar-a))
        (list color-actual 'accion-por-defecto)
    )
)

;; ========================================================
;; FUNCIÓN: validar-transiciones-p
;; NATURALEZA: Pura, recibe el estado actual y el nuevo, luego analiza si la transicion es valida o no, devuelve t o nil respectivamente
;; ESTRATEGIA: Función predicado
;; IMPACTO: no destructiva
;; ========================================================
(defun validar-transiciones-p (color-actual cambiar-a)
    (cond
        ((and (equal color-actual 'rojo) (equal cambiar-a 'rojo-intermitente)) t)
        ((and (equal color-actual 'rojo-intermitente) (equal cambiar-a 'amarillo)) t)
        ((and (equal color-actual 'amarillo) (equal cambiar-a 'amarillo-intermitente)) t)
        ((and (equal color-actual 'amarillo-intermitente) (equal cambiar-a 'verde)) t)
        ((and (equal color-actual 'verde) (equal cambiar-a 'verde-intermitente)) t)
        ((and (equal color-actual 'verde-intermitente) (equal cambiar-a 'rojo)) t)
        (t nil)
    )
)

;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Seleccion simple
;; IMPACTO: No destructiva
;; ========================================================
(defun timer (timestamp)
    (if (numberp timestamp)
        (let ((posicion (rem timestamp 225)))
            (cond
                ((< posicion 90) 'rojo)
                ((< posicion 93) 'rojo-intermitente)
                ((< posicion 99) 'amarillo)
                ((< posicion 102) 'amarillo-intermitente)
                ((< posicion 222) 'verde)
                (t 'verde-intermitente)))
        (format t "El timestamp debe ser un valor numérico")
    )
)

;; ========================================================
;; FUNCIÓN: obtener-timestamp
;; NATURALEZA: Impura, dependiendo el momento de ser invocada, da un timestamp distinto
;; ESTRATEGIA: Calculo aritmetico.
;; IMPACTO: No destructiva
;; ========================================================
(defun obtener-timestamp ()
    (- (get-universal-time)
        ;segundos minutos horas fecha mes año
        (encode-universal-time 0 0 0 1 1 1970)
    )
)

;; ========================================================
;; FUNCIÓN: informe-cronologico-semaforo
;; NATURALEZA: Impura. Guarda en un archivo de texto el historial de cambios de estado.
;; ESTRATEGIA: Aplicación de función de librería.
;; IMPACTO: No destructiva
;; ========================================================
(defun informe-cronologico-semaforo (timestamp color-anterior color-nuevo)
    (with-open-file
        (stream
         "informe-ejecucion-semaforo.txt"
         :direction :output
         :if-exists :append
         :if-does-not-exist :create)

        (format stream
                "~a - Transición: ~a -> ~a~%"
                timestamp
                color-anterior
                color-nuevo)
    )
)

;; ========================================================
;; FUNCIÓN: auditoria
;; NATURALEZA: Impura. El mensaje devuelto es distinto cuando se registra un cambio
;; ESTRATEGIA: Recursion de cola
;; IMPACTO: No destructiva
;; ========================================================
(defun auditoria (color-inicio)
    (sleep 6)
    (format t "~% testeando..")
    (let ((color-nuevo (timer (obtener-timestamp))))
        (cond
            ((not (equal color-inicio color-nuevo))
                (let ((timestamp (obtener-timestamp)))

                    (format t "~% Tiempo ~a: La luz ha cambiado de ~a a ~a"
                            timestamp
                            color-inicio
                            color-nuevo)

                    (informe-cronologico-semaforo
                     timestamp
                     color-inicio
                     color-nuevo)

                    (auditoria color-nuevo)
                )
            )
            (t (auditoria color-inicio))
        )
    )
)

(auditoria (timer (obtener-timestamp)))

;; ========================================================
;; FUNCIÓN: distribucion-temporal
;; ...
;; (resto del código exactamente como lo tenías)
