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
    ((and (equal color-actual 'rojo)
          (equal cambiar-a 'rojo-intermitente))
     t)
    ((and (equal color-actual 'rojo-intermitente)
          (equal cambiar-a 'verde))
     t)
    ((and (equal color-actual 'verde)
          (equal cambiar-a 'verde-intermitente))
     t)
    ((and (equal color-actual 'verde-intermitente)
          (equal cambiar-a 'amarillo))
     t)
    ((and (equal color-actual 'amarillo)
          (equal cambiar-a 'amarillo-intermitente))
     t)
    ((and (equal color-actual 'amarillo-intermitente)
          (equal cambiar-a 'rojo))
     t)
    (t nil))
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
          ((< posicion 213) 'verde)
          ((< posicion 216) 'verde-intermitente)
          ((< posicion 222) 'amarillo)
          (t 'amarillo-intermitente)))
      (format t "El timestamp debe ser un valor numérico"))
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

;;obtener timestamp usand local-time
(defun obtener-timestamp()
    (local-time:timestamp-to-unix (local-time:now))
)

;; ========================================================
;; FUNCIÓN: formatear-timestamp
;; NATURALEZA: Pura. Para un mismo timestamp siempre devuelve el mismo resultado.
;; ESTRATEGIA: Aplicación de función de librería.
;; IMPACTO: No destructiva.
;; ========================================================
(defun formatear-timestamp (timestamp)
  (local-time:unix-to-timestamp timestamp)
)

;; ========================================================
;; FUNCIÓN: auditoria
;; NATURALEZA: Impura. El mensaje devuelto es distito cuando se registra un cambio
;; ESTRATEGIA: Recursion de cola
;; IMPACTO: No destructiva
;; ========================================================
(defun auditoria (color-inicio)
    (sleep 6)
    (format t "~% testeando..")
    (let ((color-nuevo (timer (obtener-timestamp))))
        (cond
            ((not (equal color-inicio color-nuevo)) 
                (format t "~% Tiempo ~a: La luz ha cambiado de ~a a ~a"
                (formatear-timestamp (obtener-timestamp)) color-inicio color-nuevo)
                (auditoria color-nuevo)
            )
            (t (auditoria color-inicio))
        )
    )
)

;; ========================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura (Retorna siempre el mismo valor para los mismos parámetros)
;; ESTRATEGIA: Funciones de Orden Superior (Uso de reduce)
;; IMPACTO: No destructiva
;; ========================================================
(defun duracion-ciclo (lista-tiempos)
  (reduce #'+ lista-tiempos))

;; ========================================================
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Pura 
;; ESTRATEGIA: Condicional / Predicativa
;; IMPACTO: No destructiva
;; ========================================================
(defun recomendacion-ciclo (duracion)
  (cond ((< duracion 35) 
         "Evitar: Ciclo demasiado corto (menor a 35s). Estrés para el conductor.")
        ((> duracion 150) 
         "Evitar: Ciclo demasiado largo (mayor a 150s). Fomenta infracciones.")
        (t 
         "Recomendado: Ciclo dentro del rango óptimo (35s - 150s).")))

;; ========================================================
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura
;; ESTRATEGIA: Función matemática simple con entorno léxico local (LET)
;; IMPACTO: No destructiva
;; ========================================================
(defun ciclos-por-tiempo (minutos duracion-un-ciclo)
  (let ((segundos-totales (* minutos 60)))
    (values (floor (/ segundos-totales duracion-un-ciclo)))))

;; ========================================================
;; FUNCIÓN: distribucion-temporal
;; NATURALEZA: Pura. Calcula el porcentaje de tiempo de cada color durante un período determinado.
;; ESTRATEGIA: Estructura de Control Condicional y Cálculo Aritmético.
;; IMPACTO: No destructiva
;; ========================================================

(defun distribucion-temporal (horas)

    (if (or (not (numberp horas)) (<= horas 0))
        '(error parametro-invalido)

        (let* ((tiempo-total (* horas 3600))
               (ciclos (floor (/ tiempo-total 225)))
               (resto (mod tiempo-total 225))

               (verde (+ (* ciclos 120)
                         (min resto 120)))

               (verde-intermitente (+ (* ciclos 3)
                                      (max 0
                                           (min 3
                                                (- resto 120)))))

               (rojo (+ (* ciclos 90)
                        (max 0
                             (min 90
                                  (- resto 123)))))


               (rojo-intermitente (+ (* ciclos 3)
                                     (max 0
                                          (min 3
                                               (- resto 213)))))


               (amarillo (+ (* ciclos 6)
                            (max 0
                                 (min 6
                                      (- resto 216)))))


               (amarillo-intermitente (+ (* ciclos 3)
                                         (max 0
                                              (min 3
                                                   (- resto 222))))))
        

          (list
            (list 'verde
                  (/ (round (* (/ verde tiempo-total) 10000))
                     100.0))

            (list 'verde-intermitente
                  (/ (round (* (/ verde-intermitente tiempo-total) 10000))
                     100.0))

            (list 'rojo
                  (/ (round (* (/ rojo tiempo-total) 10000))
                     100.0))

            (list 'rojo-intermitente
                  (/ (round (* (/ rojo-intermitente tiempo-total) 10000))
                     100.0))

            (list 'amarillo
                  (/ (round (* (/ amarillo tiempo-total) 10000))
                     100.0))

            (list 'amarillo-intermitente
                  (/ (round (* (/ amarillo-intermitente tiempo-total) 10000))
                     100.0)))
    )
))
;; ========================================================
;; REQUERIMIENTO 7 - ASEGURAMIENTO DE LA CALIDAD
;; ========================================================

;; R1 - validar-transiciones-p

(validar-transiciones-p 'rojo 'rojo-intermitente) ; funcionamiento normal
(validar-transiciones-p 'verde 'verde-intermitente) ; camino alternativo
(validar-transiciones-p 'rojo 'verde) ; error

;; R2 - transicion
    
(transicion 'rojo 'rojo-intermitente) ; funcionamiento normal
(transicion 'amarillo-intermitente 'verde) ; camino alternativo
(transicion 'rojo 'verde) ; error

;; R3 - timer

(timer 50) ; funcionamiento normal
(timer 95) ; camino alternativo
(timer 'hola) ; error

;; R4 - obtener-timestamp

(obtener-timestamp) ; funcionamiento normal
(obtener-timestamp) ; camino alternativo

;; R6 - distribucion-temporal

(distribucion-temporal 1) ; funcionamiento normal
(distribucion-temporal 24) ; camino alternativo
(distribucion-temporal 'hola) ; error

;; R5 - auditoria

;(auditoria (timer (obtener-timestamp))) ; funcionamiento normal
;(auditoria 'verde) ; camino alternativo
