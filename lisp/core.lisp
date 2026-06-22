;; codigos con los requerimientos en fase 1

;;cargar local-time
(find-package :ql)
(ql:quickload "local-time")

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
;; FUNCIÓN: segundos-hasta-cambio
;; NATURALEZA: Pura, calcula la cantidad de segundos que faltan para que ocurra un cambio de estado
;; ESTRATEGIA: Calculo aritmetico
;; IMPACTO: No destructiva
;; ========================================================
(defun segundos-hasta-cambio (timestamp)
  (let ((posicion (rem timestamp 225)))
    (cond
      ((< posicion 90) (- 90 posicion))
      ((< posicion 93) (- 93 posicion))
      ((< posicion 213) (- 213 posicion))
      ((< posicion 216) (- 216 posicion))
      ((< posicion 222) (- 222 posicion))
      (t (- 225 posicion))
    )
  )
)

;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Seleccion simple
;; IMPACTO: No destructiva
;; ========================================================

;;NOTA: Para correr en SBCL se debe cambiar el nombre de esta funcion ya que 'timer' es una palabra reservada en el mismo.
;;      Necesario para probar funcion utilizando la variante con Local-time
(defun timer (timestamp)
  (typecase timestamp
    (number
      (let ((posicion (rem timestamp 225)))
        (cond
          ((< posicion 90) 'rojo)
          ((< posicion 93) 'rojo-intermitente)
          ((< posicion 213) 'verde)
          ((< posicion 216) 'verde-intermitente)
          ((< posicion 222) 'amarillo)
          (t 'amarillo-intermitente)) ))
    (t (format t "El timestamp debe ser un valor numérico")))
)

;; ========================================================
;; FUNCIÓN: obtener-timestamp
;; NATURALEZA: Impura, dependiendo el momento de ser invocada, da un timestamp distinto
;; ESTRATEGIA: Calculo aritmetico.
;; IMPACTO: No destructiva
;; ========================================================
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
  (local-time:format-timestring
    nil
    (local-time:unix-to-timestamp timestamp)
    :timezone local-time:+utc-zone+
    :format '((:year 4) "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2) ":" (:sec 2))
  )
)

;; ========================================================
;; FUNCIÓN: crear-registro
;; NATURALEZA: Pura. Construye una estructura de datos que representa un evento de transición del sistema semafórico.
;; ESTRATEGIA: Construcción de lista (mapping directo de valores a estructura).
;; IMPACTO: No destructiva
;; ========================================================
(defun crear-registro (timestamp color-actual color-nuevo)
  (list (formatear-timestamp timestamp)
        color-actual
        color-nuevo))

;; ========================================================
;; FUNCIÓN: informe
;; NATURALEZA: Impura. Realiza persistencia de datos escribiendo el log de transiciones en un archivo de texto plano.
;; ESTRATEGIA: Escritura secuencial utilizando with-open-file y recorrido con dolist.
;; IMPACTO: No destructiva en memoria, pero con efecto colateral en el sistema de archivos (creación/escritura de archivo externo).
;; ========================================================
(defun informe (datos)
  (with-open-file (stream "informe-ejecucion-semaforo.txt"
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)

    (format stream "Informe de Ejecución del Sistema Semafórico~%")
    (format stream "=========================================~%")

    (dolist (registro (reverse datos))
      (format stream "~a - Transición: ~a -> ~a~%"
              (first registro)
              (second registro)
              (third registro)))

    (format stream "~%--- Fin del Informe ---~%")))

;; ========================================================
;; FUNCIÓN: auditoria
;; NATURALEZA: Impura. El mensaje devuelto es distito cuando se registra un cambio
;; ESTRATEGIA: Recursion de cola
;; IMPACTO: No destructiva
;; ========================================================
(defun auditoria (color-inicio ciclos log)
  (if (= ciclos 0)
      (progn
        (informe log)
        (format t "~%Auditoría finalizada. Log guardado.~%"))
      
      (let* ((timestamp (obtener-timestamp))
             (espera (segundos-hasta-cambio timestamp)))

        (sleep espera)

        (let* ((nuevo-timestamp (obtener-timestamp))
               (color-nuevo (timer nuevo-timestamp))
               (registro (crear-registro nuevo-timestamp color-inicio color-nuevo)))

          (format t "~%Tiempo ~a: La luz cambió de ~a a ~a~%"
                  (formatear-timestamp nuevo-timestamp)
                  color-inicio
                  color-nuevo)

          (auditoria color-nuevo (- ciclos 1) (cons registro log))))))

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

               (rojo (+ (* ciclos 90)
                         (min resto 90)))

               (rojo-intermitente (+ (* ciclos 3)
                                      (max 0
                                           (min 3
                                                (- resto 90)))))

               (verde (+ (* ciclos 120)
                        (max 0
                             (min 120
                                  (- resto 93)))))


               (verde-intermitente (+ (* ciclos 3)
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
            (list 'rojo
                  (/ (round (* (/ rojo tiempo-total) 10000))
                     100.0))

            (list 'rojo-intermitente
                  (/ (round (* (/ rojo-intermitente tiempo-total) 10000))
                     100.0))

            (list 'verde
                  (/ (round (* (/ verde tiempo-total) 10000))
                     100.0))

            (list 'verde-intermitente
                  (/ (round (* (/ verde-intermitente tiempo-total) 10000))
                     100.0))

            (list 'amarillo
                  (/ (round (* (/ amarillo tiempo-total) 10000))
                     100.0))

            (list 'amarillo-intermitente
                  (/ (round (* (/ amarillo-intermitente tiempo-total) 10000))
                     100.0)))
    )
))