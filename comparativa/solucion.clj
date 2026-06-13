;; ========================================================
;; FUNCIÓN: validar-transiciones-p
;; NATURALEZA: Pura, recibe el estado actual y el nuevo, 
;; luego analiza si la transicion es valida o no, devuelve t o nil respectivamente
;; ESTRATEGIA: Función predicado
;; IMPACTO: no destructiva
;; ========================================================
(defn validar-transiciones-p [color-actual cambiar-a]
(cond 
  (and (= color-actual :rojo) (= cambiar-a :rojo-intermitente)) true
  (and (= color-actual :rojo-intermitente) (= cambiar-a :verde)) true
  (and (= color-actual :verde) (= cambiar-a :verde-intermitente)) true
  (and (= color-actual :verde-intermitente) (= cambiar-a :amarillo)) true
  (and (= color-actual :amarillo) (= cambiar-a :amarillo-intermitente)) true
  (and (= color-actual :amarillo-intermitente) (= cambiar-a :rojo)) true
    :else false)
  )

;; ========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura. Recibe el estado actual y el siguente,
;; devuelve la accion de cambiar si la transicion es valida.
;; ESTRATEGIA: seleccion condicional
;; IMPACTO: no destructiva
;; ========================================================

(defn transicion [color-actual cambiar-a]
  (if (validar-transiciones-p color-actual cambiar-a)
    (list (str "en-" (name color-actual))
          (str "cambiar-a-" (name cambiar-a)))
    (list (str "en-" (name color-actual))
          :accion-por-defecto))
  )

;; ========================================================
;; FUNCIÓN: obtener-timestamp
;; NATURALEZA: Impura, dependiendo el momento de ser invocada, da un timestamp distinto
;; ESTRATEGIA: Calculo aritmetico.
;; IMPACTO: No destructiva
;; ========================================================
(defn obtener-timestamp []
  (.getEpochSecond (java.time.Instant/now))
  )

;; ========================================================
;; FUNCIÓN: estados-semaforo
;; NATURALEZA: Pura.
;; ESTRATEGIA: Seleccion simple
;; IMPACTO: No destructiva
;; ========================================================

(defn estados-semaforo [posicion]
  (cond
    (< posicion 90) :rojo
    (< posicion 93) :rojo-intermitente
    (< posicion 213) :verde
    (< posicion 216) :verde-intermitente
    (< posicion 222) :amarillo
    :else :amarillo-intermitente)
  )

;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Seleccion simple
;; IMPACTO: No destructiva
;; ========================================================
(defn timer [timestamp]
  (cond
    (not (number? timestamp)) (println "El timestamp debe ser un valor numerico")
    :else (estados-semaforo (mod timestamp 225)))
  )