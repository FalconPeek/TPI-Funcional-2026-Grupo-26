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
;; NATURALEZA: Pura, recibe eñ estado actual y el nuevo, luego analiza si la transicion es valida o no, devuelve t o nil respectivamente
;; ESTRATEGIA: Función predicado
;; IMPACTO: no destructiva
;; ========================================================
(defun validar-transiciones-p (color-actual cambiar-a)
    (cond
        ((and (equal color-actual 'en-rojo) (equal cambiar-a 'verde)) t)
        ((and (equal color-actual 'en-verde) (equal cambiar-a 'amarillo)) t)
        ((and (equal color-actual 'en-amarillo) (equal cambiar-a 'rojo)) t)
        (t nil)
    )
)
