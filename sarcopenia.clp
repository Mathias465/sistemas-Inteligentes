; =========================================
; SISTEMA EXPERTO: DIAGNOSTICO DE SARCOPENIA
; =========================================

(deftemplate paciente
   (slot id)
   (slot nombre)
   (slot edad (type INTEGER))
   (slot sexo)
   (slot peso (type NUMBER))
   (slot talla (type NUMBER))
   (slot imc (type NUMBER))
)

(deftemplate sintoma
   (slot paciente-id)
   (slot nombre)
   (slot presente)
)

(deftemplate antecedente
   (slot paciente-id)
   (slot nombre)
   (slot presente)
)

(deftemplate prueba
   (slot paciente-id)
   (slot nombre)
   (slot valor)
   (slot unidad)
)

(deftemplate laboratorio
   (slot paciente-id)
   (slot nombre)
   (slot valor)
   (slot unidad)
)

(deftemplate alergia
   (slot paciente-id)
   (slot nombre)
)

(deftemplate contraindicacion
   (slot paciente-id)
   (slot nombre)
)

(deftemplate hallazgo
   (slot paciente-id)
   (slot nombre)
   (slot nivel)
)

(deftemplate diagnostico
   (slot paciente-id)
   (slot nombre)
   (slot certeza)
)

(deftemplate recomendacion
   (slot paciente-id)
   (slot tipo)
   (slot detalle)
)

(deftemplate advertencia
   (slot paciente-id)
   (slot detalle)
)

(deffunction validar-rango (?valor ?min ?max)
   (if (and (>= ?valor ?min) (<= ?valor ?max))
      then TRUE
      else FALSE))

(deffunction validar-si-no (?r)
   (if (or (eq ?r si) (eq ?r no))
      then TRUE
      else FALSE))

(deffunction preguntar-numero (?texto ?min ?max)
   (bind ?ok FALSE)
   (bind ?valor 0)
   (while (not ?ok) do
      (printout t ?texto " ")
      (bind ?valor (read))
      (if (numberp ?valor)
         then
            (if (validar-rango ?valor ?min ?max)
               then
                  (bind ?ok TRUE)
               else
                  (printout t "Dato fuera de rango. Intente nuevamente." crlf))
         else
            (printout t "Debe ingresar un numero." crlf)))
   (return ?valor))

(deffunction preguntar-texto (?texto)
   (printout t ?texto " ")
   (bind ?r (read))
   (return ?r))

(deffunction preguntar-si-no (?texto)
   (bind ?ok FALSE)
   (bind ?r no)
   (while (not ?ok) do
      (printout t ?texto " (si/no): ")
      (bind ?r (read))
      (if (validar-si-no ?r)
         then
            (bind ?ok TRUE)
         else
            (printout t "Respuesta invalida. Solo escriba si o no." crlf)))
   (return ?r))

(deffunction preguntar-sexo ()
   (bind ?ok FALSE)
   (bind ?s masculino)
   (while (not ?ok) do
      (printout t "Ingrese sexo (masculino/femenino): ")
      (bind ?s (read))
      (if (or (eq ?s masculino) (eq ?s femenino))
         then
            (bind ?ok TRUE)
         else
            (printout t "Valor invalido. Escriba masculino o femenino." crlf)))
   (return ?s))

(deffunction registrar-paciente ()
   (printout t crlf "==== REGISTRO DE PACIENTE ====" crlf)

   (bind ?id (preguntar-texto "Ingrese ID del paciente:"))
   (bind ?nombre (preguntar-texto "Ingrese nombre del paciente:"))
   (bind ?edad (preguntar-numero "Ingrese edad [1-120]:" 1 120))
   (bind ?sexo (preguntar-sexo))
   (bind ?peso (preguntar-numero "Ingrese peso en kg [20-300]:" 20 300))
   (bind ?talla (preguntar-numero "Ingrese talla en metros [0.50-2.50]:" 0.50 2.50))
   (bind ?imc (/ ?peso (* ?talla ?talla)))

   (assert (paciente
      (id ?id)
      (nombre ?nombre)
      (edad ?edad)
      (sexo ?sexo)
      (peso ?peso)
      (talla ?talla)
      (imc ?imc)))

   (assert (sintoma (paciente-id ?id) (nombre debilidad-muscular)
      (presente (preguntar-si-no "Presenta debilidad muscular"))))
   (assert (sintoma (paciente-id ?id) (nombre fatiga)
      (presente (preguntar-si-no "Presenta fatiga"))))
   (assert (sintoma (paciente-id ?id) (nombre lentitud-caminar)
      (presente (preguntar-si-no "Presenta lentitud al caminar"))))
   (assert (sintoma (paciente-id ?id) (nombre dificultad-silla)
      (presente (preguntar-si-no "Tiene dificultad para levantarse de una silla"))))
   (assert (sintoma (paciente-id ?id) (nombre caidas-frecuentes)
      (presente (preguntar-si-no "Ha tenido caidas frecuentes"))))
   (assert (sintoma (paciente-id ?id) (nombre perdida-apetito)
      (presente (preguntar-si-no "Presenta perdida de apetito"))))

   (assert (antecedente (paciente-id ?id) (nombre diabetes)
      (presente (preguntar-si-no "Tiene diabetes"))))
   (assert (antecedente (paciente-id ?id) (nombre enfermedad-renal)
      (presente (preguntar-si-no "Tiene enfermedad renal"))))
   (assert (antecedente (paciente-id ?id) (nombre desnutricion)
      (presente (preguntar-si-no "Tiene antecedente de desnutricion"))))
   (assert (antecedente (paciente-id ?id) (nombre hospitalizacion-reciente)
      (presente (preguntar-si-no "Tuvo hospitalizacion reciente"))))
   (assert (antecedente (paciente-id ?id) (nombre inmovilizacion)
      (presente (preguntar-si-no "Tuvo inmovilizacion prolongada"))))
   (assert (antecedente (paciente-id ?id) (nombre inactividad-fisica)
      (presente (preguntar-si-no "Tiene inactividad fisica"))))

   (assert (prueba (paciente-id ?id) (nombre sarc-f)
      (valor (preguntar-numero "Ingrese puntaje SARC-F [0-10]:" 0 10)) (unidad puntos)))
   (assert (prueba (paciente-id ?id) (nombre fuerza-prension)
      (valor (preguntar-numero "Ingrese fuerza de prension [1-80] kg:" 1 80)) (unidad kg)))
   (assert (prueba (paciente-id ?id) (nombre velocidad-marcha)
      (valor (preguntar-numero "Ingrese velocidad de marcha [0.1-2.5] m/s:" 0.1 2.5)) (unidad ms)))
   (assert (prueba (paciente-id ?id) (nombre test-silla)
      (valor (preguntar-numero "Ingrese tiempo test de silla [1-60] s:" 1 60)) (unidad s)))
   (assert (prueba (paciente-id ?id) (nombre pantorrilla)
      (valor (preguntar-numero "Ingrese circunferencia de pantorrilla [20-50] cm:" 20 50)) (unidad cm)))

   (assert (laboratorio (paciente-id ?id) (nombre albumina)
      (valor (preguntar-numero "Ingrese albumina [1.0-6.0] g/dL:" 1.0 6.0)) (unidad gdl)))
   (assert (laboratorio (paciente-id ?id) (nombre vitamina-d)
      (valor (preguntar-numero "Ingrese vitamina D [1-100] ng/mL:" 1 100)) (unidad ngml)))

   (if (eq (preguntar-si-no "Es alergico a suplementos proteicos") si)
      then
         (assert (alergia (paciente-id ?id) (nombre suplemento-proteico))))

   (if (eq (preguntar-si-no "Tiene limitacion para ejercicio intenso") si)
      then
         (assert (contraindicacion (paciente-id ?id) (nombre ejercicio-intenso))))

   (printout t crlf "Paciente registrado correctamente." crlf)
)

(defrule riesgo-edad
   (paciente (id ?id) (edad ?e&:(>= ?e 60)))
   =>
   (assert (hallazgo (paciente-id ?id) (nombre riesgo-edad) (nivel moderado)))
)

(defrule riesgo-inactividad
   (antecedente (paciente-id ?id) (nombre inactividad-fisica) (presente si))
   =>
   (assert (hallazgo (paciente-id ?id) (nombre riesgo-inactividad) (nivel alto)))
)

(defrule riesgo-desnutricion
   (antecedente (paciente-id ?id) (nombre desnutricion) (presente si))
   =>
   (assert (hallazgo (paciente-id ?id) (nombre riesgo-desnutricion) (nivel alto)))
)

(defrule riesgo-hospitalizacion
   (antecedente (paciente-id ?id) (nombre hospitalizacion-reciente) (presente si))
   =>
   (assert (hallazgo (paciente-id ?id) (nombre riesgo-hospitalizacion) (nivel moderado)))
)

(defrule riesgo-inmovilizacion
   (antecedente (paciente-id ?id) (nombre inmovilizacion) (presente si))
   =>
   (assert (hallazgo (paciente-id ?id) (nombre riesgo-inmovilizacion) (nivel alto)))
)

(defrule sintoma-debilidad
   (sintoma (paciente-id ?id) (nombre debilidad-muscular) (presente si))
   =>
   (assert (hallazgo (paciente-id ?id) (nombre debilidad-clinica) (nivel alto)))
)

(defrule sintoma-lentitud
   (sintoma (paciente-id ?id) (nombre lentitud-caminar) (presente si))
   =>
   (assert (hallazgo (paciente-id ?id) (nombre bajo-rendimiento) (nivel moderado)))
)

(defrule sintoma-silla
   (sintoma (paciente-id ?id) (nombre dificultad-silla) (presente si))
   =>
   (assert (hallazgo (paciente-id ?id) (nombre limitacion-funcional) (nivel moderado)))
)

(defrule sintoma-caidas
   (sintoma (paciente-id ?id) (nombre caidas-frecuentes) (presente si))
   =>
   (assert (hallazgo (paciente-id ?id) (nombre riesgo-caidas) (nivel alto)))
)

(defrule sarcf-alto
   (prueba (paciente-id ?id) (nombre sarc-f) (valor ?v&:(>= ?v 4)))
   =>
   (assert (hallazgo (paciente-id ?id) (nombre tamizaje-positivo) (nivel alto)))
)

(defrule fuerza-baja-hombre
   (paciente (id ?id) (sexo masculino))
   (prueba (paciente-id ?id) (nombre fuerza-prension) (valor ?v&:(< ?v 27)))
   =>
   (assert (hallazgo (paciente-id ?id) (nombre fuerza-baja) (nivel alto)))
)

(defrule fuerza-baja-mujer
   (paciente (id ?id) (sexo femenino))
   (prueba (paciente-id ?id) (nombre fuerza-prension) (valor ?v&:(< ?v 16)))
   =>
   (assert (hallazgo (paciente-id ?id) (nombre fuerza-baja) (nivel alto)))
)

(defrule silla-tiempo-alto
   (prueba (paciente-id ?id) (nombre test-silla) (valor ?v&:(> ?v 15)))
   =>
   (assert (hallazgo (paciente-id ?id) (nombre fuerza-funcional-baja) (nivel alto)))
)

(defrule pantorrilla-baja
   (prueba (paciente-id ?id) (nombre pantorrilla) (valor ?v&:(< ?v 31)))
   =>
   (assert (hallazgo (paciente-id ?id) (nombre masa-muscular-baja) (nivel moderado)))
)

(defrule velocidad-baja
   (prueba (paciente-id ?id) (nombre velocidad-marcha) (valor ?v&:(< ?v 0.8)))
   =>
   (assert (hallazgo (paciente-id ?id) (nombre rendimiento-fisico-bajo) (nivel alto)))
)

(defrule albumina-baja
   (laboratorio (paciente-id ?id) (nombre albumina) (valor ?v&:(< ?v 3.5)))
   =>
   (assert (hallazgo (paciente-id ?id) (nombre riesgo-nutricional) (nivel alto)))
)

(defrule vitamina-d-baja
   (laboratorio (paciente-id ?id) (nombre vitamina-d) (valor ?v&:(< ?v 20)))
   =>
   (assert (hallazgo (paciente-id ?id) (nombre deficit-vitamina-d) (nivel moderado)))
)

(defrule diagnostico-probable
   (hallazgo (paciente-id ?id) (nombre fuerza-baja))
   =>
   (assert (diagnostico (paciente-id ?id) (nombre sarcopenia-probable) (certeza alta)))
)

(defrule diagnostico-probable-por-silla
   (hallazgo (paciente-id ?id) (nombre fuerza-funcional-baja))
   (not (diagnostico (paciente-id ?id) (nombre sarcopenia-probable)))
   =>
   (assert (diagnostico (paciente-id ?id) (nombre sarcopenia-probable) (certeza media)))
)

(defrule diagnostico-confirmada
   (hallazgo (paciente-id ?id) (nombre fuerza-baja))
   (hallazgo (paciente-id ?id) (nombre masa-muscular-baja))
   =>
   (assert (diagnostico (paciente-id ?id) (nombre sarcopenia-confirmada) (certeza alta)))
)

(defrule diagnostico-severa
   (hallazgo (paciente-id ?id) (nombre fuerza-baja))
   (hallazgo (paciente-id ?id) (nombre masa-muscular-baja))
   (hallazgo (paciente-id ?id) (nombre rendimiento-fisico-bajo))
   =>
   (assert (diagnostico (paciente-id ?id) (nombre sarcopenia-severa) (certeza alta)))
)

(defrule recomendar-ejercicio
   (diagnostico (paciente-id ?id) (nombre ?n))
   (test (or (eq ?n sarcopenia-probable)
             (eq ?n sarcopenia-confirmada)
             (eq ?n sarcopenia-severa)))
   =>
   (assert (recomendacion (paciente-id ?id) (tipo ejercicio)
      (detalle "Realizar programa supervisado de ejercicios de fuerza y resistencia.")))
)

(defrule recomendar-nutricion
   (diagnostico (paciente-id ?id) (nombre ?n))
   (test (or (eq ?n sarcopenia-probable)
             (eq ?n sarcopenia-confirmada)
             (eq ?n sarcopenia-severa)))
   =>
   (assert (recomendacion (paciente-id ?id) (tipo nutricion)
      (detalle "Solicitar evaluacion nutricional y revisar ingesta proteica.")))
)

(defrule recomendar-seguimiento
   (hallazgo (paciente-id ?id) (nombre tamizaje-positivo))
   (not (diagnostico (paciente-id ?id) (nombre sarcopenia-confirmada)))
   =>
   (assert (recomendacion (paciente-id ?id) (tipo seguimiento)
      (detalle "Realizar seguimiento clinico y completar evaluacion muscular.")))
)

(defrule advertir-renal
   (antecedente (paciente-id ?id) (nombre enfermedad-renal) (presente si))
   =>
   (assert (advertencia (paciente-id ?id)
      (detalle "Ajustar plan nutricional por antecedente de enfermedad renal.")))
)

(defrule advertir-alergia
   (alergia (paciente-id ?id) (nombre suplemento-proteico))
   =>
   (assert (advertencia (paciente-id ?id)
      (detalle "Evitar suplementos proteicos que puedan generar reaccion alergica.")))
)

(defrule advertir-ejercicio
   (contraindicacion (paciente-id ?id) (nombre ejercicio-intenso))
   =>
   (assert (advertencia (paciente-id ?id)
      (detalle "El ejercicio debe adaptarse y realizarse con supervision.")))
)

(deffunction mostrar-resultados (?id)
   (printout t crlf "========== RESULTADOS ==========" crlf)
   (printout t "Paciente evaluado: " ?id crlf crlf)

   (printout t "Hallazgos detectados:" crlf)
   (do-for-all-facts ((?h hallazgo)) (eq ?h:paciente-id ?id)
      (printout t "- " ?h:nombre " [" ?h:nivel "]" crlf))

   (printout t crlf "Diagnosticos:" crlf)
   (do-for-all-facts ((?d diagnostico)) (eq ?d:paciente-id ?id)
      (printout t "- " ?d:nombre " (certeza: " ?d:certeza ")" crlf))

   (printout t crlf "Recomendaciones:" crlf)
   (do-for-all-facts ((?r recomendacion)) (eq ?r:paciente-id ?id)
      (printout t "- " ?r:detalle crlf))

   (printout t crlf "Advertencias:" crlf)
   (do-for-all-facts ((?a advertencia)) (eq ?a:paciente-id ?id)
      (printout t "- " ?a:detalle crlf))

   (printout t "================================" crlf)
)

(deffunction ejecutar-diagnostico ()
   (bind ?id (preguntar-texto "Ingrese el ID del paciente a procesar:"))
   (run)
   (mostrar-resultados ?id)
)

(deffunction caso-prueba-1 ()
   (reset)
   (assert (paciente (id p1) (nombre ana) (edad 72) (sexo femenino) (peso 55.0) (talla 1.55) (imc 22.89)))
   (assert (sintoma (paciente-id p1) (nombre debilidad-muscular) (presente si)))
   (assert (sintoma (paciente-id p1) (nombre fatiga) (presente si)))
   (assert (sintoma (paciente-id p1) (nombre lentitud-caminar) (presente no)))
   (assert (sintoma (paciente-id p1) (nombre dificultad-silla) (presente si)))
   (assert (sintoma (paciente-id p1) (nombre caidas-frecuentes) (presente no)))
   (assert (antecedente (paciente-id p1) (nombre diabetes) (presente no)))
   (assert (antecedente (paciente-id p1) (nombre enfermedad-renal) (presente no)))
   (assert (antecedente (paciente-id p1) (nombre desnutricion) (presente no)))
   (assert (antecedente (paciente-id p1) (nombre hospitalizacion-reciente) (presente no)))
   (assert (antecedente (paciente-id p1) (nombre inmovilizacion) (presente no)))
   (assert (antecedente (paciente-id p1) (nombre inactividad-fisica) (presente si)))
   (assert (prueba (paciente-id p1) (nombre sarc-f) (valor 5) (unidad puntos)))
   (assert (prueba (paciente-id p1) (nombre fuerza-prension) (valor 14) (unidad kg)))
   (assert (prueba (paciente-id p1) (nombre velocidad-marcha) (valor 0.9) (unidad ms)))
   (assert (prueba (paciente-id p1) (nombre test-silla) (valor 17) (unidad s)))
   (assert (prueba (paciente-id p1) (nombre pantorrilla) (valor 33) (unidad cm)))
   (assert (laboratorio (paciente-id p1) (nombre albumina) (valor 3.8) (unidad gdl)))
   (assert (laboratorio (paciente-id p1) (nombre vitamina-d) (valor 25) (unidad ngml)))
   (run)
   (mostrar-resultados p1)
)

(deffunction caso-prueba-2 ()
   (reset)
   (assert (paciente (id p2) (nombre luis) (edad 76) (sexo masculino) (peso 61.0) (talla 1.68) (imc 21.61)))
   (assert (sintoma (paciente-id p2) (nombre debilidad-muscular) (presente si)))
   (assert (sintoma (paciente-id p2) (nombre fatiga) (presente si)))
   (assert (sintoma (paciente-id p2) (nombre lentitud-caminar) (presente si)))
   (assert (sintoma (paciente-id p2) (nombre dificultad-silla) (presente si)))
   (assert (sintoma (paciente-id p2) (nombre caidas-frecuentes) (presente si)))
   (assert (antecedente (paciente-id p2) (nombre diabetes) (presente si)))
   (assert (antecedente (paciente-id p2) (nombre enfermedad-renal) (presente no)))
   (assert (antecedente (paciente-id p2) (nombre desnutricion) (presente si)))
   (assert (antecedente (paciente-id p2) (nombre hospitalizacion-reciente) (presente si)))
   (assert (antecedente (paciente-id p2) (nombre inmovilizacion) (presente no)))
   (assert (antecedente (paciente-id p2) (nombre inactividad-fisica) (presente si)))
   (assert (prueba (paciente-id p2) (nombre sarc-f) (valor 6) (unidad puntos)))
   (assert (prueba (paciente-id p2) (nombre fuerza-prension) (valor 24) (unidad kg)))
   (assert (prueba (paciente-id p2) (nombre velocidad-marcha) (valor 0.85) (unidad ms)))
   (assert (prueba (paciente-id p2) (nombre test-silla) (valor 19) (unidad s)))
   (assert (prueba (paciente-id p2) (nombre pantorrilla) (valor 29) (unidad cm)))
   (assert (laboratorio (paciente-id p2) (nombre albumina) (valor 3.2) (unidad gdl)))
   (assert (laboratorio (paciente-id p2) (nombre vitamina-d) (valor 18) (unidad ngml)))
   (run)
   (mostrar-resultados p2)
)

(deffunction caso-prueba-3 ()
   (reset)
   (assert (paciente (id p3) (nombre rosa) (edad 80) (sexo femenino) (peso 48.0) (talla 1.50) (imc 21.33)))
   (assert (sintoma (paciente-id p3) (nombre debilidad-muscular) (presente si)))
   (assert (sintoma (paciente-id p3) (nombre fatiga) (presente si)))
   (assert (sintoma (paciente-id p3) (nombre lentitud-caminar) (presente si)))
   (assert (sintoma (paciente-id p3) (nombre dificultad-silla) (presente si)))
   (assert (sintoma (paciente-id p3) (nombre caidas-frecuentes) (presente si)))
   (assert (antecedente (paciente-id p3) (nombre diabetes) (presente no)))
   (assert (antecedente (paciente-id p3) (nombre enfermedad-renal) (presente si)))
   (assert (antecedente (paciente-id p3) (nombre desnutricion) (presente si)))
   (assert (antecedente (paciente-id p3) (nombre hospitalizacion-reciente) (presente si)))
   (assert (antecedente (paciente-id p3) (nombre inmovilizacion) (presente si)))
   (assert (antecedente (paciente-id p3) (nombre inactividad-fisica) (presente si)))
   (assert (prueba (paciente-id p3) (nombre sarc-f) (valor 7) (unidad puntos)))
   (assert (prueba (paciente-id p3) (nombre fuerza-prension) (valor 12) (unidad kg)))
   (assert (prueba (paciente-id p3) (nombre velocidad-marcha) (valor 0.6) (unidad ms)))
   (assert (prueba (paciente-id p3) (nombre test-silla) (valor 22) (unidad s)))
   (assert (prueba (paciente-id p3) (nombre pantorrilla) (valor 28) (unidad cm)))
   (assert (laboratorio (paciente-id p3) (nombre albumina) (valor 3.0) (unidad gdl)))
   (assert (laboratorio (paciente-id p3) (nombre vitamina-d) (valor 14) (unidad ngml)))
   (assert (alergia (paciente-id p3) (nombre suplemento-proteico)))
   (assert (contraindicacion (paciente-id p3) (nombre ejercicio-intenso)))
   (run)
   (mostrar-resultados p3)
)

(deffunction menu ()
   (bind ?op 0)
   (while (neq ?op 6) do
      (printout t crlf "=========== MENU ===========" crlf)
      (printout t "1. Registrar paciente" crlf)
      (printout t "2. Ejecutar diagnostico" crlf)
      (printout t "3. Caso de prueba 1" crlf)
      (printout t "4. Caso de prueba 2" crlf)
      (printout t "5. Caso de prueba 3" crlf)
      (printout t "6. Salir" crlf)
      (printout t "Seleccione una opcion: ")
      (bind ?op (read))

      (if (= ?op 1) then (registrar-paciente))
      (if (= ?op 2) then (ejecutar-diagnostico))
      (if (= ?op 3) then (caso-prueba-1))
      (if (= ?op 4) then (caso-prueba-2))
      (if (= ?op 5) then (caso-prueba-3))
      (if (= ?op 6) then (printout t crlf "Fin del programa." crlf)))
)