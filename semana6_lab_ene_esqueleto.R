# =============================================================================
# Laboratorio Semana 6 — El ciclo completo con datos reales del INE
# Fundamentos de Programación para Análisis Económico · UdeC-EAN
#
# Autor: [TU NOMBRE]   |   Fecha: [FECHA]
#
# Objetivo: tomar un archivo público tal como lo publica el INE y llegar hasta
#           la tasa de desocupación de tu región. Nadie preparó estos datos
#           para ti: ese es exactamente el punto.
#
# CÓMO USARLO: donde veas ____ tienes que escribir tú. Corre cada bloque con
#              Cmd/Ctrl + Enter y compara con el "✅ Deberías ver".
#
# ⚠️ Ábrelo desde el .Rproj, no con RStudio a secas, o la ruta no funcionará.
#
# Regla IA: ChatGPT es CONSULTOR para errores de sintaxis. Qué hacer con un
#           faltante o un valor imposible es TU criterio — y eso se evalúa.
# =============================================================================

library(dplyr)


# -----------------------------------------------------------------------------
# PASO 1 — De dónde salió este archivo
# -----------------------------------------------------------------------------
# Antes de escribir una línea de código: documenta la fuente. Si no puedes decir
# de dónde vienen tus datos, tu análisis no es reproducible.
#
# TODO: completa la ficha (búscala en el sitio del INE).
#
# FUENTE:            Instituto Nacional de Estadísticas (INE), Chile
# ENCUESTA:          Encuesta Nacional de Empleo (ENE)
# TRIMESTRE MÓVIL:   mayo - junio - julio 2026
# ARCHIVO:           ene-2026-06-mjj.csv
# ENLACE:            ____________________________________________
# FECHA DE DESCARGA: ____________________
# UNIDAD DE OBSERVACIÓN: ____________________  (¿persona? ¿hogar? ¿vivienda?)
#
# 💡 Esta ficha es la que te van a exigir en el README del Proyecto Final.
#    Acostúmbrate ahora: se llena ANTES de analizar, no después.


# -----------------------------------------------------------------------------
# PASO 2 — El primer intento va a fallar
# -----------------------------------------------------------------------------
# 🔮 PREDICE: es un .csv y usas read.csv(). ¿Qué puede salir mal? ____________

intento <- read.csv("data/raw/ene-2026-06-mjj.csv", nrows = 5)
ncol(intento)

# ✅ Deberías ver: 1
#
# Una sola columna. El archivo NO está roto: read.csv() supone que el separador
# es la coma, y el INE separa con punto y coma. Todo entró como una sola cadena.
#
# 💡 ANTES de leer un archivo, ábrelo en un editor de texto y mira las dos
#    primeras líneas. Te ahorra media hora de depuración.

# TODO: usa la variante europea (separador ";" y decimales con coma).
ene <- ____("data/raw/ene-2026-06-mjj.csv")

dim(ene)

# ✅ Deberías ver: 97946 222
#
# ⚠️ Ojo con el decimal: si lo lees con read.csv() y el separador correcto, la
#    columna fact_cal ("1079,2562...") llegaría como TEXTO, y mean() fallaría
#    sin decirte por qué. read.csv2() arregla las dos cosas a la vez.


# -----------------------------------------------------------------------------
# PASO 3 — 222 columnas: reconocer el terreno
# -----------------------------------------------------------------------------
# Nadie trabaja con 222 variables. Aquí los selectores de la Semana 5 dejan de
# ser una comodidad y pasan a ser indispensables.

names(select(ene, starts_with("cae")))
ncol(select(ene, where(is.numeric)))

# ✅ Deberías ver: "cae_general" "cae_especifico"  y  212

# Las variables que necesitamos hoy:
#   region      código de región (1 a 16)
#   sexo        1 = hombre, 2 = mujer
#   edad        años cumplidos
#   activ       condición de actividad: 1 ocupado, 2 desocupado, 3 inactivo
#   habituales  horas que declara trabajar en una semana normal
#   fact_cal    FACTOR DE EXPANSIÓN: cuántos chilenos representa esta fila
#
# ⚠️ activ es la columna 221 de 222. Si no la ves en la cabecera, no es que
#    falte: está al final, en el bloque de indicadores que el INE construye
#    (pet, ft, activ, cae_general, fact_cal...). Las columnas b1, b2, ... son
#    PREGUNTAS del cuestionario (sección B, solo ocupados): b1 es el grupo de
#    ocupación, no la condición de actividad. Compruébalo:
which(names(ene) == "activ")
table(activ = ene$activ, tiene_b1 = !is.na(ene$b1), useNA = "ifany")

# ✅ Deberías ver: 221, y que b1 solo existe cuando activ == 1.
#
# 📖 Y sobre el tiempo: mes_central (siempre 6 = trimestre mayo-julio) dice
#    A QUÉ TRIMESTRE pertenece la base; mes_encuesta (5, 6 o 7) dice EN QUÉ
#    MES entrevistaron a cada persona. La ENE es un trimestre MÓVIL: cada mes
#    se entrevista a un tercio de la muestra. Todo esto está en el manual:
#    data/raw/codigos-ene-2020.pdf.
#
#    ¿POR QUÉ un trimestre móvil y no cifras mensuales? Por PRECISIÓN y
#    OPORTUNIDAD a la vez: un mes solo tiene ~370 personas de Aysén en la
#    fuerza de trabajo (demasiado poco para una cifra regional), y esperar
#    un trimestre cerrado daría solo 4 datos al año. La ventana móvil junta
#    tres meses (precisión) y se actualiza cada mes (oportunidad). El costo:
#    dos trimestres seguidos comparten dos meses, así que no son datos
#    independientes. Lo comprobarás en el PASO 8 al ver cuánto salta la
#    tasa de una región chica si la calculas con un solo mes.
#
# TODO: quédate solo con esas seis.
empleo <- select(ene, ____, ____, ____, ____, ____, ____)

dim(empleo)

# ✅ Deberías ver: 97946 6


# -----------------------------------------------------------------------------
# PASO 4 — Traducir los códigos a algo legible
# -----------------------------------------------------------------------------
# "activ == 2" no le dice nada a nadie que lea tu script (incluido tú en un mes).
# TODO: completa la recodificación con case_when() (Semana 5).

empleo <- empleo |>
  mutate(
    sexo_txt = case_when(
      sexo == 1 ~ "Hombre",
      sexo == 2 ~ "____"
    ),
    situacion = case_when(
      activ == 1 ~ "Ocupado",
      activ == 2 ~ "____",
      activ == 3 ~ "Inactivo",
      TRUE       ~ "Fuera de edad de trabajar"    # el resto: ¿quiénes son?
    )
  )

table(empleo$situacion)

# ✅ Deberías ver:
#    Desocupado 4412 | Fuera de edad de trabajar 15305 | Inactivo 36379 | Ocupado 41850


# -----------------------------------------------------------------------------
# PASO 5 — ¿Se perdió el dato, o nunca existió?
# -----------------------------------------------------------------------------
# 15.305 personas no tienen condición de actividad. Antes de imputar NADA,
# averigua quiénes son.

summary(empleo$edad[is.na(empleo$activ)])

# ✅ Deberías ver: máximo 14 años.
#
# TODO: responde en un comentario.
# ¿Por qué no tienen dato de actividad? _______________________________________
# ¿Convendría imputarles una situación laboral? ¿Por qué? _____________________
#
# 💡 Es un faltante ESTRUCTURAL: la pregunta no se les hizo porque no están en
#    edad de trabajar. Imputarlo equivaldría a declarar desocupado a un niño de
#    8 años. Se excluyen del cálculo, no se rellenan.
#
# Compara con el otro faltante del archivo:
sum(is.na(empleo$habituales))

# ✅ Deberías ver: 56096
#
# TODO: ¿este es estructural o real? ¿Quiénes son? ____________________________


# -----------------------------------------------------------------------------
# PASO 6 — El faltante disfrazado de número
# -----------------------------------------------------------------------------
summary(empleo$habituales)

# 🔮 PREDICE: mira el MÁXIMO. ¿Es un dato posible? ____________
#
# La semana tiene 168 horas. Un 999 no es una jornada: es un CÓDIGO que la
# encuesta usa para "no responde". is.na() no lo detecta y mean() lo promedia.

# TODO: mira TODA la cola alta, no solo el máximo.
sort(table(empleo$habituales[empleo$habituales > 80]), decreasing = TRUE)

# ✅ Deberías ver que 999 aparece 8 veces... pero 888 aparece 103.
#    Hay DOS centinelas, y el segundo es el que más pesa.
#
# ⚠️ LA TRAMPA: si limpias solo el 999 (el que salta en summary()), tu promedio
#    sigue estando mal. Mira la diferencia:

oc <- empleo$habituales[empleo$situacion == "Ocupado"]
c(crudo         = mean(oc, na.rm = TRUE),
  sin_999       = mean(oc[oc != 999], na.rm = TRUE),
  sin_999_ni_888 = mean(oc[!(oc %in% c(888, 999))], na.rm = TRUE))

# ✅ Deberías ver: 40.33 | 40.14 | 38.05
#
# Quitar el 999 mueve el promedio 0,2 horas: parece que no importaba.
# Quitar el 888 lo mueve 2,1 horas más. Esa es la lección: no basta con mirar
# el máximo, hay que mirar la distribución completa.

# TODO: convierte AMBOS centinelas en NA de verdad, con na_if().
empleo <- empleo |>
  mutate(habituales = na_if(habituales, ____),
         habituales = na_if(habituales, ____))

max(empleo$habituales, na.rm = TRUE)

# ✅ Deberías ver: 126
#
# TODO: 126 horas semanales son 18 al día, todos los días. ¿Lo dejas, lo tratas
#       como outlier o lo eliminas? Decide y JUSTIFICA:
# DECISIÓN: ___________________________________________________________________


# -----------------------------------------------------------------------------
# PASO 7 — La tasa de desocupación
# -----------------------------------------------------------------------------
# Definición oficial:  desocupados / (ocupados + desocupados)
# El denominador es la FUERZA DE TRABAJO, no la población: por eso los menores
# y los inactivos quedan fuera.

# TODO: quédate solo con la fuerza de trabajo.
ft <- empleo |>
  filter(situacion == "Ocupado" ____ situacion == "Desocupado")

nrow(ft)

# ✅ Deberías ver: 46262

# --- Primer cálculo: contando filas ---
round(100 * sum(ft$situacion == "Desocupado") / nrow(ft), 2)

# ✅ Deberías ver: 9.54

# --- Segundo cálculo: contando PERSONAS ---
# Cada fila representa a un número distinto de chilenos. fact_cal lo dice.
# TODO: en vez de contar filas, SUMA los factores de expansión.
desocupados <- sum(ft$fact_cal[ft$situacion == "____"])
fuerza      <- sum(ft$____)

round(100 * desocupados / fuerza, 2)
format(round(fuerza), big.mark = ".", decimal.mark = ",")

# ✅ Deberías ver: 9.53  y  10.295.061
#
# 💡 La tasa casi no cambió (9,54 vs 9,53). Pero el NIVEL sí: sin ponderar
#    dirías que hay 4.412 desocupados en Chile; con ponderador son 981.044.
#
# ⚠️ Que el ponderador mueva poco una tasa NO autoriza a ignorarlo. Toda
#    encuesta oficial (CASEN, ENE, EPF) trae uno, y sin él tu resultado no es
#    comparable con la cifra que publica el INE.


# -----------------------------------------------------------------------------
# PASO 8 — ¿Y en tu región?
# -----------------------------------------------------------------------------
regiones <- c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso",
              "O'Higgins", "Maule", "Biobío", "La Araucanía", "Los Lagos",
              "Aysén", "Magallanes", "Metropolitana", "Los Ríos",
              "Arica y Parinacota", "Ñuble")

# TODO: completa la tabla por región, ponderando.
ft |>
  mutate(region_txt = regiones[region]) |>
  group_by(____) |>
  summarise(
    n_muestra = n(),
    tasa = round(100 * sum(fact_cal[situacion == "Desocupado"]) / sum(fact_cal), 2)
  ) |>
  arrange(desc(____))

# ✅ Deberías ver a Ñuble en el primer lugar, con 10,96 %, y a Aysén en el
#    último, con 3,96 %. (La tabla muestra 11.0: es 10,96 redondeado.)
#
# TODO: responde en comentarios.
# ¿Cuál es la tasa de Ñuble y en qué lugar del ranking quedó? _________________
# Aysén tiene n_muestra = 1116. ¿Confiarías igual en su 3,96 % que en el
# 9,49 % de la Metropolitana (n = 9436)? ¿Por qué? ____________________________

# TODO: la misma tabla, pero por sexo.
ft |>
  group_by(____) |>
  summarise(n = n(),
            tasa = round(100 * sum(fact_cal[situacion == "Desocupado"]) / sum(fact_cal), 2))

# ✅ Deberías ver: Hombre 8,93 % | Mujer 10,32 %

# TODO (opcional): ¿por qué el INE no publica mes a mes? Calcula la tasa de
#       Aysén (region 11) usando UN solo mes de entrevista cada vez.
ft |>
  filter(region == 11) |>
  group_by(____) |>                # ¿qué columna dice en qué mes entrevistaron?
  summarise(n = n(),
            tasa = round(100 * sum(fact_cal[situacion == "Desocupado"]) / sum(fact_cal), 2))

# ✅ Deberías ver: ~370 personas por mes y tasas de 4,33 / 3,89 / 3,71.
#    El trimestre completo (1.116 personas) da 3,96. Con un mes solo, la
#    cifra salta 0,6 puntos sin que haya pasado nada: es ruido de muestra.
#    Por eso el INE junta tres meses antes de publicar.
#
# TODO: escribe UNA frase interpretando esa brecha. Recuerda: "se asocia",
#       nunca "causa".
# ____________________________________________________________________________


# -----------------------------------------------------------------------------
# PASO 9 — Guardar el resultado y dejar la bitácora
# -----------------------------------------------------------------------------
# El archivo de data/raw/ NO se toca nunca. Lo que construiste es un producto
# nuevo y va a data/processed/.

dir.create("data/processed", showWarnings = FALSE)
write.csv(empleo, "data/processed/ene_2026_mjj_limpia.csv", row.names = FALSE)

file.exists("data/processed/ene_2026_mjj_limpia.csv")

# ✅ Deberías ver: TRUE

# -----------------------------------------------------------------------------
# BITÁCORA DE LIMPIEZA — complétala. Sin esto, el trabajo no es reproducible.
# -----------------------------------------------------------------------------
# 1. Lectura: usé ____________ porque el archivo trae separador ______ y
#    decimales con ______.
# 2. Variables conservadas: ______________________________________ (de 222).
# 3. Recodifiqué: sexo y activ a texto, con case_when().
# 4. Faltantes estructurales encontrados: ____ filas en `activ`, que son
#    ______________________. Decisión: ____________________________
# 5. Centinelas encontrados: ______ y ______ en `habituales`.
#    Decisión: ____________________  Efecto en el promedio: de ____ a ____ h.
# 6. Outliers restantes (hasta 126 h): decisión ________________________
# 7. Ponderación: usé ____________ porque _______________________________
# 8. Resultado principal: tasa de desocupación nacional = ______ %


# -----------------------------------------------------------------------------
# AUTOEVALUACIÓN antes de entregar
# -----------------------------------------------------------------------------
# [ ] No quedan ____ sin completar.
# [ ] El script corre completo de una vez (reinicia R y córrelo entero).
# [ ] Puedo explicar por qué read.csv() devolvía una sola columna.
# [ ] Distingo el faltante estructural del faltante real, con un ejemplo de cada uno.
# [ ] Encontré los DOS centinelas, no solo el que salta en summary().
# [ ] Mi tasa nacional ponderada da 9,53 % y sé por qué difiere de contar filas.
# [ ] La bitácora está completa: otra persona podría repetir mi limpieza.
# [ ] No modifiqué nada dentro de data/raw/.


# -----------------------------------------------------------------------------
# ENTREGA (A3, formativa): sube este script a tu repositorio de GitHub.
# Commit sugerido: "Lab S6: limpieza de la ENE 2026 MJJ del INE"
#
# Este laboratorio es el ensayo del Proyecto Final: allá vas a bajar TUS datos,
# de una fuente que tú elijas, y nadie te va a decir dónde están los centinelas.
# -----------------------------------------------------------------------------
