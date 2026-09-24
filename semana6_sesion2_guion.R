# =============================================================================
# GUION DE CLASE — Semana 6 · Sesión 2: Datos Faltantes, Centinelas y Ponderación
# Fundamentos de Programación para Análisis Económico · UdeC-EAN
#
# Nombre: Constanza Pinilla      Fecha: Septiembre 2026
#
# CÓMO USAR: corre cada línea con Cmd/Ctrl+Enter.
#   🔵 CORRE Y OBSERVA · ✏️ COMPLETA (____) · 🔮 PREDICE · 🟢 TU TURNO · ✅ Deberías ver
#
# ⚠️ HOY NO HAY ARCHIVO DE JUGUETE. Trabajamos con la Encuesta Nacional de
#    Empleo del INE, tal como se descarga: 37 MB, 222 columnas, 97.946 personas.
#    Nadie la preparó para ti. Ese es el punto.
# =============================================================================

library(dplyr)


# -----------------------------------------------------------------------------
# BLOQUE A — Abrir un archivo real
# -----------------------------------------------------------------------------
# 🔮 PREDICE: es un .csv, así que usas read.csv(). ¿Cuántas columnas esperas? ____

intento <- read.csv("data/raw/ene-2026-06-mjj.csv", nrows = 5)
ncol(intento)

# ✅ Deberías ver: 1
#
# ⚠️ UNA sola columna. El archivo no está roto: read.csv() supone que el
#    separador es la coma, y el INE separa con punto y coma. Toda la fila
#    entró como un solo texto.

# ✏️ COMPLETA: usa la variante que entiende separador ";" y decimales con coma.
ene <- read.csv2("data/raw/ene-2026-06-mjj.csv")
dim(ene)

# ✅ Deberías ver: 97946 222
#
# 💡 read.csv2() arregla DOS cosas a la vez: el separador y la coma decimal.
#    Si solo arreglas el separador, los números con coma llegan como texto y
#    mean() falla sin decirte por qué.

# 🔵 CORRE Y OBSERVA — 222 columnas: aquí los selectores (S5) son indispensables
names(select(ene, starts_with("cae")))

# ✏️ COMPLETA: quédate con las 7 columnas que usaremos hoy.
#    region · sexo · edad · activ · habituales · efectivas · fact_cal
empleo <- select(ene, region, sexo, edad, activ, habituales, fact_cal, efectivas)
rm(ene)
dim(empleo); gc()


# ✅ Deberías ver: 97946 7
#
# 💡 activ = condición de actividad (1 ocupado · 2 desocupado · 3 inactivo)
#    habituales / efectivas = horas semanales
#    fact_cal = FACTOR DE EXPANSIÓN: cuántos chilenos representa esa fila

# -----------------------------------------------------------------------------
# 📖 LEER EL ARCHIVO ANTES DE USARLO: las cuatro columnas de tiempo
# -----------------------------------------------------------------------------
# La ENE no es una foto de un mes: es un TRIMESTRE MÓVIL. Por eso trae dos
# pares de columnas que parecen redundantes y no lo son.
#
#   ano_trimestre / mes_central  -> A QUÉ TRIMESTRE pertenece la base.
#                                   mes_central = 6 significa "mayo - julio"
#                                   (el mes CENTRAL de los tres es junio).
#                                   El nombre del archivo lo codifica:
#                                   ene-2026-06-mjj = año 2026, trimestre 06,
#                                   iniciales mayo-junio-julio.
#
#   ano_encuesta / mes_encuesta  -> EN QUÉ MES se entrevistó a ESA persona.
#                                   Toma 5, 6 o 7: cada mes se entrevista a
#                                   un tercio de la muestra (~32.000 personas).
#
# 🔵 CORRE Y OBSERVA — la primera es constante, la segunda no:
table(ene$mes_central)
table(ene$mes_encuesta)

# ✅ Deberías ver: mes_central solo tiene 6 (97.946 veces). mes_encuesta se
#    reparte en 5, 6 y 7 (~32.000 cada uno).
#
# 💡 Consecuencia práctica: la "tasa de desocupación de junio 2026" que sale
#    en la prensa es en realidad la del trimestre mayo-julio. Y si comparas
#    esta base con la de mes_central = 7 (junio-agosto), dos de sus tres
#    meses se SOLAPAN. Los trimestres móviles consecutivos no son
#    independientes.

# 🔮 PREDICE: si cada mes se entrevista a ~32.000 personas, ¿por qué el INE
#    no publica la tasa de desocupación MES A MES en vez de juntar tres? ______

# 🔵 CORRE Y OBSERVA — Aysén, la región más chica, mes a mes y en el trimestre
ft <- filter(ene, activ %in% 1:2)               # fuerza de trabajo
table(ft$mes_encuesta[ft$region == 11])          # ¿cuántos entrevistados por mes?

ft |>
  filter(region == 11) |>
  group_by(mes_encuesta) |>
  summarise(n = n(),
            tasa = round(100 * sum(fact_cal[activ == 2]) / sum(fact_cal), 2))

# ✅ Deberías ver: ~370 personas por mes, y una tasa que salta de 4,33 a 3,71
#    entre un mes y el siguiente. Con los tres meses juntos: 1.116 personas
#    y 3,96 %.
#
# 💡 POR QUÉ EL INE ARMA UN TRIMESTRE MÓVIL (y no publica mes a mes):
#
#    1. PRECISIÓN. Un mes solo tiene ~370 personas en Aysén. Con tan pocas,
#       la tasa se mueve 0,6 puntos de un mes a otro sin que haya cambiado
#       nada real: es ruido de muestreo. Tres meses juntos triplican la
#       muestra y estabilizan la cifra. En las regiones chicas, un solo mes
#       no da para nada publicable.
#
#    2. OPORTUNIDAD. Si el INE esperara a tener un trimestre "cerrado"
#       (ene-mar, abr-jun...), publicaría 4 cifras al año. Con la ventana
#       MÓVIL publica 12: cada mes entra uno nuevo y sale el más viejo.
#       Es la manera de tener frecuencia mensual con precisión trimestral.
#
#    3. OPERACIÓN. El trabajo de campo es continuo: cada mes se visita a un
#       tercio de la muestra. Por eso mes_encuesta existe: registra cuándo
#       se levantó CADA dato, aunque la base se publique como un trimestre.
#
#    La misma lógica vale para toda encuesta continua (ENE, ENUSC, EPF).
#    El costo es el solapamiento: dos trimestres seguidos comparten dos
#    meses, así que NO son observaciones independientes. Nunca hay que tratar
#    la serie de trimestres móviles como si fueran 12 datos separados al año.
#
#    ¿Y por qué DOS años (ano_trimestre y ano_encuesta)? Porque un trimestre
#    puede cruzar el cambio de año: el "diciembre-febrero" (mes_central = 1)
#    tiene entrevistas de diciembre de un año y de enero-febrero del
#    siguiente. En esta base coinciden (mayo-julio no cruza), pero la columna
#    existe para cuando no.

# -----------------------------------------------------------------------------
# 📖 ¿DÓNDE ESTÁ LA CONDICIÓN DE ACTIVIDAD? (activ vs b1, b2, ...)
# -----------------------------------------------------------------------------
# La ENE tiene DOS tipos de columnas, y confundirlas es un error frecuente:
#
#   1. RESPUESTAS AL CUESTIONARIO, nombradas por sección y número de pregunta:
#      a1, a2, ... (sección A) · b1, b2, ... (sección B: ocupados) ·
#      c1, ... · e1, ... Cada sección se le pregunta solo a quien corresponde.
#      b1 NO es "si trabaja o no": es "grupo de ocupación según CIUO-08", y
#      solo la responden quienes YA fueron clasificados como ocupados.
#
#   2. INDICADORES CONSTRUIDOS por el INE a partir de esas respuestas, al
#      FINAL del archivo (columnas 203 a 222): pet, ft, activ, cae_general,
#      categoria_ocupacion, fact_cal...
#      Estos son los que se usan para clasificar y para reproducir las cifras
#      oficiales. Son el resultado del algoritmo del INE, no una pregunta.
#
# activ es la columna 221 de 222 (por eso cuesta verla en la cabecera):
which(names(ene) == "activ")

# 🔵 CORRE Y OBSERVA — b1 solo existe para quienes activ dice que son ocupados
table(activ = ene$activ, tiene_b1 = !is.na(ene$b1), useNA = "ifany")

# ✅ Deberías ver: en activ = 1 hay 41.850 con b1; en 2, 3 y NA, b1 es
#    siempre NA. b1 depende de activ, no al revés.
#
# 💡 REGLA: para saber QUIÉN está ocupado, desocupado o inactivo, se usa
#    activ (o cae_general, que es la versión fina en 10 categorías). Las
#    columnas b* describen CÓMO es el empleo de quien ya sabemos que trabaja.


# -----------------------------------------------------------------------------
# BLOQUE B — Tipos de faltantes: NA, NaN, Inf... y el que no es un problema
# -----------------------------------------------------------------------------
# 🔵 CORRE Y OBSERVA — tres cosas distintas que se parecen
c(0/0, 1/0, -1/0)     # NaN, Inf, -Inf
NA + 5                # el NA se "contagia" a toda operación

# ✅ Deberías ver: NaN Inf -Inf  y luego NA

# 🔵 CORRE Y OBSERVA — la ENE tiene 15.305 personas sin condición de actividad
table(empleo$activ, useNA = "ifany")

# 🔮 PREDICE: ¿son datos perdidos? ¿Quiénes crees que son? personas menores

# ✏️ COMPLETA: mira la edad de los que tienen activ en NA.
summary(empleo$edad[is.na(empleo$activ)])

# ✅ Deberías ver: máximo 14 años.
#
# 💡 NINGUNO pasa de 14. No falta el dato: la pregunta NO SE LES HIZO, porque
#    no están en edad de trabajar. Eso es un faltante ESTRUCTURAL.
#
# ⚠️ LA DISTINCIÓN QUE CAMBIA TODO:
#      estructural -> la pregunta no aplica -> se EXCLUYE del denominador
#      real        -> había respuesta y se perdió -> se elimina o imputa, con criterio
#    Imputar un estructural es declarar desocupado a un niño de 8 años.


# -----------------------------------------------------------------------------
# BLOQUE C — Detectar y medir
# -----------------------------------------------------------------------------
# ✏️ COMPLETA: cuántas horas faltan, y qué proporción (patrón sum/mean de S3).
sum(is.na(empleo$habituales))
mean(is.na(empleo$habituales))

# ✅ Deberías ver: 56096 y 0.5727
#
# 🔮 PREDICE: ¿el 57 % de faltantes en horas es estructural o real? ¿Quiénes son? ____

# 🔵 CORRE Y OBSERVA — el mapa completo: NA por columna
colSums(is.na(empleo))
# ✅ Deberías ver: solo activ (15305) y las dos de horas (56096). El resto, 0.

sum(complete.cases(empleo))    # filas sin NINGÚN NA
nrow(na.omit(empleo)) # eliminó todo lo que no está completo

# ✅ Deberías ver: 41850
#
# 🔮 PREDICE: 41850 es exactamente el número de OCUPADOS. ¿Casualidad? ________


# -----------------------------------------------------------------------------
# BLOQUE D — Eliminar vs imputar (y por qué las dos pueden salir mal)
# -----------------------------------------------------------------------------
# 🔵 CORRE Y OBSERVA — la opción "fácil": botar toda fila con algún NA
sin_na <- na.omit(empleo)
nrow(sin_na)
table(sin_na$activ)

# ✅ Deberías ver: 41850 filas, y en la tabla SOLO el código 1.
#
# ⚠️ MIREN BIEN. na.omit() borró a los 4.412 desocupados, porque un desocupado
#    no tiene horas trabajadas. La tasa de desocupación de esa tabla es 0 %.
#    Eliminar filas con faltantes borró exactamente al grupo que queríamos
#    estudiar. na.omit() sobre un archivo que no conoces es peligroso.

# 🔵 CORRE Y OBSERVA — la otra opción: imputar con la mediana
imputado <- empleo |>
  mutate(habituales = if_else(is.na(habituales),
                              median(habituales, na.rm = TRUE),
                              habituales))
sum(is.na(imputado$habituales))
# imputado -> inventar un dato que está perdido

# ✅ Deberías ver: 0
#
# 🔮 PREDICE: ¿qué acabamos de hacer con las 56.096 personas que NO trabajan? ____
#
# ⚠️ Les asignamos 42 horas semanales. El código corrió sin quejarse.
#    Imputar sobre un faltante REAL puede justificarse; sobre uno ESTRUCTURAL, nunca.
#
# 💡 REGLA DEL CURSO: no hay respuesta única, hay criterio. Y toda decisión
#    sobre faltantes se DOCUMENTA.


# -----------------------------------------------------------------------------
# BLOQUE E — Outliers y centinelas
# -----------------------------------------------------------------------------
# 🔵 CORRE Y OBSERVA — ¿el máximo es sospechoso?
summary(empleo$habituales)

# 🔮 PREDICE: la semana tiene 168 horas. ¿Qué es un 999? ____________________

# ✏️ COMPLETA: la regla del IQR (recap quantile(), S4).
q    <- quantile(empleo$habituales, c(.25, .75), na.rm = TRUE)
iqr  <- q[2] - q[1]
tope <- q[2] + 1.5 * iqr
c(Q1 = q[[1]], Q3 = q[[2]], tope = tope[[1]])
sum(empleo$habituales > tope, na.rm = TRUE)

# ✅ Deberías ver: Q1 35 | Q3 44 | tope 57.5  y  2123 candidatos a outlier
#
# 💡 Guarda ese 2123. En un momento vas a descubrir que una parte de esos
#    "outliers" ni siquiera son datos.

# 🔵 CORRE Y OBSERVA — el 999 es un CÓDIGO, no una jornada
horas <- empleo$habituales[empleo$activ == 1]
c(con_999 = mean(horas, na.rm = TRUE),
  sin_999 = mean(horas[horas != 999], na.rm = TRUE))

c(con_888 = mean(horas, na.rm = TRUE),
  sin_888 = mean(horas[horas != 888], na.rm = TRUE))

# ✅ Deberías ver: 40.33 y 40.14
#
# 🔮 PREDICE: el 999 movió el promedio 0,2 horas. ¿Ya está limpio? ____________

# 🟢 TU TURNO: no mires solo el máximo. Mira TODA la cola alta, ordenada por frecuencia.
sort(table(empleo$habituales[empleo$habituales > 888]), decreasing = TRUE)

# ✅ Deberías ver que 999 aparece 8 veces... y 888 aparece 103.
#
# ⚠️ HAY DOS CENTINELAS. El segundo está escondido en la cola y es 13 veces
#    más frecuente. Quien limpia solo el que salta en summary() sigue con el
#    promedio mal. Eso lo terminan en el laboratorio.
#
# 💡 Los clásicos: 99 · 999 · 9999 · 888 · -9 · -99 · NS/NR.
#    La documentación de la encuesta dice qué significa cada uno. Leerla es
#    parte del trabajo.

#' la comilla para seguir comentando
#'
#'
#'

# -----------------------------------------------------------------------------
# BLOQUE F — Una fila no vale una persona
# -----------------------------------------------------------------------------
# La ENE es una MUESTRA. fact_cal dice cuántos chilenos representa cada fila.
head(empleo$fact_cal, 3)

# factor de expansion es un factor calculado, es cuanto porcentaje esta representando esa persona al grupo


# ✅ Deberías ver: 1079.26  80.72  94.99  <- la primera fila vale por 1.079 personas

# ✏️ COMPLETA: la tasa de desocupación, contando PERSONAS y no filas.
#    Definición: desocupados / (ocupados + desocupados)
ocupados    <- sum(empleo$fact_cal[empleo$activ == 1], na.rm = TRUE)
sum(empleo$activ[empleo$activ == 1], na.rm = TRUE)
desocupados <- sum(empleo$fact_cal[empleo$activ == 2], na.rm = TRUE)
sum(empleo$fact_cal[empleo$activ == 2], na.rm = TRUE)
round(100 * desocupados / (ocupados + desocupados), 2) #tasa de desocupación
# los 41850 esta representando a 9314017 personas.

# ✅ Deberías ver: 9.53
# es la tasa de desocupación real

# 💡 Ese es el número que sale en la prensa, y acabas de calcularlo desde el
#    archivo crudo.

# 🟢 TU TURNO: ¿cuántas personas hay en la fuerza de trabajo de Chile?
format(round(ocupados + desocupados), big.mark = ".", decimal.mark = ",")

# ✅ Deberías ver: 10.295.061  (no las 46.262 filas de la muestra)
#
# 🔮 PREDICE: sin ponderar, la tasa da 9,54 %. Ponderando, 9,53 %. Casi lo
#    mismo. ¿Eso autoriza a ignorar el ponderador? ______________________
#
# ⚠️ No. La diferencia no está en la tasa, está en el NIVEL: sin fact_cal
#    reportarías 4.412 desocupados en Chile, en vez de 981.044. Y las tasas
#    sí se mueven apenas comparas regiones muestreadas con intensidad distinta.


# -----------------------------------------------------------------------------
# CIERRE — read.csv2() para lo real · estructural vs real · na.omit() borra
#          al grupo de interés · centinelas: mirar la cola, no el máximo ·
#          fact_cal: contar personas, no filas.
# Al laboratorio (2h): el ciclo completo sobre este mismo archivo, desde
#   documentar la fuente hasta la tasa de desocupación de TU región.
# =============================================================================
