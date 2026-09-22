# =============================================================================
# GUION DE CLASE — Semana 6 · Sesión 1: Pivoteo de Datos (tidyr)
# Fundamentos de Programación para Análisis Económico · UdeC-EAN
#
# Nombre: Constanza Pinilla      Fecha: Septiembre 2026
#
# CÓMO USAR: corre cada línea con Cmd/Ctrl+Enter.
#   🔵 CORRE Y OBSERVA · ✏️ COMPLETA (____) · 🔮 PREDICE · 🟢 TU TURNO · ✅ Deberías ver
# =============================================================================

# 🔵 CORRE Y OBSERVA — hoy se suma tidyr, el paquete que cambia la FORMA de una tabla
library(dplyr)
library(tidyr)

# Este script se ejecuta desde la RAÍZ del proyecto (no desde guiones_clase/)
ingresos_wide <- read.csv("data/raw/ingresos_wide.csv")

ingresos_wide |> glimpse()

dim(ingresos_wide)

# ✅ Deberías ver: 4 regiones en filas, y los años 2020-2022 como COLUMNAS. 4 x 4.
# 💡 Así vienen los datos de casi toda institución (INE, Banco Central, BID):
#    una columna por año. Se llama formato ANCHO (wide). Cómodo para leer,
#    incómodo para analizar.


# -----------------------------------------------------------------------------
# BLOQUE A — pivot_longer(): de ancho a largo
# -----------------------------------------------------------------------------
# 🔵 CORRE Y OBSERVA — la forma completa, con los tres argumentos explícitos
ingresos_wide |>
  pivot_longer(
    cols      = c(a2020, a2021, a2022),   # columnas a "apilar"
    names_to  = "anio",                   # nueva columna con las etiquetas
    values_to = "ingreso"                 # nueva columna con los valores
  )

# ✅ Deberías ver: 12 filas x 3 columnas. Cada región aparece 3 veces (una por año).
#
# 🔮 PREDICE: ¿por qué 12 filas y no 4? porque ahora los años es una columna, entonces
# se repiten las regiones
#
# 💡 La información es LA MISMA. Solo cambió la forma: lo que antes era un
#    encabezado de columna (a2020) ahora es un VALOR dentro de una celda.

# ✏️ COMPLETA: lo mismo, pero eligiendo las columnas por PATRÓN (Semana 5).
ingresos_wide |>
  pivot_longer(cols = starts_with("a"), names_to = "anio", values_to = "ingreso")

# ✅ Deberías ver: exactamente la misma tabla de 12 x 3.


# -----------------------------------------------------------------------------
# BLOQUE B — Limpiar los nombres al pivotar
# -----------------------------------------------------------------------------
# 🔮 PREDICE: en la tabla de arriba, la columna `anio` dice "a2020", no 2020.
#    ¿De qué tipo es esa columna? ¿Se puede restar 2022 - 2020 con ella? ______

# 🔵 CORRE Y OBSERVA — names_prefix quita el prefijo; as.numeric lo vuelve número
ingresos_long <- ingresos_wide |>
  pivot_longer(cols = starts_with("a"),
               names_to = "anio", values_to = "ingreso",
               names_prefix = "a") |>       # quita la "a": "a2020" -> "2020"
  mutate(anio = as.numeric(anio))           # y a número (Semana 3)

ingresos_long
class(ingresos_long$anio)

# ✅ Deberías ver: anio como 2020, 2021, 2022 (sin la "a") y class = "numeric"
#
# ⚠️ GOTCHA: sin names_prefix la columna queda como TEXTO ("a2020"). Con texto
#    no se puede ordenar cronológicamente ni calcular diferencias de años.
#    Es el paso que más se olvida.


# -----------------------------------------------------------------------------
# BLOQUE C — Por qué el formato largo gana
# -----------------------------------------------------------------------------
# En formato largo, TODO lo de la Semana 5 funciona directo.
# ✏️ COMPLETA: ingreso promedio por año.
ingresos_long |>
  group_by(anio) |>
  summarise(ingreso_medio = mean(ingreso))

# ✅ Deberías ver: 2020 = 415000 | 2021 = 443750 | 2022 = 482500

# 🟢 TU TURNO: ahora el ingreso promedio por REGIÓN (todos los años juntos).
ingresos_long |>
  group_by(region) |>
  summarise(ingreso_medio = mean(ingreso))

# ✅ Deberías ver: Biobío la más alta (563333), Araucanía la más baja (378333)
#
# 🔮 PREDICE: intenta calcular el promedio por año sobre `ingresos_wide`
#    (la tabla ANCHA). ¿Qué tendrías que escribir? ¿Es cómodo? ______________
#
# 💡 Esa es la razón de pivotar: en formato ancho, "año" no es una variable
#    sino tres columnas, y group_by() no puede agrupar por algo que no es columna.


# -----------------------------------------------------------------------------
# BLOQUE D — pivot_wider(): la operación inversa
# -----------------------------------------------------------------------------
# 🔵 CORRE Y OBSERVA — de largo a ancho: los años vuelven a ser encabezados
ingresos_long |>
  pivot_wider(names_from  = anio,      # qué columna se vuelve encabezados
              values_from = ingreso)   # qué columna llena las celdas

# ✅ Deberías ver: la tabla original de 4 x 4 (con 2020, 2021, 2022 sin la "a")
#
# 💡 pivot_longer() y pivot_wider() son INVERSAS: ida y vuelta sin perder nada.

# 🔵 CORRE Y OBSERVA — el caso típico: una tabla de resultados para un informe
ingresos_long |>
  group_by(region, anio) |>
  summarise(ingreso = mean(ingreso), .groups = "drop") |>
  pivot_wider(names_from = anio, values_from = ingreso)

# 💡 REGLA DEL CURSO: se ANALIZA en formato largo, se PRESENTA en formato ancho.
#    El lector humano quiere años en columnas. R quiere años en filas.

ingresos_wide |> 
  pivot_longer(starts_with("a"), names_to = "anio",
               values_to = "ingreso", names_prefix = "a") |> 
  mutate(anio = as.numeric(anio)) |> 
  group_by(region) |> 
  summarise(crecimiento = last(ingreso)/first(ingreso)-1)



# -----------------------------------------------------------------------------
# BLOQUE E — Pivotar habilita el análisis temporal
# -----------------------------------------------------------------------------
# ✏️ COMPLETA: crecimiento del ingreso 2020 -> 2022 por región.
#    Pista: last() y first() toman el último y el primer valor del grupo.
ingresos_long |>
  group_by(region) |>
  summarise(crecimiento = last(ingreso) / first(ingreso) - 1)

# ✅ Deberías ver: Ñuble 0.184 | Maule 0.175 | Biobío 0.173 | Araucanía 0.111
#
# 🔮 PREDICE: ¿por qué first() y last() dan el orden correcto? ¿Qué pasaría
#    si `anio` no estuviera ordenado? ____________________________________
#
# 💡 Este cálculo es IMPOSIBLE en formato ancho sin escribir a mano
#    (a2022 - a2020) / a2020. Y si mañana llega a2023, hay que reescribirlo.


# -----------------------------------------------------------------------------
# BLOQUE F — separate() y unite(): una columna con varios datos adentro
# -----------------------------------------------------------------------------
# Un caso frecuente: la institución pegó dos variables en una sola celda.
datos <- data.frame(grupo   = c("Agricultura_Ñuble", "Servicios_Biobío", "Comercio_Maule"),
                    ingreso = c(450000, 920000, 220000))
datos

# ✏️ COMPLETA: separa `grupo` en `sector` y `region`, cortando por "_".
datos |> separate(grupo, into = c("sector", "region"), sep = "_")

# ✅ Deberías ver: dos columnas nuevas, sector y region, y ya no existe `grupo`.

# 🟢 TU TURNO: haz el camino inverso. Separa y vuelve a unir, pero con "-".
datos |>
  separate(grupo, into = c("sector", "region"), sep = "_") |>
  unite("grupo", sector, region, sep = "-")

# ✅ Deberías ver: "Agricultura-Ñuble" y "Servicios-Biobío"
#
# 💡 separate() es lo que usarás cuando una columna traiga "2024-03" (año-mes),
#    "13-101" (región-comuna) o "M-25" (sexo-edad).


# -----------------------------------------------------------------------------
# CIERRE — wide (ancho, para leer) · long (largo, para analizar) ·
#          pivot_longer() / pivot_wider() son inversas ·
#          separate() / unite() para columnas mezcladas.
# Puente Sesión 2: hasta hoy los datos tenían la forma incómoda pero estaban
#   COMPLETOS. Los reales no: vienen con faltantes, códigos raros y valores
#   imposibles. Y los vamos a abrir tal como los publica el INE.
# =============================================================================
