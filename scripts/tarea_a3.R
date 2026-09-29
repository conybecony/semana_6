
# Constanza Pinilla 
# Septiembre 2026
# ¿Qué hace?:

#1 Pregunta: 
# ¿Existen diferencias en la tasa de desocupación según el grupo etario de las 
# personas en Chile durante el trimestre móvil mayo-julio de 2026?

# FUENTE:            Instituto Nacional de Estadísticas (INE), Chile
# ENCUESTA:          Encuesta Nacional de Empleo (ENE)
# TRIMESTRE MÓVIL:   mayo - junio - julio 2026
# ARCHIVO:           ene-2026-06-mjj.csv
# ENLACE:            https://www.ine.gob.cl/estadisticas-por-tema/mercado-laboral/ocupacion-y-desocupacion
# FECHA DE DESCARGA: 22-09-2026
# UNIDAD DE OBSERVACIÓN: personas  



library(dplyr)

#2
ene <- read.csv2("data/raw/ene-2026-06-mjj.csv")
# Se utiliza read.csv2() porque la base de datos usa ";" para separar las columnas
# y "," para los decimales.

desocupacion_edad <- select(ene, edad, activ, starts_with("fact"))

#3
desocupacion_edad <- desocupacion_edad |>
  mutate(
    grupo_edad = case_when(
      edad >= 15 & edad <= 24 ~ "15-24 años",
      edad >= 25 & edad <= 34 ~ "25-34 años",
      edad >= 35 & edad <= 44 ~ "35-44 años",
      edad >= 45 & edad <= 54 ~ "45-54 años",
      TRUE                    ~ "55 años o más"
      ),
    situacion = case_when(
      activ == 1 ~ "Ocupado",
      activ == 2 ~ "Desocupado",
      activ == 3 ~ "Inactivo",
      TRUE       ~ "Fuera de edad de trabajar"  
    )
  )

table(desocupacion_edad$grupo_edad, useNA = "ifany")
table(desocupacion_edad$situacion, useNA = "ifany")

#4
sum(is.na(desocupacion_edad$edad))      # 0 NA
sum(is.na(desocupacion_edad$activ))     # 15305 NA
summary(desocupacion_edad$edad[is.na(desocupacion_edad$activ)]) # edad max 14 años
sum(is.na(desocupacion_edad$fact_cal))  # 0 NA

# ¿se perdió el dato, o nunca existió? Es un faltante estructural en la variable
# activ,ya que la pregunta no se les hizo porque no están en edad de trabajar.

sin_na <- na.omit(desocupacion_edad)
nrow(sin_na) # 82641
table(sin_na$activ)

# na.omit() borró a los 15305 desocupados, la tasa de desocupación 
# de esa tabla es 0 %, eliminar filas con faltantes borró exactamente al grupo 
# que queríamos estudiar.

#5 por hacer
sort(table(desocupacion_edad$activ), decreasing = TRUE)
sort(table(desocupacion_edad$edad), decreasing = TRUE)
sort(table(desocupacion_edad$fact_cal), decreasing = TRUE)
#  no se ven variables centinelas
#6
desocupacion_edad |>
  filter(!is.na(grupo_edad), !is.na(activ)) |>
  group_by(grupo_edad) |>
  summarise(
    N = n(),
    desocupados = sum(activ == 2),
    fuerza_trabajo = sum(activ %in% c(1, 2)),
    tasa_desocupacion = desocupados / fuerza_trabajo * 100
  ) |> 
  arrange(desc(desocupados))


desocupacion_edad |>
  filter(!is.na(grupo_edad), !is.na(activ), !is.na(fact_cal)) |>
  group_by(grupo_edad) |>
  summarise(
    N = n(),
    desocupados = sum(fact_cal[activ == 2]),
    fuerza_trabajo = sum(fact_cal[activ %in% c(1, 2)]),
    tasa_desocupacion = desocupados / fuerza_trabajo * 100 
    )|> 
      arrange(desc(desocupados))

# Al utilizar fact_cal, la tasa cambió respecto del cálculo sin ponderar,
# ya que el factor de expansión permite que los resultados representen
# a la población objetivo de la ENE

#7

# El grupo de 25-34 años presenta 310.134 desocupados, seguido por el grupo de 
# 35-44 años, con 207.304. En el grupo de 15-24 años se registran 167.334 desocupados,
# mientras que en los grupos de 45-54 años y 55 años o más se registran 154.490 y 141.781,
# respectivamente. Estos resultados muestran que la desocupación se asocia a que 
# entre los 25 y 44 años es donde más desocupados hay.
# Una limitación es que los datos corresponden únicamente al trimestre móvil
# mayo-junio-julio de 2026.

#8
dir.create("data/processed", showWarnings = FALSE)
write.csv(desocupacion_edad, "data/processed/ene_a3.csv", row.names = FALSE)
file.exists("data/processed/ene_a3.csv")

# bitácora

# 1. Lectura:
# Se utilizó read.csv2() para leer la base ene-2026-06-mjj.csv,
# debido al formato de separación utilizado por la base.

# 2. Columnas conservadas:
# Se conservaron las variables edad, activ y fact_cal,
# además de la variable grupo_edad creada para el análisis.

# 3. Recodificaciones:
# Se creó la variable grupo_edad para clasificar a las personas
# según su rango etario.

# 4. Faltantes:
# Se revisaron los valores NA de la variable activ.
# Los valores faltantes corresponden a casos donde la variable
# no presenta información aplicable. No se imputaron estos valores.

# 5. Centinela revisar
# Se revisaron los valores de las variables mediante tablas de frecuencia
# para identificar posibles valores centinela, no se encontraron valores.

# 6. Ponderación:
# Para obtener los resultados finales se utilizó fact_cal como factor
# de expansión, permitiendo que los resultados representaran a la población.

# 7. Filas iniciales y finales:
# Filas iniciales: nrow(ene)
# Filas finales: nrow(desocupacion_edad)
