#
# PROJECT: RISK OF FRACTURE UNDER COMPETING RISK
# IP: Javier Mar Medina. Osakidetza Arrasate-Mondragón. Colaboradores: Igor ...
# enero 2023


# Objetivo general: hacer un modelo predictivo de fractura de cadera con la 
# muerte como riesgo competitivo. Utilizar modelos de Fine-Grey.


# Objetivo particular de este fichero:
# Organizar la base de datos con la que vamos a trabajar.
# Cada paciente tiene que ser una fila. 
# En columnas: edad, sexo, fecha de muerte, nacionalidad, IMC, nivel de renta,
#   estancia en residencia, diagnóstico (muerte y fractura), otros diagnosticos
#   (calcular CCI), prescripciones (medicamentos)
# 
# Para diágnósticos repetidos, poner solo fecha del primer diagnóstico

# LIBRERIAS
# lubridate: para jugar con fechas
# dplyr: juntar tablas (dataframes)


# library("lubridate")
library("dplyr")

# Directorio donde tengo los datos de Igor
setwd("/home/rblasco/Documents/mondragon/predict_fracture_risk/")


# ----------------- 1ª Parte ------------------------------.-------------
# Vamos a unificar los archivos de Población Diana, con info de edad, sexo y
# fallecimiento (principalmente)
# -------------------------------------------------------------------------

#Primer archivo con info de Sexo, Mortalidad, y hospital
PobDianaAH <- read.csv(
  "Igor_Fracturas_cadera/OBI/modificados/CADERA_01_PoblacionDiana_AH_mod.csv")
PobDianaAP <- read.csv(
  "Igor_Fracturas_cadera/OBI/modificados/CADERA_01_PoblacionDiana_AP_mod.csv")
PobDianaOG <- read.csv(
  "Igor_Fracturas_cadera/OBI/modificados/CADERA_01_PoblacionDiana_OG_mod.csv")

# Junto las tablas. 
# Como las tres tienen las mismas 10 columnas, no me hace falta especificar "by".
# "all" indica que no quiero solamente los que aparecen repetidos en ambas tablas,
# si no todos (los comunes y no comunes)
PobDiana <- merge(merge(PobDianaAP, PobDianaOG, all = TRUE),
                     PobDianaAH, all = TRUE)
# Creo que realmente este merge es redundante, porque el archivo AH ya parece
# tener toda la info. Los otros dos solo repiten algunos elementos.

#compruebo que los elementos en blanco NA en Fecha.fallecimiento corresponden a 
#gente viva. Si no se imprime ningún mensaje está todo bien
for (i in 1:nrow(PobDiana)) {
  if (is.na(PobDiana$Fecha.fallecimiento[i]) & 
      PobDiana$Situacion[i]=="FALLECIDO") {
    print(sprintf("La fila %d es erronea", i))
  }
}
rm(i)

#----------------------Testear la funcion MERGE------------------------------
# 
# PobDiana <- merge(merge(PobDianaAH, PobDianaAP, 
#                         all = TRUE, 
#                         by = c("Id.Paciente", "Tipo.TIS","Sexo","Fecha.nacimiento",
#                                "Situacion", "Indice.privacion","Comarca.OSI",
#                                "UAP","Centro")),
#                   PobDianaOG,
#                   all = TRUE, 
#                   by = c("Id.Paciente", "Tipo.TIS","Sexo","Fecha.nacimiento",
#                          "Situacion", "Indice.privacion","Comarca.OSI",
#                          "UAP","Centro"))
#
# # Comparo las fechas de fallecimiento de los tres bases de datos
# # Me devuelve un vector con TRUE, FALSE or NA
# igualdad1 <- PobDiana$Fecha.fallecimiento.x == PobDiana$Fecha.fallecimiento.y
# # Transformo los NA por FALSE
# for (i in 1:length(igualdad1)) {
#   if (is.na(igualdad1[i])) {
#     igualdad1[i] <- FALSE
#   }
# }
# # Creo un vector con los número de fila en los que no hay coincidencia
# FilasDiferentes1 <- which(igualdad1 != rep(TRUE, length(igualdad1)))
# # Extraigo de PobDiana los pacientes (filas) que tienen discrepancia entre las 
# # fechas de fallecimiento. Creo otro Data Frame solo con dos columnas 
# PobDianaFallec <- slice(PobDiana, FilasDiferentes1) %>% 
#                   select(Fecha.fallecimiento.x, Fecha.fallecimiento.y)
# PobDianaTryFallec <- slice(PobDianaTry, FilasDiferentes1) %>% select(Fecha.fallecimiento)
# 
# 
# # Vamos a comprobar si en PobDianaTry hay alguna fecha de fallecimiento NA
# all(PobDiana$Fecha.fallecimiento.x == PobDianaTry$Fecha.fallecimiento)
# i <-1
# while (i < nrow(PobDiana) & !is.na(PobDiana$Fecha.fallecimiento[i])) {
#   i <- i+1
# }
# print(i)
# ----------------------Fin del testeo ------------------------------------


#Pongo las fechas en formato fecha
PobDiana$Fecha.fallecimiento <- as.Date(PobDiana$Fecha.fallecimiento)
PobDiana$Fecha.nacimiento <- as.Date(PobDiana$Fecha.nacimiento)
#Añado la columna edad a 1 enero 2010. Basta restar el año de nacimiento
PobDiana$Edad <- 2010 - as.numeric(format(PobDiana$Fecha.nacimiento,'%Y'))
# Me cargo a los que tengan 100 o más años.
# Los ID los guardo en el data frame "mayoresDe100" para poder usarlo más adelante
mayoresDe100 <- data.frame(ID = PobDiana$Id.Paciente[which(PobDiana$Edad >= 100)])
#write.csv(mayoresDe100, "Ruben_fract/Resultados_Ruben/mayoresDe100.csv", row.names = FALSE)
PobDiana <- PobDiana[PobDiana$Edad < 100, ]
nrow(PobDiana) == nrow(PobDianaAH) - nrow(mayoresDe100)

# Mi archivo tiene un mayor número de filas que el de Igor, así que supongo
# que él ha eliminado más paciente que yo, veamos por qué.
PobDianaIgor <- read.csv("Igor_Fracturas_cadera/CADERA_Poblacion_diana.csv", 
                         sep = ";",
                         header = TRUE,
                         stringsAsFactors = FALSE,
                         encoding = "UTF-8")
# # Miro si cada elemento de PobDiana está en el de Igor
# IdMissmatch <- c()
# for (i in 1:nrow(PobDiana)) {
#   if (!PobDiana$Id.Paciente[i] %in% PobDianaIgor$ID.Paciente) {
#     IdMissmatch <- c(IdMissmatch, i)
#   }
# }

# Utilizamos anti_join() para encontrar los índices de las filas de PobDiana que
# no están en PobDianaIgor. NoRepeated será un DataFrame con los pacientes que 
# están en PobDiana, pero que no están en PobDianaIgor
NoRepeated <- anti_join(PobDiana, PobDianaIgor, by = c("Id.Paciente" = "ID.Paciente"))
# Sorprendentemente me sale un Marco de Datos vacío... 
# Supongo entonces que hay elementos repetidos en PobDiana. Vamos a encontrarlos
# Primero obtengo un vector con TRUE, FALSE si están repetidos o no
rm(NoRepeated)
duplicadosLogico <- duplicated(subset(PobDiana, select = "Id.Paciente"))
# Creo otro Marco de Datos con los elementos repetidos
duplicados <- PobDiana[duplicadosLogico, ]
nrow(PobDiana)-nrow(PobDianaIgor) == nrow(duplicados)
# ¡¡BIEN!! Efectivamente hay nrow(duplicados)=127 elementos duplicados en PobDiana
# Vamos a ver si hay alguna diferencia sustancial entre ellos:
PobDiana[PobDiana$Id.Paciente == duplicados$Id.Paciente[106], ]
# Me voy a quedar con el que tenga más información (el que tenga más valores
# NA, o valores en blanco como " ", lo elimino)

for (i in 1:nrow(duplicados)) {
  Iguales <- PobDiana[PobDiana$Id.Paciente == duplicados$Id.Paciente[i], ] #df con los 2 elementos repet
  Iguales$posicion <- which(PobDiana$Id.Paciente == duplicados$Id.Paciente[i]) #añado una columna con la posición que ocupan en PobDiana
  fila_a_eliminar_en_Iguales <- which.min(nchar(apply(Iguales, 1, toString))) # calculo cual tiene menos elementos (los huecos blancos)
  fila_a_eliminar <- Iguales$posicion[fila_a_eliminar_en_Iguales] # selecciono su posición en PobDiana
  PobDiana <- PobDiana[-fila_a_eliminar, ] # lo elimino de PobDiana
}
rm(i, Iguales, fila_a_eliminar_en_Iguales, fila_a_eliminar)

# compruebo que ya no hay ducplicados en PobDiana
all(!duplicated(subset(PobDiana, select = "Id.Paciente")))

# Hemos terminado la primera parte, elimin ahora las variables que ya no me valen
rm(duplicados, duplicadosLogico, NoRepeated, PobDianaAH, PobDianaAP, PobDianaOG,
   PobDianaIgor)

# GUARDO EL ARCHIVO PobDiana
#write.csv(PobDiana, "Ruben_fract/Resultados_Ruben/PobDiana_ruben.csv", row.names = FALSE)


# Para ver el estudio poblacional, abrir el archivo /resultados_Ruben/graficas.r






