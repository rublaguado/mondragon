
#
# PROJECT: RISK OF FRACTURE UNDER COMPETING RISK
# IP: Javier Mar Medina. Osakidetza Arrasate-Mondragón. Colaboradores: Igor ...
# enero 2023

# Objetivo general: hacer un modelo predictivo de fractura de cadera con la 
# muerte como riesgo competitivo. Utilizar modelos de Fine-Grey.

# library("lubridate")
library("dplyr")

# Directorio donde tengo los datos de Igor
setwd("/home/rblasco/Documents/mondragon/predict_fracture_risk/")

# IMPORTANTE: necesitamos el archivo PobDiana de la carpeta de resultados, y el
#             archivo con los ID de las personas mayores de 100 años
PobDiana <- read.csv("Ruben_fract/Resultados_Ruben/PobDiana_ruben.csv")
MayoresDe100 <- read.csv("Ruben_fract/Resultados_Ruben/mayoresDe100.csv")


# Vamos ahora a juntar los archivos de índice de masa corporal (IMC), o BMI por 
#   sus siglas en inglés de "body mass index".Los tenemos en cuatro archivos

# OBJETIVO: Crear un nuevo data frame de diagnósticos con las columnas: Id, alcohol,
#         fechaAlc, tabaco, fechaTabac, fractura, fechaFrac, diagnost1, fecha1, 
#         diagnost2, fecha2, ...
#
# OBJETIVO PARTICULAR: hacer la media del índice de masa corporal por cada 
#         paciente, contar cuántos pacientes faltan por tener esta info, y 
#         contar cuántos están fuera de los límites [15,60]


# -------------------------------------------------------------------------
# -------------------------- 1ª Parte -------------------------------------
# ---------------------- Limpiar los datos -------------------------------
# ------------------------------------------------------------------------

BMI1 <- read.csv(
  "Igor_Fracturas_cadera/OBI/CADERA_07_IMC_01.csv")
BMI2 <- read.csv(
  "Igor_Fracturas_cadera/OBI/CADERA_07_IMC_02.csv")
BMI3 <- read.csv(
  "Igor_Fracturas_cadera/OBI/CADERA_07_IMC_03.csv")
BMI4 <- read.csv(
  "Igor_Fracturas_cadera/OBI/CADERA_07_IMC_04.csv")

# Voy a juntarlos en un solo marco de datos
BMI <- rbind(BMI1, BMI2, BMI3, BMI4)
rm(BMI1, BMI2, BMI3, BMI4)

# Vamos a eliminar los pacientes que tenían 100 años o más. Los filtro por ID.
BMI <- subset(BMI, !(Identificador.de.Paciente %in% MayoresDe100$ID))
rm(MayoresDe100)

all(BMI$Identificador.de.Paciente %in% PobDiana$Id.Paciente)

# Hay filas en blanco (valores NA) así que las elimino.
BMI <- slice(BMI, -which(is.na(BMI$Valor.DBP)))

# De cuántos pacientes tengo info de BMI
length(unique(BMI$Identificador.de.Paciente))
# De cuantos hombres tengo info de BMI
HombresID <- PobDiana$Id.Paciente[which(PobDiana$Sexo == "Hombre")]
sum( unique(BMI$Identificador.de.Paciente) %in% HombresID )
# De cuantas mujeres tengo info de BMI
MujeresID <- PobDiana$Id.Paciente[which(PobDiana$Sexo == "Mujer")]
sum( unique(BMI$Identificador.de.Paciente) %in% MujeresID )


# Me interesa la columna "Valor.DBP". Un valor normal de IMC está entre 15 y 60.
#   Sin embargo, hay valores por debajo de 1 y por encima de 200. A estos valores
#   los voy a llamar "Raros" y voy a contar si son muchos o pocos y a estudiar
#   si se alejan mucho o poco del intervalo [15,60]


# ------------------ Valores BMI entre 5 y 100 --------------------------

# No puedo guardar los ID de paciente, porque un mismo paciente puede tener un
#   diagnóstico de BMI>100, pero otros normales. Lo que voy a descartar es el 
#   diagnóstico (una fila) no el paciente (muchas filas)

# Vector con los números de fila que tienen BMI > 100
BMI_100_vector <- which(BMI$Valor.DBP > rep(100, nrow(BMI)))
# Cuantos pacientes tienen BMI <= 100.
#   (Elimino los >100 con 'slice', elijo la colimna ID, elimino los ID repetidos
#   con unique, y miro cuántos elementos me quedan con 'Length')
length(unique(slice(BMI,
             -BMI_100_vector)$Identificador.de.Paciente))

# Por sexo hago lo mismo, solo que estudio el vector  ID %in% HombresID  que
#   es un vector lógico. Por eso utilizo 'sum' en vez de 'length'
# Hombres
sum( unique(slice(BMI,
                  -BMI_100_vector)$Identificador.de.Paciente) 
     %in% HombresID )
# Mujeres
sum( unique(slice(BMI,
                  -BMI_100_vector)$Identificador.de.Paciente) 
     %in% MujeresID )

# Vector con las filas que tienen BMI < 5
BMI_5_vector <- which(BMI$Valor.DBP < rep(5, nrow(BMI)))
length(unique(slice(BMI,
                    -BMI_5_vector)$Identificador.de.Paciente))
sum( unique(slice(BMI,
                  -BMI_5_vector)$Identificador.de.Paciente)   # Hombres
     %in% HombresID )
sum( unique(slice(BMI,
                  -BMI_5_vector)$Identificador.de.Paciente)   # Mujeres
     %in% MujeresID )

# Con BMI entre 5 y 100
length(unique(slice(BMI,
                    -c(BMI_5_vector,BMI_100_vector))$Identificador.de.Paciente))
sum( unique(slice(BMI,      # Hombres
                  -c(BMI_5_vector,BMI_100_vector))$Identificador.de.Paciente) 
     %in% HombresID )
sum( unique(slice(BMI,      # Mujeres
                  -c(BMI_5_vector,BMI_100_vector))$Identificador.de.Paciente) 
     %in% MujeresID )

# Elimino de dataframe BMI aquellas filas con BMI entre 5 y 100 y lo guardo en
#   en una nueva variable.
# BMI_5_100 <- slice(BMI, -c(BMI_100_vector, BMI_5_vector))
# nrow(BMI_5_100) == nrow(BMI) - length(c(BMI_100_vector, BMI_5_vector))

# Como una fila no indica un paciente distinto, si no un diagnóstico, voy a 
#   contar cuántos pacientes tengo después de eliminar los BMI > 100 y los
#   BMI < 5. Compararé con el archivo inicial para ver si tengo los mismos 
#   pacientes

# BMI_5_100_ID <- unique(BMI_5_100$Identificador.de.Paciente)
# BMI_ID <- unique(BMI$Identificador.de.Paciente)
# length(BMI_ID) - length(BMI_5_100_ID)
# 
# # Veo que BMI_ID tiene más elementos. Eso quiere decir que al eliminar ciertos 
# #   diagnósticos (filas) también he eliminado toda la información de algunos
# #   pacientes. En total hay 1512 eliminados.
# # Creo un database con esos pacientes que tienen un BMI raro (>100 ó <5) 
# #   para estudiarlo
# 
# BMI_raros <- BMI[! BMI$Identificador.de.Paciente %in% BMI_5_100_ID, ]
# 
# length(intersect(PobDiana$Id.Paciente, BMI$Identificador.de.Paciente))


# ------------------ Valores BMI entre 10 y 70 --------------------------

# EXPLICACIÓN DE LAS FUNCIONES detallada en el caso <<BMI entre 5 y 100>>

# Vector con los números de fila que tienen BMI > 70
BMI_70_vector <- which(BMI$Valor.DBP > rep(70, nrow(BMI)))
# Cuantos pacientes tienen BMI <= 70
length(unique(slice(BMI,
                    -BMI_70_vector)$Identificador.de.Paciente))
sum( unique(slice(BMI,
                  -BMI_70_vector)$Identificador.de.Paciente)   # Hombres
     %in% HombresID )
sum( unique(slice(BMI,
                  -BMI_70_vector)$Identificador.de.Paciente)   # Mujeres
     %in% MujeresID )

# Vector con los números de fila que tienen BMI < 10
BMI_10_vector <- which(BMI$Valor.DBP < rep(10, nrow(BMI)))
# Cuantos pacientes tienen BMI > 10
length(unique(slice(BMI,
                    -BMI_10_vector)$Identificador.de.Paciente))
sum( unique(slice(BMI,
                  -BMI_10_vector)$Identificador.de.Paciente)   # Hombres
     %in% HombresID )
sum( unique(slice(BMI,
                  -BMI_10_vector)$Identificador.de.Paciente)   # Mujeres
     %in% MujeresID )

# Con BMI entre 10 y 70
length(unique(slice(BMI,
                    -c(BMI_10_vector,BMI_70_vector))$Identificador.de.Paciente))
sum( unique(slice(BMI,         # Hombres
                  -c(BMI_10_vector,BMI_70_vector))$Identificador.de.Paciente)   
     %in% HombresID )
sum( unique(slice(BMI,         # Mujeres
                  -c(BMI_10_vector,BMI_70_vector))$Identificador.de.Paciente)   
     %in% MujeresID )


# ------------------ Valores BMI entre 15 y 60 --------------------------

# La EXPLICACIÓN DE LAS FUNCIONES está detallada en el caso <<BMI entre 5 y 100>>

# Vector con los números de fila que tienen BMI > 60
BMI_60_vector <- which(BMI$Valor.DBP > rep(60, nrow(BMI)))
# Cuantos pacientes tienen BMI <= 60
length(unique(slice(BMI,
                    -BMI_60_vector)$Identificador.de.Paciente))
sum( unique(slice(BMI,
                  -BMI_60_vector)$Identificador.de.Paciente)   # Hombres
     %in% HombresID )
sum( unique(slice(BMI,
                  -BMI_60_vector)$Identificador.de.Paciente)   # Mujeres
     %in% MujeresID )

# Vector con los números de fila que tienen BMI < 15
BMI_15_vector <- which(BMI$Valor.DBP < rep(15, nrow(BMI)))
# Cuantos pacientes tienen BMI >= 15
length(unique(slice(BMI,
                    -BMI_15_vector)$Identificador.de.Paciente))
sum( unique(slice(BMI,
                  -BMI_15_vector)$Identificador.de.Paciente)   # Hombres
     %in% HombresID )
sum( unique(slice(BMI,
                  -BMI_15_vector)$Identificador.de.Paciente)   # Mujeres
     %in% MujeresID )

# Con BMI entre 15 y 60
length(unique(slice(BMI,
                    -c(BMI_15_vector,BMI_60_vector))$Identificador.de.Paciente))
sum( unique(slice(BMI,      # Hombres
                  -c(BMI_15_vector, BMI_60_vector))$Identificador.de.Paciente)   
     %in% HombresID )
sum( unique(slice(BMI,     # Mujeres
                  -c(BMI_15_vector, BMI_60_vector))$Identificador.de.Paciente)
     %in% MujeresID )



# ------------------ Valores BMI entre 17 y 50 --------------------------

# La EXPLICACIÓN DE LAS FUNCIONES está detallada en el caso <<BMI entre 5 y 100>>

# Vector con los números de fila que tienen BMI > 50
BMI_50_vector <- which(BMI$Valor.DBP > rep(50, nrow(BMI)))
# Cuantos pacientes tienen BMI <= 50
length(unique(slice(BMI,
                    -BMI_50_vector)$Identificador.de.Paciente))
sum( unique(slice(BMI,
                  -BMI_50_vector)$Identificador.de.Paciente)   # Hombres
     %in% HombresID )
sum( unique(slice(BMI,
                  -BMI_50_vector)$Identificador.de.Paciente)   # Mujeres
     %in% MujeresID )

# Vector con los números de fila que tienen BMI < 17
BMI_17_vector <- which(BMI$Valor.DBP < rep(17, nrow(BMI)))
# Cuantos pacientes tienen BMI >= 17
length(unique(slice(BMI,
                    -BMI_17_vector)$Identificador.de.Paciente))
sum( unique(slice(BMI,
                  -BMI_17_vector)$Identificador.de.Paciente)   # Hombres
     %in% HombresID )
sum( unique(slice(BMI,
                  -BMI_17_vector)$Identificador.de.Paciente)   # Mujeres
     %in% MujeresID )

# Con BMI entre 17 y 50
length(unique(slice(BMI,
                    -c(BMI_17_vector,BMI_50_vector))$Identificador.de.Paciente))
sum( unique(slice(BMI,      # Hombres
                  -c(BMI_17_vector, BMI_50_vector))$Identificador.de.Paciente)
     %in% HombresID )
sum( unique(slice(BMI,      # Mujeres
                  -c(BMI_17_vector, BMI_50_vector))$Identificador.de.Paciente)
     %in% MujeresID )

