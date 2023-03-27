
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


# Vamos ahora a juntar los archivos de diagnósticos. Los tenemos en tres categorías:
# AH, AP, OG; y hay seis archivos por cada grupo.

# OBJETIVO: Crear un nuevo data frame de diagnósticos con las columnas: Id, alcohol,
#         fechaAlc, tabaco, fechaTabac, fractura, fechaFrac, diagnost1, fecha1, 
#         diagnost2, fecha2, ...


# -------------------------------------------------------------------------
# ----------------------------   1ª PARTE   -------------------------------
# --------------------  Unir atención hospitalaria AH    -------------------
#--------------------------------------------------------------------------

diagnosticosAH1 <- read.csv(
  "Igor_Fracturas_cadera/OBI/modificados/CADERA_04_Diagnosticos_AH_01_mod.csv")
diagnosticosAH2 <- read.csv(
  "Igor_Fracturas_cadera/OBI/modificados/CADERA_04_Diagnosticos_AH_02_mod.csv")
diagnosticosAH3 <- read.csv(
  "Igor_Fracturas_cadera/OBI/modificados/CADERA_04_Diagnosticos_AH_03_mod.csv")
diagnosticosAH4 <- read.csv(
  "Igor_Fracturas_cadera/OBI/modificados/CADERA_04_Diagnosticos_AH_04_mod.csv")
diagnosticosAH5 <- read.csv(
  "Igor_Fracturas_cadera/OBI/modificados/CADERA_04_Diagnosticos_AH_05_mod.csv")
diagnosticosAH6 <- read.csv(
  "Igor_Fracturas_cadera/OBI/modificados/CADERA_04_Diagnosticos_AH_06_mod.csv")


# --------------- Depuración de los datos -----------------------

# Tenemos el problema inesperado de que hay 9 columnas en vez de 6 en los archivos
#   AH1, AH2, AH3 y AH4. Además parece que en esas columnas solo hay huecos en 
#   blanco al estilo ""

# Vamos a eliminar los pacientes que tenían 100 años o más. Los filtro por ID.
diagnosticosAH1 <- subset(diagnosticosAH1, !(Id.Paciente %in% MayoresDe100$ID))
diagnosticosAH2 <- subset(diagnosticosAH2, !(Id.Paciente %in% MayoresDe100$ID))
diagnosticosAH3 <- subset(diagnosticosAH3, !(Id.Paciente %in% MayoresDe100$ID))
diagnosticosAH4 <- subset(diagnosticosAH4, !(Id.Paciente %in% MayoresDe100$ID))
diagnosticosAH5 <- subset(diagnosticosAH5, !(Id.Paciente %in% MayoresDe100$ID))
diagnosticosAH6 <- subset(diagnosticosAH6, !(Id.Paciente %in% MayoresDe100$ID))

# Voy a juntarlos en un solo marco de datos
diagnosticosAH1_4 <- rbind(diagnosticosAH1, diagnosticosAH2,
                           diagnosticosAH3, diagnosticosAH4)
# Primero miro si todos son elementos como ""
all(diagnosticosAH1_4[ , 7] == rep("", nrow(diagnosticosAH1_4)))

# Como es un FALSE, recojo las filas en las que hay elementos distintos de "" 
#   en un vector llamado rarosH1_4vector. 
# Me creo un DataFrame con esos pacientes raros
rarosAH1_4vector <- which(diagnosticosAH1_4[ ,7] != rep("", nrow(diagnosticosAH1_4)))
rarosAH1_4 <- diagnosticosAH1_4[rarosAH1_4vector, ] # solo informativo
# Como solo hay muy pocos elementos, voy a eliminar a esos diagnósticos (filas).
diagnosticosAH1_4 <- slice(diagnosticosAH1_4, -rarosAH1_4vector)

all(diagnosticosAH1_4[ , 9] == rep("", nrow(diagnosticosAH1_4)))

# Me quedo con las 6 columnas que tienen datos
diagnosticosAH1_4 <- diagnosticosAH1_4[ , c(1,2,3,4,5,6)]

all(!is.na(diagnosticosAH1_4))
rm(rarosAH1_4, rarosAH1_4vector)
#             ----------------------------------

# Una vez depurados los archivos AH1, AH2, AH3 y AH4 los junto con AH5 y AH6
diagnosticosAH <- rbind(diagnosticosAH1_4, diagnosticosAH5, diagnosticosAH6)
rm(diagnosticosAH1, diagnosticosAH2, diagnosticosAH3, diagnosticosAH4,
   diagnosticosAH5, diagnosticosAH6, diagnosticosAH1_4)



# -------------------------------------------------------------------------
# ----------------------------   2ª PARTE   -------------------------------
# ----------------------  Unir atención Primaria AP    ---------------------
#--------------------------------------------------------------------------

# No se pueden tratar de la misma manera porque las bases de datos son ligeramente
#   distintas. Por ejemplo, los nombres de las columnas cambian.
diagnosticosAP1 <- read.csv(
  "Igor_Fracturas_cadera/OBI/modificados/CADERA_04_Diagnosticos_AP_01_mod.csv")
diagnosticosAP2 <- read.csv(
  "Igor_Fracturas_cadera/OBI/modificados/CADERA_04_Diagnosticos_AP_02_mod.csv")
diagnosticosAP3 <- read.csv(
  "Igor_Fracturas_cadera/OBI/modificados/CADERA_04_Diagnosticos_AP_03_mod.csv")
diagnosticosAP4 <- read.csv(
  "Igor_Fracturas_cadera/OBI/modificados/CADERA_04_Diagnosticos_AP_04_mod.csv")
diagnosticosAP5 <- read.csv(
  "Igor_Fracturas_cadera/OBI/modificados/CADERA_04_Diagnosticos_AP_05_mod.csv")
diagnosticosAP6 <- read.csv(
  "Igor_Fracturas_cadera/OBI/modificados/CADERA_04_Diagnosticos_AP_06_mod.csv")

# En este caso todos los archivos tienen las mismas columnas. No parece que haya
#   que depurar nada. Los agrupo
diagnosticosAP <- rbind(diagnosticosAP1, diagnosticosAP2, diagnosticosAP3,
                        diagnosticosAP4, diagnosticosAP5, diagnosticosAP6)
rm(diagnosticosAP1, diagnosticosAP2, diagnosticosAP3, diagnosticosAP4,
   diagnosticosAP5, diagnosticosAP6)

# Vamos a eliminar los pacientes que tenían 100 años o más. Los filtro por ID.
diagnosticosAP <- subset(diagnosticosAP,
                         !(Identificador.de.Paciente %in% MayoresDe100$ID))



# -------------------------------------------------------------------------
# ----------------------------   3ª PARTE   -------------------------------
# ----------------------  Unir osabide Global OG    -----------------------
#--------------------------------------------------------------------------

# No se pueden tratar de la misma manera porque las bases de datos son ligeramente
#   distintas. Por ejemplo, los nombres de las columnas cambian.
diagnosticosOG1 <- read.csv(
  "Igor_Fracturas_cadera/OBI/modificados/CADERA_04_Diagnosticos_OG_01_mod.csv")
diagnosticosOG2 <- read.csv(
  "Igor_Fracturas_cadera/OBI/modificados/CADERA_04_Diagnosticos_OG_02_mod.csv")
diagnosticosOG3 <- read.csv(
  "Igor_Fracturas_cadera/OBI/modificados/CADERA_04_Diagnosticos_OG_03_mod.csv")
diagnosticosOG4 <- read.csv(
  "Igor_Fracturas_cadera/OBI/modificados/CADERA_04_Diagnosticos_OG_04_mod.csv")
diagnosticosOG5 <- read.csv(
  "Igor_Fracturas_cadera/OBI/modificados/CADERA_04_Diagnosticos_OG_05_mod.csv")
diagnosticosOG6 <- read.csv(
  "Igor_Fracturas_cadera/OBI/modificados/CADERA_04_Diagnosticos_OG_06_mod.csv")

# En este caso todos los archivos tienen las mismas columnas. No parece que haya
#   que depurar nada. Los agrupo
diagnosticosOG <- rbind(diagnosticosOG1, diagnosticosOG2, diagnosticosOG3,
                        diagnosticosOG4, diagnosticosOG5, diagnosticosOG6)
rm(diagnosticosOG1, diagnosticosOG2, diagnosticosOG3, diagnosticosOG4,
   diagnosticosOG5, diagnosticosOG6)

# Vamos a eliminar los pacientes que tenían 100 años o más. Los filtro por ID.
diagnosticosOG <- subset(diagnosticosOG,
                         !(Id.Paciente %in% MayoresDe100$ID))


# -------------------------------------------------------------------------
# ----------------------------   4ª PARTE   -------------------------------
# ------------------------   Unir AH, AP y OG    --------------------------
#--------------------------------------------------------------------------

# Pongo el mismo nombre de columnas en todos los marcos de datos AH, AP y OG
names(diagnosticosAH) <- c("Id.Paciente", "Fecha", "Cod.CIE9",
                           "Descripcion.CIE9", "Cod.CIE10", "Descripcion.CIE10")
names(diagnosticosAP) <- c("Id.Paciente", "Fecha", "Cod.CIE9",
                           "Descripcion.CIE9", "Cod.CIE10", "Descripcion.CIE10")
names(diagnosticosOG) <- c("Id.Paciente", "Fecha", "Cod.CIE9",
                           "Descripcion.CIE9", "Cod.CIE10", "Descripcion.CIE10")

# Unifico todo a lo bruto
diagnosticos <- rbind(diagnosticosAH, diagnosticosAP, diagnosticosOG)
rm(diagnosticosAH, diagnosticosAP, diagnosticosOG)

# Establezco formato fecha
diagnosticos$Fecha <- as.Date(diagnosticos$Fecha, format="%Y-%m-%d")

# Mejor no guardarlo como csv porque es demasiado grande



# -------------------------------------------------------------------------
# ----------------------------              -------------------------------
# -----------------------------   TABACO    -------------------------------
#--------------------------------------------------------------------------

# Algunos códigos CIE9 y CIE10 tienen puntos entre medias. Por ejemplo, tengo
#   "V45.61" en vez de "V4561". Por eso, lo primero que hago será eliminar los 
#   puntos de las cadenas de caracteres
diagnosticos$Cod.CIE9 <- gsub("\\.", "", diagnosticos$Cod.CIE9)
diagnosticos$Cod.CIE10 <- gsub("\\.", "", diagnosticos$Cod.CIE10)

# Me creo una lista con los códigos de CIE9 y CIE10 de tabaquismo que están en el 
# archivo 'diagnosticos'.
# NOTA 1: Me creo una lista de listas en vez de un dataframe porque no tienen 
#         la misma longitud 
# NOTA 2: Los códigos CIE9 y CIE10 de estas enfermedades están obtenidos del 
#         paper de Livingston. (Igor y Javier me lo dieron en un Word)
codTabaco <- list(CIE9=list(), CIE10=list())
codTabaco[["CIE9"]] <- 
  unique(diagnosticos$Cod.CIE9[
    startsWith(diagnosticos$Cod.CIE9, "9898") |
      startsWith(diagnosticos$Cod.CIE9, "3051") |
      startsWith(diagnosticos$Cod.CIE9, "V1582") 
  ])
codTabaco[["CIE10"]] <-
  unique(diagnosticos$Cod.CIE10[
    startsWith(diagnosticos$Cod.CIE10, "F17")
  ])

# Me creo un dataframe simple para los pacientes que presentan tabaquismo. Solo
#   tendrá dos columnas: el ID y la primera fecha en la que le detectaron algún
#   diagnóstico de tabaquismo

# 1º elijo los id que tienen algún diagnóstico de tabaquismo en su historial
fumadores <- filter(diagnosticos,
                    Cod.CIE9 %in% codTabaco[["CIE9"]] | 
                      Cod.CIE10 %in% codTabaco[["CIE10"]] )
# 2º Agrupo los Id, y elijo la menor fecha
fumadores <- summarise(group_by(fumadores, Id.Paciente), #group_by no devuelve nada por sí sola
                       tabaquismo = min(Fecha))
# La siguiente notación es equivalente a la del paso 2º
# tabacoAH1_agrupados <- tabacoAH1 %>%
#   group_by(Id.Paciente) %>%
#   summarise(tabaquismo = min(Fecha..ingreso.diag.))

# GUARDO EL ARCHIVO tabaco
write.csv(fumadores, 
          "Ruben_fract/Resultados_Ruben/tabaquismo.csv",
          row.names = FALSE)



# -------------------------------------------------------------------------
# ----------------------------              -------------------------------
# -----------------------------   Alcohol    -------------------------------
#--------------------------------------------------------------------------

# En el paso anterior ya hemos eliminado los puntos que pudiera haber en los
#   códigos CIE9 y CIE10 (por ejemplo, "V45.61" en vez de "V4561"). Por eso, 
#   ya no hace falta volver a hacer esa depuración
# diagnosticos$Cod.CIE9 <- gsub("\\.", "", diagnosticos$Cod.CIE9)
# diagnosticos$Cod.CIE10 <- gsub("\\.", "", diagnosticos$Cod.CIE10)

# Me creo una lista con los códigos de CIE9 y CIE10 de alcoholismo que están en 
#   el archivo 'diagnosticos'.
# NOTA 1: Me creo una lista de listas en vez de un dataframe porque no tienen 
#         la misma longitud 
# NOTA 2: Los códigos CIE9 y CIE10 de estas enfermedades están obtenidos del 
#         paper de Livingston. (Igor y Javier me lo dieron en un Word)
codAlcohol <- list(CIE9=list(), CIE10=list())
codAlcohol[["CIE9"]] <- 
  unique(diagnosticos$Cod.CIE9[
    startsWith(diagnosticos$Cod.CIE9, "V113") |
      startsWith(diagnosticos$Cod.CIE9, "303") |
      startsWith(diagnosticos$Cod.CIE9, "3050") 
  ])
codAlcohol[["CIE10"]] <-
  unique(diagnosticos$Cod.CIE10[
    startsWith(diagnosticos$Cod.CIE10, "F10")
    ])

# Me creo un dataframe simple para los pacientes que presentan tabaquismo. Solo
#   tendrá dos columnas: el ID y la primera fecha en la que le detectaron algún
#   diagnóstico de tabaquismo

# 1º elijo los id que tienen algún diagnóstico de tabaquismo en su historial
alcoholicos <- filter(diagnosticos,
                      Cod.CIE9 %in% codAlcohol[["CIE9"]] | 
                        Cod.CIE10 %in% codAlcohol[["CIE10"]] )
# 2º Agrupo los Id, y elijo la menor fecha
alcoholicos <- summarise(group_by(alcoholicos, Id.Paciente), #group_by no devuelve nada por sí sola
                         alcoholismo = min(Fecha))
# La siguiente notación es equivalente a la del paso 2º
# tabacoAH1_agrupados <- tabacoAH1 %>% 
#   group_by(Id.Paciente) %>% 
#   summarise(tabaquismo = min(Fecha..ingreso.diag.))

# GUARDO EL ARCHIVO alcoholicos
write.csv(alcoholicos, 
          "Ruben_fract/Resultados_Ruben/alcoholismo.csv",
          row.names = FALSE)
