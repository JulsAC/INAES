#Inclusión Financiera
#Infraestructura, Captación, Crédito y Género

library(tidyverse)
library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)
library(reshape2)

##INFRAESTRUCTURA
#Base de datos de inclusión financiera
url <- "https://www.cnbv.gob.mx/Inclusi%C3%B3n/BasesDeDatos/Base_de_Datos_de_Inclusion_Financiera_202312.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Descargar el archivo y leer la hoja "BD Infraestructura Mun" sin nombres de columnas 
download.file(url, destfile = temp_file, mode = "wb")
data <- read_excel(temp_file, sheet = "BD Infraestructura Mun", col_names = FALSE)

#Limpiar base de datos y ordenarla
colnames(data) <- data[11,] # Establecer la fila 11 como nombres de columnas
colnames(data) <- gsub("\n ", "", colnames(data))
colnames(data) <- gsub("Número", "", colnames(data))
# Distinción de columnas por tipo de banca
colnames(data)[12:19] <- paste0(colnames(data)[12:19], "Todos") 
colnames(data)[20:26] <- paste0(colnames(data)[20:26], "BM")
colnames(data)[27:33] <- paste0(colnames(data)[27:33], "BD")
colnames(data)[34:40] <- paste0(colnames(data)[34:40], "SOCAP")
colnames(data)[41:47] <- paste0(colnames(data)[41:47], "SOFIPO")
data <- data[-c(1:11), ] # Eliminar filas innecesarias
#Por alguna razón, si no le cambias el nombre manualmente a las columnas, no jala. Por eso se hace esto.
colnames(data)[20] <- "Sucursales_BM"
colnames(data)[27] <- "Sucursales_BD"
colnames(data)[34] <- "Sucursales_SOCAP"
colnames(data)[41] <- "Sucursales_SOFIPO"

#Convertir data frame a numérico
data[, 12:47] <- lapply(data[, 12:47], as.numeric)

#Cálculo de porcentajes
infraestructura <- data %>%
  group_by(`Tipo de población`) %>%
  summarise(Multiple = sum(Sucursales_BM),
            Desarrollo = sum(Sucursales_BD),
            SOCAP = sum(Sucursales_SOCAP),
            SOFIPO = sum(Sucursales_SOFIPO)) 

infraestructura <- as.data.frame(t(infraestructura))
# Asignar la primera fila como nombres de las columnas
colnames(infraestructura) <- infraestructura[1, ]
# Eliminar la primera fila (ahora redundante)
infraestructura <- infraestructura[-1, ]

colnames(infraestructura) <- c("Transicion", "Metropoli", "Rural", "Semi_Metropoli", "Semi_Urbano", "Sin_identificar", "Urbano")

#Convertir data frame a numérico
infraestructura[,1:7] <- lapply(infraestructura[,1:7], as.numeric)

infraestructura <-infraestructura %>%
  mutate(Transicion = 100 *  Transicion / sum(Transicion),
         Metropoli = 100 * Metropoli / sum(Metropoli),
         Rural = 100 * Rural / sum(Rural),
         Semi_Metropoli = 100 * Semi_Metropoli / sum(Semi_Metropoli),
         Semi_Urbano = 100 * Semi_Urbano / sum(Semi_Urbano),
         Sin_identificar = 100 * Sin_identificar / sum(Sin_identificar),
         Urbano = 100 * Urbano / sum(Urbano))

#Volver a transponer
infraestructura <- as.data.frame(t(infraestructura))
#Poner Categoría
infraestructura$Categoria <- "Infraestructura"
#Poner Texto
infraestructura$Texto <- "Las SOCAP tienen una importante presencia en los municipios con poca población. Junto con la Banca de Desarrollo, son las instituciones que apuestan por la inclusión financiera de los municipios pequeños"


##CAPTACIÓN
# Descargar el archivo y leer la hoja "BD Infraestructura Mun" sin nombres de columnas 
data <- read_excel(temp_file, sheet = "BD Captación Mun", col_names = FALSE)

#Limpiar base de datos y ordenarla
colnames(data) <- data[11,] # Establecer la fila 11 como nombres de columnas
# Distinción de columnas por tipo de banca
colnames(data)[12:18] <- paste0(colnames(data)[12:18], "_Banca") 
colnames(data)[19:23] <- paste0(colnames(data)[19:23], "_EACP")
colnames(data)[24:31] <- paste0(colnames(data)[24:31], "_BM")
colnames(data)[32:39] <- paste0(colnames(data)[32:39], "_BD")
colnames(data)[40:44] <- paste0(colnames(data)[40:44], "_SOCAP")
colnames(data)[45:49] <- paste0(colnames(data)[45:49], "_SOFIPO")
data <- data[-c(1:11), ] # Eliminar filas innecesarias
#Convertir data frame a numérico
data[, 12:49] <- lapply(data[, 12:49], as.numeric)

#Por alguna razón, si no le cambias el nombre manualmente a las columnas, no jala. Por eso se hace esto.
colnames(data)[31] <- "TotalCuentasAhorro_BM"
colnames(data)[39] <- "TotalCuentasAhorro_BD"
colnames(data)[44] <- "TotalCuentasAhorro_SOCAP"
colnames(data)[49] <- "TotalCuentasAhorro_SOFIPO"

#Cálculo de porcentajes
captacion <- data %>%
  group_by(`Tipo de población`) %>%
  summarise(Multiple = sum(TotalCuentasAhorro_BM),
            Desarrollo = sum(TotalCuentasAhorro_BD),
            SOCAP = sum(TotalCuentasAhorro_SOCAP),
            SOFIPO = sum(TotalCuentasAhorro_SOFIPO)) 

captacion <- as.data.frame(t(captacion))
# Asignar la primera fila como nombres de las columnas
colnames(captacion) <- captacion[1, ]
# Eliminar la primera fila (ahora redundante)
captacion <- captacion[-1, ]

colnames(captacion) <- c("Transicion", "Metropoli", "Rural", "Semi_Metropoli", "Semi_Urbano", "Sin_identificar", "Urbano")

#Convertir data frame a numérico
captacion[,1:7] <- lapply(captacion[,1:7], as.numeric)

captacion <- captacion %>%
  mutate(Transicion = 100 *  Transicion / sum(Transicion),
         Metropoli = 100 * Metropoli / sum(Metropoli),
         Rural = 100 * Rural / sum(Rural),
         Semi_Metropoli = 100 * Semi_Metropoli / sum(Semi_Metropoli),
         Semi_Urbano = 100 * Semi_Urbano / sum(Semi_Urbano),
         Sin_identificar = 100 * Sin_identificar / sum(Sin_identificar),
         Urbano = 100 * Urbano / sum(Urbano))


#Volver a transponer
captacion <- as.data.frame(t(captacion))
#Poner Categoría
captacion$Categoria <- "Captación"
#Poner Texto
captacion$Texto <- "6 de cada 10 cuentas de captación aperturadas en municipios rurales pertenecen a una SOCAP, lo cual muestra la importancia de estas instituciones financieras en llevar servicios de ahorro para la población de estos municipios."




##CRÉDITO
# Descargar el archivo y leer la hoja "BD Infraestructura Mun" sin nombres de columnas 
data <- read_excel(temp_file, sheet = "BD Crédito Mun", col_names = FALSE)

#Limpiar base de datos y ordenarla
colnames(data) <- data[11,] # Establecer la fila 11 como nombres de columnas
# Distinción de columnas por tipo de banca
colnames(data)[12:18] <- paste0(colnames(data)[12:18], "_Banca") 
colnames(data)[19:23] <- paste0(colnames(data)[19:23], "_EACP")
colnames(data)[24:31] <- paste0(colnames(data)[24:31], "_BM")
colnames(data)[32:39] <- paste0(colnames(data)[32:39], "_BD")
colnames(data)[40:44] <- paste0(colnames(data)[40:44], "_SOCAP")
colnames(data)[45:49] <- paste0(colnames(data)[45:49], "_SOFIPO")
data <- data[-c(1:11), ] # Eliminar filas innecesarias
#Convertir data frame a numérico
data[, 12:49] <- lapply(data[, 12:49], as.numeric)

#Por alguna razón, si no le cambias el nombre manualmente a las columnas, no jala. Por eso se hace esto.
colnames(data)[31] <- "TotalCuentasCredito_BM"
colnames(data)[39] <- "TotalCuentasCredito_BD"
colnames(data)[44] <- "TotalCuentasCredito_SOCAP"
colnames(data)[49] <- "TotalCuentasCredito_SOFIPO"

#Cálculo de porcentajes
credito <- data %>%
  group_by(`Tipo de población`) %>%
  summarise(Multiple = sum(TotalCuentasCredito_BM),
            Desarrollo = sum(TotalCuentasCredito_BD),
            SOCAP = sum(TotalCuentasCredito_SOCAP),
            SOFIPO = sum(TotalCuentasCredito_SOFIPO)) 

credito <- as.data.frame(t(credito))
# Asignar la primera fila como nombres de las columnas
colnames(credito) <- credito[1, ]
# Eliminar la primera fila (ahora redundante)
credito <- credito[-1, ]

colnames(credito) <- c("Transicion", "Metropoli", "Rural", "Semi_Metropoli", "Semi_Urbano", "Sin_identificar", "Urbano")

#Convertir data frame a numérico
credito[,1:7] <- lapply(credito[,1:7], as.numeric)

credito <- credito %>%
  mutate(Transicion = 100 *  Transicion / sum(Transicion),
         Metropoli = 100 * Metropoli / sum(Metropoli),
         Rural = 100 * Rural / sum(Rural),
         Semi_Metropoli = 100 * Semi_Metropoli / sum(Semi_Metropoli),
         Semi_Urbano = 100 * Semi_Urbano / sum(Semi_Urbano),
         Sin_identificar = 100 * Sin_identificar / sum(Sin_identificar),
         Urbano = 100 * Urbano / sum(Urbano))


#Volver a transponer
credito <- as.data.frame(t(credito))
#Poner Categoría
credito$Categoria <- "Crédito"
#Poner Texto
credito$Texto <- "Las SOCAP es la segunda entidad financiera que proporciona la mayor cantidad de créditos en municipios rurales, en transición y semi-urbanos, después de la banca múltiple."



##Género
# Descargar el archivo y leer la hoja "BD Infraestructura Mun" sin nombres de columnas 
data <- read_excel(temp_file, sheet = "BD Crédito Mun", col_names = FALSE)

#Limpiar base de datos y ordenarla
colnames(data) <- data[11,] # Establecer la fila 11 como nombres de columnas
# Distinción de columnas por tipo de banca
colnames(data)[12:18] <- paste0(colnames(data)[12:18], "_Banca") 
colnames(data)[19:23] <- paste0(colnames(data)[19:23], "_EACP")
colnames(data)[24:31] <- paste0(colnames(data)[24:31], "_BM")
colnames(data)[32:39] <- paste0(colnames(data)[32:39], "_BD")
colnames(data)[40:44] <- paste0(colnames(data)[40:44], "_SOCAP")
colnames(data)[45:49] <- paste0(colnames(data)[45:49], "_SOFIPO")
data <- data[-c(1:11), ] # Eliminar filas innecesarias
#Convertir data frame a numérico
data[, 12:49] <- lapply(data[, 12:49], as.numeric)

#Por alguna razón, si no le cambias el nombre manualmente a las columnas, no jala. Por eso se hace esto.
colnames(data)[31] <- "TotalCuentasCredito_BM"
colnames(data)[39] <- "TotalCuentasCredito_BD"
colnames(data)[44] <- "TotalCuentasCredito_SOCAP"
colnames(data)[49] <- "TotalCuentasCredito_SOFIPO"

#Cálculo de porcentajes
credito <- data %>%
  group_by(`Tipo de población`) %>%
  summarise(Multiple = sum(TotalCuentasCredito_BM),
            Desarrollo = sum(TotalCuentasCredito_BD),
            SOCAP = sum(TotalCuentasCredito_SOCAP),
            SOFIPO = sum(TotalCuentasCredito_SOFIPO)) 

credito <- as.data.frame(t(credito))
# Asignar la primera fila como nombres de las columnas
colnames(credito) <- credito[1, ]
# Eliminar la primera fila (ahora redundante)
credito <- credito[-1, ]

colnames(credito) <- c("Transicion", "Metropoli", "Rural", "Semi_Metropoli", "Semi_Urbano", "Sin_identificar", "Urbano")

#Convertir data frame a numérico
credito[,1:7] <- lapply(credito[,1:7], as.numeric)

credito <- credito %>%
  mutate(Transicion = 100 *  Transicion / sum(Transicion),
         Metropoli = 100 * Metropoli / sum(Metropoli),
         Rural = 100 * Rural / sum(Rural),
         Semi_Metropoli = 100 * Semi_Metropoli / sum(Semi_Metropoli),
         Semi_Urbano = 100 * Semi_Urbano / sum(Semi_Urbano),
         Sin_identificar = 100 * Sin_identificar / sum(Sin_identificar),
         Urbano = 100 * Urbano / sum(Urbano))

completo <- rbind(infraestructura, captacion, credito)
write_csv(completo, "Inclusion_Financiera202312.csv")

