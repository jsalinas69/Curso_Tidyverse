######################################
# EXPLORACIÓN DE DATOS CON TIDYVERSE #
#  ANÁLISIS GRÁFICO Y EXPLORATORIO   #
#          CASO: SEGUROS             #
#     Mg. Jesús Salinas Flores       #  
#     jsalinas@lamolina.edu.pe       #
######################################

# Enviar a jsalinas@lamolina.edu.pe

#-------------------------------------------------------------------
# Para limpiar el workspace, por si hubiera algun dataset 
# o informacion cargada
rm(list = ls())

#-------------------------------------------------------------------
# Para limpiar el área de gráficos
dev.off()

#---------------------------------------------------------
# Limpiar la consola
cat("\014")

#-------------------------------------------------------------------
# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Enviar a correo: jsalinas@lamolina.edu.pe

#-------------------------------------------------------------------
# Paquetes
library(ggplot2) 
library(dplyr)

#-------------------------------------------------------------------
# Lectura de datos 

library(readxl)
datos  <- read_excel("Seguros.xlsx")

str(datos)

datos$Seguro    <- factor(datos$Seguro)
datos$T_credito <- factor(datos$T_credito)
datos$Sexo      <- factor(datos$Sexo)
datos$Ingresos  <- factor(datos$Ingresos)
datos$Educacion <- factor(datos$Educacion)
datos$Automovil <- factor(datos$Automovil)
datos$Trabajo   <- factor(datos$Trabajo)

str(datos)

attach(datos)

# 1. Presente un gráfico que muestre la distribución de los clientes
#    según si accedieron o no al seguro  (3 puntos)


# 2. Para cada una de las variables predictoras indique si podrían
#    ser buenas o malas predictoras de la variable target SEGURO
#    justifique gráficamente su respuesta
#    (8 puntos)


# 3. Para las personas que si accedieron al seguro, presente
#    un gráfico que muestre su distribución según el nivel de
#    educación (3 puntos)

# 4. Presente un gráfico que muestre el número de hijos promedio
#    según si accedieron o no al seguro   (3 puntos)

# 5. Presente un gráfico que muestre la edad promedio según el 
#    nivel de ingresos para los que si accedieron y los que no 
#    accedieron al seguro (3 puntos)

