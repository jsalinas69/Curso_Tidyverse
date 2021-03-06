######################################
# EXPLORACI�N DE DATOS CON TIDYVERSE #
#  AN�LISIS GR�FICO Y EXPLORATORIO   #
#          CASO: SEGUROS             #
#     Mg. Jes�s Salinas Flores       #  
#     jsalinas@lamolina.edu.pe       #
######################################

# Enviar a jsalinas@lamolina.edu.pe

#-------------------------------------------------------------------
# Para limpiar el workspace, por si hubiera algun dataset 
# o informacion cargada
rm(list = ls())

#-------------------------------------------------------------------
# Para limpiar el �rea de gr�ficos
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

# 1. Presente un gr�fico que muestre la distribuci�n de los clientes
#    seg�n si accedieron o no al seguro  (3 puntos)


# 2. Para cada una de las variables predictoras indique si podr�an
#    ser buenas o malas predictoras de la variable target SEGURO
#    justifique gr�ficamente su respuesta
#    (8 puntos)


# 3. Para las personas que si accedieron al seguro, presente
#    un gr�fico que muestre su distribuci�n seg�n el nivel de
#    educaci�n (3 puntos)

# 4. Presente un gr�fico que muestre el n�mero de hijos promedio
#    seg�n si accedieron o no al seguro   (3 puntos)

# 5. Presente un gr�fico que muestre la edad promedio seg�n el 
#    nivel de ingresos para los que si accedieron y los que no 
#    accedieron al seguro (3 puntos)

