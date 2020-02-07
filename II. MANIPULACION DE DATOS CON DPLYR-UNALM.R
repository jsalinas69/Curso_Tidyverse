######################################
# EXPLORACIÓN DE DATOS CON TIDYVERSE #
#  MANIPULACIÓN DE DATOS CON DPLYR   #  
#       Mg. Jesús Salinas Flores     # 
#       jsalinas@lamolina.edu.pe     #
######################################


#---------------------------------------------------------
# Para limpiar el workspace, por si hubiera algun dataset 
# o informacion cargada
rm(list = ls())

#------------------------------------------------------------------
# Limpiar la consola
cat("\014")

#-------------------------------------------------------------------
# Para limpiar el área de gráficos
dev.off()

#---------------------------------------------------------
# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
options(scipen=999)

##############
#  Paquetes  #
##############

library(dplyr)
library(ggplot2)
library(datos)
library(data.table)
library(tictoc)
library(gganimate)

#############################
# CASO DE ESTUDIO 1: PAÍSES #
#############################

#####################################
# I. TRABAJANDO CON TODOS LOS DATOS #
#####################################

#---------------------------------------------------------
# 1.1 Describiendo los datos

library(datos)

paises

View(paises)

str(paises)

# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	1704 obs. of  6 variables:
# $ pais             : Factor w/ 142 levels "Afganistán","Albania",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ continente       : Factor w/ 5 levels "África","Américas",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ anio             : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
# $ esperanza_de_vida: num  28.8 30.3 32 34 36.1 ...
# $ poblacion        : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
# $ pib_per_capita   : num  779 821 853 836 740 ...

#---------------------------------------------------------
# 1.1 Gráfico de dispersión PIB vs esperanza de vida

library(ggplot2)
library(gganimate)

ggplot(paises, aes(pib_per_capita, esperanza_de_vida)) +
  geom_point() +
  theme_bw() 

# Usando escala logarítmica 
# log10(10)=1;log10(100)=2; log10(1000)=3; log10(10000)=4
ggplot(paises, aes(pib_per_capita, esperanza_de_vida)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() 

ggplot(paises, aes(log10(pib_per_capita), esperanza_de_vida)) +
  geom_point() +
  theme_bw() 

#---------------------------------------------------------
# 1.2 Gráfico de dispersión PIB vs esperanza de vida por año
ggplot(paises, aes(pib_per_capita, esperanza_de_vida)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  facet_wrap(~anio) 

#---------------------------------------------------------
# 1.3 Gráfico de dispersión PIB vs esperanza de vida
#     por continente
ggplot(paises, aes(pib_per_capita, esperanza_de_vida)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  facet_wrap(~continente) 

#---------------------------------------------------------
# 1.4 Gráfico de dispersión PIB vs esperanza de vida por
#     país,continente
ggplot(paises, aes(pib_per_capita, esperanza_de_vida,color=pais)) +
  geom_point(show.legend = FALSE) +
  scale_x_log10() +
  theme_bw() +
  facet_wrap(~continente) 

#---------------------------------------------------------
# 1.5 Gráfico de dispersión pib vs esperanza de vida,
#     animado por año
ggplot(paises, aes(pib_per_capita, esperanza_de_vida, size = poblacion, color = continente)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() 
  
ggplot(paises, aes(pib_per_capita, esperanza_de_vida, size = poblacion, color = continente)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  # Animando el gráfico:
  labs(title = 'Año: {frame_time}', 
       x = 'PIB per cápita', 
       y = 'Esperanza de vida') +
  transition_time(anio) 


#---------------------------------------------------------
# 1.6 Gráfico de dispersión pib vs esperanza de vida por 
#     continente, animado por año
ggplot(paises, aes(pib_per_capita, esperanza_de_vida, size = poblacion, colour = pais)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continente) 
  
# Animar el gráfico



#######################################
# II. Data wrangling                  #
#     Transformación y mapeo de datos #
#######################################

####################
# El paquete dplyr #
####################

attach(paises)

round(prop.table(table(continente)),3)

# Usando dplyr 


#________________________
# ________ \\|// ________
# ________( o o ) _______ 
# ___oo0____(_)____Ooo___
#      Ejercicio   1     #
# Obtenga la esperanza de vida promedio con 2 decimales



#---------------------------------------------------------
# 2.1 Filtrando para un año, por ejemplo para 1957


#---------------------------------------------------------
# 2.2 Filtrando para un país y un año
paises %>% filter(pais=="China",anio==2002)

# Otra forma


#---------------------------------------------------------
# 2.3 Ordenando observaciones según la esperanza de vida
paises %>% arrange(esperanza_de_vida)  # por defecto ascendente

paises %>% arrange(desc(esperanza_de_vida))

# equivalente anterior



#---------------------------------------------------------
# 2.4 Filtrando y ordenando

filtro <- filter(paises,anio==1957)
arrange(filtro,desc(poblacion))

# En una sola sentencia


#________________________
# ________ \\|// ________
# ________( o o ) _______ 
# ___oo0____(_)____Ooo___
#      Ejercicio   2     #
# Muestre la información de Asia del año 1987 ordenada 
# según el PIB



#---------------------------------------------------------
# 2.5 Usando mutate para cambiar o crear una columna
# ¿Cómo se crea o cambia una variable en R?




#---------------------------------------------------------
# 2.6 Combinando filter, mutate y arrange

paises %>% filter(anio==2007) %>%
  mutate(esperanza_de_vida_mensual=12*esperanza_de_vida) %>%
  arrange(desc(esperanza_de_vida_mensual))

# Creando una variable lógica
paises %>% filter(anio==2007,continente=="Asia") %>%
  mutate(esperanza_de_vida_mayor81=esperanza_de_vida>81) %>%
  arrange(desc(esperanza_de_vida))


#---------------------------------------------------------
# 2.7 Seleccionando columnas con select()

paises[,c(1,3,5)]

paises %>% select(pais,anio,poblacion)

seleccion <- paises %>% select(pais,anio,poblacion)
seleccion

# Seleccionando 2 columnas y luego todas las demás
paises %>% select(pais,anio, everything())

#---------------------------------------------------------
# 2.8 Renombrando columnas con rename()

paises2 <- paises %>% rename(Pais=pais, Año=anio, PBI=pib_per_capita)
paises2


###############################
# III. Visualización de datos #
###############################

#---------------------------------------------------------
# 3.1 Asignación a una variable 

paises_1952 <- paises %>% filter(anio==1952)


#---------------------------------------------------------
# 3.2 Comparando población y PBI per cápita

paises_1952 <- paises %>% filter(anio==1952)

paises_1952

ggplot(paises_1952, aes(x = poblacion, y = pib_per_capita)) +
  geom_point()

# Poner en una sola sentencia


#---------------------------------------------------------
# 3.3 Comparando población y esperanza de vida

paises_1952 <- paises %>% filter(anio==1952)

ggplot(paises_1952, aes(x=poblacion, y=esperanza_de_vida)) + geom_point()

#---------------------------------------------------------
# 3.4 Cambiando el eje X a una escala logarítmica

paises_1952 <- paises %>% filter(anio==1952)

ggplot(paises_1952, aes(x = poblacion, y = esperanza_de_vida)) +
  geom_point() + scale_x_log10()

#---------------------------------------------------------
# 3.5 Cambiando los ejex X e Y a una escala logarítmica

paises_1952 <- paises %>% filter(anio==1952)

ggplot(paises_1952, aes(x=poblacion, y=pib_per_capita)) +
  geom_point() + scale_x_log10() + scale_y_log10()

#---------------------------------------------------------
# 3.6 Añadiendo color al gráfico de dispersión

paises_1952 <- paises %>% filter(anio==1952)

ggplot(paises_1952, aes(x=poblacion,y=esperanza_de_vida,color=continente)) + 
  geom_point() + scale_x_log10()

#---------------------------------------------------------
# 3.7 Añadiendo tamaño y color al gráfico de dispersión

paises_1952 <- paises %>% filter(anio==1952)

ggplot(paises_1952, aes(x = poblacion, y = esperanza_de_vida,
                        color = continente, size=pib_per_capita)) +
  geom_point() +
  scale_x_log10()


#---------------------------------------------------------
# 3.8 Creando un subgráfico para cada continente

paises_1952 <- paises %>% filter(anio==1952)

ggplot(paises_1952, aes(x=poblacion, y=esperanza_de_vida)) +
  geom_point()+
  scale_x_log10() + facet_wrap(~continente)

#---------------------------------------------------------
# 3.9 Facetas por año

ggplot(paises, aes(x=pib_per_capita,y=esperanza_de_vida, 
                   color=continente,size=poblacion))+ 
  geom_point() + scale_x_log10() + facet_wrap(~anio)


#________________________
# ________ \\|// ________
# ________( o o ) _______ 
# ___oo0____(_)____Ooo___
#      Ejercicio   3     #
# Presente un gráfico que muestre la población y 
# el PIB por país en Asia




##############################
# IV. Agrupando y resumiendo #
##############################

#---------------------------------------------------------
# 4.1 Resumiendo la esperanza de vida 

paises %>% summarize(esperanza_de_vida_media = mean(esperanza_de_vida))

# Tendencia Central: mean(), median()
# Dispersión: sd(), IQR()
# Rango: min(), max(), quantile()
# Posición: first(), last()
# Conteo: n(), n_distinct()
# Lógica: any(), all()

#---------------------------------------------------------
# 4.2 Resumiendo la esperanza de vida en 1957

paises %>% filter(anio==1957) %>% 
  summarize(esperanza_de_vida_media=mean(esperanza_de_vida))

#---------------------------------------------------------
# 4.3 Resumiendo con varios indicadores en 1957

paises %>% filter(anio==1957) %>% 
  summarize(esperanza_de_vida_media=mean(esperanza_de_vida),pib_per_capita_maximo=max(pib_per_capita))


#---------------------------------------------------------
# 4.4 Resumiendo por año

paises %>% group_by(anio) %>% 
  summarize(esperanza_de_vida_media=mean(esperanza_de_vida),pib_per_capita_maximo=max(pib_per_capita))

#________________________
# ________ \\|// ________
# ________( o o ) _______ 
# ___oo0____(_)____Ooo___
#      Ejercicio   4     #
# Presente un gráfico que muestre la evolución por año
# de la esperanza de vida promedio en Asia




# País con la menor esperanza de vida de América en 2007
paises %>% filter(continente=="Américas",anio==2007) %>% 
  arrange(esperanza_de_vida) %>%
  summarize(first(pais)) 

paises %>% filter(continente=="Américas",anio==2007) %>% 
  summarize(min(esperanza_de_vida)) 


#---------------------------------------------------------
# 4.5 Resumiendo por continente

paises %>% filter(anio==1957) %>% group_by(continente)%>%
  summarize(esperanza_de_vida_media=mean(esperanza_de_vida),pib_per_capita_maximo=max(pib_per_capita))


#---------------------------------------------------------
# 4.6 Resumiendo por continente y año

paises %>% group_by(continente,anio) %>%
  summarize(esperanza_de_vida_mediana=median(esperanza_de_vida),
            pib_per_capita_maximo=max(pib_per_capita))



#---------------------------------------------------------
# 4.7 Visualizando la esperanza de vida media

by_anio <- paises %>%
  group_by(anio) %>%
  summarize(esperanza_de_vida_media = mean(esperanza_de_vida),
            pib_per_capita_maximo = max(pib_per_capita))

by_anio

ggplot(by_anio,aes(x=anio,y=esperanza_de_vida_media)) + 
  geom_point() + expand_limits(y=0)


#---------------------------------------------------------
# 4.8 Visualizando PBI medio por continente

by_anio_continente <- paises %>% group_by(anio,continente) %>%
  summarize(pib_per_capita_medio=mean(pib_per_capita))

by_anio_continente

ggplot(by_anio_continente, aes(x=anio,y=pib_per_capita_medio, color=continente)) + 
  geom_point() + expand_limits(y=0)

#---------------------------------------------------------
# 4.9 Comparando la esperanza de vida media  y el PBI medio
#     por continente en 2007

por_continente_2007 <- paises %>% filter(anio==2007) %>% 
  group_by(continente) %>%
  summarize(esperanza_de_vida_media = mean(esperanza_de_vida),
            pib_per_capita_medio    = mean(pib_per_capita))

por_continente_2007

ggplot(por_continente_2007,aes(x=pib_per_capita_medio,y=esperanza_de_vida_media,
                               color=continente)) + geom_point() 

#---------------------------------------------------------
# 4.10 Número de paises por continente en 2007

paises %>% filter(anio==2007) %>% 
  group_by(continente) %>% summarize(Número_Paises = n())


#---------------------------------------------------------
# 4.11 Gráfico de dispersión pib vs esperanza de vida,
#      animado por año de Perú



########################
# V. Tipos de Gráficos #
########################

#---------------------------------------------------------
# 5.1 Visualizando el PBI medio con gráfico de líneas

por_anio <- paises %>% group_by(anio) %>%
  summarize(pib_per_capita_medio=mean(pib_per_capita))

por_anio

ggplot(por_anio, aes(x=anio, y=pib_per_capita_medio)) +
  geom_line()+ expand_limits(y=0)

#---------------------------------------------------------
# 5.2 Visualizando el PBI medio por continente

por_anio_continente <- paises %>% group_by(anio,continente) %>%
  summarize(pib_per_capita_medio=mean(pib_per_capita))

por_anio_continente

ggplot(por_anio_continente, aes(x=anio,y=pib_per_capita_medio,
                                color=continente)) + 
  geom_line() +expand_limits(y=0)

ggplot(por_anio_continente, aes(x=anio,y=pib_per_capita_medio,
                                color=continente)) + 
   geom_point() +expand_limits(y=0)

ggplot(por_anio_continente, aes(x=anio,y=pib_per_capita_medio,
                                color=continente)) + 
  geom_line() + geom_point()+ expand_limits(y=0)

#---------------------------------------------------------
# 5.3 Visualizando el PBI medio por continente en 1952

por_continente <- paises %>% filter(anio==1952) %>%
  group_by(continente) %>%
  summarize(pib_per_capita_medio=mean(pib_per_capita))

por_continente

# Create a bar plot showing medianGdp by continent
ggplot(por_continente, aes(x=continente,y=pib_per_capita_medio))+
  geom_col()   # geom_col usa por defecto stat="identity"

ggplot(por_continente, aes(x=continente,y=pib_per_capita_medio))+
  geom_bar(stat="identity")   # geom_bar usa por defecto stat="count"

#---------------------------------------------------------
# 5.4 Visualizando PBI por país en Oceanía

oceania_1952 <- paises %>% 
  filter(continente=="Oceanía",anio==1952)

oceania_1952

ggplot(oceania_1952, aes(x=pais,y=pib_per_capita)) + 
  geom_col()


#---------------------------------------------------------
# 5.5 Visualizando la población

paises_1952 <- paises %>% filter(anio == 1952)

paises_1952

ggplot(paises_1952, aes(x=poblacion)) + geom_histogram()


#---------------------------------------------------------
# 5.6 Visualizando la población con el eje X en escala 
#     logarítmica

paises_1952 <- paises %>% filter(anio == 1952)

paises_1952

ggplot(paises_1952, aes(x=poblacion)) + geom_histogram() + 
  scale_x_log10()


#---------------------------------------------------------
# 5.7 Comparando el PBI entre continentes

paises_1952 <- paises %>% filter(anio == 1952)

ggplot(paises_1952, aes(x=continente,y=pib_per_capita)) + 
  geom_boxplot() + scale_y_log10()


#________________________
# ________ \\|// ________
# ________( o o ) _______ 
# ___oo0____(_)____Ooo___
#      Ejercicio   5     #
# Presente un gráfico de barras que muestre el PIB promedio
# por continente en el año 2007


#---------------------------------------------------------
# 5.8 Gráficos circulares
paises %>% group_by(continente) %>% 
  summarize(n=n()) %>% mutate(prop=round(n*100/sum(n),1)) -> conti
conti

ggplot(conti, aes(x = "", y = n, fill = continente)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(prop), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, 
       y = NULL, 
       fill = NULL, 
       title = "Distribución de países por continente") +
  theme_void()



########################################
# CASO DE ESTUDIO 2: FRAUDE            #
# dplyr con grandes volúmenes de datos #
########################################

library(data.table)
library(tictoc)

tic()
data_dt <-fread("ccFraud.csv",
                header=T, 
                verbose =FALSE, 
                showProgress =TRUE)
toc()

str(data_dt)

data_dt$gender <- factor(data_dt$gender, levels=c(1,2),
                         labels=c("Masculino","Femenino"))

plot(data_dt$gender,col=c("red","darkgreen"))

tic()
ggplot(data_dt, aes(gender)) + geom_bar()
toc()

tic()
tabla <- data_dt %>% group_by(gender) %>% summarize(count = n())
tabla
ggplot(tabla, aes(x=gender,y=count))+ 
  geom_bar(stat='identity',fill=c("red","darkgreen"))
toc()

ggplot(tabla, aes(gender,count))+ 
  geom_col(fill=c("red","darkblue"))

ggplot(tabla, aes(gender,count/sum(count)))+ 
  geom_col(fill=c("red","darkgreen")) +
  geom_text(aes(label=round(100*count/sum(count),2)),vjust=-1.0) +
  ylim(0,0.8) +
  labs(title="Distribución de clientes según el género",
         y="Género",
         x="Proporción")


################################
# Datos relacionales con dplyr #
################################
library(datos)

# vuelos contiene todos los vuelos 336, 776 que partieron
#        de la ciudad de Nueva York durante el 2013. 
#        Los datos provienen del Departamento de Estadísticas 
#        de Transporte de los Estados Unidos
vuelos

# aerolineas permite observar el nombre completo de la
#            aerolínea a partir de su código abreviado:
aerolineas

# aeropuertos entrega información de cada aeropuerto, 
#             identificado por su código:
aeropuertos

# aviones entrega información de cada avión, identificado
#         por su codigo_cola:
aviones

# clima entrega información del clima en cada aeropuerto 
#       de Nueva York para cada hora:
clima

# En estos datos:

# - vuelos se connecta con aviones a través de la 
#   variable codigo_cola.
# - vuelos se conecta con aerolineas a través de la
#   variable codigo_carrier.
# - vuelos se conecta con aeropuertos de dos formas: 
#   a través de las variables origen y destino.
# - vuelos se conecta con clima a través de las variables
#   origen (la ubicación), anio, mes, dia y hora (el horario).

vuelos2 <- vuelos %>%
           select(anio:dia, hora, origen, destino, codigo_cola, aerolinea)
vuelos2

aerolineas

vuelos2 %>%
  select(-origen, -destino) %>%
  left_join(aerolineas, by = "aerolinea")

# Definiendo las columnas clave

vuelos2 %>%
  left_join(clima)

vuelos2 %>%
  left_join(aviones, by = "codigo_cola")

vuelos2 %>%
  left_join(aeropuertos, c("origen" = "codigo_aeropuerto"))

vuelos2 %>%
  left_join(aeropuertos, c("destino" = "codigo_aeropuerto"))