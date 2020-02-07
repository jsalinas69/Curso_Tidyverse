######################################
# EXPLORACIÓN DE DATOS CON TIDYVERSE #
#        GRÁFICOS CON GGPLOT2        #    
#       Mg. Jesús Salinas Flores     # 
#       jsalinas@lamolina.edu.pe     #
######################################

#-------------------------------------------------------------------
# Para limpiar el workspace, por si hubiera algun dataset 
# o informacion cargada
rm(list = ls())

#-------------------------------------------------------------------
# Para limpiar el área de gráficos
dev.off()

#------------------------------------------------------------------
# Limpiar la consola
cat("\014")

#-------------------------------------------------------------------
# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#-------------------------------------------------------------------
# Paquetes
library(foreign)
library(colourpicker)
library(plotrix)
library(PerformanceAnalytics)
library(ggplot2) 
library(gganimate)
library(gifski)
library(png)
library(plotly)
library(forcats)
library(RColorBrewer)
library(maps)
library(mapdata)
library(lubridate)
library(scales)
library(esquisse)
library(ggplotgui)
library(bdpar)
library(cowplot)

##########################
#  GRÁFICOS CON GGPLOT2  #
##########################

library(foreign)
datos <- read.spss("Riesgo_morosidad.sav", 
                  use.value.labels = T,  
                  to.data.frame=TRUE)

attr(datos,"variable.labels") <- NULL

attach(datos)

#####################################
# Gráficos con el paquete ggplot2   #
#####################################

# Hay 48 Geometries en ggplot2
# abline area bar bind2d blank boxplot col contour 
# count crossbar curve density density2d density_2d
# dotplot errorbar errorbarh freqpoly hex histogram
# hline jitter label line linerange map path point
# pointrage polygon qq qq_line quantile raster rect 
# ribbon rug segment sf sf_label sf_text smooth spoke
# step text tile violin vline

#############################################
# I. Gráficos para una variable cualitativa #
#############################################

######################################
# 1.1 Gráficos de Barras con ggplot2 #
######################################

library(ggplot2) 

#-------------------------------------------------------------------
# Gráfico de Barras para una variable categórica
ggplot(data=datos, aes(x=morosidad))

ggplot(data=datos, aes(x=morosidad)) + geom_bar(stat="count")

# Equivalente al anterior




# Gráfico horizontal con coord_flip()
ggplot(datos, aes(morosidad)) + geom_bar() + coord_flip()

# Añadiendo temas de fondo con theme_light()
ggplot(datos, aes(morosidad)) + geom_bar() + theme_light()

# Temas: theme_bw()      theme_classic()   theme_dark()
#        theme_get()     theme_gray()      theme_grey()
#        theme_light()   theme_linedraw()  theme_minimal()
#        theme_replace() theme_set()       theme_update()
#        theme_void()

# Añadiendo más temas
library(ggthemes)

# Añadiendo títulos al gráfico con labs()
ggplot(datos, aes(morosidad)) + 
       geom_bar() + 
       theme_light() + 
       labs(title = "Gráfico de Barras Vertical", 
            x="Condición de la morosidad", 
            y="Frecuencia") 

# Cambiando color de la barra por defecto usando la opción fill






# Especificando colores de la barra
ggplot(datos, aes(morosidad)) + 
       geom_bar(color="blue",fill="White") + 
       theme_light() + 
       labs(title = "Gráfico de Barras Vertical", 
            x= "Condición de la morosidad", 
            y= "Frecuencia") 

# Usando diferentes colores en las barras
ggplot(datos, aes(morosidad)) + 
       geom_bar(color="black",fill=c("darkgreen","orange")) +
       theme_light() + 
       labs(title = "Gráfico de Barras Vertical", 
            x= "Condición de la morosidad", 
            y= "Frecuencia")

# Cambiando los colores por defecto usando scale_fill_manual



# Asignando colores a las categorías de la variable
ggplot(datos, aes(morosidad,fill=morosidad)) + 
  geom_bar(color="black") +
  theme_light() + 
  labs(title = "Gráfico de Barras Vertical", 
       x= "Condición de la morosidad", 
       y= "Frecuencia") +
  scale_fill_manual(values=c("Moroso"="coral4","No Moroso"="cadetblue")) +
  theme(legend.position = "none")

# Eligiendo colores con colourpicker
library(colourpicker)

# Permite elegir colores usando Tools - Addins - Browse Addins...
# Otra forma es usando Addins - Colour Picker
c("#EEEE00", "#FFFFFF", "#FFFFFF")









# Aplicando escalas de colores a partir de las funciones 
# rainbow(n), heat.colors(n), terrain.colors(n), 
# topo.colors(n), and cm.colors(n)
ggplot(datos, aes(morosidad)) + 
  geom_bar(color="black",fill=terrain.colors(2)) +
  theme_light() + 
  labs(title = "Gráfico de Barras Vertical", 
       x= "Condición de la morosidad", 
       y= "Frecuencia")

# length(levels(morosidad))

# Gráfico interactivo con el paquete plotly




#________________________
# ________ \\|// ________
# ________( o o ) _______ 
# ___oo0____(_)____Ooo___
#      Ejercicio   1     #
# Construya un gráfico de barras para la variable dpto






#-------------------------------------------------------------------
# Diagrama de barras con animación
library(gganimate)
b1 <- ggplot(datos, aes(morosidad)) + 
             geom_bar(color="black",fill=c("darkgreen","orange"))




animate(b1)

# enter_ y exit_ se usan para los efectos
# en el ingreso y la salida de la animación

#________________________
# ________ \\|// ________
# ________( o o ) _______ 
# ___oo0____(_)____Ooo___
#      Ejercicio   2     #
# Construya un gráfico de barras animado para la variable dpto








#-------------------------------------------------------------------
# Reordenando las barras con el paquete forcats
levels(dpto)

# Gráfico de barras ordenada por defecto según los niveles 
ggplot(datos,aes(x=dpto)) + 
  geom_bar(color="blue",fill="White") + 
  theme_light() + 
  labs(title = "Gráfico de Barras Vertical", 
       x= "Departamento", 
       y= "Frecuencia")  

# Gráfico de barras descendente
library(forcats)


# Gráfico de barras ascendente



# Mostrando varias gráficas en una sola pantalla



###############################################
# II. Gráficos para dos variables categóricas #
###############################################

#####################################################
# 2.1 Gráfico de barras apiladas (Stacked Bar Plot) #
#     en valor absoluto                             #
#####################################################

ggplot(datos, aes(x=tiporenta,fill=morosidad ) ) +
       geom_bar(position=position_stack()) +
       theme_bw() +
       labs(title = "Situación de la Morosidad según el Tipo de Renta", 
            x= "Tipo de Renta",
            y= "Frecuencia")  +
       scale_fill_manual(values=c("darkolivegreen3", "firebrick2")) 

##### La opción position = "stack" es por default




#####################################################
# 2.2 Gráfico de barras apiladas (Stacked Bar Plot) #
#     en proporción                                 #
#####################################################





#-------------------------------------------------------------------
# Usando paletas de colores
library(RColorBrewer)
display.brewer.all()

# Si se desea ver una de las paletas con más detalles, puede
# hacerlo especificando su nombre y el número de franjas que
# se quiere visualizar (mínimo 3 y máximo 8 variables)

display.brewer.pal(n = 4, name = 'Accent')

ggplot(datos, aes(x=tiporenta,fill=morosidad ) ) +
  geom_bar(position=position_fill()) +
  theme_bw() +
  labs(title = "Situación de la Morosidad según el Tipo de Renta", 
       x = "Tipo de Renta",
       y = "Proporción") + 
  scale_fill_brewer(palette = "Accent")

#-------------------------------------------------------------------
# Aplicando escalas de colores a partir de las funciones 
# rainbow(n), heat.colors(n), terrain.colors(n), 
# topo.colors(n), and cm.colors(n)

ggplot(datos, aes(x=tiporenta,fill=morosidad ) ) +
  geom_bar(position=position_fill()) +
  theme_bw() +
  labs(title = "Situación de la Morosidad según el Tipo de Renta", 
       x = "Tipo de Renta",
       y = "Proporción") + 
  scale_fill_manual(values=heat.colors(2))

# Para eliminar legenda:  theme(legend.position = "none")

#________________________
# ________ \\|// ________
# ________( o o ) _______ 
# ___oo0____(_)____Ooo___
#      Ejercicio   3     #
# Construya un gráfico de barras apilado en proporción para 
# fonolab vs morosidad






######################################################
# 2.3 Gráfico de Barras Agrupadas (Grouped Bar Plot) #
#     en valor absoluto                              #
######################################################

ggplot(datos, aes(x=tiporenta,fill=morosidad ) ) +
  geom_bar(position=position_dodge()) +
  theme_bw() +
  labs(title = "Situación de la Morosidad según el Tipo de Renta", 
       x= "Tipo de Renta",
       y= "Frecuencia")  +
  scale_fill_manual(values=c("darkolivegreen3", "firebrick2")) 



#________________________
# ________ \\|// ________
# ________( o o ) _______ 
# ___oo0____(_)____Ooo___
#      Ejercicio  4     #
# Construya un gráfico de barras agrupadas (sin apilar)
# con valores absolutos para fonolab vs morosidad








############################################
# III. Gráficos para una variable continua #
############################################

###################
# 3.1 Histogramas #
###################

ggplot(datos, aes(edad)) + geom_histogram()

ggplot(datos, aes(edad)) + geom_histogram(stat="bin")

ggplot(datos, aes(edad)) + geom_histogram(color="white")

ggplot(datos, aes(edad)) + geom_histogram(stat="bin",
                                          color="white")

ggplot(datos, aes(edad)) + geom_histogram(color="white",
                                          binwidth =10)

ggplot(datos, aes(edad)) + geom_histogram(color="white",
                                          bins =10)

ggplot(datos, aes(edad)) + geom_histogram(color="white",
                                          fill="deepskyblue3") + 
       labs(title = "Histograma de la Edad", 
            x="Edad", 
            y="Frecuencia") +
       theme_bw() 

ggplotly() # Para crear gráficos interactivos

# Cambiando la posición de la leyenda con legend.position()
ggplot(datos, aes(x=edad,color=morosidad,fill=morosidad)) +
  geom_histogram(alpha=0.5) + 
  theme(legend.position="bottom")

# legend.position="right"
# legend.position="left"
# legend.position="top"
# legend.position="bottom"
# legend.position="none"

# Cambiando los colores de los grupos con scale_fill_manual()
ggplot(datos, aes(x=edad,fill=morosidad)) +
  geom_histogram(alpha=0.5, color="azure4") + 
  theme(legend.position="bottom") +
  scale_fill_manual(values=c("darkgreen", "white")) 

# Histograma por grupo usando facets()
ggplot(datos, aes(edad, fill=morosidad)) + 
  geom_histogram(alpha=0.5,color="azure4") +
  facet_grid(morosidad~.)  + 
  theme(legend.position="none")

ggplot(datos, aes(edad, fill=morosidad)) + 
  geom_histogram(alpha=0.5,color="azure4") +
  facet_wrap(morosidad~.)  + 
  theme(legend.position="none")

#________________________
# ________ \\|// ________
# ________( o o ) _______ 
# ___oo0____(_)____Ooo___
#      Ejercicio   5     #
# Construya un histograma para la variable antiguedad 
# según la morosidad




# Polígono de Frecuencias
ggplot(datos, aes(edad)) + geom_freqpoly(color="black") 

# Gráfico de Densidad
ggplot(datos, aes(edad)) + geom_density(color="black")

# Gráfico de Densidad por grupo
ggplot(datos, aes(x=edad,color=morosidad,fill=morosidad)) +
  geom_density(position="identity",alpha=0.5) 

# Gráfico de Densidad por grupo con animación
p3 <-ggplot(datos, aes(x=edad,color=morosidad,fill=morosidad)) +
  geom_density(position="identity",alpha=0.5)  +
  transition_states(morosidad) +
  enter_fade() +
  exit_fade()

animate(p3)

anim_save("Densidad por grupo.jpeg",p3)


# Cambiando los colores de los grupos con scale_fill_manual()
ggplot(datos, aes(x=edad,color=morosidad,fill=morosidad)) +
  geom_density(position="identity",alpha=0.5) + 
  theme(legend.position="bottom") +
  scale_fill_manual(values=c("darkgreen", "white")) +
  scale_color_manual(values=c("black","black")) 

# Gráfico de Densidad por grupo usando facets()
ggplot(datos, aes(edad, fill=morosidad)) + 
  geom_density(position="identity",alpha=0.5) +
  facet_grid(morosidad~.) +
  scale_fill_brewer(palette = "Set1")

ggplot(datos, aes(edad, fill=morosidad)) + 
  geom_density(position="identity",alpha=0.5) +
  facet_wrap(morosidad~.) +
  scale_fill_brewer(palette = "Set1")


#________________________
# ________ \\|// ________
# ________( o o ) _______ 
# ___oo0____(_)____Ooo___
#      Ejercicio   6     #
# Presente un gráfico de densidad  por grupo
# de la antiguedad según la morosidad 




#-------------------------------------------------------------------
# Boxplot de una variable cuantitativa
ggplot(datos, aes(x=" ",y=edad)) + 
  labs(title = "Boxplot de la Edad de los clientes", 
       y= "Edad") +    
  geom_boxplot(fill="blue") +
  theme_gray()

# Boxplot de una variable cuantitativa por categorías usando ggplot2
ggplot(datos, aes(x=morosidad, y=antiguedad)) +
  geom_boxplot(fill=c("cadetblue","firebrick1")) +
  theme_light() +
  labs(title = "BoxPlot de la Antigüedad según la Morosidad", 
       x = "Condición de la morosidad", 
       y = "Antiguedad") 

#________________________
# ________ \\|// ________
# ________( o o ) _______ 
# ___oo0____(_)____Ooo___
#      Ejercicio   7     #
# Presente un boxplot de la edad según la morosidad 






##########################################
# IV. Gráfico para una variable discreta #
##########################################

#-------------------------------------------------------------------
# Gráfico de Varas
ggplot(datos, aes(nrodepen)) + 
  geom_bar(color="black", width=0)+
  theme_light() + 
  labs(title = "Gráfico de Varas Vertical", 
       x= "Número de dependientes", 
       y= "Frecuencia")

################################################
# V. Gráficos para dos variables cuantitativas #
################################################

ggplot(datos, aes(x=antiguedad,y=edad)) + 
  geom_point(stat="identity")

ggplot(datos, aes(x=antiguedad,y=edad)) + 
  geom_point()

# Añadiendo títulos con labs()
ggplot(datos, aes(x=antiguedad,y=edad)) +
       geom_point() +
       labs(title = "Diagrama de Dispersión", 
            x= "Antiguedad", 
            y= "Edad") +
       theme_replace() 

# Añadiendo tendencia con geom_smooth()
ggplot(datos, aes(x=antiguedad,y=edad)) + 
  geom_point(stat="identity") +
  geom_smooth()

####################################################################
# VI. Gráficos para dos variables cuantitativas y otra cualitativa #
####################################################################

# Diagrama de dispersión con cambio de color por grupos 
ggplot(datos,aes(x=antiguedad, y=edad, color=morosidad)) +
       geom_point()

# Diagrama de dispersión con otros cambios por grupos







# Diagrama de dispersión por grupos con animación
library(gganimate)
p1 <- ggplot(datos, aes(x=antiguedad,y=edad, color=morosidad)) + 
        geom_point() +
        transition_states(morosidad) +
        enter_fade() +
        exit_fade()

animate(p1)

anim_save("Diagrama de Dispersion.jpeg",p1)

# enter_ y exit_ se usan para los efectos en el ingreso 
# y la salida de la animación
# Se puede usar otros formatos como *.gif , *.mp4

####################################
# VII. Gráfico de Series de Tiempo #
####################################

library(ggplot2)

# Data economics
# data frame con 478 filas y 6 variables

# date      Año, mes y día 
# pce       Gastos de consumo personal (miles de millones de dólares)
# pop       población total (miles)
# psavert   tasa de ahorro personal
# uempmed   duración mediana del desempleo (semanas)
# unemploy  número de desempleados en miles

str(economics)
View(economics)

# geom_path() conecta las observaciones en el orden en el cual 
#             ellos aparecen en los datos
# geom_line() conecta las observaciones en el orden de la
#             variable que están en el eje x

ggplot(economics, aes(x=date, y=unemploy)) + 
      geom_line(size = 0.8, color = "#E46726") +
      theme_minimal()

library(lubridate)
library(scales)
ggplot(economics, aes(x=date, y=unemploy)) + 
  geom_line(size = 0.8, color = "#E46726") +
  theme_minimal() + 
  scale_x_date(breaks=date_breaks("5 year"), labels=date_format("%Y"))

# data_format("%d %m %Y")

ggplot(economics, aes(x=date, y=unemploy)) + 
  geom_line(size = 0.8, color = "#E46726") +
  theme_minimal() + 
  scale_x_date(breaks=date_breaks("5 year"), labels=date_format("%y"))

library(lubridate)
ggplot(economics, aes(x=date, y=unemploy)) + 
  geom_line(size = 1.0, color = "#E46726") +
  theme_bw() +
  labs(title ="Serie de Tiempo", 
       x="Años", 
       y= "Desempleo") +
  scale_x_date(limits = as.Date(c("1980-01-01","2010-12-31")))


# Serie de tiempo de proporción de la población que está
# desempleada 
ggplot(economics, aes(x=date, y=unemploy/pop)) + 
  geom_line(size = 1.0, color = "firebrick2") +
  theme_bw() +
  labs(title ="Serie de Tiempo", 
       x="Años", 
       y= "Desempleo")



# Serie de Tiempo con animación
library(gganimate)
p2 <- ggplot(economics, aes(x=date, y=unemploy)) + 
      geom_line(size = 0.8, color = "#E46726") +
      theme_minimal() + 
      labs(title ="Serie de Tiempo", 
           x="Años", 
           y="Desempleo") +
      transition_reveal(date) +
      enter_fade() +
      exit_fade() 

p2 

anim_save("Serie de Tiempo.jpeg",p2)

#________________________
# ________ \\|// ________
# ________( o o ) _______ 
# ___oo0____(_)____Ooo___
#      Ejercicio   8     #
# Construya un gráfico de serie de tiempo animado para 
# la variable uempmed

