######################################
# EXPLORACI�N DE DATOS CON TIDYVERSE #
#        GR�FICOS CON GGPLOT2        #    
#       Mg. Jes�s Salinas Flores     # 
#       jsalinas@lamolina.edu.pe     #
######################################

#-------------------------------------------------------------------
# Para limpiar el workspace, por si hubiera algun dataset 
# o informacion cargada
rm(list = ls())

#-------------------------------------------------------------------
# Para limpiar el �rea de gr�ficos
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
#  GR�FICOS CON GGPLOT2  #
##########################

library(foreign)
datos <- read.spss("Riesgo_morosidad.sav", 
                  use.value.labels = T,  
                  to.data.frame=TRUE)

attr(datos,"variable.labels") <- NULL

attach(datos)

#####################################
# Gr�ficos con el paquete ggplot2   #
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
# I. Gr�ficos para una variable cualitativa #
#############################################

######################################
# 1.1 Gr�ficos de Barras con ggplot2 #
######################################

library(ggplot2) 

#-------------------------------------------------------------------
# Gr�fico de Barras para una variable categ�rica
ggplot(data=datos, aes(x=morosidad))

ggplot(data=datos, aes(x=morosidad)) + geom_bar(stat="count")

# Equivalente al anterior




# Gr�fico horizontal con coord_flip()
ggplot(datos, aes(morosidad)) + geom_bar() + coord_flip()

# A�adiendo temas de fondo con theme_light()
ggplot(datos, aes(morosidad)) + geom_bar() + theme_light()

# Temas: theme_bw()      theme_classic()   theme_dark()
#        theme_get()     theme_gray()      theme_grey()
#        theme_light()   theme_linedraw()  theme_minimal()
#        theme_replace() theme_set()       theme_update()
#        theme_void()

# A�adiendo m�s temas
library(ggthemes)

# A�adiendo t�tulos al gr�fico con labs()
ggplot(datos, aes(morosidad)) + 
       geom_bar() + 
       theme_light() + 
       labs(title = "Gr�fico de Barras Vertical", 
            x="Condici�n de la morosidad", 
            y="Frecuencia") 

# Cambiando color de la barra por defecto usando la opci�n fill






# Especificando colores de la barra
ggplot(datos, aes(morosidad)) + 
       geom_bar(color="blue",fill="White") + 
       theme_light() + 
       labs(title = "Gr�fico de Barras Vertical", 
            x= "Condici�n de la morosidad", 
            y= "Frecuencia") 

# Usando diferentes colores en las barras
ggplot(datos, aes(morosidad)) + 
       geom_bar(color="black",fill=c("darkgreen","orange")) +
       theme_light() + 
       labs(title = "Gr�fico de Barras Vertical", 
            x= "Condici�n de la morosidad", 
            y= "Frecuencia")

# Cambiando los colores por defecto usando scale_fill_manual



# Asignando colores a las categor�as de la variable
ggplot(datos, aes(morosidad,fill=morosidad)) + 
  geom_bar(color="black") +
  theme_light() + 
  labs(title = "Gr�fico de Barras Vertical", 
       x= "Condici�n de la morosidad", 
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
  labs(title = "Gr�fico de Barras Vertical", 
       x= "Condici�n de la morosidad", 
       y= "Frecuencia")

# length(levels(morosidad))

# Gr�fico interactivo con el paquete plotly




#________________________
# ________ \\|// ________
# ________( o o ) _______ 
# ___oo0____(_)____Ooo___
#      Ejercicio   1     #
# Construya un gr�fico de barras para la variable dpto






#-------------------------------------------------------------------
# Diagrama de barras con animaci�n
library(gganimate)
b1 <- ggplot(datos, aes(morosidad)) + 
             geom_bar(color="black",fill=c("darkgreen","orange"))




animate(b1)

# enter_ y exit_ se usan para los efectos
# en el ingreso y la salida de la animaci�n

#________________________
# ________ \\|// ________
# ________( o o ) _______ 
# ___oo0____(_)____Ooo___
#      Ejercicio   2     #
# Construya un gr�fico de barras animado para la variable dpto








#-------------------------------------------------------------------
# Reordenando las barras con el paquete forcats
levels(dpto)

# Gr�fico de barras ordenada por defecto seg�n los niveles 
ggplot(datos,aes(x=dpto)) + 
  geom_bar(color="blue",fill="White") + 
  theme_light() + 
  labs(title = "Gr�fico de Barras Vertical", 
       x= "Departamento", 
       y= "Frecuencia")  

# Gr�fico de barras descendente
library(forcats)


# Gr�fico de barras ascendente



# Mostrando varias gr�ficas en una sola pantalla



###############################################
# II. Gr�ficos para dos variables categ�ricas #
###############################################

#####################################################
# 2.1 Gr�fico de barras apiladas (Stacked Bar Plot) #
#     en valor absoluto                             #
#####################################################

ggplot(datos, aes(x=tiporenta,fill=morosidad ) ) +
       geom_bar(position=position_stack()) +
       theme_bw() +
       labs(title = "Situaci�n de la Morosidad seg�n el Tipo de Renta", 
            x= "Tipo de Renta",
            y= "Frecuencia")  +
       scale_fill_manual(values=c("darkolivegreen3", "firebrick2")) 

##### La opci�n position = "stack" es por default




#####################################################
# 2.2 Gr�fico de barras apiladas (Stacked Bar Plot) #
#     en proporci�n                                 #
#####################################################





#-------------------------------------------------------------------
# Usando paletas de colores
library(RColorBrewer)
display.brewer.all()

# Si se desea ver una de las paletas con m�s detalles, puede
# hacerlo especificando su nombre y el n�mero de franjas que
# se quiere visualizar (m�nimo 3 y m�ximo 8 variables)

display.brewer.pal(n = 4, name = 'Accent')

ggplot(datos, aes(x=tiporenta,fill=morosidad ) ) +
  geom_bar(position=position_fill()) +
  theme_bw() +
  labs(title = "Situaci�n de la Morosidad seg�n el Tipo de Renta", 
       x = "Tipo de Renta",
       y = "Proporci�n") + 
  scale_fill_brewer(palette = "Accent")

#-------------------------------------------------------------------
# Aplicando escalas de colores a partir de las funciones 
# rainbow(n), heat.colors(n), terrain.colors(n), 
# topo.colors(n), and cm.colors(n)

ggplot(datos, aes(x=tiporenta,fill=morosidad ) ) +
  geom_bar(position=position_fill()) +
  theme_bw() +
  labs(title = "Situaci�n de la Morosidad seg�n el Tipo de Renta", 
       x = "Tipo de Renta",
       y = "Proporci�n") + 
  scale_fill_manual(values=heat.colors(2))

# Para eliminar legenda:  theme(legend.position = "none")

#________________________
# ________ \\|// ________
# ________( o o ) _______ 
# ___oo0____(_)____Ooo___
#      Ejercicio   3     #
# Construya un gr�fico de barras apilado en proporci�n para 
# fonolab vs morosidad






######################################################
# 2.3 Gr�fico de Barras Agrupadas (Grouped Bar Plot) #
#     en valor absoluto                              #
######################################################

ggplot(datos, aes(x=tiporenta,fill=morosidad ) ) +
  geom_bar(position=position_dodge()) +
  theme_bw() +
  labs(title = "Situaci�n de la Morosidad seg�n el Tipo de Renta", 
       x= "Tipo de Renta",
       y= "Frecuencia")  +
  scale_fill_manual(values=c("darkolivegreen3", "firebrick2")) 



#________________________
# ________ \\|// ________
# ________( o o ) _______ 
# ___oo0____(_)____Ooo___
#      Ejercicio  4     #
# Construya un gr�fico de barras agrupadas (sin apilar)
# con valores absolutos para fonolab vs morosidad








############################################
# III. Gr�ficos para una variable continua #
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

ggplotly() # Para crear gr�ficos interactivos

# Cambiando la posici�n de la leyenda con legend.position()
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
# seg�n la morosidad




# Pol�gono de Frecuencias
ggplot(datos, aes(edad)) + geom_freqpoly(color="black") 

# Gr�fico de Densidad
ggplot(datos, aes(edad)) + geom_density(color="black")

# Gr�fico de Densidad por grupo
ggplot(datos, aes(x=edad,color=morosidad,fill=morosidad)) +
  geom_density(position="identity",alpha=0.5) 

# Gr�fico de Densidad por grupo con animaci�n
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

# Gr�fico de Densidad por grupo usando facets()
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
# Presente un gr�fico de densidad  por grupo
# de la antiguedad seg�n la morosidad 




#-------------------------------------------------------------------
# Boxplot de una variable cuantitativa
ggplot(datos, aes(x=" ",y=edad)) + 
  labs(title = "Boxplot de la Edad de los clientes", 
       y= "Edad") +    
  geom_boxplot(fill="blue") +
  theme_gray()

# Boxplot de una variable cuantitativa por categor�as usando ggplot2
ggplot(datos, aes(x=morosidad, y=antiguedad)) +
  geom_boxplot(fill=c("cadetblue","firebrick1")) +
  theme_light() +
  labs(title = "BoxPlot de la Antig�edad seg�n la Morosidad", 
       x = "Condici�n de la morosidad", 
       y = "Antiguedad") 

#________________________
# ________ \\|// ________
# ________( o o ) _______ 
# ___oo0____(_)____Ooo___
#      Ejercicio   7     #
# Presente un boxplot de la edad seg�n la morosidad 






##########################################
# IV. Gr�fico para una variable discreta #
##########################################

#-------------------------------------------------------------------
# Gr�fico de Varas
ggplot(datos, aes(nrodepen)) + 
  geom_bar(color="black", width=0)+
  theme_light() + 
  labs(title = "Gr�fico de Varas Vertical", 
       x= "N�mero de dependientes", 
       y= "Frecuencia")

################################################
# V. Gr�ficos para dos variables cuantitativas #
################################################

ggplot(datos, aes(x=antiguedad,y=edad)) + 
  geom_point(stat="identity")

ggplot(datos, aes(x=antiguedad,y=edad)) + 
  geom_point()

# A�adiendo t�tulos con labs()
ggplot(datos, aes(x=antiguedad,y=edad)) +
       geom_point() +
       labs(title = "Diagrama de Dispersi�n", 
            x= "Antiguedad", 
            y= "Edad") +
       theme_replace() 

# A�adiendo tendencia con geom_smooth()
ggplot(datos, aes(x=antiguedad,y=edad)) + 
  geom_point(stat="identity") +
  geom_smooth()

####################################################################
# VI. Gr�ficos para dos variables cuantitativas y otra cualitativa #
####################################################################

# Diagrama de dispersi�n con cambio de color por grupos 
ggplot(datos,aes(x=antiguedad, y=edad, color=morosidad)) +
       geom_point()

# Diagrama de dispersi�n con otros cambios por grupos







# Diagrama de dispersi�n por grupos con animaci�n
library(gganimate)
p1 <- ggplot(datos, aes(x=antiguedad,y=edad, color=morosidad)) + 
        geom_point() +
        transition_states(morosidad) +
        enter_fade() +
        exit_fade()

animate(p1)

anim_save("Diagrama de Dispersion.jpeg",p1)

# enter_ y exit_ se usan para los efectos en el ingreso 
# y la salida de la animaci�n
# Se puede usar otros formatos como *.gif , *.mp4

####################################
# VII. Gr�fico de Series de Tiempo #
####################################

library(ggplot2)

# Data economics
# data frame con 478 filas y 6 variables

# date      A�o, mes y d�a 
# pce       Gastos de consumo personal (miles de millones de d�lares)
# pop       poblaci�n total (miles)
# psavert   tasa de ahorro personal
# uempmed   duraci�n mediana del desempleo (semanas)
# unemploy  n�mero de desempleados en miles

str(economics)
View(economics)

# geom_path() conecta las observaciones en el orden en el cual 
#             ellos aparecen en los datos
# geom_line() conecta las observaciones en el orden de la
#             variable que est�n en el eje x

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
       x="A�os", 
       y= "Desempleo") +
  scale_x_date(limits = as.Date(c("1980-01-01","2010-12-31")))


# Serie de tiempo de proporci�n de la poblaci�n que est�
# desempleada 
ggplot(economics, aes(x=date, y=unemploy/pop)) + 
  geom_line(size = 1.0, color = "firebrick2") +
  theme_bw() +
  labs(title ="Serie de Tiempo", 
       x="A�os", 
       y= "Desempleo")



# Serie de Tiempo con animaci�n
library(gganimate)
p2 <- ggplot(economics, aes(x=date, y=unemploy)) + 
      geom_line(size = 0.8, color = "#E46726") +
      theme_minimal() + 
      labs(title ="Serie de Tiempo", 
           x="A�os", 
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
# Construya un gr�fico de serie de tiempo animado para 
# la variable uempmed

