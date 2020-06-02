# librerías a utilizar
library(tidyverse)
library(wordcloud)
library(RColorBrewer)

# Cargo la malla de datos
datos <- 
  read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/tiempo_pantalla.csv") %>% 
  print()

# genero el gráfico
wordcloud(datos$nombre, 
          datos$minutos_pantalla, 
          min.freq = 5, 
          colors = brewer.pal(6,"Dark2"), 
          random.order = FALSE)

  
  
