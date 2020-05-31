# Cargo las librerías a utilizar
library(tidyverse)
library(tsibble)
library(streamgraph)

# Descargamos los datos a visualizar
pm25 <- 
  read_csv("http://www.aire.cdmx.gob.mx/opendata/red_manual/red_manual_particulas_susp.csv", 
           skip = 8) %>% 
  filter(cve_parameter == "PM2.5" & cve_station %in% c("COY", "SAG", "XAL")) %>% 
  mutate(fecha = as.Date(Date, "%d/%m/%Y")) %>% 
  select(fecha, cve_station, cve_parameter, value) %>% 
  print()

# Genero objeto tsibble para dar "formato" de serie de tiempo
pm25_ts <- 
  as_tsibble(pm25, 
             key = cve_station, 
             index = fecha) %>% 
  print()

# Genero el gráfico
pm25_ts %>% 
  streamgraph("cve_station", "value", "fecha") %>% 
  sg_axis_x(1, "fecha", "%Y") %>% 
  sg_legend(show = TRUE, label = "Estación de monitoreo: ")
