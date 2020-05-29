# Librerías a utilizar
library(tidyverse)
library(sf)
library(osmdata)
library(ggmap)


# descargamos las capas de estaciones de metribús y metro
metrobus <- st_read("https://datos.cdmx.gob.mx/explore/dataset/estaciones-metrobus/download/?format=geojson&timezone=America/Mexico_City&lang=es")
metro <- st_read("https://datos.cdmx.gob.mx/explore/dataset/estaciones-metro/download/?format=geojson&timezone=America/Mexico_City&lang=es")

# Genero como mapa de fondo a la Ciudad de México
cdmx_map <- get_map(getbb("Ciudad de México"), maptype = "toner-background")

# Genero el gráfico
ggmap(cdmx_map) +
  geom_sf(data = metrobus, 
          inherit.aes = FALSE, 
          aes(colour = "tomato3", 
              fill = "tomato3"), 
          alpha = 0.7, 
          size = 3, 
          shape = 21) + 
  geom_sf(data = metro, 
          inherit.aes = FALSE, 
          aes(colour = "dodgerblue4", 
              fill = "dodgerblue"), 
          alpha = 0.8, 
          size = 3, 
          shape = 21) + 
  scale_fill_manual(values = c("dodgerblue", "tomato3"),
                     labels = c("Metro", "Metrobús")) + 
  scale_colour_manual(values = c("dodgerblue4", "tomato3"),
                    labels = c("Metro", "Metrobús")) + 
  guides(colour = FALSE) + 
  labs(title = "Ubicación de estaciones de Metrobús y Metro en la Ciudad de México") + 
  coord_sf(crs = st_crs(4326)) + 
  theme(plot.title = element_text(size = 18, colour = "grey20", hjust = 0.5), 
        axis.title = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.text = element_text(size = 13), 
        legend.key = element_blank())


