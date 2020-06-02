# librerías a utilizar
library(tidyverse)
library(sf)
library(jcolors)
library(ggsflabel)

# Capas de la Ciudad de México y estaciones de monitoreo
cdmx <- st_read(".datos/cdmx.shp")

sites <-st_read(".datos/monitores.shp")

# concentraciones de PM2.5 (Fuente: Informe de calidad del aire 2017. Disponible en: aire.cdmx.gob.mx)
contam <- 
  tibble(site = c("TLA", "XAL", "MGH", "HGM", "BJU", "SFE", "CCA", "AJM", "UAX", "NEZ"), 
         pm25 = c(26, 32, 22, 25, 24, 20, 20, 19, 22, 24)) %>% 
  print()

# calculo los porcentajes y genero columna con las etiquetas pàra el mapa
contam <- 
  contam %>% 
  mutate(porcentaje = round(pm25 * 100 / 12 - 100, 2), 
         etiqueta = str_c(site, "\n", format(porcentaje, nsmall = 2), " %")) %>% 
  print()

# uno la capa de monitores y la malla con las concentraciones por estación 
sites <- inner_join(sites, contam, by = "site") %>% print()

# genero el gráfico
ggplot() + 
  geom_sf(data = cdmx, fill = NA, colour = "darkgreen") + 
  geom_sf_label_repel(data = sites, aes(label = etiqueta), colour = "darkgreen", 
                      fill = alpha("palegreen", 0.3), force = 100) +
  geom_sf(data = sites, aes(size = pm25, colour = porcentaje)) + 
  scale_colour_jcolors_contin(palette = "pal2") + 
  labs(title = expression(paste("Porcentaje por arriba del promedio anual de 12 ", mu, "g/", m^3, " de ", PM[2.5])),
       subtitle = "\n establecido como valor máximo permisible en México - 2017") +
  theme_void() + 
  theme(plot.title = element_text(size = 18, colour = "darkslategrey", hjust = 0.5), 
        plot.subtitle = element_text(size = 18, colour = "darkslategrey", hjust = 0.5), 
        legend.position = "none")

