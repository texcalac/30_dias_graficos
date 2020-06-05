# Librerías a utilizar
library(tidyverse)
library(sf)
library(RColorBrewer)

# descargamos mapa de cdmx
cdmx <- st_read("https://datos.cdmx.gob.mx/explore/dataset/alcaldias/download/?format=geojson&timezone=America/Mexico_City&lang=es")

# Descargar datos de aquí, la base varía de acuerdo a la fecha de descarga
# http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip

# Cargo los datos a trabajar
defunciones <- read_csv("./datos/200527COVID19MEXICO.csv") %>% print()

# Proceso los datos para el gráfico
defun <- 
  defunciones %>% 
  filter(!is.na(FECHA_DEF), 
         RESULTADO == 1, 
         ENTIDAD_RES == "09") %>% 
  select(ENTIDAD_RES, MUNICIPIO_RES) %>% 
  count(ENTIDAD_RES, MUNICIPIO_RES, name = "tot_casos") %>% 
  print()

# Agrego datos de defunciones a capa de la cdmx
cdmx_defun <- 
  inner_join(cdmx, 
             defun, 
             by = c("cve_ent" = "ENTIDAD_RES", "cve_mun" = "MUNICIPIO_RES"))

cdmx_defun <- 
  cdmx_defun %>% 
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]), 
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
  select(cvegeo, nomgeo, tot_casos, lon, lat) %>% 
  print()

ggplot(data = cdmx_defun) + 
  geom_sf(aes(fill = tot_casos), colour = "white") + 
  geom_text(aes(label = nomgeo, x = lon, y = lat), size = 2.5) +
  scale_fill_distiller(palette = "Oranges", 
                       breaks = seq(50, 500, 50)) + 
  labs(title = "COVID-19: Defunciones acumuladas en la Ciudad de México", 
       caption = "https://datos.cdmx.gob.mx") + 
  theme_bw() + 
  theme(plot.title = element_text(size = 20, hjust = 0.5, colour = "white", 
                                  margin = margin(10, 0, 10, 0)), 
        plot.caption = element_text(face = "italic", size = 10, colour = "grey95"), 
        axis.title = element_blank(), 
        axis.text = element_text(colour = "grey70"), 
        plot.background = element_rect(fill = "grey20"), 
        panel.background = element_rect(fill = "grey20"), 
        panel.grid = element_line(colour = "grey30"), 
        legend.title = element_blank(), 
        legend.key.height = unit(2,"cm"), 
        legend.background = element_rect(fill = "grey20"),
        legend.text = element_text(colour = "grey95"))

