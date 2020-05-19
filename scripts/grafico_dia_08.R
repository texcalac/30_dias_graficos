# Librerías a utilizar
library(tidyverse)
library(sf)
library(gstat)

# genero malla de datos para trabajar
# Datos del "2020-04-30")
contam <- 
  tibble(pm25 = c(12.2, 11.8, 13.2, 12.2, 12.7, 13.2, 11.9, 10.8, 14.5, 16.0, 
                 18.7, 15.5, 10.6, 13.5, 11.3, 11.9), 
         lon = c(-99.20774, -99.16261, -99.15960, -99.16979, -99.17611, -99.04618, 
                -99.38052, -99.11959, -99.02821, -99.20414, -99.00938, -99.03032, 
                -99.26287, -99.20460, -99.10363, -99.07388), 
         lat = c(19.27216, 19.15429, 19.37046, 19.46840, 19.32611, 19.47369, 
                19.29197, 19.42461, 19.39373, 19.32515, 19.34561, 19.53297, 
                19.35736, 19.52908, 19.30444, 19.36079), 
         elev = c(2548, 2942, 2249, 2233, 2294, 2230, 3082, 2245, 2235, 2326, 
                 2293, 2241, 2599, 2311, 2246, 2221)) %>% 
  print()

# Genero capa de estaciones de monitoreo
contam_geog <- 
  st_as_sf(contam, coords = c('lon', 'lat'), crs = 4326) %>% 
  print()

# área que abarca las posiciones extremas de las estaciones de monitoreo
area <- st_as_sfc(st_bbox(contam_geog))

# Genero un grid del área a interpolar
contam_grid <- st_make_grid(area, what = "centers", n = c(100, 100)) %>% print()

# Interpolación de los valores del grid por IDW ponderada al cuadrado
pm25_idw <- 
  idw(formula = pm25 ~ 1, 
      locations = contam_geog, 
      newdata = contam_grid, 
      idp = 2.0)

# Generamos el gráfico
ggplot(pm25_idw, aes(x = unlist(map(pm25_idw$geometry, 1)), 
                     y = unlist(map(pm25_idw$geometry, 2)), 
                     z = var1.pred)) + 
  geom_raster(aes(fill = var1.pred), interpolate = TRUE) + 
  geom_contour(colour = "white", binwidth = 0.5, size = 0.2) + 
  scale_fill_viridis_c(option = "A", name = "") + 
  labs(title = expression(paste("Promedio de 24h de ", PM[2.5], " en la Ciudad de México")), 
       subtitle = "datos del 30 de abril de 2020", 
       caption = "Fuente: http://www.aire.cdmx.gob.mx/opendata/red_manual/red_manual_particulas_susp.csv") + 
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.02))) + 
  coord_fixed() + 
  theme_void(base_family = "Avenir") + 
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 12), 
        legend.key.width = unit(2, "cm"), 
        plot.caption = element_text(face = "italic", size = 9), 
        plot.title = element_text(size = 16, color = "grey20", hjust = 0.5), 
        plot.subtitle = element_text(size = 14, color = "grey20", hjust = 0.5, face = "italic"))

