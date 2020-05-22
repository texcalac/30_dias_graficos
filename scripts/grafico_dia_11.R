# Librerías a utilizar
library(tidyverse)
library(ggTimeSeries)
library(ggthemes)
library(scales)
library(gridExtra)
library(grid)

# descargar información
download.file("https://datos.cdmx.gob.mx/explore/dataset/afluencia-preliminar-en-transporte-publico/download/?format=csv&timezone=America/Mexico_City&lang=es&use_labels_for_header=true&csv_separator=%2C",
                       destfile = "datos_movilidad.csv")

# Proceso los datos
datos <- 
  datos %>% 
  rename(linea_serv = `LINEA/SERVICIO`, 
         tot_afluencia = `AFLUENCIA TOTAL\n(Cifras preliminares)`) %>% 
  select(FECHA, ORGANISMO, linea_serv, tot_afluencia) %>% 
  rename_all(tolower) %>% 
  mutate(semana = as.integer(format(fecha, '%W')), 
         mes = factor(format(fecha, '%B'))) %>% 
  print()

# Gráfico de ecobici
ecobici <- 
  datos %>% 
  filter(organismo == "Ecobici") %>% 
  ggplot_calendar_heatmap("fecha", "tot_afluencia", 
                          dayBorderColour = "grey30", 
                          dayBorderSize = 0.1, 
                          monthBorderColour = "grey50", 
                          monthBorderSize = 2) + 
  labs(title = "ecobici") + 
  scale_fill_gradient(low = "orange3", high = "green", label = comma) + 
  theme_tufte(base_family = "Avenir") + 
  theme(plot.title = element_text(size = 15, colour = "sienna3", hjust = 0.5, 
                                  face = "bold", margin = margin(10, 0, 0, 0)), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 11), 
        strip.text = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.key.width = unit(2,"cm"), 
        legend.margin=margin(t = -0.2, unit='cm'))

ecobici

# Gráfico de metrobús
metrobus <- 
  datos %>% 
  filter(organismo == "Metrobús") %>% 
  ggplot_calendar_heatmap("fecha", "tot_afluencia", 
                          dayBorderColour = "grey30", 
                          dayBorderSize = 0.1, 
                          monthBorderColour = "grey50", 
                          monthBorderSize = 2) + 
  labs(title = "metrobús") + 
  scale_fill_gradient(low = "orange3", high = "green", label = comma) + 
  theme_tufte(base_family = "Avenir") + 
  theme(plot.title = element_text(size = 15, colour = "sienna3", hjust = 0.5, 
                                  face = "bold", margin = margin(10, 0, 0, 0)), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 11), 
        strip.text = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.key.width = unit(2,"cm"), 
        legend.margin=margin(t = -0.2, unit='cm'))

metrobus

# Gráfico de metro
metro  <- 
  datos %>% 
  filter(organismo == "STC") %>% 
  ggplot_calendar_heatmap("fecha", "tot_afluencia", 
                          dayBorderColour = "grey30", 
                          dayBorderSize = 0.1, 
                          monthBorderColour = "grey50", 
                          monthBorderSize = 2) + 
  labs(title = "metro") + 
  scale_fill_gradient(low = "orange3", high = "green", label = comma) + 
  theme_tufte(base_family = "Avenir") + 
  theme(plot.title = element_text(size = 15, colour = "sienna3", hjust = 0.5, 
                                  face = "bold", margin = margin(10, 0, 0, 0)), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 11), 
        strip.text = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.key.width = unit(2,"cm"), 
        legend.margin=margin(t = -0.2, unit='cm'))

metro

# Gráfico de tren ligero
tren_ligero <- 
  datos %>% 
  filter(organismo == "STE-Tren Ligero") %>% 
  ggplot_calendar_heatmap("fecha", "tot_afluencia", 
                          dayBorderColour = "grey30", 
                          dayBorderSize = 0.1, 
                          monthBorderColour = "grey50", 
                          monthBorderSize = 2) + 
  labs(title = "tren ligero") + 
  scale_fill_gradient(low = "orange3", high = "green", label = comma) + 
  theme_tufte(base_family = "Avenir") + 
  theme(plot.title = element_text(size = 15, colour = "sienna3", hjust = 0.5, 
                                  face = "bold", margin = margin(10, 0, 0, 0)), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 11), 
        strip.text = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.key.width = unit(2,"cm"), 
        legend.margin=margin(t = -0.2, unit='cm'))

tren_ligero

# Gráfico final
grid.arrange(ecobici, tren_ligero, metro, metrobus, nrow = 2, 
             top = textGrob("Número de viajes en el transporte público de la Ciudad de México - 2020", 
                            gp = gpar(fontsize = 18, col = "gray30", margin = margin(0, 0, 30, 0))), 
             bottom = textGrob("Fuente: https://datos.cdmx.gob.mx/explore/dataset/afluencia-preliminar-en-transporte-publico", 
                               gp = gpar(fontface = 3, 
                                         fontsize = 9, 
                                         col = "gray30"), 
                               hjust = 1, x = 0.99))


