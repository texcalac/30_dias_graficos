# Librerías a utilizar
library(tidyverse)
library(gridExtra)
library(grid)
library(viridis)
library(RColorBrewer)
library(jcolors)

# Descargo datos de contaminantes de la página del Gobierno de la Ciudad de México
aire <- 
  read_csv("http://www.aire.cdmx.gob.mx/opendata/promedios_diarios/promedios_2020_ps.csv", 
           skip = 8) %>% 
  print()

# Preparo los datos
pm25 <- 
  aire %>% 
  filter(id_parameter == "PM2.5") %>% 
  group_by(date) %>% 
  summarise(pm25 = max(value, na.rm = T)) %>% 
  mutate(date = as.Date(date, "%d/%m/%Y"), 
         cat = case_when(pm25 <= 25 ~ "Buena", 
                         pm25 > 25 & pm25 <= 45 ~ "No OMS", 
                         pm25 > 45 ~ "No NOM", 
                         TRUE ~ NA_character_)) %>% 
  arrange(date) %>% 
  print()

# Descargo el tema para el gráfico
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# Genero gráfico con colores por defecto
graf_1 <- 
  ggplot() + 
  geom_point(data = pm25, 
             aes(date, pm25, color = factor(cat), size = pm25, alpha = 0.5), 
             stroke = 1) + 
  labs(title = "colores por defecto")  + 
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 0.5)), 
         size = "none", 
         alpha = "none") + 
  scale_color_discrete(breaks = c("Buena","No OMS","No NOM"), 
                       labels = c("Calidad del \naire Buena", 
                                  "NO cumple valor \nguía de la OMS", 
                                  "NO cumple \nNorma Oficial Mexicana")) + 
  theme_bw(base_family = "Avenir") + 
  theme(axis.title = element_blank(), 
        axis.text.x = element_text(colour = "grey50", size = 12), 
        axis.text.y = element_text(colour = "grey50", size = 11), 
        axis.ticks = element_line(colour = "grey60"), 
        plot.title = element_text(size = 14, hjust = 0.5, colour = "grey40"), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.border = element_rect(colour = "grey60"), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.text = element_text(colour = "grey30", size = 10))

# Genero gráfico con colores con la paleta Viridis
graf_2 <- 
  ggplot() + 
  geom_point(data = pm25, 
             aes(date, pm25, color = factor(cat), size = pm25, alpha = 0.5), stroke = 1) + 
  scale_color_viridis(discrete = T, 
                      breaks = c("Buena","No OMS","No NOM"), 
                      labels = c("Calidad del \naire Buena", 
                                 "NO cumple valor \nguía de la OMS", 
                                 "NO cumple \nNorma Oficial Mexicana")) + 
  labs(title = "viridis")  + 
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 0.5)), 
         size = "none", 
         alpha = "none") + 
  theme_bw(base_family = "Avenir") + 
  theme(axis.title = element_blank(), 
        axis.text.x = element_text(colour = "grey50", size = 12), 
        axis.text.y = element_text(colour = "grey50", size = 11), 
        axis.ticks = element_line(colour = "grey60"), 
        plot.title = element_text(size = 14, hjust = 0.5, colour = "grey40"), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.border = element_rect(colour = "grey60"), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.text = element_text(colour = "grey30", size = 10))

# Genero gráfico con colores con la paleta RColorBrewer
graf_3 <- 
  ggplot() + 
  geom_point(data = pm25, 
             aes(date, pm25, color = factor(cat), size = pm25, alpha = 0.5), stroke = 1) + 
  scale_colour_brewer(palette = "Accent", 
                      breaks = c("Buena","No OMS","No NOM"), 
                      labels = c("Calidad del \naire Buena", 
                                 "NO cumple valor \nguía de la OMS", 
                                 "NO cumple \nNorma Oficial Mexicana")) + 
  labs(title = "rcolorbrewer")  + 
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 0.5)), 
         size = "none", 
         alpha = "none") + 
  theme_bw(base_family = "Avenir") + 
  theme(axis.title = element_blank(), 
        axis.text.x = element_text(colour = "grey50", size = 12), 
        axis.text.y = element_text(colour = "grey50", size = 11), 
        axis.ticks = element_line(colour = "grey60"), 
        plot.title = element_text(size = 14, hjust = 0.5, colour = "grey40"), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.border = element_rect(colour = "grey60"), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.text = element_text(colour = "grey30", size = 10))

# Genero gráfico con colores con la paleta jcolors
graf_4 <- 
  ggplot() + 
  geom_point(data = pm25, 
             aes(date, pm25, color = factor(cat), size = pm25, alpha = 0.5), stroke = 1) + 
  scale_color_jcolors(palette = "pal2", 
                      breaks = c("Buena","No OMS","No NOM"), 
                      labels = c("Calidad del \naire Buena", 
                                 "NO cumple valor \nguía de la OMS", 
                                 "NO cumple \nNorma Oficial Mexicana")) + 
  labs(title = "jcolors")  + 
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 0.5)), 
         size = "none", 
         alpha = "none") + 
  theme_bw(base_family = "Avenir") + 
  theme(axis.title = element_blank(), 
        axis.text.x = element_text(colour = "grey50", size = 12), 
        axis.text.y = element_text(colour = "grey50", size = 11), 
        axis.ticks = element_line(colour = "grey60"), 
        plot.title = element_text(size = 14, hjust = 0.5, colour = "grey40"), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.border = element_rect(colour = "grey60"), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.text = element_text(colour = "grey30", size = 10))

# Genero el gráfico con todas las paletas de colores
grid.arrange(graf_1, graf_2, graf_3, graf_4, nrow = 2, 
             top = textGrob(label = expression(paste("Concentración de ", PM[2.5], 
                                                     " en estaciones de la Ciudad de México - datos 2020")), 
                            gp = gpar(fontsize = 16, col = "dodgerblue4")), 
             bottom = textGrob("Fuente: http://www.aire.cdmx.gob.mx/", 
                               gp = gpar(fontface = 3, 
                                         fontsize = 9), 
                               hjust = 1, x = 0.99))


             