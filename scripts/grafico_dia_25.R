# Librerías a utilizar
library(tidyverse)

# Descargo y proceso los datos necesarios para la visualización
pm25 <- 
  read_csv("http://www.aire.cdmx.gob.mx/opendata/red_manual/red_manual_particulas_susp.csv", 
           skip = 8) %>% 
  filter(cve_parameter == "PM2.5") %>% 
  mutate(fecha = as.Date(Date, "%d/%m/%Y")) %>% 
  select(fecha, cve_station, cve_parameter, value) %>% 
  print()

# Genero vector de colores para el gráfico
colores <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628", "#EE1289")

# Genero el gráfico
ggplot(data = pm25) + 
  geom_jitter(aes(x = cve_station, y = value, colour = cve_station), alpha = 0.2, size = 1) + 
  geom_violin(aes(x = cve_station, y = value, colour = cve_station), fill = NA, size = 0.7, draw_quantiles = 0.5) + 
  scale_y_continuous(breaks = seq(10, 110, 10), expand = c(0, 0), limits = c(0, 113)) + 
  scale_color_manual(values = colores) + 
  labs(title = expression(paste("Variación de los promedios de 24 h de ", PM[2.5], " por estación de monitoreo")), 
       subtitle = "Ciudad de México: 2003 - 2020", 
       caption = "Fuente: http://www.aire.cdmx.gob.mx/opendata/red_manual") + 
  theme_bw(base_family = "Avenir") + 
  theme(plot.title = element_text(size = 20, hjust = 0.5, colour = "grey20", 
                                  margin = margin(10, 0, 10, 0)),
        plot.subtitle = element_text(size = 18, hjust = 0.5, colour = "grey20", 
                                  margin = margin(0, 0, 10, 0)), 
        plot.caption = element_text(face = "italic", size = 10, colour = "grey30"), 
        plot.background = element_rect(fill = "#F5F5DC"), 
        panel.background = element_rect(fill = alpha("#F5F5DC", 0.3)), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title = element_blank(), 
        axis.text.x = element_text(size = 11), 
        axis.text.y = element_text(size = 10), 
        legend.position = "none")


