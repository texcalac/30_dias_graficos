# Cargo las librerías a utilizar
library(tidyverse)
library(tsibble)

# Descargamos los datos a visualizar
pm25 <- 
  read_csv("http://www.aire.cdmx.gob.mx/opendata/red_manual/red_manual_particulas_susp.csv", 
           skip = 8) %>% 
  filter(cve_parameter == "PM2.5") %>% 
  mutate(fecha = as.Date(Date, "%d/%m/%Y")) %>% 
  select(fecha, cve_station, cve_parameter, value) %>% 
  print()

# Genero objeto tsibble para dar "formato" de serie de tiempo
pm25_ts <- 
  as_tsibble(pm25, key = cve_station, index = fecha) %>% 
  print()

# Genero gráfica
ggplot(data = pm25_ts, aes(x = fecha, y = value)) + 
  geom_line(aes(colour = cve_station)) + 
  labs(title = expression(paste("Variación de los promedios de 24 h de ", PM[2.5], " en la Ciudad de México")), 
       caption = "fuente: http://www.aire.cdmx.gob.mx/opendata/red_manual") + 
  facet_grid(vars(cve_station)) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  theme_bw(base_family = "Avenir") + 
  theme(plot.title = element_text(size = 16, colour = "dodgerblue4", hjust = 0.5), 
        panel.border = element_rect(colour = "grey40", fill = NA), 
        plot.caption = element_text(face = "italic"), 
        axis.title = element_blank(), 
        axis.text = element_text(size = 9, colour = "grey40"), 
        axis.ticks = element_line(colour = "grey50"), 
        legend.position = "none", 
        strip.background = element_rect(fill = alpha('dodgerblue4', 0.9)), 
        strip.text = element_text(size = 10, colour = "white", face = "bold"))

