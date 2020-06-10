# Librerías a utlizar
library(tidyverse)

# Descargar datos de aquí, la base varía de acuerdo a la fecha de descarga
# http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip

# Cargo los datos a trabajar
defunciones <- read_csv("./datos/200609COVID19MEXICO.csv") %>% print()

# Proceso los datos a utilizar
defun <- 
  defunciones %>% 
  filter(!is.na(FECHA_DEF), 
         RESULTADO == 1, 
         INTUBADO == 1, 
         ENTIDAD_RES == "09") %>% 
  mutate(ENT_MUN = str_c(ENTIDAD_RES, MUNICIPIO_RES)) %>% 
  select(FECHA_DEF, ENT_MUN, HIPERTENSION, DIABETES, OBESIDAD) %>% 
  mutate(fecha = format(FECHA_DEF, "%b-%d")) %>%
  print()

# genero el gráfico
ggplot(defun, aes(x = factor(fecha))) +
  geom_bar(width = 1, colour = "dodgerblue4", fill = "dodgerblue4", alpha = 0.5) + 
  labs(title = "COVID-19: Defunciones diarias en la Ciudad de México") + 
  coord_polar() + 
  theme_bw() + 
  theme(plot.title = element_text(colour = "grey30", size = 18, hjust = 0.5), 
        panel.border = element_blank(), 
        axis.title = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())

# exporto el gráfico
ggsave("./salidas/plot_dia_30.png", 
       width = 8.5, height = 8.5, units = 'in', dpi = 128)
