# Liberías a utilizar
library(tidyverse)
library(treemapify)
library(viridis)
library(scales)

# descargamos los datos necesarios para la visualización
defunciones <- 
  read_csv("https://coronavirus.gob.mx/datos/Downloads/Files/Casos_Diarios_Estado_Nacional_Defunciones_20200524.csv") %>% 
  print()

# proceso los datos
defun_tot <- 
  defunciones %>% 
  pivot_longer(-c(cve_ent, poblacion, nombre), 
             names_to = "fecha", 
             values_to = "casos") %>% 
  filter(nombre != "Nacional") %>% 
  group_by(nombre) %>% 
  summarise(tot_def = sum(casos, na.rm = T)) %>% 
  mutate(porcentaje = round((tot_def * 100) / sum(tot_def), 2), 
         nombre = str_to_title(nombre), 
         nombre = recode(nombre, 
                         "Distrito Federal" = "Ciudad de México", 
                         "Mexico" = "Estado de México")) %>% 
  print()

# Genero el gráfico
ggplot(data = defun_tot, aes(area = porcentaje, fill = tot_def, label = nombre)) +
  geom_treemap() + 
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre", grow = TRUE) + 
  labs(title = "COVID-19: Defunciones por entidad en México (corte al 24 de mayo 2020)", 
       caption = "https://coronavirus.gob.mx/datos/") + 
  scale_fill_viridis(label = comma) + 
  theme(plot.title = element_text(size = 24, hjust = 0.5, colour = "maroon4", 
                                  margin = margin(10, 0, 10, 0)), 
        plot.caption = element_text(face = "italic", size = 10), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 12, colour = "blue4"), 
        legend.position = "bottom", 
        legend.key.width = unit(3,"cm"), 
        legend.margin = margin(t = -0.2, unit='cm'))

