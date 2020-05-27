# Lbrerías a utilizar
library(tidyverse)
library(waffle)
library(extrafont)

# descargo los datos necesarios para la visualización
defunciones <- read_csv("https://coronavirus.gob.mx/datos/Downloads/Files/Casos_Diarios_Municipio_Defunciones_20200526.csv")
defunciones

# Proceso los datos
d_tot <- 
  defunciones %>% 
  pivot_longer(-c(cve_ent, poblacion, nombre), 
               names_to = "fecha", 
               values_to = "casos") %>% 
  filter(nombre != "Nacional") %>% 
  group_by(cve_ent, nombre) %>% 
  summarise(tot_def = sum(casos, na.rm = T)) %>% 
  filter(str_detect(cve_ent, "^09")) %>% 
  mutate(def_round = round(tot_def/5)* 5, 
         nombre = factor(nombre)) %>% 
  print()

# Genero el gráfico
ggplot(data = d_tot, aes(label = nombre, 
                         values = def_round/5, 
                         color = nombre)) + 
  geom_pictogram(n_rows = 20) + 
  scale_color_manual(name = NULL,
    values = c("#1a476f", "#90353b", "#55752f", "#e37e00", "#6e8e84", "#c10534", 
               "#938dd2", "#cac27e", "#a0522d", "#7b92a8", "#2d6d66", "#9c8847", 
               "#bfa19c", "#ffd200", "#d9e6eb", "#4F94CD"), 
    labels = c("Azcapotzalco", "Coyoacan", "Cuajimalpa de Morelos", 
               "Gustavo A. Madero", "Iztacalco", "Iztapalapa", 
               "Magdalena Contreras", "Milpa Alta", "Alvaro Obregon", 
               "Tlahuac", "Tlalpan", "Xochimilco", "Benito Juarez", 
               "Cuauhtemoc", "Miguel Hidalgo", "Venustiano Carranza")) + 
  scale_label_pictogram(name = NULL, 
                        values = "male", 
                        labels = c("Azcapotzalco", "Coyoacan", "Cuajimalpa de Morelos", 
                                   "Gustavo A. Madero", "Iztacalco", "Iztapalapa", 
                                   "Magdalena Contreras", "Milpa Alta", "Alvaro Obregon", 
                                   "Tlahuac", "Tlalpan", "Xochimilco", "Benito Juarez", 
                                   "Cuauhtemoc", "Miguel Hidalgo", "Venustiano Carranza")) + 
  labs(title = "COVID-19: Defunciones acumuladas en la Ciudad de México", 
       caption = "Cada persona representa 5 defunciones \n Fuente: https://coronavirus.gob.mx/datos") + 
  coord_equal() + 
  theme_void() + 
  theme(plot.title = element_text(size = 18, colour = "dodgerblue4"), 
        plot.caption = element_text(size = 10, face = "italic", colour = "grey30"), 
        legend.key.height = unit(1.2, "cm"), 
        legend.position = "left", 
        legend.text = element_text(size = 10, colour = "grey20"))


