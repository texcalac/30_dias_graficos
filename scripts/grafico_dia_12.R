# Librería a utilizar
library(tidyverse)
library(hrbrthemes)
library(scales)

# Defunciones por entidad al 22 de mayo 
# https://www.gob.mx/cms/uploads/attachment/file/553562/Comunicado_Tecnico_Diario_COVID-19_2020.05.22.pdf
defun_edo <- 
  tibble(entidad = c("CIUDAD DE MEXICO", "MEXICO", "BAJA CALIFORNIA", "TABASCO", 
                     "SINALOA", "VERACRUZ", "QUINTANA ROO", "CHIHUAHUA", "PUEBLA", 
                     "MORELOS", "HIDALGO", "GUERRERO", "TLAXCALA", "MICHOACAN", 
                     "YUCATAN", "JALISCO", "OAXACA", "GUANAJUATO", "TAMAULIPAS", 
                     "QUERETARO", "CHIAPAS", "COAHUILA", "NUEVO LEON", "SONORA", 
                     "CAMPECHE", "NAYARIT", "BAJA CALIFORNIA SUR", "ZACATECAS", 
                     "DURANGO", "SAN LUIS POTOSI", "AGUASCALIENTES", "COLIMA"), 
         defunciones = c(1854, 809, 648, 371, 366, 331, 283, 245, 244, 228, 201, 
                         154, 119, 110, 109, 102, 93, 85, 80, 69, 67, 64, 62, 
                         56, 50, 40, 31, 27, 26, 26, 25, 14)) %>% 
  print()

# genero el gráfico
ggplot(data = defun_edo, aes(x = fct_reorder(entidad, defunciones), y = defunciones)) + 
  geom_segment(aes(xend = entidad, yend = 0), size = 2, colour = "gold4") + 
  geom_point(size = 3, colour = "gold4") + 
  labs(title = "COVID-19: Defunciones acumuladas en México por entidad federativa al 22 de mayo", 
       x = NULL, 
       y = NULL) + 
  coord_flip() + 
  scale_y_continuous(breaks = seq(100, 1850, 100), label = comma) + 
  theme_ft_rc(base_family = "Avenir") + 
  theme(plot.title = element_text(size = 17, colour = "khaki2", hjust = 0.5), 
        axis.ticks = element_blank(), 
        axis.text.x = element_text(size = 11, colour = "khaki3"), 
        axis.text.y = element_text(size = 9, margin = margin(-10, 0, 0, 0), colour = "darkorange3"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

