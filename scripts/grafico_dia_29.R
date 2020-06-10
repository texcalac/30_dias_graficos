# Librerías a utlizar
library(tidyverse)
library(GGally)
library(viridis)

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
  select(ENT_MUN, HIPERTENSION, DIABETES, OBESIDAD) %>% 
  group_by(ENT_MUN) %>% 
  summarise(across(c("HIPERTENSION", "DIABETES", "OBESIDAD"), 
               ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(ENT_MUN = recode(ENT_MUN, "09002" = "Azcapotzalco", 
                          "09003" = "Coyoacan", 
                          "09004" = "Cuajimalpa", 
                          "09005" = "G. A. Madero", 
                          "09006" = "Iztacalco", 
                          "09007" = "Iztapalapa", 
                          "09008" = "Magdalena Contreras", 
                          "09009" = "Milpa Alta", 
                          "09010" = "Alvaro Obregon", 
                          "09011" = "Tlahuac", 
                          "09012" = "Tlalpan", 
                          "09013" = "Xochimilco", 
                          "09014" = "Benito Juarez", 
                          "09015" = "Cuauhtemoc", 
                          "09016" = "Miguel Hidalgo", 
                          "09017" = "Venustiano Carranza")) %>% 
  print()

# genero el gráfico
ggparcoord(defun, 
           columns = 2:4, 
           scale = "globalminmax", 
           groupColumn = 1, 
           showPoints = TRUE, 
           title = "COVID-19: Defunciones acumuladas en la Ciudad de México y principales comorbilidades") + 
  scale_color_manual(values = c("#1a476f", "#90353b", "#55752f", "#e37e00", "#6e8e84", "#c10534", 
                                  "#938dd2", "#cac27e", "#a0522d", "#7b92a8", "#2d6d66", "#9c8847", 
                                  "#bfa19c", "#ffd200", "#d9e6eb", "#4F94CD")) + 
  theme_bw(base_family = "Avenir") + 
  theme(plot.title = element_text(size = 18, colour = "grey30"), 
        axis.title = element_blank(), 
        legend.title = element_blank(), 
        legend.text = element_text(colour = "grey20"))

# exporto el gráfico 
ggsave("./salidas/plot_dia_29.png", 
       width = 11, height = 8.5, units = 'in', dpi = 128)
