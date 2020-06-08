# Librerías a utilizar
library(tidyverse)
library(thatssorandom)
library(viridis)


# Descargar datos de aquí, la base varía de acuerdo a la fecha de descarga
# http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip

# Cargo los datos a trabajar
defunciones <- read_csv("./datos/200602COVID19MEXICO.csv") %>% print()

# top estados con más casos
top_edos <- 
  defunciones %>% 
  filter(!is.na(FECHA_DEF), 
         RESULTADO == 1) %>% 
  count(ENTIDAD_RES, name = "tot_defun") %>% 
  top_n(5) %>% 
  arrange(desc(tot_defun)) %>% 
  select(ENTIDAD_RES) %>% 
  pull()

# Proceso los datos para el gráfico
defun <- 
  defunciones %>% 
  filter(!is.na(FECHA_DEF), 
         RESULTADO == 1) %>% 
  select(ENTIDAD_RES, HIPERTENSION, DIABETES, OBESIDAD) %>% 
  mutate(HIPERTENSION = case_when(HIPERTENSION == 1 ~ "Hipertensión", TRUE ~ NA_character_), 
         DIABETES = case_when(DIABETES == 1 ~ "Diabetes", TRUE ~ NA_character_), 
         OBESIDAD = case_when(OBESIDAD == 1 ~ "Obesidad", TRUE ~ NA_character_)) %>% 
  filter_at(vars(HIPERTENSION, DIABETES, OBESIDAD), any_vars(!is.na(.))) %>% 
  unite("comorbs", HIPERTENSION:OBESIDAD, na.rm = TRUE, remove = FALSE) %>% 
  select(ENTIDAD_RES, comorbs) %>% 
  filter(ENTIDAD_RES %in% top_edos) %>% 
  mutate(ENTIDAD_RES = recode(ENTIDAD_RES, 
                      "02" = "Baja California", 
                      "09" = "Ciudad de México", 
                      "15" = "Estado de México", 
                      "27" = "Tabasco", 
                      "30" = "Veracruz")) %>% 
  print()

# Genero el gráfico
ggmm(defun, ENTIDAD_RES, comorbs, add_text = "perc") + 
  scale_fill_viridis(discrete = T, direction = -1) + 
  theme(axis.title = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        legend.title = element_blank(), 
        legend.key.size = unit(1, "cm"),  
        legend.text = element_text(size = 11, colour = "grey20"))

# Guardo el gráfico
ggsave("./salidas/plot_dia_26.png", width = 11, height = 8.5, units = 'in', dpi = 128)

