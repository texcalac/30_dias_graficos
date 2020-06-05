library(tidyverse)
library(sunburstR)

# Descargar datos de aquí, la base varía de acuerdo a la fecha de descarga
# http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip

# Cargo los datos a trabajar
defunciones <- read_csv("./datos/200602COVID19MEXICO.csv") %>% print()

# Proceso los datos para el gráfico
defun <- 
  defunciones %>% 
  filter(!is.na(FECHA_DEF), 
         RESULTADO == 1, 
         INTUBADO %in% c(1,2)) %>% 
  select(INTUBADO, EMBARAZO:TABAQUISMO) %>% 
  rename_all(tolower) %>% 
  mutate(intubado = factor(intubado, 
                           levels = c(1, 2), 
                           labels = c("Intubado", "No intubado"))) %>% 
  mutate_at(vars(embarazo:tabaquismo), function(x) case_when(x == 1 ~ 1, 
                                                             x == 2 ~ 0, 
                                                             TRUE ~ NA_real_)) %>% 
  pivot_longer(-intubado, 
               names_to = "condicion", 
               values_to = "casos") %>% 
  group_by(intubado, condicion) %>% 
  summarise(tot_casos = sum(casos, na.rm = T)) %>% 
  mutate(condicion = factor(condicion, 
                            levels = c("embarazo", "asma", "habla_lengua_indig", 
                                       "inmusupr", "otra_com", "cardiovascular", 
                                       "epoc", "renal_cronica", "tabaquismo", 
                                       "obesidad", "diabetes", "hipertension"), 
                            labels = c("embarazo", "asma", "habla lengua indigena", 
                                       "inmusupresión", "otra complicación", "cardiovascular", 
                                       "epoc", "renal cronica", "tabaquismo", 
                                       "obesidad", "diabetes", "hipertension"))) %>%
  ungroup() %>% 
  unite("casos", intubado:condicion, sep = "-") %>% 
  print()

# Genero colores para el gráfico
colores <-  c("#8B1A1A", "#EEEE00", "#B0C4DE", "#9DA700", "#00BA38", "#00BFC4", 
              "#EEDD82", "#00A9FF", "#619CFF", "#9F8CFF", "#F564E3", "#FF6C91", 
              "#F8766D", "#EE2C2C", "#009ACD")

# genero el gráfico
defun_sunb <- 
  sund2b(defun, 
         valueField = "tot_casos",
         rootLabel = "Defunciones",
         width = 800, 
         height = 600, 
         colors = colores)

defun_sunb

