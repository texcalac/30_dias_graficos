# librerías a utilizar
library(igraph)
library(tidyverse)

# leemos los datos
defunciones <- read_csv("./datos/200527COVID19MEXICO.csv") %>% print()

# Proceso los datos para el gráfico
defun <- 
  defunciones %>% 
  filter(!is.na(FECHA_DEF), 
         RESULTADO == 1, 
         INTUBADO %in% c(1,2)) %>% 
  select(INTUBADO, SEXO, EMBARAZO:TABAQUISMO) %>% 
  rename_all(tolower) %>% 
  mutate(intubado = factor(intubado, 
                           levels = c(1, 2), 
                           labels = c("Intubado", "No intubado")), 
         sexo = factor(sexo, 
                       levels = c(1, 2), 
                       labels = c("Mujer", "Hombre"))) %>% 
  mutate_at(vars(embarazo:tabaquismo), function(x) case_when(x == 1 ~ 1, 
                                                             x == 2 ~ 0, 
                                                             TRUE ~ NA_real_)) %>% 
  pivot_longer(-c(intubado, sexo), 
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
  rename(source = intubado, target = condicion, weight = tot_casos) %>% 
  graph_from_data_frame() %>% 
  print()

# genero el gráfico
plot(defun, layout = layout.circle(defun))


