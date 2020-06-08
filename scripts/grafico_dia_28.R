# Librerías a utlizar
library(tidyverse)
library(circlize)
library(RColorBrewer)


# Descargo los datos de aquí
# http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip


# Cargo los datos a trabajar
defunciones <- read_csv(".datos/200607COVID19MEXICO.csv") %>% print()


# Proceso los datos a utilizar
defun <- 
  defunciones %>% 
  filter(!is.na(FECHA_DEF), 
         RESULTADO == 1, 
         INTUBADO == 1) %>% 
  select(INTUBADO, HIPERTENSION, DIABETES, OBESIDAD, TABAQUISMO, EPOC, ASMA, RENAL_CRONICA) %>% 
  mutate(HIPERTENSION = case_when(HIPERTENSION != 1 ~ NA_real_, TRUE ~ 1), 
         DIABETES = case_when(DIABETES != 1 ~ NA_real_, TRUE ~ 1), 
         OBESIDAD = case_when(OBESIDAD != 1 ~ NA_real_, TRUE ~ 1), 
         TABAQUISMO = case_when(TABAQUISMO != 1 ~ NA_real_, TRUE ~ 1), 
         EPOC = case_when(EPOC != 1 ~ NA_real_, TRUE ~ 1), 
         ASMA = case_when(ASMA != 1 ~ NA_real_, TRUE ~ 1), 
         RENAL_CRONICA = case_when(RENAL_CRONICA != 1 ~ NA_real_, TRUE ~ 1), 
         INTUBADO = recode(INTUBADO, "1" = "Intubado")) %>% 
  filter_at(vars(HIPERTENSION, DIABETES, OBESIDAD, TABAQUISMO), 
            any_vars(!is.na(.))) %>% 
  pivot_longer(-INTUBADO, 
               names_to = "COMORB", 
               values_to = "VALOR") %>% 
  group_by(INTUBADO, COMORB) %>% 
  summarise(TOT_CASOS = sum(VALOR, na.rm = T)) %>% 
  mutate(COMORB = str_to_title(COMORB)) %>% 
  mutate(COMORB = recode(COMORB, Renal_cronica = "Renal crónica")) %>% 
  print()


# genero paleta de colores
dark2_pal <- brewer.pal(n = 7, name = 'Dark2')
names(dark2_pal) <- c("Asma", "Diabetes", "EPOC", "Hipertensión", "Obesidad", 
                      "Renal Crónica", "Tabaquismo")


# Genero el gráfico
png("~/Desktop/plot1.png", width = 8, height = 8, units = "in", res = 300)
chordDiagram(defun, grid.col = dark2_pal)
title("COVID-19: Pacientes intubados y comorbilidades", cex = 0.8)
dev.off()

