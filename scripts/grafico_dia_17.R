# Librerías a utilizar
library(tidyverse)
library(ggalluvial)
library(RColorBrewer)

# Descargar datos de aquí, la base varía de acuerdo a la fecha de descarga
# http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip

# Cargo los datos a trabajar
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
  group_by(intubado, sexo, condicion) %>% 
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
  print()

# genero el gráfico
ggplot(data = defun, 
       aes(axis1 = intubado, axis2 = sexo, axis3 = condicion, y = tot_casos)) + 
  scale_x_discrete(limits = c("intubado", "sexo", "condicion"), expand = c(0.1, 0.2)) + 
  geom_alluvium(aes(fill = sexo), width = 0.7) + 
  geom_stratum(alpha = 0.3, width = 0.7, colour = "dodgerblue4") + 
  geom_text(stat = "stratum", infer.label = TRUE, color = "dodgerblue4") + 
  scale_fill_brewer(type = "qual", palette = "Set1") + 
  labs(title = "COVID-19: Defunciones en México según tratamiento, sexo y comorbilidades", 
       caption = "Fuente: https://datos.gob.mx/busca/dataset/informacion-referente-a-casos-covid-19-en-mexico") + 
  theme_void(base_family = "Avenir") + 
  theme(plot.title = element_text(size = 22, hjust = 0.5, colour = "maroon4", 
                                  margin = margin(10, 0, -10, 0)), 
        plot.caption = element_text(size = 10, face = "italic", colour = "gray30"), 
        legend.title = element_blank(), 
        legend.position = "bottom", 
        legend.key.size = unit(1, "cm"), 
        legend.text = element_text(size = 12, colour = "dodgerblue4", 
                                   margin = margin(r = 1, unit = "cm")), 
        legend.margin=margin(t = -0.7, unit='cm'))

