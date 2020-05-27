# Librerías a utilizar
library(tidyverse)
library(factoextra)

# Datos para la visualización
defunciones <- read_csv("https://coronavirus.gob.mx/datos/Downloads/Files/Casos_Diarios_Estado_Nacional_Defunciones_20200525.csv")

# Proceso los datos
d_tot <- 
  defunciones %>% 
  pivot_longer(-c(cve_ent, poblacion, nombre), 
               names_to = "fecha", 
               values_to = "casos") %>% 
  filter(nombre != "Nacional") %>% 
  group_by(nombre, poblacion) %>% 
  summarise(tot_def = sum(casos, na.rm = T)) %>% 
  mutate(nombre = str_to_title(nombre), 
         nombre = recode(nombre, 
                         "Distrito Federal" = "Ciudad de México", 
                         "Mexico" = "Estado de México"), 
         tasa100 = round((tot_def/poblacion)*100000, 1)) %>% 
  select(nombre, tasa100) %>% 
  column_to_rownames(var = "nombre") %>% 
  as.data.frame() %>% 
  print()

# Clúster de 4
d_hc <- hcut(d_tot, k = 4, stand = T)

# Genero los gráficos
fviz_dend(d_hc, 
          k = 4, 
          cex = 0.8, 
          horiz = TRUE, 
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
          color_labels_by_k = TRUE, 
          rect = T, 
          rect_border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
          rect_fill = T, 
          main = "COVID-19: Dendograma de la tasa por 100 mil habitantes en entidades de México", 
          xlab = "",
          ylab = "")

