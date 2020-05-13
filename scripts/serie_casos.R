# Librería a utilizar
library(tidyverse)

# genero malla con serie de casos COVID-19 del 19 de abril al 12 de mayo
serie_casos <-
  list.files(path = "./datos",
             pattern = c("^20", "*.csv"), 
             full.names = T) %>% 
  map(~ read_csv(., col_types = cols(.default = "c")) %>% 
        filter(RESULTADO == 1) %>% 
        rename(fecha = FECHA_ACTUALIZACION, casos = RESULTADO) %>% 
        mutate(edo_mun = str_c(ENTIDAD_RES, MUNICIPIO_RES)) %>% 
        select(., fecha, edo_mun, casos)) %>% 
  map_df(., bind_rows)

serie_casos

# write_excel_csv(serie_casos, "./datos/serie_casos.csv")
