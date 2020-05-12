# Librería a utilizar
library(tidyverse)

### Datos diarios - Mayo 11
m11 <- tempfile()
# descargar información
download.file("http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip", m11)
# descomprimir archivos
unzip(m11)
# lectura de datos
may11 <- read_csv(unzip(m11, list = T)$Name, col_types = NULL) %>% print()


### Datos diarios - Mayo 10
m10 <- tempfile()
# descargar información
download.file("http://187.191.75.115/gobmx/salud/datos_abiertos/historicos/datos_abiertos_covid19_10.05.2020.zip", m10)
# descomprimir archivos
unzip(m10)
# lectura de datos
may10 <- read_csv(unzip(m10, list = T)$Name, col_types = NULL) %>% print()


### Datos diarios - Mayo 09
m09 <- tempfile()
# descargar información
download.file("http://187.191.75.115/gobmx/salud/datos_abiertos/historicos/datos_abiertos_covid19_09.05.2020.zip", m09)
# descomprimir archivos
unzip(m09)
# lectura de datos
may09 <- read_csv(unzip(m09, list = T)$Name, col_types = NULL) %>% print()

# Datos diarios - Mayo 08
m08 <- tempfile()
# descargar información
download.file("http://187.191.75.115/gobmx/salud/datos_abiertos/historicos/datos_abiertos_covid19_08.05.2020.zip", m08)
# descomprimir archivos
unzip(m08)
# lectura de datos
may08 <- read_csv(unzip(m08, list = T)$Name, col_types = NULL) %>% print()


# Datos diarios - Mayo 07
m07 <- tempfile()
# descargar información
download.file("http://187.191.75.115/gobmx/salud/datos_abiertos/historicos/datos_abiertos_covid19_07.05.2020.zip", m07)
# descomprimir archivos
unzip(m07)
# lectura de datos
may07 <- read_csv(unzip(m07, list = T)$Name, col_types = NULL) %>% print()

casos_covid <- 
  bind_rows(select(may07, FECHA_ACTUALIZACION, ENTIDAD_RES, MUNICIPIO_RES, RESULTADO), 
            select(may08, FECHA_ACTUALIZACION, ENTIDAD_RES, MUNICIPIO_RES, RESULTADO), 
            select(may09, FECHA_ACTUALIZACION, ENTIDAD_RES, MUNICIPIO_RES, RESULTADO), 
            select(may10, FECHA_ACTUALIZACION, ENTIDAD_RES, MUNICIPIO_RES, RESULTADO), 
            select(may11, FECHA_ACTUALIZACION, ENTIDAD_RES, MUNICIPIO_RES, RESULTADO)) %>% 
  filter(RESULTADO == 1) %>% 
  mutate(EDO_MUN = str_c(ENTIDAD_RES, MUNICIPIO_RES)) %>% 
  count(EDO_MUN, RESULTADO, name = "casos") %>% 
  arrange(desc(casos)) %>% 
  top_n(n = 10) %>% 
  print()

rm(list = setdiff(ls(), "casos_covid"))

# gráfico
ggplot(casos_covid, aes(x = fct_reorder(EDO_MUN, desc(casos)), y = casos)) + 
  geom_bar(stat = "identity", fill= "dodgerblue4", alpha = .6, width = .8) + 
  labs(title = "México - Municipios con mayor número de casos de COVID-19", 
       subtitle = "datos de los últimos 5 días (07 a 11 de mayo 2020)", 
       x = "Municipios de México",
       y = "Casos confirmados en los últimos 5 días") + 
  scale_x_discrete(breaks = c("09007","02004","09005", "27004", "02002", 
                              "25006", "23005", "15058", "09012", "15033"), 
                   labels = c("Iztapalapa \n CDMX", "Tijuana \n BC", 
                              "Gustavo A. Madero \n CDMX", "Centro \n Tab", 
                              "Mexicali \n BC", "Culiacán \n Sin", 
                              "Benito Juárez \n Q. Roo", "Nezahualcoyotl \n Edo. Méx.", 
                              "Tlalpan \n CDMX", "Ecatepec \n Edo. Méx.")) + 
  theme_bw(base_family = "Avenir") + 
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(size = 8))


