# Librerías a utilizar
library(tidyverse)
library(viridis)


### Datos de casos confirmados diarios por municipio de residencia
casos <- read_csv("https://coronavirus.gob.mx/datos/Downloads/Files/Casos_Diarios_Municipio_Confirmados_20200516.csv")
casos

# Agrego los casos diarios por municipio
casos <- 
  casos %>% 
  pivot_longer(-c(cve_ent, poblacion, nombre), 
               names_to = "fecha", 
               values_to = "casos") %>% 
  group_by(cve_ent) %>% 
  summarise(tot_casos = sum(casos, na.rm = T)) %>% 
  print()

# malla del Sistema Urbano Nacional
sun <-
  read_csv("./datos/Base_SUN_2018.csv",
           locale = readr::locale(encoding = "ISO-8859-1")) %>%
  print()

# filtro zonas metropolitanas y selecciono columnas
sun <- 
  sun %>% 
  filter(str_detect(CVE_SUN, "^M")) %>% 
  select(CVE_MUN, NOM_SUN) %>% 
  print()

# uno mallas de datos
zm_covid <- 
  inner_join(sun, casos, by = c("CVE_MUN" = "cve_ent")) %>% 
  print()

# calculo casos por zona matropolitana
# selecciono las 10 zonas metropolitanas con más casos acumulados
zm_covid <- 
  zm_covid %>% 
  group_by(NOM_SUN) %>% 
  summarise(casos_tot = sum(tot_casos)) %>% 
  ungroup() %>% 
  arrange(desc(casos_tot)) %>% 
  top_n(n = 5) %>% 
  print()

# Casos totales de las 10 zonas metropolitanas
total <- sum(zm_covid$casos_tot)

zm_covid <- 
  zm_covid %>% 
  mutate(porcentaje = (casos_tot * 100)/total, 
         superior = cumsum(porcentaje), 
         minimo = lag(superior)) %>% 
  mutate(minimo = replace_na(minimo, 0), 
         ubicacion_etiq = (superior + minimo)/2, 
         etiqueta = str_c(NOM_SUN, "\n value: ", casos_tot)) %>% 
  print()

ggplot(zm_covid, aes(ymax = superior, ymin = minimo, xmax = 4, xmin = 3, fill = NOM_SUN)) + 
  geom_rect() + 
  geom_label(x = 3.5, aes(y = ubicacion_etiq, label = comma(casos_tot)), 
             size = 4.5, color = "white", fontface = "bold", show.legend = FALSE) + 
  scale_fill_viridis(discrete = T, direction = -1, name = "Zona Metropolitana") + 
  coord_polar(theta = "y") + 
  labs(title = "Zonas Metropolitanas de México con mayor número de casos \n acumulados de COVID-19", 
       caption = "https://coronavirus.gob.mx/datos/#DownZCSV") + 
  xlim(c(2, 4)) + 
  theme_void() + 
  theme(legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12), 
        legend.key.size = unit(1, "cm"), 
        plot.title = element_text(size = 16, colour = "gray30", hjust = 0.5), 
        plot.caption = element_text(face = "italic"))

