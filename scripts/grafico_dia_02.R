# Librería a utilizar
library(tidyverse)
library(viridis)
library(gganimate)

rm(list = ls())

### Datos de casos confirmados diarios por municipio de residencia
covid <- read_csv("./datos/serie_casos.csv") %>% print()

# malla del Sistema Urbano Nacional
sun <- 
  read_csv("./datos/Base_SUN_2018.csv", 
           locale = readr::locale(encoding = "ISO-8859-1")) %>% 
  print()

# filtro zonas metropolitanas y selecciono columnas
sun <- 
  sun %>% 
  filter(str_detect(CVE_SUN, "^M")) %>% 
  select(CVE_MUN, CVE_SUN, NOM_SUN, POB_2018) %>% 
  print()

# calculo población por zona metropolitana
# selecciono población mayor a 1 millón de habitantes
pob_zm <- 
  sun %>% 
  group_by(CVE_SUN) %>% 
  summarise(POB_ZM = sum(POB_2018, na.rm = T)) %>% 
  filter(POB_ZM >= 1000000) %>% 
  print()

# uno mallas de datos
zm_covid <- inner_join(sun, covid, by = c("CVE_MUN" = "edo_mun")) %>% print()

# calculo casos por zona matropolitana
zm_covid <- 
  zm_covid %>% 
  group_by(fecha, CVE_SUN, NOM_SUN) %>% 
  summarise(ZM_CASOS = sum(casos, na.rm = T)) %>% 
  print()

# Pego la población total por zona metropolitana y caculo la tasa
zm_covid <- 
  inner_join(zm_covid, pob_zm, by = "CVE_SUN") %>% 
  mutate(TASA100 = round((ZM_CASOS/POB_ZM)* 100000, 1)) %>% 
  print()

# gráfico
# ggplot() + 
#   geom_line(data = zm_covid, 
#             aes(x = fecha, y = TASA100, group = NOM_SUN, colour = NOM_SUN)) + 
#   scale_y_continuous(breaks = seq(10, 90, 10)) + 
#   scale_colour_viridis_d(option = "plasma", name = "Zona Metropolitana") + 
#   labs(title = "COVID-19 - Zonas Metropolitanas de México de más de 2 millones de hab.", 
#        x = "",
#        y = "Tasa por 100 mil habitantes") + 
#   theme_bw(base_family = "Avenir")
# 
# ggsave("./datos/plot_dia_02.png", width = 8, height = 6.2, units = 'in', dpi = 128)

plot_covid <-
  ggplot() +
  geom_line(data = zm_covid,
            aes(x = fecha, y = TASA100, group = NOM_SUN, colour = NOM_SUN)) +
  scale_y_continuous(breaks = seq(10, 90, 10)) +
  scale_colour_viridis_d(option = "plasma", name = "Zona Metropolitana") +
  labs(title = "COVID-19 - Zonas Metropolitanas de México de más de 2 millones de hab.",
       x = "",
       y = "Tasa por 100 mil habitantes") +
  transition_reveal(fecha) +
  theme_bw(base_family = "Avenir")

plot_anim <- animate(plot_covid, nframes = 50, fps = 5)

# Guardo el gif
anim_save("./salidas/plot_dia_02.gif")




