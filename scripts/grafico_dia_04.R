# Librería a utilizar
library(tidyverse)

rm(list = ls())
dev.off()
cat("\014")

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
  ungroup() %>% 
  print()


# gráfico
ggplot() + 
  geom_line(data = zm_covid,
            aes(x = fecha, y = TASA100, group = NOM_SUN), colour = "#3D8DC3", size = 1) + 
  scale_y_continuous(breaks = seq(15, 90, 15)) + 
  facet_wrap(vars(fct_reorder(NOM_SUN, TASA100, .desc = T))) + 
  labs(title = "COVID-19 - Zonas Metropolitanas de México de más de 2 millones de hab.",
       x = "",
       y = "Tasa por 100 mil habitantes") + 
  theme_bw(base_family = "Avenir") + 
  theme(legend.position = "none", 
        axis.text = element_text(size = 7), 
        strip.background = element_rect(fill = 'dodgerblue4'), 
        strip.text = element_text(size = 10, colour = "white", face = "bold"))

