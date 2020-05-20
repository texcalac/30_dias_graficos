# Librería a utilizar
library(tidyverse)
library(viridis)

### Datos de casos confirmados diarios por municipio de residencia
casos <- read_csv("https://coronavirus.gob.mx/datos/Downloads/Files/Casos_Diarios_Municipio_Confirmados_20200518.csv")

# Agrego los casos diarios por municipio
casos <- 
  casos %>% 
  pivot_longer(-c(cve_ent, poblacion, nombre), 
               names_to = "fecha", 
               values_to = "casos") %>% 
  mutate(fecha = as.Date(fecha, "%d-%m-%Y")) %>% 
  filter(fecha >= "2020-04-01" & fecha <= "2020-05-17") %>%
  print()

# Cargo la malla del Sistema Urbano Nacional y filtro zonas metropolitanas
sun <- 
  read_csv("/Users/tex/OneDrive/GitHub/30_dias_graficos/datos/Base_SUN_2018.csv", 
           locale = readr::locale(encoding = "ISO-8859-1")) %>% 
  filter(str_detect(CVE_SUN, "^M")) %>% 
  select(CVE_MUN, NOM_SUN) %>% 
  print()

# Añado la Zona Metropolitana a tabla de casos y agrego casos por ZM
casos_sun <- 
  inner_join(casos, sun, by = c("cve_ent" = "CVE_MUN")) %>% 
  group_by(fecha, NOM_SUN) %>% 
  summarise(casos_zm = sum(casos, na.rm = T)) %>% 
  print()

# Genero tabla de población total por Zona Metropolitana
pob_sun <- 
  inner_join(casos, sun, by = c("cve_ent" = "CVE_MUN")) %>% 
  distinct(NOM_SUN, cve_ent, poblacion) %>% 
  group_by(NOM_SUN) %>% 
  summarise(pob_zm = sum(poblacion, na.rm = T)) %>% 
  print()

# Genero vector de las 5 Zonas Metropolitanas con mayor número de casos
top5 <- 
  casos_sun %>% 
  group_by(NOM_SUN) %>% 
  summarise(casos_zm = sum(casos_zm, na.rm = T)) %>% 
  arrange(desc(casos_zm)) %>% 
  top_n(5) %>% 
  select(NOM_SUN) %>% 
  pull()

# Genero tabla con casos, población y tasa por 100 mil en las 5 ZM con más 
# casos acumulados
tasa_casos <- 
  inner_join(casos_sun, pob_sun, by = "NOM_SUN") %>% 
  filter(NOM_SUN %in% top5) %>% 
  arrange(fecha, NOM_SUN) %>% 
  mutate(tasa100 = round((casos_zm/pob_zm)* 100000, 1)) %>% 
  print()

# Descargo el tema para el gráfico
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# Finalmente genero el gráfico
ggplot(data = tasa_casos, aes(x = fecha, y = tasa100, fill = NOM_SUN)) + 
  geom_area(color = "white", alpha = 0.4) + 
  scale_fill_viridis(discrete = T, option = "plasma") + 
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d", expand = c(0, 0)) + 
  scale_y_continuous(breaks = seq(1, 20, 1), expand = expansion(mult = c(0.01, 0.02))) + 
  labs(title = "Variación diaria de la tasa de casos de COVID-19 por Zona Metropolitana", 
       subtitle = "México: abril-mayo 2020", 
       caption = "Fuente: https://coronavirus.gob.mx/datos/#DownZCSV", 
       x = NULL, 
       y = "Tasa por 100 mil hab.", 
       fill = NULL) + 
  theme_lab() + 
  theme(panel.grid.major.x = element_blank(),
        axis.title = element_text(hjust = 0.5, size = 12), 
        axis.text = element_text(size = 9),
        plot.title = element_text(size = 16, hjust = 0.5, face = "plain"), 
        plot.subtitle = element_text(size = 12, hjust = 0.5), 
        plot.caption = element_text(face = "italic", size = 9), 
        legend.position = "bottom", 
        legend.key = element_rect(color = "grey70"), 
        legend.text = element_text(size = 11))

