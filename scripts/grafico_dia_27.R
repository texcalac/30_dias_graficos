# Librerías a utilizar
library(tidyverse)
library(gganimate)


# Cargo los datos a trabajar
defunciones <- read_csv("./datos/200602COVID19MEXICO.csv") %>% print()

# Identifico los municipios con mayor número de casos
top_defun <- 
  defunciones %>% 
  filter(!is.na(FECHA_DEF), 
         RESULTADO == 1) %>% 
  count(ENTIDAD_RES, MUNICIPIO_RES, name = "tot_casos") %>% 
  top_n(8) %>% 
  mutate(ENT_MUN = str_c(ENTIDAD_RES, MUNICIPIO_RES)) %>% 
  pull(ENT_MUN)

# proceso los datos a utlizar
defun <- 
  defunciones %>% 
  filter(!is.na(FECHA_DEF), 
         RESULTADO == 1) %>% 
  mutate(ENT_MUN = str_c(ENTIDAD_RES, MUNICIPIO_RES)) %>% 
  complete(ENT_MUN, FECHA_DEF) %>% 
  filter(ENT_MUN %in% top_defun) %>% 
  group_by(ENT_MUN, FECHA_DEF) %>% 
  summarise(TOT_CASOS = sum(RESULTADO, na.rm = T)) %>% 
  mutate(SUMA_ACUM = cumsum(TOT_CASOS), 
         MUN = case_when(ENT_MUN == "02002" ~ "Mexicali, B.C.", 
                         ENT_MUN == "02004" ~ "Tijuana, B.C.", 
                         ENT_MUN == "08037" ~ "Juárez, Chih.", 
                         ENT_MUN == "09005" ~ "G.A. Madero, CDMX", 
                         ENT_MUN == "09007" ~ "Iztapalapa, CDMX", 
                         ENT_MUN == "23005" ~ "Benito Juárez, Q.Roo", 
                         ENT_MUN == "25006" ~ "Culiacán, Sin", 
                         ENT_MUN == "27004" ~ "Centro, Tab.")) %>% 
  ungroup() %>% 
  arrange(FECHA_DEF, ENT_MUN) %>% 
  print()

# genero el gráfico estático
plot_defun <- 
  ggplot(defun, 
         aes(x = SUMA_ACUM, 
             y = ENT_MUN, 
             colour = as.factor(ENT_MUN), 
             fill = as.factor(ENT_MUN))) + 
  geom_bar(stat = "identity") + 
  scale_x_continuous(breaks = seq(50, 550, 50), expand = c(0,0)) + 
  theme_bw(base_family = "Avenir") + 
  theme(axis.title = element_blank(), 
        axis.ticks.y = element_blank(), 
        legend.position = "none", 
        panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank())

plot_defun

# Animación del gráfico
plot_defun + 
  transition_states(FECHA_DEF, 
                    transition_length = 2, 
                    state_length = 0)

