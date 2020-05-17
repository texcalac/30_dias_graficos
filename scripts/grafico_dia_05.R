# librerías a utilizar
library(tidyverse)
library(tidygraph)
library(ggraph)
library(jcolors)

# Juegos de México en fase de grupos desde 1930 a 2018
mex_fut <- 
  tibble(equipo = rep("México", 48), 
         rival = c("Argentina", "Chile", "Francia", "Brasil", "Yugoslavia", 
                   "Suiza", "Brasil", "Yugoslavia", "Francia", "Suecia", "Gales", 
                   "Hungría", "Brasil", "Checoslovaquia", "España", "Inglaterra", 
                   "Uruguay", "Francia", "Unión Soviética", "Bélgica", 
                   "El Salvador", "Polonia", "Alemania Fed.", "Túnez",
                   "Paraguay", "Bélgica", "Irak", "Irlanda", "Italia", "Holanda", 
                   "Bélgica", "Corea del Sur", "Noruega", "Italia", "Croacia", 
                   "Ecuador", "Portugal", "Angola", "Irán", "Sudáfrica", 
                   "Uruguay", "Francia", "Brasil", "Croacia", "Camerún", "Suecia", 
                   "Corea del Sur", "Alemania")) %>% 
  print()

# agrego el total de encuentros por selección
mex_fut <- 
  mex_fut %>% 
  count(rival, name = "tot_juegos") %>% 
  mutate(equipo = "Mexico") %>% 
  arrange(tot_juegos) %>% 
  select(equipo, rival, tot_juegos) %>% 
  print()

# doy formato a la malla
mex_edge <- 
  tbl_graph(edges = mex_fut, directed = TRUE) %>% 
  print()

# genero vector de etiquetas para el eje x
etiqueta <- c("México", mex_fut$rival)

# genero gráfica
ggraph(mex_fut, layout = 'linear') + 
  geom_edge_arc(aes(width = tot_juegos), color = "dodgerblue4", alpha = 0.5) +
  scale_x_continuous(breaks = seq(1, 35, by = 1), labels = etiqueta) + 
  labs(title = "Rivales de México en fase de grupos desde 1930 a 2018", 
       subtitle = "(Número de juegos contra cada equipo)") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5), 
        panel.grid = element_blank(), 
        axis.title = element_blank(), 
        axis.text.x = element_text(angle = 90, 
                                   hjust = 1, 
                                   vjust = 0.5, 
                                   size = 10), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.key.width = unit(2.5, "cm"))

