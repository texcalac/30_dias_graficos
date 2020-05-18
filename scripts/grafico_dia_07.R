# Librerías a utilizar
# devtools::install_github('diegovalle/aire.zmvm')
library(aire.zmvm)
library(tidyverse)
library(ggridges)

ozono <- 
  get_station_data(criterion = "MAXIMOS", pollutant = "O3", year = 2020) %>% 
  tibble() %>% 
  print()

ozono <- 
  ozono %>% 
  group_by(date) %>% 
  summarize(o3_max = max(value, na.rm = T)) %>% 
  mutate(mes = format(date, "%B"), 
         mes_num = as.integer(format(date, "%m"))) %>% 
  print()

ggplot(ozono, aes(x = o3_max, 
                y = fct_reorder(mes, desc(mes_num)), 
                fill = mes, 
                color = mes)) + 
  geom_density_ridges(alpha = 0.5) + 
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.25))) + 
  labs(title = 'Concentraciones máximas diarias de ozono en la Ciudad de México', 
       subtitle = 'Año 2020, concentración en partes por billón (ppb)') + 
  theme_minimal(base_family = "Avenir") + 
  theme(axis.title = element_blank(), 
        axis.text = element_text(size = 10), 
        panel.grid.minor = element_blank(), 
        legend.position = "none")

