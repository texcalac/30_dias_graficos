# librerías a utilizar
library(tidyverse)
library(readxl)
library(ggrepel)
library(jcolors)

# descargar información: https://www.who.int/airpollution/data/en/
download.file("https://www.who.int/airpollution/data/pm25_modelled_exposure_bycountry_2016_v0.xlsx?ua=1", 
              destfile = './datos/pm25_oms.xlsx')
# lectura de datos
pm25 <- 
  read_xlsx("./datos/pm25_oms.xlsx", sheet = "data ", skip = 1) %>% 
  print()

pm25 <- 
  pm25 %>% 
  filter(area == "Urban") %>% 
  print()

ggplot(data = pm25, aes(iso3, median, color = wbinc16, label = whoname)) + 
  geom_point() + 
  geom_text_repel(data = subset(pm25, median > 50), aes(label = whoname), size = 3) + 
  geom_text_repel(data = subset(pm25, median > 30 & median <= 50), aes(label = whoname), size = 2.5) + 
  scale_color_jcolors(palette = "pal6", 
                      name = "Ingreso", 
                      labels = c("Bajo", 
                                 "Medio bajo", 
                                 "Medio alto", 
                                 "Alto")) + 
  geom_hline(yintercept = 10, color = "red") + 
  annotate(geom = "text", x = 32, y = 7, label = "Límite permisible - OMS", color = "gray30") + 
  labs(title = expression(paste("Promedio anual de ", PM[2.5], " ponderado por población, según nivel de ingreso de país")), 
       x = "países",
       y = expression(paste("microgramos por metro cúbico  (", mu, "g/", m^3, ")")), 
       caption = "Fuente: www.who.int/airpollution/ambient/AAP_exposure_Apr2018_final.pdf") + 
  scale_y_continuous(breaks = seq(10, 100, 10)) + 
  theme_bw(base_family = "Avenir") + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        panel.grid.major.x = element_blank(), 
        legend.position = "bottom", 
        legend.text = element_text(size = 11), 
        plot.caption = element_text(face = "italic"))

