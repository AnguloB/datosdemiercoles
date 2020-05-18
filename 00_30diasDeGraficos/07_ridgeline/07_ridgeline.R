library(tidyverse)

# dia 6 donut
#Datos "https://www.meteo.cat/
backcolor<-"white"
colortext<-"black"
#Defino paleta de colores
palette30<- c("#FD7FC4",  "#FF8A33", "#EC4176", "#A13770" , "#33DDFF", "#FFEC33", "#5564eb", "#4c2882")
#Eligo la fuente que quiero utilizar
library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
font<- "Trebuchet MS" #Fuente que voy a utlizar
datos <- read_table2("https://static-m.meteo.cat/content/climatologia/series-climatiques/hotxm00000080d.txt", col_names = FALSE, skip = 8)

names(datos)<-c("year", "Enero", "Febrero", "Marzo", "Abril", "Mayo","Junio", 
                "Julio","Agosto", "Septiembre", "Octubre","Noviembre","Diciembre")

datos$year<-factor(datos$year)
  
library(ggridges)
datos<-reshape2::melt(datos)

ggplot(datos, aes(x = value, y =fct_rev(as_factor(variable)) ,fill = factor(stat(quantile)))) +
stat_density_ridges(
  geom = "density_ridges_gradient", calc_ecdf = TRUE,
  quantiles = 4, quantile_lines = TRUE)+
  scale_fill_manual(values=palette30[c( 7, 4, 3, 1 )])+
  theme_bw()+
  theme(text=element_text(family = font, color="#9B77CF"),
        plot.background = element_rect(fill = "white", color=NA), 
        strip.background =element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(), 
        #axis.text.x = element_blank(), 
        legend.position = "none", 
        plot.title = element_text(size=20, hjust=0,face="bold", color="#9B77CF"), 
        plot.subtitle = element_text(face="italic", size=12, color="black"), 
        plot.caption = element_text( face="italic", size=10, hjust = 1, color="black"))+
  labs(title ="Día 7: ridgeline",
       subtitle="Temperatura máxima en Barcelona (1950-2018)",
       caption = "Hecho por @AnguloBrunet \n #30díasdegráficos \n Fuente: https://www.meteo.cat", 
       y="", 
       x="Temperatura máxima (°C)")

ggsave("07_ridgeline.png", height = 5.89, width=8.58)

