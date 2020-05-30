library(tidyverse)

# dia 19 stream
#Datos "https://www.meteo.cat/
backcolor<-"white"
colortext<-"black"
#Defino paleta de colores
palette30<- c("#FD7FC4",  "#FF8A33", "#EC4176", "#A13770" , "#33DDFF", "#FFEC33", "#5564eb", "#4c2882")
#Eligo la fuente que quiero utilizar
library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
font<- "Trebuchet MS" #Fuente que voy a utlizar

#He bajado maximo, minimo y precipitacion
#Finalmente solo uso precipitacion
datosmax <- read_table2("https://static-m.meteo.cat/content/climatologia/series-climatiques/hotxm00000080d.txt", col_names = FALSE, skip = 8)
datosmax <-data.frame("Temp. máxima", datosmax)
datosmin <- read_table2("https://static-m.meteo.cat/content/climatologia/series-climatiques/hotnm00000080d.txt", col_names = FALSE, skip = 8)
datosmin <-data.frame("Temp. minima", datosmin)

datosprec <- read_table2("https://static-m.meteo.cat/content/climatologia/series-climatiques/horrm00000080d.txt", col_names = FALSE, skip = 8)
datosprec <-data.frame("Precipitación", datosprec)



nombres<-c("grupo","year", "Enero", "Febrero", "Marzo", "Abril", "Mayo","Junio", 
                "Julio","Agosto", "Septiembre", "Octubre","Noviembre","Diciembre")

names(datosmax)<-nombres
names(datosmin)<-nombres
names(datosprec)<-nombres

datos<-rbind(datosmax, datosmin, datosprec)

paleta<-c("#577590","#66829a","#4d908e","#43aa8b","#6ab47c","#90be6d","#f9c74f",
"#f8961e","#f3722c","#f94144", "#3B4F62", "#293643") 


datos$year<-factor(datos$year)
  
datos<-reshape2::melt(datos)
library(ggTimeSeries)

# base plot
datos%>%
  #filter(grupo=="Temp. máxima")%>%
  #filter(grupo=="Temp. minima")%>%
 filter(grupo=="Precipitación")%>%
ggplot(aes(x = year,
           y = value,
           group =variable,
           fill = variable)) +
  stat_steamgraph() +
  theme_bw()+
  theme(text=element_text(family = font),
        plot.background = element_rect(fill = "white", color=NA), 
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle=90),
        axis.ticks = element_blank(),
        strip.background =element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.background = element_rect(size=0.5, linetype="solid"),
        plot.title = element_text(size=20, hjust=0,face="bold", color="#9B77CF"), 
        plot.subtitle = element_text(face="italic", size=12, color="black"), 
        plot.caption = element_text( face="italic", size=10, hjust = 1, color="black"))+
  scale_fill_manual(values=paleta)+
  labs(fill="", y="", x="",
       title ="Día 19: stream graph",
       subtitle="Precipitaciones en Barcelona (1950-2018)",
       caption = "Hecho por @AnguloBrunet \n #30díasdegráficos \n Fuente: https://www.meteo.cat" )





ggsave("19_stream.png", height = 5.89, width=8.58)

