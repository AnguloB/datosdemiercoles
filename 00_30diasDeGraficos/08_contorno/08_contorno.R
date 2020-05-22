library(tidyverse)

# dia 8 contornos
backcolor<-"white"
colortext<-"black"
  
#Defino paleta de colores
palette30<-c("#FFEC33",  "#FF8A33", "#EC4176", "#33DDFF", "#A13770", "#FD7FC4", "#5564eb", "#4c2882", "#094293")
#crear una paleta con N valores del color al color que le asigno
#palette <- colorRampPalette(c("#9B77CF" ,"#262253"))

#Eligo la fuente que quiero utilizar
library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
font<- "Trebuchet MS" #Fuente que voy a utlizar

#https://github.com/cienciadedatos/datos-de-miercoles/tree/master/datos/2019/2019-06-12
vinos <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-12/vinos.csv")

vinos%>%
  filter(pais=="España")%>%
  mutate(precio_e= precio*0.92)%>%

  ggplot(aes(x= puntos, y = precio_e))+ 
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha=.5)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient(low= palette30[1], high=palette30[3])+
  theme_bw()+
  theme(text=element_text(family = font),
       plot.background = element_rect(fill = "white", color=NA), 
        strip.background =element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
        legend.background = element_rect(fill=backcolor,
                                         size=0.5, linetype="solid"),
        plot.title = element_text(size=20, hjust=0,face="bold", color="#9B77CF"), 
        plot.subtitle = element_text(face="italic", size=12, hjust=0), 
        plot.caption = element_text( face="italic", size=10, hjust = 1), 
        axis.text.x = element_text(angle = 90, hjust = 1, color=colortext),
        axis.text.y = element_text(color=colortext), 
        legend.position = "top")+
  scale_color_manual(name="Zona",values = palette30) +
labs(title= "Día 8: contorno", 
     subtitle = "Vinos españoles: puntuación y precio de una botella", 
     y = "€", 
     x = "Puntuación 0-100", 
     caption = "Hecho por @AnguloBrunet \n #30díasdegráficos \n Fuente #datosdemiércoles https://www.kaggle.com/zynicide/wine-reviews/")
     
ggsave("08_contorno.png", height = 5.89, width=8.58)

