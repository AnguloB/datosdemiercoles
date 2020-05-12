library(tidyverse)

# dia 1: barras
# https://github.com/cienciadedatos/datos-de-miercoles
# De 2	2019-04-17	Juego de Tronos,	varias	Tiempo en pantalla personajes

backcolor<-"white"
colortext<-"black"
  
tiempo_pantalla <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/tiempo_pantalla.csv")

cambio_lealtades <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/cambio_lealtades.csv")

datos<- left_join(tiempo_pantalla, cambio_lealtades, by="nombre")
#Defino paleta de colores
palette30<-c("#FFA45E",  "#262253", "#EC4176", "#543884", "#A13770", "9B77CF")

#Eligo la fuente que quiero utilizar
library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
font<- "Trebuchet MS" #Fuente que voy a utlizar

#Creo un conjunto de datos con la misma longitud que "Tiempo pantalla" para saber el origen

imagenes <-c("imagenes/01.png", "imagenes/02.png", "imagenes/03.png", NA,  "imagenes/05.png",NA,
             "imagenes/07.png" ,NA, "imagenes/09.png", "imagenes/10.png",
             "imagenes/11.png", "imagenes/12.png", NA)
#crear una paleta con N valores del color al color que le asigno
palette <- colorRampPalette(c("#9B77CF" ,"#262253"))


library(ggimage)
library(magick)
datos%>%
  group_by(origen)%>%
  summarise(n=n(),
            minutos= mean(minutos_pantalla), #media de los personajes
            suma = sum(minutos_pantalla))%>%#tiempo total en pantalla
  arrange(desc(suma))%>% # lo ordeno de mayor a menor
  mutate(path= imagenes)%>% #ańado enlace a las imagenes previamente definidas
  mutate(ximag= 1:13)%>%  #creo el lugar de la x donde pondre la imagen
  mutate(yimag= suma+5)%>% # para posicionar imagen: añado cinco puntos por encima del max
  drop_na(origen)%>%
  ggplot()+
  geom_col(aes(x=reorder(origen, -suma), y=suma, fill=reorder(origen, suma)))+
  geom_image(aes(x=ximag, y = yimag, image=path), size =.07)+
  theme_bw()+
  scale_fill_manual(values = palette(13))+
  theme(text=element_text(family = font),
        plot.background = element_rect(fill = backcolor, color=NA), 
        strip.background =element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.background = element_rect(fill=backcolor,
                                         size=0.5, linetype="solid"),
        plot.title = element_text(size=20, hjust=0,face="bold", color="#9B77CF"), 
        plot.subtitle = element_text(face="italic", size=15, hjust=0), 
        plot.caption = element_text( face="italic", size=12, hjust = 1), 
        axis.text.x = element_text(angle = 90, hjust = 1, color=colortext),
        axis.text.y = element_text(color=colortext), 
        legend.position = "none")+ 
  labs(title= "Día 1: barras/columnas", 
       subtitle = "Juego de tronos",  x="", 
       y = "Minutos en pantalla de los personajes", 
       caption = "Hecho por @AnguloBrunet \n #30díasdegráficos")


ggsave("01_barras.png", height = 5.89, width=8.58)
  
