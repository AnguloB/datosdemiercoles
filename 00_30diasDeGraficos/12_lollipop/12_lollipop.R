library(tidyverse)

# dia 12 lollipop
#mismos datos que dia 11

#Defino paleta de colores
palette30<-c("#FFA45E",  "#262253", "#EC4176", "#543884", "#A13770", "9B77CF")
backcolor<-"white"
colortext<-"black"
#Eligo la fuente que quiero utilizar
library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
font<- "Trebuchet MS" #Fuente que voy a utlizar

# install_packages("readr")
felicidad <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-07/felicidad.csv")

#paises lengua oficial castellano
paises<-c("Argentina", "Bolivia", "Chile", "Colombia", "Costa Rica", "Cuba", "República Dominicana",
           "Ecuador", "El Salvador", "Guinea", "Guatemala", "Honduras", "México", "Nicaragua", 
           "Panamá", "Paraguay", "Perú", "España", "Uruguay",  "Venezuela") 

#paises[!paises %in% unique(felicidad$pais)]
felicidadESP<-felicidad %>%
  filter(pais %in% paises)

orden<-felicidadESP%>%
  group_by(pais)%>%
  summarise(media = mean(escalera_vida, na.rm=TRUE))%>%
  arrange(desc(media))

orden$pais<- factor(orden$pais, levels=orden$pais)

orden%>%
  ggplot(aes(x= media, y = reorder(pais, -media), label=round(media,1), color=media))+
  geom_segment(aes(x = 0, y = pais,  xend = media, yend = pais), color = "grey50") +
  geom_text(aes(x= media+0.3, y = reorder(pais, -media), label=round(media,1), color=media))+
  geom_point(size=6)+
  scale_color_gradient(low=palette30[2], high= palette30[3])+ # 2 3
  
theme_bw()+
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
  labs(title= "Día 12: lollipop", 
       subtitle = "Puntución media de felicidad en países con lengua oficial castellano",  x="", 
       y = "", fill="Puntuación\nde felicidad",
       caption = "Hecho por @AnguloBrunet \n #30díasdegráficos \n Fuente :#datosdemiercoles")
  
  
ggsave("12_lollipop.png", height = 5.89, width=8.58)

