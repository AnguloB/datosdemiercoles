library(tidyverse)

# dia 11 heatmap

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
felicidadESP$pais<-factor(felicidadESP$pais, levels= orden$pais)

felicidadESP%>%
  ggplot(aes(x= pais, y = factor(anio), fill=escalera_vida))+
  geom_tile()+
  theme_bw()+
  scale_fill_gradient(low=palette30[2], high= palette30[3])+ # 2 3
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
        legend.position = "bottom")+ 
  labs(title= "Día 11: heatmap", 
       subtitle = "Puntución de felicidad en países con lengua oficial castellano",  x="", 
       y = "", fill="Puntuación\nde felicidad",
       caption = "Hecho por @AnguloBrunet \n #30díasdegráficos \n Fuente :#datosdemiercoles")
ggsave("11_heatmap.png", height = 5.89, width=8.58)

