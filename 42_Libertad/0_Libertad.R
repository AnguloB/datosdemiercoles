# Índice de libertad de prensa
## https://tcdata360.worldbank.org/indicators/h3f86901f?country=BRA&indicator=32416&viz=line_chart&years=2001,2019
color1 <-"ivory1" #background color
color2<-"ivory1"  #color de fondo


library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
font<- "Trebuchet MS" #Fuente que voy a utlizar
library(tidyverse)
library(gganimate) # para gifs
library(ggimage) #anyadir imagenes
libertad_prensa <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2020/2020-03-25/libertad_prensa.csv")

# preparo los datos para el mapa 
library(maps)
mapp<-map_data("world") #Abro los datos para hacer el mundo 


iso <- maps::iso3166 # Busco codigos iso para poder usar con map_data

iso<-data.frame(iso$a3, iso$sovereignty) #selecciono solo las columnas que voy a usar
names(iso)<-c("codigo_iso", "region")  #pongo el mismo nombre que en libertad_prensa y maps


# Hay unos valores no coherentes para el ranking (Kuwait)
data<-libertad_prensa%>%
  filter(!anio == "2016")%>%
  left_join(iso) %>%           ## Junto los datos de iso con libertad de prensa y asigno nuevo nombre
  filter(!ranking >=200)

dat<-mapp%>%
  left_join(data) #junto datos del mapa con prensa

## Para anadir imagenes a los bot y los top

# https://www.flaticon.com/authors/swifticons
top<- "https://image.flaticon.com/icons/svg/213/213697.svg" #icono para top
bot<- "https://image.flaticon.com/icons/svg/213/213599.svg" # icono para bot


centres<-mapp %>%  #busco los centros de cada pais para poder añadir los iconos
  group_by(region)%>%
  summarize(longi = mean(range(long)), lati = mean(range(lat)))%>%
  left_join(iso) # añado la iso 

datTop<-data%>%  #creo datos para top
  group_by(anio)%>% #agrupo por año
  top_n(-1, wt=ranking) #busco los casos con mayor ranking

datTop<-data.frame(datTop, replace( 1:nrow(datTop) ,values=top))

names(datTop)[7]<- "images"

datBot<-data%>% #mismo para bot
  group_by(anio)%>%
  top_n(1, wt=ranking)

datBot<-data.frame(datBot, replace( 1:nrow(datBot) ,values=bot))
names(datBot)[7]<- "images"

datImages<-rbind(datTop, datBot) #junto los datos de top y bot

datImages<-datImages%>%
  left_join(centres)%>% # añado la latitud y longitud del centro
  select(region,anio, images, longi, lati)
names(datImages)<- c("pais1", "anio","images", "longi", "lati")

dataMap<-data%>%full_join(mapp)%>%
  select(-subregion)%>%
na.omit()


plot1<-ggplot()+
  geom_polygon(data=dataMap, aes(x = long, y = lat, group = group, fill=ranking))+
  geom_image(data=datImages, aes( x=longi, y =lati,image=images), size=0.05)+ #Añado imagen
  labs(title = "Evolución de la libertad de prensa",
       caption= "Hecho por @AnguloBrunet",fill= " Ranking \n Libertad de Prensa \n 1 = Top, 180 = Bot")+
  scale_fill_gradient2(low="#009E73", mid="#F0E442", high = "#D55E00", midpoint=90 )+
  theme(#Background
    plot.background = element_rect(fill = color1, color=NA),
    panel.background = element_rect(fill = color1),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    #axis
    axis.title.x=element_blank(), axis.text.x=element_blank(),
    axis.ticks.x=element_blank(), axis.title.y=element_blank(),
    axis.text.y=element_blank(), axis.ticks.y=element_blank(),
    axis.line=element_blank(),  
    #legend
    legend.position="bottom", 
    legend.background = element_rect(fill=color1,
                                     size=0.5, linetype="solid"), 
    legend.text = element_text(colour="#D55E00", size=10, angle =45, hjust = 1),
    legend.title = element_text(colour="#009E73", size=10), 
    #titles 
    plot.title = element_text(family=font,face = "bold",
                              colour = "#D55E00", size = 25, hjust = 0.5), 
    plot.caption = element_text(family=font,face = "bold",
                                colour = "#D55E00", size = 15), 
    plot.subtitle = element_text(family=font,face = "bold",
                                 colour = "#D55E00", size = 16, hjust=0.5))

#ggsave("holi.png")

  anim<-plot1+  transition_manual(factor(anio))+
    enter_fade() +
    exit_fade()+
    labs(subtitle = 'Año: {current_frame}')#+
  
  
animate(anim, fps= 4,height = 1748/4, width =2480/4, nframes = 16)
beepr::beep(sound = 8, expr = NULL)


anim_save("Libertad.gif") 

