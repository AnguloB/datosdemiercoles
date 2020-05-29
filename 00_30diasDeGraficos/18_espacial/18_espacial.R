library(tidyverse)

backcolor<-"white"
colortext<-"black"
#Defino paleta de colores
palette30<- c("#FD7FC4",  "#FF8A33", "#EC4176", "#A13770" , "#33DDFF", "#FFEC33", "#5564eb", "#4c2882")
#Eligo la fuente que quiero utilizar
library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
font<- "Trebuchet MS" #Fuente que voy a utlizar


library(rgdal) 
library(broom)


sf_regional <- readOGR("mapas/bseccenv10sh1f1_20160101_0.shp")
sf_regional<- spTransform(sf_regional , CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
regional_df <- tidy(sf_regional)

#municipi
temp_df <- data.frame(sf_regional$MUNICIPI)
temp_df$id <- as.character(seq(0,nrow(temp_df)-1))
regional_df2 <- left_join(regional_df, temp_df, by="id")
names(regional_df2)[8]<- "municipio"

#districte
temp_df <- data.frame(sf_regional$DISTRICTE)
temp_df$id <- as.character(seq(0,nrow(temp_df)-1))
regional_df3 <- left_join(regional_df2, temp_df, by="id")
names(regional_df3)[9]<- "distrito"
manresa<-regional_df3%>%
  filter(municipio =="081136")

#https://www.idescat.cat/poblacioestrangera/?b=10&geo=mun:081136
datos_ma1 <- read_delim("map1.csv", ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 2)

names(datos_ma1)<- c("distrito","poblacion", "total", "porc1" , "porctotal")

map1<-manresa%>%
  left_join(datos_ma1)


m1<-map1%>%
  ggplot() +
  aes(x=long, y =lat, fill=poblacion, group=group)+
  geom_polygon( color="black")+
  labs(subtitle=  "Población total en Manresa 2019", 
       x="", 
       y="")+
  theme_bw()+
  scale_fill_gradient(name= "Total",low=palette30[2], high = palette30[4])+
  theme(text=element_text(family = font),
        plot.background = element_rect(fill = "white", color=NA), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background =element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.background = element_rect(size=0.5, linetype="solid"),
        plot.subtitle = element_text(face="italic", size=12, hjust=.5))


m2<-map1%>%
  ggplot() +
  aes(x=long, y =lat, fill=porctotal, group=group)+
  geom_polygon( color="black")+
  labs(subtitle="Porcentaje de población estrangera \n respecto a la población de cada distrito", 
       x="", 
       y="")+
  theme_bw()+
  scale_fill_gradient(name= "",low=palette30[1], high = palette30[8])+
  theme(text=element_text(family = font),
        plot.background = element_rect(fill = "white", color=NA), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background =element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.background = element_rect(size=0.5, linetype="solid"),
        plot.subtitle = element_text(face="italic", size=10, hjust=.5))



m3<-map1%>%
  ggplot() +
  aes(x=long, y =lat, fill=porc1, group=group)+
  geom_polygon( color="black")+
  labs(subtitle="Porcentaje de población estrangera \n respecto al total estrangeros (n = 13724)", 
       x="", 
       y="")+
  theme_bw()+
  scale_fill_gradient(name= "",low=palette30[5], high = palette30[3])+
  theme(text=element_text(family = font),
        plot.background = element_rect(fill = "white", color=NA), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background =element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.background = element_rect(size=0.5, linetype="solid"),
        plot.subtitle = element_text(face="italic", size=10, hjust=.5))


#title="Día 18: datos espaciales", 

library(cowplot)


title <- ggdraw() + draw_label("Día 18: datos espaciales", size=20,fontface="bold", color="#9B77CF", fontfamily = font, hjust=1.2)
caption <- ggdraw() + draw_label("Hecho por @AnguloBrunet #30díasdegráficos", fontface="italic", size=12, hjust = -.1, fontfamily = font)

row1<- plot_grid(NULL, m1, NULL, nrow = 1, rel_widths = c(0.25, 0.5, 0.25))
row2<-plot_grid(m2,m3, nrow=1)

plot_grid(title, row1, row2,caption, nrow=4, rel_heights = c(.1,.4,.4, .1))
ggsave("18_espacial.png", height = 5.89, width=8.58)

  


