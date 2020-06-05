#https://analisi.transparenciacatalunya.cat/Mem-ria/Fosses-comunes-a-Catalunya/6js6-vud6
setwd("~/Documents/GitHub/datosdemiercoles/00_30diasDeGraficos/24_coropletas")
library(tidyverse)
options(scipen=10000)
library(tidyverse)
backcolor<-"white"
colortext<-"black"
#Defino paleta de colores
palette30<- c("#FD7FC4",  "#FF8A33", "#EC4176", "#A13770" , "#33DDFF", "#FFEC33", "#5564eb", "#4c2882")
#Eligo la fuente que quiero utilizar
library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
font<- "Trebuchet MS" #Fuente que voy a utlizar
theme_ari<- function(){ #defino plot para los dos graficos
  theme(text=element_text(family = font),
        plot.background = element_rect(fill = "white", color=NA), 
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill=backcolor,
                                         size=0.5, linetype="solid"),
        plot.title = element_text(size=20, hjust=0,face="bold", color="#9B77CF"), 
        plot.subtitle = element_text(face="italic", size=12), 
        legend.position = "none")}


####### maps


library(rgdal) #to import shapefiles
library(broom) #to convert shapefiles into the data frame structure we need
shp_comarques <- readOGR("mapes/bm5mv21sh0tpc1_20200101_0.shp") #Provincia
shp_comarques <- spTransform(shp_comarques , CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))



#Converting it to df:
Comarques_df <- tidy(shp_comarques)

#temp_df <- data.frame(shp_Provincies$NOMPROV)
temp_df <- data.frame(shp_comarques$NOMCOMAR)

temp_df$id <- as.character(seq(0,nrow(temp_df)-1))

#Joining
comarques_df2 <- left_join(Comarques_df, temp_df, by="id")
names(comarques_df2)[8]<-"Comarca"

#recuento$Comarca %in% unique(comarques_df2$Comarca)
comarques_df2$Comarca<- tolower(comarques_df2$Comarca)

########
#Mapa general
data <- read_csv("Fosses_comunes_a_Catalunya.csv")
data$Comarca<- tolower(data$Comarca)
recuento_g<-data%>%
  group_by(Comarca)%>%
  count()


datos_g<-comarques_df2%>%
  left_join(recuento_g)

centros_g<-comarques_df2 %>%  #look for the center of each 
  group_by(Comarca)%>%
  summarize(long1 = mean(range(long)), lat1 = mean(range(lat)))%>%
  left_join(recuento_g)


general<-ggplot()+
  geom_polygon(data=datos_g, aes(x=long, y = lat, group = group,fill=n))+
  geom_text(data=centros_g, aes(x=long1, y=lat1,label=n), size=2.5)+
  theme_bw()+theme_ari()+ 
  scale_fill_gradient(low = "white", high = "#EC4176")+
  coord_fixed()+
  labs(subtitle = "General", x="", y="")

###civiles

recuento_c<-data%>%
  filter(TipusFossa=="Civils")%>%
  group_by(Comarca)%>%
  count()

datos_c<-comarques_df2%>%
  left_join(recuento_c)

centros_c<-comarques_df2 %>%  #look for the center of each 
  group_by(Comarca)%>%
  summarize(long1 = mean(range(long)), lat1 = mean(range(lat)))%>%
  left_join(recuento_c)


civils<-ggplot()+
  geom_polygon(data=datos_c, aes(x=long, y = lat, group = group,fill=n))+
  geom_text(data=centros_c, aes(x=long1, y=lat1,label=n),size=2.5)+
  theme_bw()+theme_ari()+ 
  scale_fill_gradient(low = "white", high = "#EC4176")+
  coord_fixed()+
  labs(subtitle = "Civiles", x="", y="")


###hospital militar

recuento_h<-data%>%
  filter(TipusFossa=="Hospital Militar")%>%
  group_by(Comarca)%>%
  count()

datos_h<-comarques_df2%>%
  left_join(recuento_h)

centros_h<-comarques_df2 %>%  #look for the center of each 
  group_by(Comarca)%>%
  summarize(long1 = mean(range(long)), lat1 = mean(range(lat)))%>%
  left_join(recuento_h)


hospital<-ggplot()+
  geom_polygon(data=datos_h, aes(x=long, y = lat, group = group,fill=n))+
  geom_text(data=centros_h, aes(x=long1, y=lat1,label=n), size=2.5)+
  theme_bw()+theme_ari()+ 
  scale_fill_gradient(low = "white", high = "#EC4176")+
  coord_fixed()+
  labs(subtitle = "Hospital militar", x="", y="")



###hospital militar
#Soldats 

recuento_s<-data%>%
  filter(TipusFossa=="Soldats")%>%
  group_by(Comarca)%>%
  count()

datos_s<-comarques_df2%>%
  left_join(recuento_s)

centros_s<-comarques_df2 %>%  #look for the center of each 
  group_by(Comarca)%>%
  summarize(long1 = mean(range(long)), lat1 = mean(range(lat)))%>%
  left_join(recuento_s)


soldados<-ggplot()+
  geom_polygon(data=datos_s, aes(x=long, y = lat, group = group,fill=n))+
  geom_text(data=centros_s, aes(x=long1, y=lat1,label=n), size=2.5)+
  theme_bw()+theme_ari()+ 
  scale_fill_gradient(low = "white", high = "#EC4176")+
  coord_fixed()+
  labs(subtitle = "Soldados", x="", y="")



library(cowplot)

title <- ggdraw() + draw_label("Día 24: coropletas", size=20,fontface="bold", color="#9B77CF", fontfamily = font, hjust=1.2)
subtitle<- ggdraw() + draw_label("Fosas comunes en Cataluña", size=12, hjust=1.2,fontface="bold", color="black", fontfamily = font)

caption <- ggdraw() + draw_label("Hecho por @AnguloBrunet #30díasdegráficos \n https://analisi.transparenciacatalunya.cat", fontface="italic", size=12, hjust = -.2, fontfamily = font)
plot<-plot_grid(general, civils, hospital, soldados)

p1<-plot_grid(title, subtitle,plot, caption, rel_heights = c(.1, .1, .8, .1), nrow = 4)
ggsave( "24_coropletas.png",plot=p1, height = 5.89, width=8.58)

