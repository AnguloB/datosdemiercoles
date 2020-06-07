library(tidyverse)
options(scipen=10000)


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
        plot.title = element_text(size=25, hjust=0,face="bold", color="#9B77CF"), 
        plot.subtitle = element_text(face="italic", size=15), 
        plot.caption  = element_text(face="italic", size=15),
        legend.position = "none")}
library(rvest)
url<-"https://totmontserrat.cat/muntanya/agulles/agulles/"
webpage <- xml2::read_html(url)
Data <- rvest::html_table(webpage, fill=TRUE, header=TRUE)[[1]] %>% 
  tibble::as_tibble(.name_repair = "unique") # repair the repeated columns

names(Data)<- c("seccion", "nombre", "num", "altura", "lat", "long")

Data$altura<-as.numeric(Data$altura)
#Data$altura<-sub("\\.", "", Data$altura)

Data$altura<-as.numeric(sub("\\.", "", Data$altura))
Data<-na.omit(Data)



#recodifico a mano un error en la tabla html
Data$altura[Data$altura==	398571]<- 1025

Data$altura[Data$altura==	1]<- 1000

Data$altura[Data$altura==	11]<- 1100
Data$altura[Data$altura==	12]<- 1200

Data<-Data%>%
  mutate(alturaBuena=case_when(
    altura<=122~ altura*10, 
    TRUE ~ .$altura))




summar<-Data%>%
  group_by(nombre)%>%
  summarise(alturaminima= min(alturaBuena, na.rm = TRUE), 
            alturamaxima= max(alturaBuena, na.rm = TRUE))%>%
  arrange(desc(alturaminima))


#
#Data%>%
#  group_by(seccion)%>%
#  summarise(alturaminima= min(alturaBuena, na.rm = TRUE), 
#            alturamaxima= max(alturaBuena, na.rm = TRUE))%>%

#  arrange(desc(alturaminima))


orden<-Data%>%
    group_by(seccion)%>%
    summarise(latitude= mean(lat, na.rm = TRUE))%>%
  arrange(latitude)
Data$seccion<-factor(Data$seccion, levels= c(orden$seccion))



library(gganimate)
plot<-Data%>%
ggplot(aes(x=reorder(nombre,lat), y = as.numeric(alturaBuena)))+
geom_point(color=palette30[1])+
  theme_bw()+theme_ari()+
  ylim(0,1500)+
  labs(title= "Día 27: animado", 
       y = "", 
       x = "", 
       caption="Hecho por @AnguloBrunet \n #30díasdegráficos \n Fuente: https://totmontserrat.cat/muntanya/agulles/agulles/")+
  labs(subtitle = 'Sección: {closest_state}') +
  transition_states(seccion, transition_length = 3, state_length = 1) +
  shadow_mark(color = "grey66") 
p1<-animate(plot, duration = 25,  height = 565.44, width=823.68) 


anim_save("27_animado.gif", p1)
