library(tidyverse)

# dia 6 donut
#Datos "https://www.idescat.cat/pub/?id=aec&n=825"
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
       axis.ticks = element_blank(),
       legend.background = element_rect(fill=backcolor,
                                        size=0.5, linetype="solid"),
       plot.title = element_text(size=20, hjust=0,face="bold", color="#9B77CF"), 
       plot.subtitle = element_text(face="italic", size=12), 
       legend.position = "none")}
datos <- read_delim("datos.csv", ";", escape_double = FALSE, trim_ws = TRUE, skip = 7)
names(datos)<- c("idioma", "Freq", "prop")
datos<-datos[1:13,] #elimino la fila del total

datosRed<-datos%>%
  mutate(etiquetas=case_when(
    idioma=="Català" ~ "Catalán",                         
    idioma=="Castellà" ~ "Castellano",                       
    idioma=="Català i castellà" ~ "Catalán y castellano",
    TRUE  ~ "Otro"))%>%
  group_by(etiquetas)%>%
  summarise(Freq=sum(Freq))%>%
  mutate(prop=round(Freq/sum(Freq)*100,1))
#ordeno los datos para el donut
datosRed<-rbind(datosRed[2,], datosRed[4,], datosRed[3,], datosRed[1,]) 


# https://www.r-graph-gallery.com/128-ring-or-donut-plot.html
datosRed$ymax = cumsum(datosRed$prop) #maximo
datosRed$ymin = c(0, head(datosRed$ymax, n=-1))
datosRed$ycenter<-(datosRed$ymax+ datosRed$ymin)/2
datosRed$ycenter2<-(datosRed$ymax+ datosRed$ymin)/2+1



p1<-datosRed%>%
  ggplot() +
  geom_rect(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=etiquetas),alpha=.7) +
  coord_polar(theta="y") + 
  geom_text( x=3.3, aes(y=ycenter, label=prop), size=3) +  
  geom_label( x=4, aes(y=ycenter2, label=etiquetas), size=3, alpha=.5) + 
  xlim(c(2, 4))+ 
  scale_fill_manual(values=palette30[c(1,5,3,7)])+
  theme_bw()+
  theme_ari()+theme(plot.margin = unit(c(-0,-10,0,-10), "cm"), 
                    axis.text.y = element_blank(), axis.text.x = element_blank())+ylab("")+
  labs(title= "   Día 6: donut", 
       subtitle = "     Lengua habitual en Cataluña")


#Traduzco etiquetas al castellano
Other<-datos%>%
  mutate(etiquetaRed=case_when(
    idioma=="Català" ~ "Catalán",                         
    idioma=="Castellà" ~ "Castellano",                       
    idioma=="Català i castellà" ~ "Catalán y castellano",              
    idioma=="Aranès" ~ "Aranés",                         
    idioma=="Àrab"~ "Árabe",                           
    idioma=="Anglès" ~ "Inglés",                         
    idioma=="Romanès" ~ "Rumano",                       
    idioma=="Berber Amazic"  ~ "Berber",                
    idioma=="Xinès" ~ "Chino",                           
    idioma== "Francès"~ "Francés",                        
    idioma== "Altres llengües"    ~ "Otras lenguas",             
    idioma== "Altres combinacions de llengües" ~ "Otras combinaciones \n de lenguas",
    idioma== "No consta" ~ "No consta"))%>%
  filter(!etiquetaRed %in% c("Catalán y castellano", "Catalán", "Castellano"))%>%
  group_by(etiquetaRed)%>%
  summarise(Freq=sum(Freq))%>%
  mutate(prop=round(Freq/sum(Freq)*100,1))

p2<-Other%>%
  ggplot(aes(x=reorder(etiquetaRed,prop), y=prop, label=paste0(prop, "%")))+
  geom_col(fill=palette30[5], alpha=.7)+
  geom_text()+
  coord_flip()+
  theme_bw()+theme_ari()+labs(x="", y ="")+
  labs(caption = "Hecho por @AnguloBrunet \n #30díasdegráficos \n Fuente: Idescat https://www.idescat.cat/pub/?id=eulp&n=3192" )


library(cowplot)

p2_1<-plot_grid(NULL, p2, nrow=2, rel_heights = c(0.6, 0.4))

plot_grid(p1, p2_1, nrow=1, rel_widths = c(0.6, 0.4))
ggsave("06_donut.png", height = 5.89, width=8.58)
