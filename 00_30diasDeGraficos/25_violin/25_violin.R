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
       axis.ticks = element_blank(),
       legend.background = element_rect(fill=backcolor,
                                        size=0.5, linetype="solid"),
       plot.title = element_text(size=20, hjust=0,face="bold", color="#9B77CF"), 
       plot.subtitle = element_text(face="italic", size=12), 
       legend.position = "none")}

apps <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-24/apps_googleplaystore.csv")

apps$tamanio<-gsub('.{1}$', '', apps$tamanio)
apps$tamanio<-as.numeric(apps$tamanio)


orden<-apps%>%
group_by(categoria)%>%
summarise(media= mean(tamanio, na.rm=TRUE))%>%
arrange(desc(media))

filtro<-c("Conocer personas", "Eventos", "Entretenimiento", "Social", "Comunicación", "Reproductores y editores de video")

apps$categoria<- factor(apps$categoria, levels= orden$categoria)
apps%>%
filter(categoria %in% filtro)%>%
filter(tamanio <=500)%>%
ggplot(aes(x=categoria, y= as.numeric(tamanio)))+
geom_boxplot()+
geom_violin(aes(x=categoria, y= as.numeric(tamanio),fill=categoria),alpha=.5)+
scale_fill_manual(values=palette30)+
coord_flip()+theme_bw()+theme_ari()+
   labs( title= "Día 25: violín", 
         subtitle = "Tamaño aplicaciones en Google Play *", 
         caption="*Algunas categorías seleccionadas\n** Excluída una app on >500M\nHecho por @AnguloBrunet \n #30díasdegráficos \n#datosdemiercoles", 
         y= "tamaño (MB)", x="")


ggsave("25_violin.png", height = 5.89, width=8.58)

