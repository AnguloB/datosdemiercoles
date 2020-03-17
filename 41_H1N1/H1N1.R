# Casos Nuevo h1n1
# Datos de https://www.kaggle.com/de5d5fe61fcaa6ad7a66/pandemic-2009-h1n1-swine-flu-influenza-a-dataset


font<-"Trebuchet MS" #elegir la fuente de defecto

library(tidyverse)
h1n1 <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2020/2020-03-11/h1n1.csv")


library(extrafont) # la primera vez ejecutar font_import()
loadfonts()

#Defino un tema comun para los diversos plots
theme_ari<- function(legpos="bottom"){  
  theme(text=element_text(family = font),
        legend.position = legpos)
}


ariDivPalette <- c("#FF6F61", "grey54", "deepskyblue1", "purple") #Creo una paleta
#paises con idioma oficial ESP segun wikipedia
paises1<-c("Argentina", "Bolivia", "Chile", "Colombia", "Costa Rica", "Cuba", "República Dominicana",
           "Ecuador", "El Salvador", "Guinea Ecuatorial", "Guatemala", "Honduras", "México", "Nicaragua", 
           "Panamá", "Paraguay", "Perú", "España", "Uruguay",  "Venezuela") 


h1n1<-h1n1%>%mutate(region=case_when(
  pais =='Argentina'~ "Argentina",
  pais == 'Bolivia' ~ "Bolivia",
  pais == 'Chile'~ "Chile",
  pais == 'Colombia'~ "Colombia",
  pais == 'Costa Rica'~ "Costa Rica",
  pais == 'Cuba'~ "Cuba",
  pais == 'República Dominicana'~ "Dominican Republic",
  pais == 'Ecuador'~ "Ecuador",
  pais == 'El Salvador'~ "El Salvador",
  pais == 'Guinea Ecuatorial'~ "Guinea Ecuatorial", 
  pais == 'Guatemala' ~ "Guatemala",
  pais == 'Honduras'~ "Honduras",
  pais == 'México'~ "Mexico",
  pais == 'Nicaragua' ~ "Nicaragua",
  pais == 'Panamá'~ "Panama",
  pais == 'Paraguay' ~ "Paraguay",
  pais == 'Perú'~ "Peru",
  pais == 'España'~ "Spain",
  pais == 'Uruguay' ~ "Uruguay",
  pais == 'Venezuela'~ "Venezuela"))#paises con idioma oficial ESP

#Creo categorias segun numero de casos
h1n1<-h1n1%>%
  mutate(categ = case_when(
    between(casos, 501, 1000) ~ "501 a 1000",
    between(casos, 1001, 5000) ~ "1001 a 5000",
    between(casos, 5001, 50000) ~ "5001 a 50000"))
h1n1$categ<-factor( h1n1$categ,levels= c("501 a 1000","1001 a 5000","5001 a 50000"))


dat1<-h1n1%>%
  group_by(pais)%>%
  filter(casos>=501)%>%
  #top_n(1)%>%
  top_n(1,  casos)%>%
  select(pais, casos,  categ)%>%distinct()



dat1%>%
  group_by(pais)%>%
  filter(casos>=501)%>%
  #top_n(1)%>%
  top_n(1,  casos)%>%
  select(pais, casos,  categ)%>%distinct()%>%
  ggplot()+
  geom_col(aes(x= reorder(pais, -casos), y=casos, fill=categ))+coord_flip()+theme_bw()+
  labs (title = "Países con más de 500 casos con H1N1 reportados", 
        caption = "Hecho por @AnguloBrunet \n #datosdemiercoles") + xlab("")+ ylab("Casos")+
  scale_fill_manual("Categorías",values= ariDivPalette)+theme_ari()
ggsave("Paises_n500.png")

#h1n1_500<-h1n1%>%
# filter (pais %in% dat1$pais)
#h1n1_500%>%
#  ggplot()+ geom_point(aes(x=fecha_actualizacion, y =casos, color=pais))
dat2<-h1n1%>%
  filter(pais %in%paises1)%>%
  group_by(pais)%>%
  top_n(1, wt=casos)%>%
  select(-fecha_actualizacion, -muertes)%>%
  distinct()

dat2%>%  ggplot(aes(x= reorder(pais, -casos), y=casos, label=casos))+
  geom_col( fill="#FF6F61")+
  geom_text(hjust=-0)+coord_flip()+theme_bw()+
  labs (title = "Casos reportados de H1N1 en países con lengua oficial castellano", 
        caption = "*No se han encontrado casos para Guinea Ecuatorial \n Hecho por @AnguloBrunet \n #datosdemiercoles") + xlab("")+ ylab("Casos")+
  scale_fill_manual("Categorías",values= ariDivPalette)+theme_ari()+
  scale_y_continuous(breaks = seq(0, 12000, by = 2000))
ggsave("Paises_ESP.png")


mapp<-map_data("world") #Abro los datos para hacer el mundo 


dat3<-mapp%>%
  left_join(dat2)


color1 <-"white" #color de los paises
color2<-"white"  #color de fondo

ggplot()+
  geom_polygon(data=dat3,aes(x = long, y = lat, group = group, fill=casos), color="grey60")+
  theme_classic()+
  scale_fill_gradient(low= "deepskyblue2",  high= "#FF6F61", na.value="white")+
  labs(title = "Casos reportados de H1N1 en países con lengua oficial castellano", subtitle ="",
       caption= "Hecho por @AnguloBrunet \n #datosdemiercoles")+
  theme(plot.background = element_rect(fill = color1, color=NA),
        panel.background = element_rect(fill = color1), 
        axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        legend.position="none", 
        legend.background = element_rect(fill=color1,
                                         size=0.5, linetype="solid"), 
        legend.text = element_text( size=10, angle =45, hjust = 1),
        legend.title = element_text(colour="white", size=10, 
                                    face="bold"), 
        axis.line=element_blank())+theme_ari()
ggsave("Paises_ESP_mapa.png")



