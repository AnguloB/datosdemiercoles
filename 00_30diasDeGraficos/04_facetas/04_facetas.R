library(tidyverse)

# dia 4 facetas
#Datos del SIDC Cat (link  directo en el script )
backcolor<-"white"
colortext<-"black"
#Defino paleta de colores
palette30<- c("#FD7FC4",  "#FF8A33", "#EC4176", "#A13770" , "#33DDFF", "#FFEC33", "#5564eb", "#4c2882")

#Eligo la fuente que quiero utilizar
library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
font<- "Trebuchet MS" #Fuente que voy a utlizar

#https://github.com/cienciadedatos/datos-de-miercoles/tree/master/datos/2019/2019-06-12
vinos <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-12/vinos.csv")

vinos<-vinos%>%
  filter(pais=="España")%>%
  mutate(precio_e= precio*0.92)%>% #paso a euros
  mutate(provinciaRE= case_when(
    provincia == "Spain Other" ~ "Otro", 
    provincia == "Catalonia" ~ "Cataluña", 
    provincia == "España Central" ~ "Centro",
    provincia == "Northern Spain" ~ "Norte", 
    provincia == "Spanish Islands" ~ "Islas", 
    TRUE ~ .$provincia))%>%
  group_by(provinciaRE)

#ordeno de mayor a menor 
vinos$provinciaRE<- factor(vinos$provinciaRE, 
                           levels=c("Norte","Cataluña","Centro", "Levante", "Galicia", 
                                    "Andalucía", "Otro", "Islas"))
caros<-vinos %>%   #busco los mas caros por zona
  group_by(provinciaRE)%>%
  top_n(1, precio)%>%
  select(provinciaRE,titulo_resena, vina)%>%
  mutate(etiquetaCaro = vina)

buenos<-    vinos%>% #busco los mas buenos (mayor puntuacion) por zona
  group_by(provinciaRE)%>%
  top_n(1, puntos)%>%
  select(provinciaRE,titulo_resena, vina)%>%
  mutate(etiquetaBueno = vina)

vinos<-vinos%>%
  left_join(caros)%>%
  left_join(buenos)
etiquetas1<-vinos%>%
  select(provinciaRE,puntos, precio_e,  etiquetaCaro)
etiquetas2<-vinos%>%
  select(provinciaRE,puntos, precio_e,etiquetaBueno)

etiquetas1<-data.frame(etiquetas1[1:4], "caro")
names(etiquetas1)[4]<-"vina"
names(etiquetas1)[5]<-"etigroup"


etiquetas2<-data.frame(etiquetas2[1:4], "bueno")
names(etiquetas2)[4]<-"vina"
names(etiquetas2)[5]<-"etigroup"

etiquetas<-na.omit(rbind(etiquetas1, etiquetas2))
library(ggrepel)

vinos%>%
  ggplot()+ 
  geom_jitter(data= vinos, aes(x= puntos, y = precio_e),
              alpha=0.3, show.legend = FALSE, color="#9B77CF")+
  # #  geom_text_repel(data= etiquetas, aes(x=puntos, y= precio_e, color=provinciaRE, label=name), max.iter = 50)+
  geom_label_repel(data= etiquetas, aes(x=puntos, y= precio_e,fill=etigroup, label=vina),
                   alpha = .7, size=1.8)+
  facet_wrap(~provinciaRE, nrow=2, scales = "free_y")+
  theme_bw()+
  xlim(80, 100)+
  theme(text=element_text(family = font),
        plot.background = element_rect(fill = "white", color=NA), 
        strip.background =element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.background = element_rect(fill=backcolor,
                                         size=0.5, linetype="solid"),
        plot.title = element_text(size=20, hjust=0,face="bold", color="#9B77CF"), 
        plot.subtitle = element_text(face="italic", size=12, hjust=0), 
        plot.caption = element_text( face="italic", size=10, hjust = 1), 
        axis.text.x = element_text(angle = 90, hjust = 1, color=colortext),
        axis.text.y = element_text(color=colortext), 
        legend.position = "top")+
  scale_fill_manual(values = palette30[c(3,5)], labels = c("Mejor puntuación", "Más caro")) +
  labs(title= "Día 4: facetas", 
       fill="", 
       subtitle = "Vinos españoles: Viñas con los vinos mejor puntuados y más caros por zona", 
       y = "€", 
       x = "Puntuación 0-100", 
       caption = "Hecho por @AnguloBrunet \n #30díasdegráficos \n Fuente #datosdemiércoles https://www.kaggle.com/zynicide/wine-reviews/")+
  guides( fill = guide_legend(override.aes = aes(label = "")))


ggsave("04_facetas.png", height = 5.89, width=8.58)

