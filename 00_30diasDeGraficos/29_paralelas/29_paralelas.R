library(tidyverse)

setwd("~/Documents/GitHub/datosdemiercoles/00_30diasDeGraficos/29_paralelas")
#https://datahippo.org
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
       legend.position = "bottom")}
andalucia <- "https://datahippo.org/media/regions/6da45532-20a3-41e6-ac0f-522d6877f68e/5992304e8a46554e81f28877_airbnb.csv"
catalunya<- "https://datahippo.org/media/regions/db8babab-86bd-4a46-a68e-3aefadec5aaf/599230508a46554e81f2887c_airbnb.csv"
valencia <- "https://datahippo.org/media/regions/aed8bed0-16a0-41f6-ac3e-05179075878e/599230508a46554e81f28880_airbnb.csv"
canaries<- "https://datahippo.org/media/regions/4c70a355-34ee-4a01-a0c8-65c6abe80009/599230518a46554e81f28884_airbnb.csv"
balears <- "https://datahippo.org/media/regions/966f2578-9c96-44e3-bd35-6937175c9a43/599230518a46554e81f28883_airbnb.csv"


db1 <- data.frame(grupo='Andalucía' ,readr::read_csv(andalucia))
db2 <- data.frame(grupo= "Cataluña",readr::read_csv(catalunya))
db3 <- data.frame(grupo= "C.Valenciana",readr::read_csv(valencia))
db4 <- data.frame(grupo= "I. Canarias",readr::read_csv(canaries))
db5 <- data.frame(grupo= "I. Balears",readr::read_csv(balears))

data<-rbind(db1, db2, db3, db4, db5)
library(ggrepel)
data%>%
  select(grupo,bedrooms, capacity,  min_nights, price)%>%
  group_by(grupo)%>%
  summarise_all(mean, na.rm=TRUE)%>%
reshape2::melt()%>%
  group_by(variable)%>%
  mutate(zscore= (value-mean(value, na.rm = TRUE))/ sd(value, na.rm = TRUE))%>%
  ggplot(aes(x=variable, y =zscore, group=grupo, color=grupo, label=round(value,1)))+
  geom_point( )+geom_line()+
  geom_label_repel( hjust=1,  alpha=.5)+
  scale_color_manual(values=palette30)+
  theme_bw()+theme_ari()+
  labs( title= "Día 29: coordenadas paralelas", 
        subtitle = "Características alojamientos airbnb *", 
        caption="*5 comunidades autónomas con más propiedades\nHecho por @AnguloBrunet \n #30díasdegráficos\n Fuente: https://datahippo.org", 
        y= "Z", 
        fill="", x="")+  guides( color = guide_legend(override.aes = aes(label = "")))

ggsave("29_paralelas.png", height = 5.89, width=8.58)


