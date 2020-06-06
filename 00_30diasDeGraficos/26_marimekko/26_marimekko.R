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
        #axis.text = element_blank(),
        #axis.ticks = element_blank(),
        legend.background = element_rect(fill=backcolor,
                                         size=0.5, linetype="solid"),
        plot.title = element_text(size=20, hjust=0,face="bold", color="#9B77CF"), 
        plot.subtitle = element_text(face="italic", size=12), 
        legend.position = "bottom")}

#Data from https://www.kaggle.com/gulsahdemiryurek/harry-potter-dataset


library(readr)
Characters <- read_delim("Characters.csv",    ";", escape_double = FALSE, trim_ws = TRUE)

Characters<-Characters%>%
  filter(House %in% c("Gryffindor",  "Hufflepuff","Ravenclaw","Slytherin"))

#Characters$Job<-factor(Characters$Job, levels=c( "Professor","Student", "Other"))
#Necesario con 
#devtools::install_github('rmcd1024/ggmosaic', ref="ggplot_3.3.0-scales/fix")
devtools::install_github('haleyjeppson/ggmosaic')


library(ggmosaic)
Characters%>%
  filter(Gender %in% c("Female", "Male"))%>%
ggplot() +
  geom_mosaic(aes(x = product(Job, House), fill=Job, conds=product(Gender)), na.rm=TRUE) +
  #facet_grid(Gender~.)+
  labs(x="", y="", title='Día 26: marimekko', 
       subtitle= "Personajes de Harry Potter",
       caption= "Done by @AnguloBrunet \n #tidytuesday \n Datos de https://www.kaggle.com/gulsahdemiryurek/harry-potter-dataset") +
  theme_bw()+ theme_ari()+
  scale_fill_manual(values = palette30[c(1,5, 7)])+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggsave("26_marimekko.png", height = 5.89, width=8.58)

