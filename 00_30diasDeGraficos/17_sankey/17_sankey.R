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

#Data from https://www.kaggle.com/gulsahdemiryurek/harry-potter-dataset


library(readr)
Characters <- read_delim("Characters.csv",    ";", escape_double = FALSE, trim_ws = TRUE)



library(ggalluvial)


Characters%>%
  filter(House %in% c("Gryffindor",  "Hufflepuff","Ravenclaw","Slytherin"))%>%
  group_by( House,Gender,`Blood status` , Job)%>%
  summarise(n = n())%>%
  arrange(desc(n))%>%
    ggplot(aes(axis1 = reorder(House, -n), axis2 = reorder(Gender,n), axis3 = reorder(`Blood status`,-n),axis4=Job,
             y = n)) +
  scale_x_discrete(limits = c("", "", ""), expand = c(.1, .05)) + #remove s acis
  geom_stratum(aes(fill = House), color="black") + #change background of squares # "#B3B39C"
  geom_alluvium(aes(fill = House)) +geom_text(stat = "stratum", infer.label = TRUE) +
  theme_minimal() + #remove leegend
  scale_fill_manual(values= c("#7f0909","#eee117" ,"#000a90","#0d6217" ), na.value="#B3B39C")+ #set manual colors
  labs(title ="Día 17: Diagrama de Sankey",
subtitle= "Personajes de Harry Potter",
x="" ,caption= "Done by @AnguloBrunet \n #tidytuesday \n Datos de https://www.kaggle.com/gulsahdemiryurek/harry-potter-dataset")+
  theme_ari()


ggsave("17_sankey.png", height = 5.89, width=8.58)


