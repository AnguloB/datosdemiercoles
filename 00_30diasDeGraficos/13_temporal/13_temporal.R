library(tidyverse)

# dia 2: lineas
#Datos del SIDC Cat (link  directo en el script )
backcolor<-"white"
colortext<-"black"
  
#Defino paleta de colores
palette30<-c("#FFEC33",  "#FF8A33", "#EC4176", "#33DDFF", "#A13770", "#FD7FC4", "#5564eb", "#4c2882", "#094293")


#Eligo la fuente que quiero utilizar
library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
font<- "Trebuchet MS" #Fuente que voy a utlizar


library(tidyverse) 
library(pdftools)#necesario para bajar el pdf


###Datos 
url<-"http://universitats.gencat.cat/web/.content/01_acces_i_admissio/preinscripcio/documentacio/Notes_i_places/Notes-de-tall-via-PAU-CFGS.pdf"



pdf_text <- pdf_text(url)%>%
  readr::read_lines()
datos<-data.frame(pdf_text[642:647] )

datos<-data.frame(str_replace(gsub("\\s+", " ", str_trim(datos$pdf_text.642.647.)), "B", "b"))

removestring<- c("barcelona", "Cerdanyola del Vallès", "Girona", "Lleida", "Tarragona", "Vic")


u1<-str_replace(datos[1,],"barcelona",  "")[1]
u2<-str_replace(datos[2,],"Cerdanyola del Vallès",  "")[1]
u3<-str_replace(datos[3,],"Girona",  "")[1]
u4<-str_replace(datos[4,], "Lleida",  "")[1]
u5<-str_replace(datos[5,],"Tarragona",  "")[1]
u6<-str_replace(datos[6,],"Vic",  "")[1]
datos<-rbind(u1, u2, u3, u4, u5, u6)

dat<-data.frame(t(data.frame(str_split(datos, " "))))
rownames(dat)<-c()
names(dat)<- c("codigo", "grado","blank", "Universidad", "2014ini", "2014fin", "2015ini", "2015fin", "2016ini", "2016fin", 
               "2017ini", "2017fin", "2018ini", "2018fin", "2019ini", "2019fin")


p1<-dat%>%
  select(codigo, Universidad, `2014fin`, `2015fin`, `2016fin`, `2017fin`, `2018fin`, `2019fin`)%>%
  pivot_longer(names_to = "name", cols= starts_with("201"))%>%
  mutate(valor= as.numeric(as.character( sub(",", ".", value))) )%>% #tranformo las comas a puntos 
  mutate(anio=substr(name, 0, 4))%>%
  ggplot(aes(x=anio, y =valor, color=Universidad, group=Universidad))+geom_line()+
  scale_y_continuous( limits=c(5,14), breaks = seq(5, 14, by = 2))+
  scale_color_manual(values = palette30)+
  theme_bw()+
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
  labs(title= "Día 13: datos temporales", 
       subtitle = "Nota de corte de acceso al Grado en Psicología \n en Cataluña (2014-19)",  x="", 
       y = "")


p2<-dat%>%
  select(codigo, Universidad, `2014fin`, `2015fin`, `2016fin`, `2017fin`, `2018fin`, `2019fin`)%>%
  pivot_longer(names_to = "name", cols= starts_with("201"))%>%
  mutate(valor= as.numeric(as.character( sub(",", ".", value))) )%>% #tranformo las comas a puntos 
  mutate(anio=substr(name, 0, 4))%>%
  ggplot(aes(x=anio, y =Universidad, fill=valor, group=valor))+geom_tile()+
  scale_fill_gradient(low=palette30[1], high= palette30[3])+ # 2 3
  theme_bw()+
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
  labs(  x="", 
       y = "", 
       caption = "Hecho por @AnguloBrunet \n #30díasdegráficos \n Fuente http://universitats.gencat.cat/web")

library(cowplot)
plot_grid(p1, p2, nrow = 1)

ggsave("13_temporal.png", height = 5.89, width=8.58)



  