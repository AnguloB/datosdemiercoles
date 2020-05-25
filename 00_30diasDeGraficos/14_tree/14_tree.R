

library(tidyverse)

# dia 2: lineas
#Datos del SIDC Cat (link  directo en el script )
backcolor<-"white"
colortext<-"black"

#Defino paleta de colores
palette30<-c("#FFEC33",  "#FF8A33", "#EC4176", "#33DDFF", "#A13770", "#FD7FC4", "#5564eb", "grey")


#Eligo la fuente que quiero utilizar
library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
font<- "Trebuchet MS" #Fuente que voy a utlizar


library(tidyverse) 
library(pdftools)#necesario para bajar el pdf


#Datos de  procesados manualmente
#http://muntanyamontserrat.gencat.cat/web/.content/temes/el_parc/escalada/Docs/Vies_Escalada_Ordxnom.pdf"
datos <- read_excel("data.xlsx", na = "NA")

#devtools::install_github("wilkox/treemapify")
library(treemapify)
library(treemap)

resumen<-datos%>%
  mutate(categorias1= case_when( #recodifico en categorias
    any <=1899 ~ "<1900",
    any <=1919 ~ "1900 a 1919",
    any <=1939 ~ "1920 a 1939",
    any <=1959 ~ "1940 a 1959",
    any <=1979 ~ "1960 a 1979",
    any <=1999 ~ "1980 a 1999",
    any <=2014 ~ "2000 a 2014",
    is.na(any)==TRUE ~ "Desconocido"))%>%
  group_by(categorias1)%>% #agrupo
  summarise(total=n())%>% #calculo el total 
  mutate(perc= round(total/sum(total)*100,2))%>% #Calculo porcentaje
  mutate (etiqueta = paste0( categorias1, "\n", perc, "%")) # etiqueta para el grafico


  ggplot( resumen, aes(area = total, fill = categorias1, label=etiqueta)) +
  geom_treemap()+
    geom_treemap_text(fontface = "italic", colour = "black", place = "centre")+
    scale_fill_manual(name= "Año",values=palette30)+
    theme(text=element_text(family = font),
          plot.background = element_rect(fill = backcolor, color=NA), 
          strip.background =element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.background = element_rect(fill=backcolor,
                                           size=0.5, linetype="solid"),
          plot.title = element_text(size=20, hjust=0,face="bold", color="#9B77CF"), 
          plot.subtitle = element_text(face="italic", size=15, hjust=0), 
          plot.caption = element_text( face="italic", size=12, hjust = 1), 
          axis.text.x = element_text(angle = 90, hjust = 1, color=colortext),
          axis.text.y = element_text(color=colortext), 
          legend.position = "bottom")+ 
    labs(title= "Día 14: treemap", 
         subtitle = "Inventario de vias de escalada en Montserrat",  x="", 
         y = "", fill="Puntuación\nde felicidad",
         caption = "Hecho por @AnguloBrunet \n #30díasdegráficos \n Fuente :http://muntanyamontserrat.gencat.cat")

  ggsave("14_tree.png", height = 5.89, width=8.58)
  
    
    
               
               