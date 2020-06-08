library(tidyverse)

# dia 20 networks
backcolor<-"white"
colortext<-"black"
#Defino paleta de colores
palette30<- c("#FD7FC4",  "#FF8A33", "#EC4176", "#A13770" , "#33DDFF", "#FFEC33", "#5564eb", "#4c2882")

#Eligo la fuente que quiero utilizar
library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
font<- "Trebuchet MS" #Fuente que voy a utlizar



# Charge the circlize library
library(circlize)
Citaciones <- read_csv("Citaciones.csv")

palette30<- c("#FD7FC4",  "#FF8A33", "#EC4176", "#A13770" , "#33DDFF", "#FFEC33", "#5564eb", "#4c2882")

#selecciono solo las variables que me interesan
Citaciones%>%
  select(Authors, Title) ->data

data<-data%>%separate_rows(Authors, sep = ",") #Separo por coma los autores en cada linea
data<-data[seq(1,nrow(data),2) ,] #me quedo solo con los pares
data$Authors<-str_trim(data$Authors) #saco espacios en blanco
data<-data%>%
  group_by(Title) %>% 
  mutate(titleid=factor(group_indices())) #cambio el titulo por un ID
#####


data<-data[,c("titleid","Authors")]
library(stringi)
data$Authors<-stri_trans_general(data$Authors, "Latin-ASCII")


totals<-data%>% #Creo el total de articulos de cada 
  group_by(Authors)%>%
  count()%>%
  arrange(desc(n))
names(totals)<-c("from", "totalreal") 

dta <- full_join(data, data, c('titleid' = 'titleid')) %>% 
  select(-titleid) %>% 
  filter(Authors.x != Authors.y) %>% 
  group_by(Authors.x, Authors.y) %>% 
  summarise(total = n())

library(igraph)
names(dta)<- c("from", "to", "total")

# Create the graph object
names(totals)<- c("nodes", "count")

g <- graph.data.frame(dta, directed=F, vertices=totals)
                      
###
#######Grafico interactivo

library(edgebundleR)
graph<-edgebundle( g, tension= .6, fontsize = 8)

saveEdgebundle(graph, 
               "graph.html", selfcontained = TRUE)

####### no interactivo 

radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}

## Obviously labeling in this way this only makes sense for graphs
## laid out as a circle to begin with

V(g)$color <-  "salmon"
png("plot1.png", 600, 600)
plot(g, layout = layout.circle,
     vertex.label=NA, vertex.size=degree(g))

library(cowplot)
library(magick)
plotmiddle<-ggdraw() + 
  draw_image("plot.png") 




title <- ggdraw() + draw_label("Día 28: cuerdas", size=20,fontface="bold", color="#9B77CF", fontfamily = font, hjust=2)
subtitle<- ggdraw() + draw_label("A journey around alpha and omega", size=12, hjust=1.5,fontface="bold", color="black", fontfamily = font)

caption <- ggdraw() + draw_label("Hecho por @AnguloBrunet #30díasdegráficos", fontface="italic", size=12, hjust = -.2, fontfamily = font)

library(cowplot)
plot_grid(title, subtitle,plotmiddle, caption, nrow = 4, rel_heights = c(.1, .1, .8, .1))
ggsave("28_cuerdas.png", height = 5.89, width=8.58)


