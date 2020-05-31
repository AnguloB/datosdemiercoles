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

library(readr)
Citaciones <- read_csv("Citaciones.csv")

#selecciono solo las variables que me interesan
Citaciones%>%
  select(Authors, Title) ->data

data<-data%>%separate_rows(Authors, sep = ",") #Separo por coma los autores en cada linea
data<-data[seq(1,nrow(data),2) ,] #me quedo solo con los pares
data$Authors<-str_trim(data$Authors) #saco espacios en blanco
data<-data%>%
  group_by(Title) %>% 
  mutate(titleid=factor(group_indices())) #cambio el titulo por un ID

data<-data[,c("titleid","Authors")]
library(stringi)
data$Authors<-stri_trans_general(data$Authors, "Latin-ASCII")


totals<-data%>% #Creo el total de articulos de cada 
  group_by(Authors)%>%
  count()%>%
  arrange(desc(n))
names(totals)<-c("from", "totalreal") 

# transformo los datos de forma que haya la correspondencia entre autores
dta <- full_join(data, data, c('titleid' = 'titleid')) %>% 
  select(-titleid) %>% 
  filter(Authors.x != Authors.y) %>% 
  group_by(Authors.x, Authors.y) %>% 
  summarise(total = n())


names(dta)<- c("from", "to", "total")

dta<-dta%>%
  left_join(totals)%>%
  select(from, to, totalreal)




library(ggraph)
library(tidygraph)
data<-as_tbl_graph(dta, mode="out", directed = TRUE) #%>%
 # mutate(Popularity = centrality_degree(mode = 'in'))

#palette30  <- c("grey60","#FFEC33","#33DDFF","#EC4176","#FF8A33","#5564eb")
set_graph_style()
ggraph(data, layout = "kk") + 
  geom_node_point(size=3) + geom_edge_link(color=palette30[1], alpha = .5)+
theme(text=element_text(family = font),
        plot.background = element_rect(fill = "white", color=NA), 
        strip.background =element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill=backcolor,
                                         size=0.5, linetype="solid"),
        plot.title = element_text(size=20, hjust=0,face="bold", color="#9B77CF"), 
        plot.subtitle = element_text(face="italic", size=12), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")+
  labs(title= "Día 20: redes", 
       subtitle = "A journey around alpha and omega to estimate internal consistency reliability: \n
       autores y autoras que han citado el articulo y relación entre ellos", 
       y = "", 
       x = "", 
       caption="Viladrich, Angulo-Brunet y Doval (2017) \n Hecho por @AnguloBrunet \n #30díasdegráficos \n Fuente: Scopus 29 mayo 2020")
ggsave("20_redes.png", height = 5.89, width=8.58)

