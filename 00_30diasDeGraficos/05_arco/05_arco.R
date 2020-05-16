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



palette30  <- c("grey60","#FFEC33","#33DDFF","#EC4176","#FF8A33","#5564eb")
                
               

p1<-ggraph(dta, 'linear') +
  geom_edge_arc(aes(color=factor(totalreal), alpha=factor(totalreal)),  fold=FALSE)+theme_bw()+
  geom_node_point(size=2,alpha=0.5) +
  scale_edge_colour_manual(values=palette30)+
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
  labs(title= "Día 5: arco", 
       fill="", 
       subtitle = "A journey around alpha and omega to estimate internal consistency reliability: \n
       autores y autoras que han citado el articulo y relación entre ellos", 
       y = "", 
       x = "")+
  expand_limits(x = c(-1.2, 1.2), y = c(-5.6, 1.2)) 
  
  
p2<-totals%>%
  group_by(totalreal)%>%
  count()%>%
  ggplot(aes(x=factor(totalreal), y=n, fill=factor(totalreal)))+
  geom_col(aes( alpha=factor(totalreal)))+
  geom_text(aes(label=paste0("N = ",n), hjust=-.25))+
  scale_fill_manual(values=palette30)+
  coord_flip()+theme_bw()+
  theme(text=element_text(family = font, color="#9B77CF"),
        plot.background = element_rect(fill = "white", color=NA), 
        strip.background =element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
legend.position = "none", 
plot.caption = element_text( face="italic", size=10, hjust = 1, color="black"))+
  labs(title ="",
       subtitle="\n \n",
       caption = "Viladrich, Angulo-Brunet y Doval (2017) \n Hecho por @AnguloBrunet \n #30díasdegráficos \n Fuente: Scopus 15 mayo 2020", 
       y="Autores", 
       x="Nº articulos")+
  scale_y_continuous(position = "right", limits=c(0,180))
library(cowplot)
  
plot_grid(p1, p2, nrow=1, rel_widths = c(0.8, .2))
  ggsave("05_arco.png", height = 5.89, width=8.58)
  
