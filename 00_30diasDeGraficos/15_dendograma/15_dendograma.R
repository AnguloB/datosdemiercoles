
#datafrom

#https://www.kaggle.com/jessemostipak/hotel-booking-demand/download
#https://www.kaggle.com/jessemostipak/hotel-booking-demand?select=hotel_bookings.csv


backcolor<-"white"
colortext<-"black"

#Defino paleta de colores
palette30<-c("#FFEC33",  "#FF8A33", "#EC4176", "#33DDFF", "#A13770", "#FD7FC4", "#5564eb", "grey")


#Eligo la fuente que quiero utilizar
library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
font<- "Trebuchet MS" #Fuente que voy a utlizar


library(tidyverse) 

hotel_bookings <- read_csv("~/Documents/GitHub/datosdemiercoles/00_30diasDeGraficos/15_dendograma/hotel_bookings.csv")

hotel_bookings<-hotel_bookings%>%
  filter(arrival_date_year=="2017")%>%
  filter(hotel=="Resort Hotel")%>%
  filter(reservation_status=="Canceled")%>%
  filter(arrival_date_month=="August")
variables<-c( "stays_in_week_nights",
             "adults", "children", 
            "booking_changes", 
             "adr" ,"total_of_special_requests" )

data<-hotel_bookings[,variables]
  #selecciono las variables numericas


library(ggdendro)
# Compute distances and hierarchical clustering
dd <- dist(scale(data), method = "euclidean")
hc <- hclust(dd, method = "complete")

#http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning#ggdendro-package-ggplot2-and-dendrogram
# Build dendrogram object from hclust results
dend <- as.dendrogram(hc)
# Extract the data (for rectangular lines)
hcdata <- dendro_data(hc, k=8)


ggplot(hcdata$segments) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend), color=palette30[3])+
 
  ylim(0, 15)+
  theme_bw()+
  theme(text=element_text(family = font),
        plot.background = element_rect(fill = backcolor, color=NA), 
        strip.background =element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.background = element_rect(fill=backcolor,
                                         size=0.5, linetype="solid"),
        plot.title = element_text(size=20, hjust=0,face="bold", color="#9B77CF"), 
        plot.subtitle = element_text(face="italic", size=15, hjust=0), 
        plot.caption = element_text( face="italic", size=12, hjust = 1), 
        axis.text.x = element_text(),
        axis.text.y = element_text(), 
        legend.position = "bottom")+ 
  labs(title= "Día 15: dendograma", 
       subtitle = "Perfil de la cancelación de estancias en Resorts (Agosto 2017)",  x="", 
       y = "", fill="Puntuación\nde felicidad",
       caption = "Hecho por @AnguloBrunet \n #30díasdegráficos \n Fuente: https://www.kaggle.com/jessemostipak/hotel-booking-demand")
ggsave("15_dendograma.png", height = 5.89, width=8.58)

