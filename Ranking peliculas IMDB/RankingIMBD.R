#Ranking películas IMDB
#datos de miercoles https://github.com/cienciadedatos/datos-de-miercoles
## 20 de Febrero de 2020, Ariadna Angulo-Brunet

library(tidyverse)
library(png)
library(grid)

imdb <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2020/2020-02-19/ranking_imdb.csv")

#Recodificar datos cada 50 años y hacer factor
imdb<-imdb%>%
mutate(year=case_when(anio <= 1950 ~ "1900 a 1950",
  anio  <= 2000 ~ "1951 a 2000",
  anio <= 2050 ~ "A partir de 2000") %>% 
    factor(levels=c('A partir de 2000',  "1951 a 2000","1900 a 1950")))

imdb<-imdb%>%
  mutate(yeareng=case_when(anio <= 1950 ~ "1900 to 1950",
                                     anio  <= 2000 ~ "1951 to 2000",
                                     anio <= 2050 ~ "From 2000") %>% 
                             factor(levels=c('From 2000',  "1951 to 2000","1900 to 1950")))
         
#Filtrar peliculas con ganancias superiores a 0.1 millones
data1<-imdb1%>%
  group_by(year)%>%
  top_n(1, wt= puntaje)%>%  #buscar en funcion de agrupacion la pel con mayor puntaje
       full_join(    #Juntar con 
             imdb1%>%
               group_by(year)%>%
                top_n(1, wt= ganancias)) #buscar en funcion de agrupacion la pel con mayores ganancias

#Busco los titulos de las peliculas top para buscar las imagenes
data1$titulo
#Estas imagenes las tengo en el directorio de trabajo
images<- c("Shawshank.png", "wonderful.png", "Andhadhun.png", "titanic.png", "starwars.png", "gonewiththewind.png")

#Ann1ñado una columna al conjunto creado con la referencia de las imagenes
data2<-data.frame(data1,images)

library(ggimage) #para añadir imagenes

  ggplot()+
    geom_point(data=imdb1, aes(x=ganancias, y=puntaje, color=puntaje))+ # Creo los puntos
    geom_image(data=data2,aes( x=ganancias, y =puntaje,image=images), size=0.15)+ #Añado imagen
          facet_wrap(~year, nrow=3)+ #Creo facetas por año
         labs(title = "Puntuación y ganancias en IMDb",
            subtitle = "Películas con ganancias iguales o superiores a 0.1 millones de dólares", 
            caption= "Hecho por @AnguloBrunet \n #datosdemieRcoles \n Datos de https://www.kaggle.com/isaactaylorofficial/imdb-10000-most-voted-feature-films-041118/data")+
          scale_y_continuous(name="Puntuación IMBb", limits=c(0,11), breaks = 1:10)+
          scale_x_continuous(name= "Ganancias (millones de dólares)", limits=c(0,1000))+
          theme(plot.background = element_rect(fill = "#F6C619", color=NA), 
               panel.background = element_rect(fill = "#F6C619"), 
          legend.position="none",
          strip.text.x = element_text(size=12, face = "bold"),
                  strip.background =element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "#F9D45B"), #cambia color liias de fondo
          plot.title = element_text(face = "bold"))+
    scale_color_gradient(low = "white", high = "green4" )
  
  ggsave("IMBD_esp.png") # guardar grafico
  
  
  ##Lo mism en ingles
 
  ggplot()+
    geom_point(data=imdb1, aes(x=ganancias, y=puntaje, color=puntaje))+ # Creo los puntos
    geom_image(data=data2,aes( x=ganancias, y =puntaje,image=images), size=0.15)+ #Añado imagen
    facet_wrap(~yeareng, nrow=3)+ #Creo facetas por año
    labs(title = "Film score and earnings in IMDb",
         subtitle = "Films with earnings equal or greater than 0.1 million dollars", 
         caption= "Done by  @AnguloBrunet \n #datosdemieRcoles \n Data from https://www.kaggle.com/isaactaylorofficial/imdb-10000-most-voted-feature-films-041118/data")+
    scale_y_continuous(name="IMDb score", limits=c(0,11), breaks = 1:10)+
    scale_x_continuous(name= "Earnings (million dollars)", limits=c(0,1000))+
    theme(plot.background = element_rect(fill = "#F6C619", color=NA), 
          panel.background = element_rect(fill = "#F6C619"), 
          legend.position="none",
          strip.text.x = element_text(size=12, face = "bold"),
          strip.background =element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "#F9D45B"), #cambia color liias de fondo
          plot.title = element_text(face = "bold"))+
    scale_color_gradient(low = "white", high = "green4" )
  
  ggsave("IMBD_eng.png") # guardar grafico
  
