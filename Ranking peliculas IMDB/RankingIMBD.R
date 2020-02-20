#Ranking películas IMDB


#datos de miercoles https://github.com/cienciadedatos/datos-de-miercoles
## 20 de Febrero de 2020, Ariadna Angulo-Brunet


library(tidyverse)
imdb <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2020/2020-02-19/ranking_imdb.csv")

head(imdb)

table(imdb$genero)



any<-recode(imdb$anio, c(1901:1950) = '1901 to 1950')

imdb<-imdb%>%
mutate(year=case_when(anio <= 1950 ~ "1900 a 1950",
  anio  <= 2000 ~ "1951 a 2000",
  anio <= 2050 ~ "A partir de 2000") %>% 
    factor(levels=c('A partir de 2000',  "1951 a 2000","1900 a 1950")))
  


#+facet_wrap(~year, ncol=1)+
#theme_classic()

imdb1<-imdb%>%
  filter(ganancias >=0.1)


imdb1%>%
  group_by(year)%>%
  top_n(1, wt= ganancias)

data1<-imdb1%>%
  group_by(year)%>%
  top_n(1, wt= puntaje)%>% 
       full_join(
             imdb1%>%
               group_by(year)%>%
                top_n(1, wt= ganancias))

data1$titulo

images<-c("https://www.r-project.org/logo/Rlogo.png",
          "https://jeroenooms.github.io/images/frink.png", 
          "https://www.r-project.org/logo/Rlogo.png",
          "https://jeroenooms.github.io/images/frink.png", 
          "https://www.r-project.org/logo/Rlogo.png",
          "https://jeroenooms.github.io/images/frink.png")
data2<-data.frame(data1,images)
library(ggimage)

  ggplot()+
    geom_point(data=imdb1, aes(x=ganancias, y=puntaje, color=year))+
    geom_image(data=data2,aes( x=ganancias, y =puntaje,image=images))+facet_wrap(~year, nrow=3)
    
  
  
  
  data2%>%
  ggplot()+geom_image(aes(x=ganancias, y =puntaje, image= images))

ggplot(data2, aes(x=ganancias, y =puntaje)) + geom_image(aes(image=images))

ggplot() +
 geom_image(data=data2,aes( x=ganancias, y =puntaje,image=images))
#imdb1%>%
#  group_by(year)%>%
#  top_n(1, wt= puntaje)

  theme(plot.background = element_rect(fill = "#414141", color=NA), 
         panel.background = element_rect(fill = "#414141"), 
        # axis.title.x="Ganancias (millones de dooooooo), axis.text.x=element_blank(),
         axis.ticks.x=element_blank(), axis.title.y=element_blank(),
         axis.text.y=element_blank(), axis.ticks.y=element_blank(),
         legend.position="bottom", 
         legend.background = element_rect(fill="#414141",
                                          size=0.5, linetype="solid"), 
         legend.text = element_text(colour="white", size=10, angle =45, hjust = 1),
         legend.title = element_text(colour="white", size=10, 
                                     face="bold")) 
         axis.line=element_bl