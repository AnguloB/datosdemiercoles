# PARA OBTENER LOS DATOS DE MI CUENTA SPOTIFY
#https://www.rcharlie.com/spotifyr/
#library('spotifyr')
#library(lubridate)
#
#library(tidyverse)
#library(knitr)
#
#Sys.setenv(SPOTIFY_CLIENT_ID = 'XXX')
#Sys.setenv(SPOTIFY_CLIENT_SECRET = 'XXX')
#
#access_token <- get_spotify_access_token()
#
#
#        
# top<-get_my_top_artists_or_tracks(type = 'tracks', time_range = 'short_term', limit = 50) %>% 
#   mutate(artist.name = map_chr(artists, function(x) x$name[1])) %>% 
#   select(name, artist.name, album.name)
# 
# write.table(top, "top50songs.txt", sep= "*", row.names = FALSE)
# 
library(tidyverse)
options(scipen=10000)


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
 
 library(tidytext)
 text1<-top %>%
   unnest_tokens(word, name)%>%
   count(word, sort=TRUE)%>%
   mutate(angle = 90 * sample(c(0, 1), n(), 
                              replace = TRUE, prob = c(60, 40)))
 

 library(ggwordcloud) # for doing wordclouds
 
 text1%>%
   ggplot( aes(label = word, size=n, color=factor(n), angle=angle))+
   geom_text_wordcloud(shape="cardioid", family=font) +
   scale_radius(range=c(1,15), limits=c(0,NA))+
   scale_color_manual(values=palette30[c(7, 6, 5, 4, 3, 2, 1)])+
   theme_bw()+theme_ari()+ 
   theme(plot.background = element_rect(colour ="white")) +#remove margin
   labs( title= "Día 22: datos textuales", 
         subtitle = "Top 50 canciones de mi Spotify: título", 
         caption="Hecho por @AnguloBrunet \n #30díasdegráficos")
 
 
 
 ggsave("22_texto.png", height = 5.89, width=8.58)
 
 
 
 
 