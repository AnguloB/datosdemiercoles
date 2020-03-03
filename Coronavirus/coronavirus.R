# Casos Nuevo Coronavirus
# Datos de https://github.com/CSSEGISandData/COVID-19
# Actualizado el 3 de marzo de 2020 

color1 <-"#414141" #color de los paises
color2<-"gray51"  #color de fondo

library(tidyverse)

confirmed<-read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

#deaths<- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19//master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")

#recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19//master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")



library(maps)
mapp<-map_data("world") #Abro los datos para hacer el mundo 


conf<-confirmed %>%
  pivot_longer(   #tranformo los datos a formato largo
    cols = names(confirmed)[5:length(confirmed)],
    names_to = "day",
    names_prefix = "wk",
    values_to = "value",
    values_drop_na = TRUE)%>%
  filter(value >=1)
conf$day<-factor(lubridate::mdy(conf$day)) #Transformo los datos para poder usar en  transition_states


library(gganimate)

  animation1<-ggplot()+
  geom_polygon(data=mapp,aes(x = long, y = lat, group = group), colour = color1, fill=color2)+
  geom_point(data = conf, aes(x=Long, y =Lat, group =value,size=value),  color ="hotpink", alpha = 0.8)+
    theme_classic()+
    labs(title = "COVID-19 total confirmed cases",  caption= "Done by  @AnguloBrunet")+
  theme(plot.background = element_rect(fill = color1, color=NA),
        panel.background = element_rect(fill = color1), 
        axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        legend.position="none", 
        legend.background = element_rect(fill=color1,
                                         size=0.5, linetype="solid"), 
        legend.text = element_text(colour="white", size=10, angle =45, hjust = 1),
        legend.title = element_text(colour="white", size=10, 
                                    face="bold"), 
        axis.line=element_blank(),  
        plot.title = element_text(face = "bold",
                    colour = "hotpink", size = 24, hjust = 0.5), 
        plot.caption = element_text(face = "bold",
                    colour = "hotpink", size = 16), #pie del grafico
        plot.subtitle = element_text(face = "bold",
                    colour = "white", size = 16))+
    scale_size_continuous(range =c(5,40))+
     transition_states(conf$day) +  #añado annimacion de dia
   labs(subtitle = "Day: {closest_state}" ) #titulo
animate(animation1, height = 1748/2, width =2480/2) #defino las medidas del output
anim_save("coronavirus.gif") #guardo en formato gif
