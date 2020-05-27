library(tidyverse)

# dia 6 donut
#Datos "https://www.idescat.cat/pub/?id=aec&n=825"
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
       legend.position = "bottom")}
library(readr)
datos <- read_delim("datos.csv", ";", escape_double = FALSE, trim_ws = TRUE, skip = 7)
names(datos)<- c("idioma", "Freq", "prop")
datos<-datos[1:13,] #elimino la fila del total


#Traduzco etiquetas al castellano
Other<-datos%>%
  mutate(etiquetaRed=case_when(
    idioma=="Català" ~ "Catalán",                         
    idioma=="Castellà" ~ "Castellano",                       
    idioma=="Català i castellà" ~ "Catalán y castellano",              
    idioma=="Aranès" ~"Otras lenguas",                            
    idioma=="Àrab"~ "Otras lenguas",                             
    idioma=="Anglès" ~ "Otras lenguas",                            
    idioma=="Romanès" ~"Otras lenguas",                       
    idioma=="Berber Amazic"  ~ "Otras lenguas",                    
    idioma=="Xinès" ~ "Otras lenguas",                        
    idioma== "Francès"~ "Otras lenguas",                      
    idioma== "Altres llengües"    ~ "Otras lenguas",             
    idioma== "Altres combinacions de llengües" ~ "Otras combinaciones \n de lenguas",
    idioma== "No consta" ~ "No consta"))%>%
  group_by(etiquetaRed)%>%
  summarise(Freq=sum(Freq))%>%
  mutate(prop=round(Freq/sum(Freq)*100,0))




# Waffles
#https://stats.stackexchange.com/questions/17842/how-to-make-waffle-charts-in-r
ndeep <- 10

tb4waffles <- expand.grid(y = 1:ndeep,
                          x = seq_len(ceiling(sum(Other$prop) / ndeep)))

regionvec <- rep(Other$etiquetaRed, Other$prop)

tb4waffles$region <- c(regionvec, rep(NA, nrow(tb4waffles) - length(regionvec)))

tb4waffles$region<- factor(tb4waffles$region, levels=c("Castellano", "Catalán",
                                                       "Catalán y castellano","Otras combinaciones \n de lenguas", 
                                                       "Otras lenguas", "No consta"))
# Plot it
ggplot(tb4waffles, aes(x = x, y = y, fill = region)) + 
  geom_tile(color = "white")  +
  scale_fill_manual(values=palette30[c(7,1,3,5,2,4)])+theme_bw()+theme_ari()+
  labs(title= " Día 16: waffle", 
       subtitle = "Lengua habitual en Cataluña", 
       x="",
       y="",
       caption = "Hecho por @AnguloBrunet \n #30díasdegráficos \n Fuente: Idescat https://www.idescat.cat/pub/?id=eulp&n=3192" )
ggsave("16_waffle.png", height = 5.89, width=8.58)


