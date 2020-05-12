library(tidyverse)

# dia 2: lineas
#Datos del SIDC Cat (link  directo en el script )
backcolor<-"white"
colortext<-"black"
  
#Defino paleta de colores
palette30<-c("#FFA45E",  "#262253", "#EC4176", "#543884", "#A13770", "9B77CF")
#crear una paleta con N valores del color al color que le asigno
#palette <- colorRampPalette(c("#9B77CF" ,"#262253"))

#Eligo la fuente que quiero utilizar
library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
font<- "Trebuchet MS" #Fuente que voy a utlizar


library(tidyverse) 
library(pdftools)#necesario para bajar el pdf

graficos<-data.frame(dia= c("01", "02"), 
  imagen= c("01_barras/01_barras.png","02_lineas/02_lineas.png"))

library(cowplot)
library(magick)
#day1
day1<-ggdraw() + 
  draw_image("01_barras/01_barras.png") 
day2<-ggdraw() + 
  draw_image("02_lineas/02_lineas.png") 

plot_grid(day1, day2, nrow=2)
ggsave("summary.png")
