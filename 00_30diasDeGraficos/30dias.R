library(tidyverse)


library(cowplot)
library(magick)
#day1
day1<-ggdraw() + 
  draw_image("01_barras/01_barras.png") 
day2<-ggdraw() + 
  draw_image("02_lineas/02_lineas.png") 
day3<-ggdraw() + 
  draw_image("03_puntos/03_puntos.png") 
day4<-ggdraw() + 
  draw_image("04_facetas/04_facetas.png") 
day5<-ggdraw() + 
  draw_image("05_arco/05_arco.png") 
day6<-ggdraw() + 
  draw_image("06_donut/06_donut.png") 
day7<-ggdraw() + 
  draw_image("07_ridgeline/07_ridgeline.png") 
day8<-ggdraw() + 
  draw_image("08_contorno/08_contorno.png") 
day9<-ggdraw() + 
  draw_image("09_areasapiladas/09_areasapiladas.png") 
day10<-ggdraw() + 
  draw_image("10_colores/10_colores.png") 
day11<-ggdraw() + 
  draw_image("11_heatmap/11_heatmap.png") 

day12<-ggdraw() + 
  draw_image("12_lollipop/12_lollipop.png") 
row1<-plot_grid(day1, day2, day3, day4, nrow=1)
row2<-plot_grid( day5, day6, day7,day8,nrow=1)
row3<- plot_grid(day9, day10, day11,day12, nrow=1)


plot_grid(row1, row2, row3, nrow=3)
ggsave("summary.png")

