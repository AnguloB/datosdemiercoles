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
day13<-ggdraw() + 
  draw_image("13_temporal/13_temporal.png") 
day14<-ggdraw() + 
  draw_image("14_tree/14_tree.png") 
day15<-ggdraw() + 
  draw_image("15_dendograma/15_dendograma.png") 
day16<-ggdraw() + 
  draw_image("16_waffle/16_waffle.png") 
day17<-ggdraw() + 
  draw_image("17_sankey/17_sankey.png") 
day18<-ggdraw() + 
  draw_image("18_espacial/18_espacial.png") 
day19<-ggdraw() + 
  draw_image("19_stream/19_stream.png") 
#day20<-ggdraw() + 
#  draw_image("20_redes/20_redes.png") 


row1<-plot_grid(day1, day2, day3, day4, day5, nrow=1)
row2<-plot_grid(day6,  day7,  day8 , day9 , day10, day12,nrow=1)
row3<- plot_grid(day11 ,day12 ,day13, day14, day15,nrow=1)
row4<- plot_grid( day16, day17 ,day18, day19,nrow=1)


plot_grid(row1, row2, row3, row4, nrow=4)
ggsave("summary.png")
