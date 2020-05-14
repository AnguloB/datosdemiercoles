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

row1<-plot_grid(day1, day2, nrow=1)
row2<-plot_grid(NULL, day3, NULL, nrow=1, rel_widths = c(0.25, 0.5, 0.25))
plot_grid(row1, row2, nrow = 2)
ggsave("summary.png")

