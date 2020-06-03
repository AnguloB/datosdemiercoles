library(tidyverse)
options(scipen=10000)

library(tidyverse)
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
     

df <- read_excel("sunburst.xlsx")

## code from 
#https://stackoverflow.com/questions/50004058/multiple-dependent-level-sunburst-doughnut-chart-using-ggplot2
######
lvl0 <- tibble(name = "Parent", value = 0, level = 0, fill = NA)

lvl1 <- df %>%
  group_by(name) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(level = 1) %>%
  mutate(fill = name)

lvl2 <- df %>%
  select(name = type, value, fill = name) %>%
  mutate(level = 2)


data<-bind_rows(lvl0, lvl1, lvl2) %>%
  mutate(name = as.factor(name) %>% fct_reorder2(fill, value)) %>%
  arrange(fill, name) %>%
  mutate(level = as.factor(level)) %>%
  mutate(etiqueta = paste0(name, "\nn = ", value))
data$etiqueta[2]<- NA

  plot1<- ggplot(data, aes(x = level, y = value, fill = fill, alpha = level)) +
  geom_col(width = 1, color = "white", size = 0.25, position = position_stack()) +
  geom_text(aes(label = etiqueta), size = 2.5, position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_alpha_manual(values = c("0" = 0, "1" = 1, "2" = 0.7), guide = F) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  scale_fill_manual(values=palette30[c(1,3)])+
  labs(x = NULL, y = NULL) +
  theme_bw()+theme_ari()+
  theme(plot.margin = unit(c(-0,-10,0,-10), "cm"))+
  annotate("text", x =0 , y = 0,
           label = "N = 228", color="#9B77CF", size=8)


title <- ggdraw() + draw_label("Día 23: sunburst graph", size=20,fontface="bold", color="#9B77CF", fontfamily = font, hjust=1.2)
subtitle<- ggdraw() + draw_label("Resumen muestra Angulo-Brunet (2019)", size=12, hjust=1.2,fontface="bold", color="black", fontfamily = font)

caption <- ggdraw() + draw_label("Hecho por @AnguloBrunet #30díasdegráficos", fontface="italic", size=12, hjust = -.2, fontfamily = font)

library(cowplot)
plot_grid(title, subtitle, plot1, caption, nrow = 4, rel_heights = c(.1, .1, .8, .1))
ggsave("23_sunburst.png", height = 5.89, width=8.58)


