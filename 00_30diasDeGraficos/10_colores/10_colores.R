# dia 10 paletas de colores
library(cowplot)
library(ggplot2)
library(magick)
#Eligo la fuente que quiero utilizar
library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
font<- "Trebuchet MS" #Fuente que voy a utlizar


data<-data.frame(x=c("a", "b", "c", "a", "b", "c"),
                y=c("A", "A", "A", "B", "B", "B"))

pic1<-ggdraw() + 
  draw_image("picture1.png") 
paleta1<- c("#FD4165","#91DF46","#01AEFB","#F08159","#E3FFC6","#C8EFFF")
data1<- data.frame(data,paleta1)
data1$paleta1<-factor(data1$paleta1, levels=paleta1)
plot1<-data1%>% 
  ggplot(aes(x = x, y = y, fill = factor(paleta1),label=paleta1) ) + 
  geom_tile()+geom_text(family=font)+
  scale_fill_manual(values = paleta1)+theme_bw()+
    theme(text=element_text(family = font),
         panel.border = element_blank(),
         panel.grid.minor = element_blank(),
         panel.grid.major = element_blank(),
         axis.ticks = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_blank(),
          legend.position = "none")+
  labs(x="", y ="")+
  coord_fixed(ratio=1)
picture1<-plot_grid(pic1, plot1, rel_widths = c(0.45, 0.55))



pic2<-ggdraw() + 
  draw_image("picture2.png") 
paleta2<- c("#9E0102","#FFA913","#907C7B","#FA7570", "#FED748", "#E7C28E")
data2<- data.frame(data,paleta2)

data2<- data.frame(data,paleta2)
data2$paleta2<-factor(data2$paleta2, levels=paleta2)
plot2<-data2%>% 
  ggplot(aes(x = x, y = y, fill = factor(paleta2),label=paleta2) ) + 
  geom_tile()+geom_text(family=font)+
  scale_fill_manual(values = paleta2)+theme_bw()+
  theme(text=element_text(family = font),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")+
  labs(x="", y ="")+
  coord_fixed(ratio=1)
picture2<-plot_grid(pic2, plot2, rel_widths = c(0.45, 0.55))


pic3<-ggdraw() + 
  draw_image("picture3.png") 
paleta3<-c("#DA7A64","#ED4B2C","#A7694F", "#FFC5A8","#616CA0","#30221F")
data3<- data.frame(data,paleta3)

data3<- data.frame(data,paleta3)
data3$paleta3<-factor(data3$paleta3, levels=paleta3)
plot3<-data3%>% 
  ggplot(aes(x = x, y = y, fill = factor(paleta3),label=paleta3) ) + 
  geom_tile()+geom_text(family=font)+
  scale_fill_manual(values = paleta3)+theme_bw()+
  theme(text=element_text(family = font),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")+
  labs(x="", y ="")+
  coord_fixed(ratio=1)
picture3<-plot_grid(pic3, plot3, rel_widths = c(0.45, 0.55))


pic4<-ggdraw() + 
  draw_image("picture4.png") 
paleta4<-c("#EA454B","#FECE7A","#9DC10C","#4F6100" ,"#7DF6EC","#D1C19F")
data4<- data.frame(data,paleta4)

data4<- data.frame(data,paleta4)
data4$paleta4<-factor(data4$paleta4, levels=paleta4)
plot4<-data4%>% 
  ggplot(aes(x = x, y = y, fill = factor(paleta4),label=paleta4) ) + 
  geom_tile()+geom_text(family=font)+
  scale_fill_manual(values = paleta4)+theme_bw()+
  theme(text=element_text(family = font),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")+
  labs(x="", y ="")+
  coord_fixed(ratio=1)
picture4<-plot_grid(pic4, plot4, rel_widths = c(0.45, 0.55))

title <- ggdraw() + draw_label("Día 10: explorar colores", size=20,fontface="bold", color="#9B77CF", fontfamily = font, hjust=1.2)
subtitle<- ggdraw() + draw_label("Colores de (mis vacaciones en) Tailandia", size=12, hjust=1.2,fontface="bold", color="black", fontfamily = font)

caption <- ggdraw() + draw_label("Hecho por @AnguloBrunet #30díasdegráficos", fontface="italic", size=12, hjust = -.2, fontfamily = font)


row1<-plot_grid(picture1, picture2, nrow=1) 
row2<-plot_grid(picture3, picture4, nrow=1)
plot_grid(title,subtitle, row1, row2, caption, nrow=5, rel_heights = c(0.10, 0.10, 0.4, 0.4, 0.1))
ggsave("10_colores.png", height = 5.89, width=8.58)

