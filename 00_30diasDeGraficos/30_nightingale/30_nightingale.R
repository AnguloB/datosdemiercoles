#https://analisi.transparenciacatalunya.cat/Salut/Registre-de-casos-de-COVID-19-realitzats-a-Catalun/jj6z-iyrp/data
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

theme_ari<- function(legpos="none"){ #defino plot para los dos graficos
  theme(text=element_text(family = font, size=8),
        plot.background = element_rect(fill = "white", color=NA), 
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill=backcolor,
                                         size=0.5, linetype="solid"),
        plot.title = element_text(size=20, hjust=0,face="bold", color="#9B77CF"), 
        plot.subtitle = element_text(face="italic", size=12), 
        plot.caption  = element_text(face="italic", size=9, hjust=-1.5), 
        
        legend.position = legpos)}
library(lubridate)
covid <- read_csv("registre.csv")

covid<-covid%>%
  mutate(data= dmy(TipusCasData))%>%
  mutate(dia = day(data))%>%
  mutate(mes = month(data))%>%
  mutate(semana = week(data))%>%
  mutate(mesE= case_when(
    mes == 2 ~ "Febrero", 
    mes == 3 ~ "Marzo", 
    mes == 4 ~ "Abril", 
    mes == 5 ~ "Mayo", 
    mes == 6 ~ "Junio"))%>%
  mutate(etiqueta = paste( "Semana", semana, "\n", mesE))%>%
  mutate(castellano=case_when(
   TipusCasDescripcio== "Epidemiològic" ~ "Epidemiológico",
   TipusCasDescripcio== "Positiu PCR"~ "Positivo PCR",
   TipusCasDescripcio== "Positiu per ELISA"~ "Positivo ELISA",
   TipusCasDescripcio== "Positiu per Test Ràpid"~ "Positivo test rápido",
   TipusCasDescripcio== "Sospitós"~ "Sospechoso"))%>%
  filter(mes!="2")

  

covid%>%
  filter(ComarcaDescripcio=="Bages")%>%
 # group_by(MunicipiDescripcio)%>%
  summarise(total= sum(NumCasos, na.rm=TRUE))%>%
  arrange(desc(total))

graph1<-covid%>%
  filter(ComarcaDescripcio=="Bages")%>%
  group_by(mes, etiqueta,castellano)%>%
  summarise(total= sum(NumCasos, na.rm=TRUE))%>%
  mutate(caso="2020")%>%
  ggplot(aes(fill=castellano,x=etiqueta, y = total))+
  geom_col()+coord_polar()+
  scale_fill_manual(values= palette30)+
  theme_bw()+theme_ari(legpos="bottom")+
  labs(x="", y="", fill="", 
     title= "Día 30: Florence Nightingale", 
            subtitle = "Casos covid en comarca Bages (Cataluña)\n N = 11450")+
  guides(fill=guide_legend(nrow=2))




graph2<-covid%>%
  filter(MunicipiDescripcio=="Manresa")%>%
  group_by(mes, etiqueta,castellano)%>%
  summarise(total= sum(NumCasos, na.rm=TRUE))%>%
  mutate(caso="2020")%>%
  ggplot(aes(fill=castellano,x=etiqueta, y = total))+
  geom_col()+coord_polar()+
  scale_fill_manual(values= palette30)+
  theme_bw()+theme_ari()+theme( axis.text  = element_blank())+
  labs(x="", y="", fill="", 
       subtitle = "Manresa (N = 5030)",
       caption = "")




graph3<-covid%>%
  filter(MunicipiDescripcio=="Sant Joan de Vilatorrada")%>%
  group_by(mes, etiqueta,castellano)%>%
  summarise(total= sum(NumCasos, na.rm=TRUE))%>%
  mutate(caso="2020")%>%
  ggplot(aes(fill=castellano,x=etiqueta, y = total))+
  geom_col()+coord_polar()+
  scale_fill_manual(values= palette30[c(1,3,4,5,6)])+
  theme_bw()+theme_ari()+theme( axis.text  = element_blank())+
  labs(x="", y="", fill="", 
       subtitle = "Sant Joan de Vilatorrada (N = 683)",
       caption = "Hecho por @AnguloBrunet
       #30díasdegráficos")

library(cowplot)

pgrid<-plot_grid(NULL, graph2, graph3, nrow=3, rel_heights = c(0.2, 0.4,0.4))
plot_grid(graph1, pgrid, nrow=1)
ggsave("30_nightingale.png", height = 5.89, width=8.58)






