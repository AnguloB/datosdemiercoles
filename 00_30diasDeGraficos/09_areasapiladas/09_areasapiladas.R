library(tidyverse)

# dia 8 contornos
backcolor<-"white"
colortext<-"black"
  
#Defino paleta de colores
palette30<-c("#FFEC33",  "#FF8A33", "#EC4176", "#33DDFF", "#A13770", "#FD7FC4", "#5564eb", "#4c2882", "#094293")
#crear una paleta con N valores del color al color que le asigno
#palette <- colorRampPalette(c("#9B77CF" ,"#262253"))

#Eligo la fuente que quiero utilizar
library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
font<- "Trebuchet MS" #Fuente que voy a utlizar


#############Grafico 1
#obtengo la url de los datos de la pagina web
d2012<-"http://opendata.manresa.cat/dataset/34544507-1e99-4a96-85d6-d4d7e4767279/resource/bddd5494-bd05-441d-a445-c68a61085e4f/download/2012.csv"
d2013<- "http://opendata.manresa.cat/dataset/34544507-1e99-4a96-85d6-d4d7e4767279/resource/6caa7882-1bcf-45cb-9d59-a94176bb1c3c/download/2013.csv"
d2014<- "http://opendata.manresa.cat/dataset/34544507-1e99-4a96-85d6-d4d7e4767279/resource/e2a1d44d-4a8e-4353-bd0d-727fa81f278c/download/2014.csv"
d2015<- "http://opendata.manresa.cat/dataset/34544507-1e99-4a96-85d6-d4d7e4767279/resource/3fafead1-c4b2-48bb-8899-a35ae845a996/download/2015.csv"
d2016<-"http://opendata.manresa.cat/dataset/34544507-1e99-4a96-85d6-d4d7e4767279/resource/28501af1-d7ac-412c-8a3d-da632c048c45/download/2016.csv"
d2017<- "http://opendata.manresa.cat/dataset/34544507-1e99-4a96-85d6-d4d7e4767279/resource/f2974e9c-8ffa-40ea-a193-8a6cc06e0a54/download/2017.csv"
d2018<-"http://opendata.manresa.cat/dataset/34544507-1e99-4a96-85d6-d4d7e4767279/resource/183b23d3-c2b8-4450-8c72-db720fd33f8f/download/2018.csv"
d2019<-"http://opendata.manresa.cat/dataset/34544507-1e99-4a96-85d6-d4d7e4767279/resource/ca8dbfc9-7fbc-4d7f-a7b1-8d3bcebcae1a/download/2019.csv"
#abro datos
d2012 <- read_delim(d2012, ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 1)
d2013 <- read_delim(d2013, ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 1)
d2014 <- read_delim(d2014, ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 1)
d2015 <- read_delim(d2015, ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 1)
d2016 <- read_delim(d2016, ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 1)
d2017 <- read_delim(d2017, ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 1)
d2018 <- read_delim(d2018, ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 1)
d2019 <- read_delim(d2019, ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 1)

#ann1ñado columna del año y cambio nombre 
nombrevariables<-c("year", "area", "edad", "hombre", "mujer")
d2012<-data.frame("2012", d2012)
names(d2012)<- nombrevariables
d2013<-data.frame("2013", d2013)
names(d2013)<- nombrevariables
d2014<-data.frame("2014", d2014)
names(d2014)<- nombrevariables
d2015<-data.frame("2015", d2015)
names(d2015)<- nombrevariables
d2016<-data.frame("2016", d2016)
names(d2016)<- nombrevariables
d2017<-data.frame("2017", d2017)
names(d2017)<- nombrevariables
d2018<-data.frame("2018", d2018)
names(d2018)<-nombrevariables
d2019<-data.frame("2019", d2019)
names(d2019)<- nombrevariables

#junto los datasets
datos<-rbind(d2012, d2013, d2014, d2015, d2016, d2017, d2018, d2019)


#calculo los totales y creo rangos de edad
datos<-datos%>%
  group_by(year,edad)%>%
  mutate(total = hombre+mujer)%>%
  summarise_at(c("hombre", "mujer", "total"), sum, na.rm = TRUE)%>%
  mutate(edadRE=case_when(
    edad <=16~ "0 a 16",
    edad <=24~ "16 a 24",
    edad <=54 ~ "25 a 54",
    edad <=64 ~ "55 a 64",
    edad >=65 ~  "65 o más"))

#acabo de obtener los totales y calculo %
datos<-datos%>%
  group_by(year, edadRE)%>%
  summarise(n = sum(total)) %>%
  mutate(freq = round(n / sum(n)*100,1))%>%
mutate(prop = n / sum(n))

p1<-ggplot(datos,
         aes(x=year, y=freq))+
  geom_area(aes(fill=edadRE, group=edadRE), color="black", alpha=.8)+
    theme_bw()+
    scale_fill_manual(name="Rango de edad",values=palette30[c(1,2,3,4,6)])+
    theme(text=element_text(family = font),
          plot.background = element_rect(fill = backcolor, color=NA), 
          strip.background =element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.text=element_text(size=7),
          legend.title=element_text(size=7),
          legend.background = element_rect(fill=backcolor,
                                           size=0.5, linetype="solid"),
          plot.title = element_text(size=20, hjust=0,face="bold", color="#9B77CF"), 
          plot.subtitle = element_text(face="italic", size=13, hjust=0), 
          plot.caption = element_text( face="italic", size=12, hjust = 1), 
          axis.text.x = element_text(angle = 90, hjust = 1, color=colortext),
          axis.text.y = element_text(color=colortext), 
          legend.position = "bottom")+ 
    labs(title= "Día 9: àreas apiladas", 
         subtitle = "Población de Manresa \nsegún rango de Edad y Nacionalidad", x="", y="Porcentaje(%)")
  
  #############Grafico 2
#igual a anterior  

d2012n<-"http://opendata.manresa.cat/dataset/e9f600ff-983a-4861-95d1-a748c372a33a/resource/5907c460-0e37-497b-987f-d9c0db8f0296/download/2012.csv"
d2013n<-"http://opendata.manresa.cat/dataset/e9f600ff-983a-4861-95d1-a748c372a33a/resource/5907c460-0e37-497b-987f-d9c0db8f0296/download/2012.csv"
d2014n<-"http://opendata.manresa.cat/dataset/e9f600ff-983a-4861-95d1-a748c372a33a/resource/fafe5db6-9375-44ff-af51-283dfb0af6b8/download/2014.csv"
d2015n<-"http://opendata.manresa.cat/dataset/e9f600ff-983a-4861-95d1-a748c372a33a/resource/2e91fe03-8c45-4476-bda5-f98007f17a46/download/2015.csv"
d2016n<-"http://opendata.manresa.cat/dataset/e9f600ff-983a-4861-95d1-a748c372a33a/resource/1bd9b715-8b7e-43fd-84fb-461256c49327/download/2016.csv"
d2017n<-"http://opendata.manresa.cat/dataset/e9f600ff-983a-4861-95d1-a748c372a33a/resource/c65fac6b-522f-4588-8b62-300538df41da/download/2017.csv"
d2018n<-"http://opendata.manresa.cat/dataset/e9f600ff-983a-4861-95d1-a748c372a33a/resource/9723893b-83e7-481d-b5da-4093242f65b5/download/2018.csv"
d2019n<-"http://opendata.manresa.cat/dataset/e9f600ff-983a-4861-95d1-a748c372a33a/resource/86e67296-ed78-4409-ac9c-8cd4374121f2/download/2019.csv"
  

d2012n <- read_delim(d2012n, ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 1)
d2013n <- read_delim(d2013n, ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 1)
d2014n <- read_delim(d2014n, ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 1)
d2015n <- read_delim(d2015n, ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 1)
d2016n <- read_delim(d2016n, ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 1)
d2017n <- read_delim(d2017n, ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 1)
d2018n <- read_delim(d2018n, ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 1)
d2019n <- read_delim(d2019n, ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE, skip = 1)


dataname<-c("year", "codigo", "UNSD" , "codigodes", "hombres", "mujeres")

d2012n<-data.frame("2012", d2012n)
names(d2012n)<- dataname
d2013n<-data.frame("2013", d2013n)
names(d2013n)<- dataname
d2014n<-data.frame("2014", d2014n)
names(d2014n)<- dataname
d2015n<-data.frame("2015", d2015n)
names(d2015n)<- dataname
d2016n<-data.frame("2016", d2016n)
names(d2016n)<- dataname
d2017n<-data.frame("2017", d2017n)
names(d2017n)<- dataname
d2018n<-data.frame("2018", d2018n)
names(d2018n)<- dataname
d2019n<-data.frame("2019", d2019n)
names(d2019n)<- dataname
  
datosn<-rbind(d2012n, d2013n, d2014n, d2015n, d2016n, d2017n, d2018n, d2019n)

datosn$UNSD<-as.character(datosn$UNSD)

#abro un libro de codigos con correspondencia entre codigos y continentes

paises <- read_excel("paises.xlsx")

datosn_<-datosn%>%
  left_join(paises)%>% #uno los continentes a los datos que ya tengo
  mutate(regionCAT=case_when(
    UNSD=="724" ~ "España",
    TRUE~ .$Continent))%>%
  mutate(region=case_when( #traduzco del catalan al castellano
    regionCAT=="Àfrica"  ~ "África",
    regionCAT=="Amèrica" ~  "América",
    regionCAT=="Àsia" ~ "Asia",
    regionCAT=="España"  ~  "España",
    regionCAT=="Europa" ~"Europa",
    regionCAT=="Oceania"  ~"Oceanía", 
    is.na(regionCAT)==TRUE ~ "Desconocido"))%>%
  select(codigo,year,UNSD, region, hombres, mujeres)%>%
  distinct()
  
  datosn_$region<- factor(datosn_$region, levels= c("España", "África", "América", "Asia","Europa","Oceanía", "Desconocido"))

  datosn_<-datosn_%>%
  mutate(total= hombres+mujeres)%>%
  group_by(year, region)%>%
  summarise_at(c("hombres", "mujeres", "total"), sum, na.rm = TRUE)%>%
  mutate(prop = round(total / sum(total)*100,2))

p2<-ggplot(datosn_,
       aes(x=year, y=prop))+
  geom_area(aes(fill=region , group=region), color="black", alpha=.8)+
  theme_bw()+
  scale_fill_manual(name="Nacionalidad",values=palette30[c(6, 1,2,3,4,5,7)])+
  theme(text=element_text(family = font),
        plot.background = element_rect(fill = backcolor, color=NA), 
        strip.background =element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text=element_text(size=7),
        legend.title=element_text(size=7),
        legend.background = element_rect(fill=backcolor,
                                         size=0.5, linetype="solid"),
        plot.title = element_text(size=20, hjust=0,face="bold", color="#9B77CF"), 
        plot.subtitle = element_text(face="italic", size=15, hjust=0), 
        plot.caption = element_text( face="italic", size=11, hjust = 1), 
        axis.text.x = element_text(angle = 90, hjust = 1, color=colortext),
        axis.text.y = element_text(color=colortext), 
        legend.position = "bottom")+ 
  labs( x="", y="Porcentaje(%)",
       caption = "Hecho por @AnguloBrunet \n #30díasdegráficos \n Fuente: http://opendata.manresa.cat")

library(patchwork)
p1+p2
ggsave("09_areasapiladas.png", height = 5.89, width=8.58+0.3)
