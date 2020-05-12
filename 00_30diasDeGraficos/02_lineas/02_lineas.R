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


###Datos del 2017
url2017<-"http://drogues.gencat.cat/web/.content/minisite/drogues/professionals/epidemiologia/docs/SIDC-Informe-2017-en-rev-23-07-2018.pdf"


sidc_text17 <- pdf_text(url2017)%>%
  readr::read_lines()
uhd17<-sidc_text17[964:977] 


encabezado17<-data.frame(str_split(str_replace(gsub("\\s+", " ", str_trim(uhd17[1])), "B", "b"), " "))
total17<-data.frame(str_split(str_replace(gsub("\\s+", " ", str_trim(uhd17[14])), "B", "b"), " "))[4:9,]


data17<-data.frame(encabezado17, total17)
names(data17)<-c("encabezado17", "total")
data17$total<-as.numeric(data17$total)
data17<-data17%>%
  mutate(perc = round(total*100/974,1))%>%
  mutate(year="2017")%>%
  mutate(encabezado_esp=case_when( #recodifico etiquetas de catalan a castellano
    encabezado17=="HEROÏNA" ~  "Heroína",
    encabezado17=="COCAÏNA"  ~ "Cocaína",
    encabezado17=="CÀNNABIS"  ~ "Cannabis",
    encabezado17=="ALCOHOL"  ~ "Alcohol",
    encabezado17=="TAbAC"  ~ "Tabaco",
    encabezado17=="ALTRES" ~ "Otros" ))%>%
  select(year, encabezado_esp, perc)
 

###Datos del 2018
url2018<-"http://drogues.gencat.cat/web/.content/minisite/drogues/professionals/epidemiologia/docs/Informe_2018_SIDC_ok.pdf"

sidc_text <- pdf_text(url2018)%>%
  readr::read_lines()
uhd18<-sidc_text[922:947] 


encabezado18<-data.frame(str_split(str_replace(gsub("\\s+", " ", str_trim(uhd18[1])), "B", "b"), " "))[2:8,]
total18<-data.frame(str_split(str_replace(gsub("\\s+", " ", str_trim(uhd18[26])), "B", "b"), " "))[1:7,]

data18<-data.frame(encabezado18, total18)

data18$total<-str_extract(data18$total, "\\-*\\d+\\.*\\d*")


data18<-data18[1:6,]%>%
  mutate(perc=total)%>%
  mutate(year="2018")%>%
  mutate(encabezado_esp=case_when( #recodifico etiquetas de catalan a castellano
    encabezado18=="Heroïna" ~  "Heroína",
    encabezado18=="Cocaïna"  ~ "Cocaína",
    encabezado18=="Cànnabis"  ~ "Cannabis",
    encabezado18=="Alcohol"  ~ "Alcohol",
    encabezado18=="Tabac"  ~ "Tabaco",
    encabezado18=="Altres" ~ "Otros" ))%>%
  select(year, encabezado_esp, perc)

data<-  rbind(data17, data18)


data$encabezado_esp<-factor(data$encabezado_esp, levels = c("Alcohol","Cocaína", "Heroína", "Cannabis", "Otros", "Tabaco" ))

library(ggrepel) #para que las etiquetas no se superpongan
data%>%
ggplot(aes(x= encabezado_esp, y = as.numeric(perc), color=year,group=year, label=perc))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_label_repel( show.legend = FALSE )+
  theme_bw()+
  scale_color_manual(values = palette30[c(1,3)], name = "Año", labels = c("2017\nN = 974", "2018\nN = 1062"))+
  theme(text=element_text(family = font),
        plot.background = element_rect(fill = "white", color=NA), 
        strip.background =element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.background = element_rect(fill=backcolor,
                                         size=0.5, linetype="solid"),
        plot.title = element_text(size=20, hjust=0,face="bold", color="#9B77CF"), 
        plot.subtitle = element_text(face="italic", size=12, hjust=0), 
        plot.caption = element_text( face="italic", size=10, hjust = 1), 
        axis.text.x = element_text(angle = 90, hjust = 1, color=colortext),
        axis.text.y = element_text(color=colortext), 
        legend.position = "top")+ 
  labs(title= "Día 2: líneas", 
       subtitle = "Altas notificadas por unidad hospitalaria de desintoxicación en Cataluña \nsegún la droga principal que ha motivado el ingreso",  x="", 
       y = "%", 
       caption = "Hecho por @AnguloBrunet \n #30díasdegráficos \n Fuente http://drogues.gencat.cat/ca/professionals/epidemiologia/sid/")
ggsave("02_lineas.png", height = 5.89, width=8.58)

