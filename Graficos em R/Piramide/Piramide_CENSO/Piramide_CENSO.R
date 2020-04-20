

library(ggplot2)
library(Cairo)
library(ggthemes)
library(RColorBrewer)
library(dplyr)

# dataset
piramide_municipios <- read.csv2("piramide_etaria_municipal.csv") 


# fazendo filtro e selecionando municipio
piramide_filter <- piramide_municipios %>% filter(Ano==2010) %>% filter(Municipios =="Salvador") %>%
  group_by(faixa_etarias,sexo) %>% summarise(cont_total=sum(total_faixa))

# ordendando o eixo y
piramide_filter$faixa_etarias <- factor(piramide_filter$faixa_etarias, levels = c("Entre.0.a.4",	"Entre.5.a.9",
                                                                                  "Entre.10.a.14",	"Entre.15.a.19",
                                                                                  "Entre.20.a.24",	"Entre.25.a.29",
                                                                                  "Entre.30.a.34",	"Entre.35.a.39",
                                                                                  "Entre.40.a.44",	"Entre.45.a.49",
                                                                                  "Entre.50.a.54",	"Entre.55.a.59",
                                                                                  "Entre.60.a.64",	"Entre.65.a.69",
                                                                                  "Entre.70.a.74",	"Entre.75.a.79",
                                                                                  "Entre.80.a.84",	"Entre.85.a.89",
                                                                                  "Entre.90.a.94",	"Entre.95.a.99",
                                                                                  "Mais.de.100"),
                                        labels = c("0 a 4","5 a 9","10 a 14","15 a 19","20 a 24","25 a 29",
                                                   "30 a 34","35 a 39","40 a 44","45 a 49","50 a 54","55 a 59",
                                                   "60 a 64","65 a 69","70 a 74","75 a 79","80 a 84","85 a 89",
                                                   "90 a 94","95 a 99","+100"))



# grafico
ggplot(piramide_filter, aes(x=faixa_etarias, y=cont_total, fill=sexo)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_manual(values = c("#b51b8f","#1b6db5")) +
  labs(x="Faixa Etaria (em anos)", y="População", caption = "Fonte: CENSO", title = "Piramide Etaria, Salvador, 2010.") +
  theme(legend.position="bottom", legend.title = element_text(colour="Black", size=12, face="bold"), legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank"))+         
  theme(plot.title = element_text(hjust = 0.5,face="bold", colour = "black",size = 12),
        axis.title.x = element_text(size=12, face="bold", colour = "black"),
        axis.title.y = element_text(size=12, face="bold", colour = "black"),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=7),
        axis.text.x = element_text(face="bold", color="#000000", 
                                   size=7)) +
  scale_y_continuous(breaks = seq(-200000,200000, 50000),
                     labels = paste0(as.character(c(seq(200, 0, -50),
                                                    seq(50, 200, 50))), "Mil"))




