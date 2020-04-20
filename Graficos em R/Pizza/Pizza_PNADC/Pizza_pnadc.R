

library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(Cairo)
library(dplyr)


# dataset
pnadCsexo <- read.csv2("pnadCsexo.csv", h=T, dec=",") # ok 

# Grafico
ggplot(pnadCsexo,aes(x="", y=percent, fill=sexo)) +
  geom_bar(width=1, stat = "identity") +
  coord_polar("y", start=0) +
  geom_text(aes(y=yposicao_legenda, label = sprintf("%1.2f%%",percent)), size = 4,
            color = 'black',fontface='bold') +
  scale_fill_manual(values = c("#1b6db5","#b51b8f"))+
  labs(x="",y="", title="Percentual da população, por gênero - Bahia, 2019",
       caption = "Fonte: IBGE - PNADC 2019 - 3 trimestre") +
  theme_minimal() +
  theme(legend.position="bottom", legend.title = element_text(colour="Black", size=12, face="bold"), legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank") )+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(face="bold", color="#000000", 
                                   size=9))
