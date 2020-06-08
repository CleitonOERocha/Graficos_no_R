################################################################################################### 
######################################### Gráfico no R ############################################
################################################################################################### 
#####   DESCRIÇÃO:        Conjunto de gráficos elaborados no R.
#####   ESCRITO POR:      Cleiton Rocha - www.linkedin.com/in/cleitonoerocha/
#####   EMAIL:  cleitonrocha@sei.ba.gov.br // cleitonotavio058@gmail.com 
#####   LICENÇA:          GPLv3
################################################################################################### 
################################################################################################### 

library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(Cairo)
library(dplyr)

# dataset
popfaixaquinquenal <- read.csv2("Pop_Faixa_Quiquena_BA_Proj2018.csv", dec=",", header=TRUE) 

# dataset filtrando apenas 2020
popfaixaquinquenal_2020 <- popfaixaquinquenal %>% filter(Ano==2020)

# grafico
ggplot(popfaixaquinquenal_2020, aes(x=FaixaEtaria, y=Populacao, fill=Genero)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_manual(values = c("#1b6db5","#b51b8f"))+
  labs(x="Faixa Etária", y="Ano", title = "Piramide Etária", caption = "Fonte: IBGE - Projeção (2010-2060)") +
  theme(legend.position="bottom", legend.title = element_text(colour="Black", size=12, face="bold"), legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size=12, face="bold", colour = "black"),
        axis.title.y = element_text(size=12, face="bold", colour = "black"),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=7),
        axis.text.x = element_text(face="bold", color="#000000", 
                                   size=7)) +
  scale_y_continuous(breaks = seq(-700000,700000, 200000),
                     labels = paste0(as.character(c(seq(700, 0, -200),
                                                    seq(100, 700, 200))), "Mil"))
