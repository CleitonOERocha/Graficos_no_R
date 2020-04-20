


library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(Cairo)
library(dplyr)

# dataset
proj18 <- read.csv2("proj18.csv") 

# grafico
ggplot(proj18,aes(x=Ano)) +
  geom_area(aes(y=TBM), fill="brown4", alpha=0.6) +
  geom_area(aes(y=TBN), fill="orange1",alpha=0.3) +
  geom_line(aes(y=TBM,color="red")) +
  geom_line(aes(y=TBN,color="orange")) +           
  labs(title = "Taxa Bruta de Natalidade (TBN) e\n Taxa Bruta de Mortalidade (TBM), Bahia, 2010-2060.",
       caption = "Fonte: IBGE - Projeção (2010-2060)") +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=7),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        plot.title = element_text(colour = "black", size = 12, hjust=0.5),
        axis.text.x = element_text(face="bold", color="#000000", 
                                   size=9)) +
  scale_colour_manual(name = 'Legenda', 
                      values =c('red'='red','orange'='orange'), labels = c('TBN','TBM')) +
  theme(legend.position = "bottom", legend.background = element_rect(fill="ghostwhite",
                                                                     size=0.5, linetype="blank"))
