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
raca_long <- read.csv2("raca_long.csv")

# filtrando ano e municipio 
raca_long_filtrado <- raca_long %>% 
  filter(Ano==2010) %>% filter(Municipios =="Salvador")


# grafico
ggplot(raca_long_filtrado, aes(y=PercentCor,x=Cor, fill=Cor)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=sprintf("%1.2f%%",PercentCor)),size = 3, position =position_dodge(width=0.9),
            vjust=-0.25, color = 'black',fontface='bold') +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=7),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold", color="#000000", size=12),
        legend.text = element_text(size=9, face="bold"),
        legend.title = element_text(size = 9, face = "bold"),
        plot.title = element_text(colour = "black", size = 12, hjust=0.5, face="bold")) +
  labs(y="Percentual", x="Municipio", title="Cor/Raca da população em Salvador, BA.") + 
  scale_fill_brewer(palette = "YlGnBu") +
  guides(fill=FALSE)
