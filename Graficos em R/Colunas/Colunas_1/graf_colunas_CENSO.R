
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(Cairo)


#### Grafico de Colunas com dados do Censo na Bahia ####

# dataset
populacao_censo <- read.csv2("populacao_censo.csv", dec=",",h=T) 

# grafico
ggplot(populacao_censo) + 
  geom_col(aes(x=factor(Ano),y=Populacao), show.legend = FALSE, fill="brown4",width = 0.7) +
  geom_text(aes(x=factor(Ano),y=Populacao,label=Populacao), position=position_dodge(width=0.9),
            vjust=-0.25, size=3, fontface='bold') +
  labs(x = "Anos", y = "Populacao") +
  labs(title = "População Baiana de acordo com o CENSO Demográfico (1872 - 2010)",
       caption = "Fonte: IBGE - Tabela 1.4 - População nos Censos Demográficos,\n segundo as Grandes Regiões e as Unidades da Federação - 1872/2010") +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=7),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        plot.title = element_text(colour = "black", size = 12, hjust=0.5),
        axis.text.x = element_text(face="bold", color="#000000", 
                                   size=9)) 
