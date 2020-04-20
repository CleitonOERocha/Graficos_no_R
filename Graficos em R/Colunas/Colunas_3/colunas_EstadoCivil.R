

library(ggplot2)
library(Cairo)
library(ggthemes)
library(RColorBrewer)
library(dplyr)


# dataset
estado_civil_agreg <- read.csv2("estado_civil_agreg.csv") # ok 

### Ajustando dataset ###

# ordenando eixo X
legenda_estadocivil <- c("Casado", "Divorciado", "Separado\n Judicialmente", "Solteiro", "Viuvo")


# filtrando ano e municipio
  estado_civil_v2 <- estado_civil_agreg %>% filter(Ano==2010) %>%
    filter(Municipios =="Salvador") %>%
    group_by(situacao_conjugal) %>% summarise(cont_civil=sum(total_civil))
  
# calculando a porcentagem
  estado_civil_v2 = mutate(estado_civil_v2,civil_percent = cont_civil/sum(cont_civil)*100 )
 
  
### grafico ###   
  ggplot(estado_civil_v2, aes(y=civil_percent,x=situacao_conjugal, fill=situacao_conjugal)) + 
    geom_bar(stat="identity") +
    geom_text(aes(label=sprintf("%1.2f%%",civil_percent)),size = 4, position =position_dodge(width=0.9),
              vjust=-0.25, color = 'black',fontface='bold') +
    theme(axis.title.x = element_text(colour = "black"),
          axis.title.y = element_text(colour = "black"),
          axis.text.y = element_text(face="bold", color="#000000", 
                                     size=7),
          axis.line = element_line(colour = "black", 
                                   size = 1, linetype = "solid"),
          axis.text=element_text(size=6, face="bold"),
          axis.text.x = element_text(face="bold",color="#000000", size=10),
          plot.title = element_text(colour = "black", size = 12, hjust=0.5, face="bold")) +
    labs(y="Percentual", x="Estado Civil", title="Estado Civil da população em Salvador, BA.") +
    scale_fill_brewer(palette = "Reds") +
    scale_x_discrete(labels = legenda_estadocivil) +
    guides(fill=FALSE)
  
  
  