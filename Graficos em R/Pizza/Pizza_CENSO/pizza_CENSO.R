
library(ggplot2)
library(dplyr)
library(Cairo)
library(RColorBrewer)
library(ggthemes)

# dataset
fxaetaria_long <- read.csv2("faixa_etaria_tidyr.csv")

## Ajustes no Dataset ##

# filtrando apenas Salvador e criando a coluna porcentagem
pizza_graf_fxa <- fxaetaria_long %>% filter(Ano==2010) %>% filter(Municipios =="Salvador") %>%
  group_by(status) %>% summarise(cont_fxa=sum(total))

# criando coluna com a posicao da legenda
pizza_graf_fxa <- pizza_graf_fxa %>% arrange(desc(status)) %>%
  mutate(yposicao_legenda = cumsum(cont_fxa)- 0.5*cont_fxa)


# alterando o nome das variaveis 
pizza_graf_fxa$status <- factor(pizza_graf_fxa$status, levels=c("fxa_0a14",
                                                                "fxa_15a59",
                                                                "fxa_60mais"),
                                labels=c("0 a 14", "15 a 59", "+60"))


### Grafico ###
ggplot(pizza_graf_fxa,aes(x="", y=cont_fxa, fill=status)) +
  geom_bar(width=1, stat = "identity") +
  coord_polar("y", start=0) +
  geom_text(aes(y=yposicao_legenda, label = sprintf("%1.2f%%",cont_fxa)), size = 4,
            color = 'black',fontface='bold') +
  scale_fill_brewer(palette = "YlOrBr") +
  labs(x="",y="",caption = "Fonte: CENSO") +
  theme_minimal() +
  theme(legend.position="bottom", legend.title = element_text(colour="Black", size=10, face="bold"), legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank") )+
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=7),
        axis.text.x = element_text(face="bold",color="#000000", size=12),
        axis.text=element_text(size=6, face="bold"),
        legend.text = element_text(size=10, face="bold"),
        legend.title = element_text(size = 8, face = "bold"),
        plot.title = element_text(colour = "black", size = 12, hjust=0.5, face="bold")) +
  ggtitle(paste0("Percentual das Faixas Etárias em Salvador, BA.")) +
  labs(fill="Faixa etária (em anos)")
  

