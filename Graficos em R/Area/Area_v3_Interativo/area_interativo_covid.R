


library(ggplot2)
library(ggiraph)
library(dplyr)
library(gdtools)

### dataset ###
covid_BA_serie <- read.csv2("serie_casos_BA.csv") 

### Ajustes no dataset ###

# criando colunas
covid_BA_serie_tx <- covid_BA_serie %>%
  mutate(date=as.Date(date,format("%d/%m/%Y")),
         casos=as.numeric(casos),
         casos_novos=as.numeric(casos_novos),
         tx_percent = ((casos-lag(casos,1))/lag(casos,1)) * 100,
         dia=c(as.factor(date))) 

# removendo dataset nao utilizado
rm(covid_BA_serie)

# calculando a media de crescimento de casos
media_cres_BA = mean(covid_BA_serie_tx$tx_percent, na.rm = TRUE)

### Grafico ###
   
   # grafico no ggplot
casos_dias<- ggplot(covid_BA_serie_tx, aes(x=as.POSIXct(date))) +
  geom_ribbon(aes(ymin=0, ymax=casos_novos), fill="#0779e4", alpha=0.2) +
  geom_line(aes(y=casos_novos), color = "#0779e4", size =2) +
  labs(x="Dias da epidemia", y="Numero de casos") +
  theme_classic()
 
  # grafico convertindo no ggigraph
casos_dias_ggiraph <- casos_dias +
  geom_point_interactive(aes(y=casos_novos, size=3, tooltip = casos_novos), color="#0779e4") +
  theme(legend.position = "none")

  # mostrando o grafico
ggiraph(code ={print(casos_dias_ggiraph)},width_svg=9)



