
library(RCurl)
library(dplyr)
library(ggplot2)
library(gganimate)
library(gifski)
library(png)
library(lubridate)
library(tidyr)
library(ggthemes)
library(magrittr)
library(Cairo)
library(scales)

setwd("C:\\Users\\pc\\Desktop\\coronavirus")

# DATASET
dados <- getURL("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv")

# Ajustes no dataset
covid <- read.csv(text = dados)

covid$date <- ymd(covid$date)

# filtrando casos e Estados
covid_estados_TOTALCASOS <- covid %>%
  filter(state %in% c("AL",
                      "BA",
                      "CE",	
                      "MA", 
                      "PB", 
                      "PE",
                      "PI","RN","SE")) %>%
  filter(state != "TOTAL") %>%
  select(date, state, deaths_per_100k_inhabitants) %>% 
   group_by(state)


######## Ordenando variáveis  

covid_estados_TOTALCASOS$state <- factor(covid_estados_TOTALCASOS$state, levels = c("AL",
                                                                                    "BA",
                                                                                    "CE",	
                                                                                    "MA", 
                                                                                    "PB", 
                                                                                    "PE",
                                                                                    "PI","RN","SE"))

###### Deixando data automatica e ajustando para o formato brasileiro 

data_att <- format(as.Date(max(covid_estados_TOTALCASOS$date)),'%d/%m/%Y')

### Grafico 

estados_covid <- ggplot(covid_estados_TOTALCASOS, aes(x = date,
                                                      y = deaths_per_100k_inhabitants,
                                                      colour=state)) +
  geom_line(size=2) +
  geom_point(size = 2.5) + 
  geom_text(aes(x = date, label = state), hjust = -0.5, size=4, fontface='bold') + 
  geom_text(aes(x = date, label =format(round(deaths_per_100k_inhabitants, digits = 0),big.mark = ".", decimal.mark = ",")),
            vjust = 2,hjust= -0.6, size=4, fontface='bold') + 
  transition_reveal(date) + 
  ease_aes(interval=3) +
  coord_cartesian(clip = 'off') + 
  scale_y_continuous(name = 'Mortes/100.000 habitantes', limits = c(0, 111), breaks = c(0,10,20,30,40,50,60,70,80,90,100,110),
                     labels = paste0(c("0","10","20","30","40","50","60","70","80","90","100","110"))) +
  scale_x_date(date_breaks="1 month", limits = as.Date(c("2020-03-06","2021-01-07")), labels = date_format("%m/%y")) +
  theme_minimal() +
  theme(legend.position="bottom",
        axis.title.x = element_text(colour = "black", size = 12),
        axis.title.y = element_text(colour = "black", size = 12),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=12, hjust=1),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        plot.title = element_text(colour = "black", size = 16),
        plot.caption = element_text(colour = "black", size = 10),
        axis.text.x = element_text(face="bold", color="#000000", 
                                   size=12, vjust = 1))+
  labs(title = 'Mortes relacionadas ao COVID-19, por 100.000 habitantes. Nordeste',
       x="Dia da epidemia", color="",
       caption = paste0("Fonte: Ministério da Saúde.\nDados sistematizados por: covid19br.wcota.me\nElaborado por Cleiton Rocha (cleitonotavio058@gmail.com)\nÚltima atualização: ",data_att)
  )  
 
# aumentando qualidade da renderização 
animate(estados_covid, height = 422, width = 750, fps = 40, duration = 20, type ="cairo") 



