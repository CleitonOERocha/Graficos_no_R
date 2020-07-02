

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

setwd("C:\\Users\\pc\\Desktop\\coronavirus")


# DATASETS
dados <- getURL("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv")
dupli_time <- read.csv2("tempo_duplicacao_v5.csv")

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
                                   select(date, state, totalCases) %>% 
                                      group_by(state) %>%
                                         mutate(dia=1:length(state))

#covid_estados_TOTALCASOS <- covid %>%
#  filter(state =="BA") %>%
#  filter(state != "TOTAL") %>%
#  select(date, state, totalCases) %>% 
#  group_by(state) %>%
# mutate(dia=1:length(state))


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

estados_covid <- ggplot(covid_estados_TOTALCASOS, aes(dia, log10(totalCases), group = state)) +
  geom_line(aes(color=state),size=2) +
  geom_point(size = 2.5) + 
  geom_text(aes(x = dia, label = state), hjust = -0.5, size=6, fontface='bold') + 
  geom_text(aes(x = dia, label =format(round(totalCases, digits = 0),big.mark = ".", decimal.mark = ",")),
            vjust = 2,hjust= -0.6, size=6, fontface='bold') + 
  transition_reveal(dia) + 
  ease_aes(interval=3) +
  coord_cartesian(clip = 'off') + 
  scale_y_continuous(name = 'Número de Casos', limits = c(0, 5.06), breaks = c(0,1,2,3,4,5, 5.06),
                     labels = paste0(c("0","10","100","1.000","10.000","100.000","115.000"))) +
  scale_x_continuous(limits=c(0,max(covid_estados_TOTALCASOS$dia)), 
                     breaks  = seq(0,max(covid_estados_TOTALCASOS$dia), by = 5)) +
  theme_economist() +
  theme(legend.position="bottom",
        axis.title.x = element_text(colour = "black", size = 12),
        axis.title.y = element_text(colour = "black", size = 12),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=12, hjust=1),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        plot.title = element_text(colour = "black", size = 16, hjust=0.5),
        plot.caption = element_text(colour = "black", size = 10),
        axis.text.x = element_text(face="bold", color="#000000", 
                                   size=12, vjust = 1))+
  labs(title = 'Evolução de casos do COVID-19, por estados do Nordeste',
       x="Dia da epidemia", color="",
       caption = paste0("Fonte: Ministério da Saúde.\nDados sistematizados por: covid19br.wcota.me\nElaborado por Cleiton Rocha, Jackson Conceição, Jonatas Silva e Rodrigo Cerqueira\nÚltima atualização: ",data_att)
  ) +  
  geom_line(data = dupli_time, aes(x=days, y=log10(doubles_every_day)),inherit.aes = FALSE,size=1) +
  geom_line(data = dupli_time, aes(x=days, y=log10(doubles_every_2_days)),inherit.aes = FALSE,size=1) +
  geom_line(data = dupli_time, aes(x=days, y=log10(doubles_every_3_days)),inherit.aes = FALSE,size=1) +
  geom_line(data = dupli_time, aes(x=days, y=log10(doubles_every_week)),inherit.aes = FALSE,size=1) +
  annotate("text", x=17,y=4.5,label="Casos dobram a cada um dia", colour="black") +
  annotate("text", x=29,y=4.4,label="Casos dobram a cada\ndois dias", colour="black") +
  annotate("text", x=45,y=4.5,label="Casos dobram a cada\n três dias", colour="black") +
  annotate("text", x=49,y=2.3,label="Casos dobram toda\n semana", colour="black") 

# aumentando qualidade da renderização 
animate(estados_covid, height = 1000, width = 1000, fps = 
          20, duration = 20, type ="cairo")
