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

# datasets
dados_morte <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
dados_casos <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

# Ajustes no dataset
  # mortes
covid_morte <- read.csv(text = dados_morte)

  # casos
covid_casos <- read.csv(text = dados_casos)

##### ajustando dataset para um formato de dataframe mais adequado

  # mortes
covid_morte <- covid_morte %>% gather(key=data, value = total_mortes, -Province.State, -Country.Region, -Lat, -Long,)
 
 # casos
covid_casos <- covid_casos %>% gather(key=data, value = total_casos, -Province.State, -Country.Region, -Lat, -Long,)

###### ajustando coluna data

  # mortes
covid_morte$data <- sub(".", "", covid_morte$data)

covid_morte$data <- mdy(covid_morte$data)
  
# casos
covid_casos$data <- sub(".", "", covid_casos$data)

covid_casos$data <- mdy(covid_casos$data)

####### criando dataset agrupado por paises

  # mortes
covid_morte <- covid_morte %>% 
  group_by(Country.Region, data) %>% 
  summarise(total_obitos= sum(total_mortes)) %>%
  filter(total_obitos!=0) %>% 
  group_by(Country.Region) 

 # casos
covid_casos <- covid_casos %>% 
  group_by(Country.Region, data) %>% 
  summarise(total_registros= sum(total_casos)) %>%
  filter(total_registros!=0) %>% 
  group_by(Country.Region)  


####### Unindo datasets

covid_agreg <- full_join(covid_casos,covid_morte, by=c("Country.Region","data"))

### coluna da txa de letalidade

covid_agreg <- covid_agreg %>% mutate(letalidade=(total_obitos/total_registros)*100)

####### filtrando paises

covid_paises <- covid_agreg %>% filter(Country.Region %in% c("Brazil",
                                                             "China",
                                                             "Italy",
                                                             "Spain",
                                                             "US"))

# removendo datasets não utilizados
rm(covid_casos,covid_morte,covid_agreg)

###### Deixando data automatica e ajustando para o formato brasileiro 
data_att <- format(as.Date(max(covid_paises$data)),'%d/%m/%Y')

######## Ajustando nome dos países para lingua portuguesa 

covid_paises$Country.Region <- recode(covid_paises$Country.Region, Brazil = "Brasil",
                                      Italy="Itália",
                                      US="EUA",
                                      Spain="Espanha")

######## Ordenando variáveis  

covid_paises$Country.Region <- factor(covid_paises$Country.Region, levels = c("Brasil",
                                                                              "China",
                                                                              "Espanha",
                                                                              "EUA", 
                                                                              "Itália"))



### gráfico
let_paises <- ggplot(covid_paises, aes(data, letalidade, group = Country.Region)) +
  geom_line(aes(color=Country.Region),size=2) +
  geom_point(size = 2.5) + 
  geom_text(aes(x = data, label = Country.Region), hjust = -0.5, size=4, fontface='bold') + 
  geom_text(aes(x = data, label =paste0(round(letalidade, digits = 2)," %")),
            hjust= -2.0, size=4, fontface='bold') + 
  transition_reveal(data) + 
  ease_aes(interval=3) +
  coord_cartesian(clip = 'off') + 
  scale_y_continuous(name = 'Letalidade', limits = c(0, 18), breaks = c(0,2,4,6,8,10,12,14,16,18),
                     labels = paste0(c("0","2%","4%","6%","8%","10%","12%","14%","16%","18%"))) +
  scale_x_date(date_breaks="7 day", limits = as.Date(c("2020-01-22","2020-07-01")), labels = date_format("%d/%m")) +
  theme_economist_white() +
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
  theme(axis.text.x = element_text(angle=90)) +
  labs(title = 'Taxa de Letalidade do Covid-19, por países\n(Brasil, China, EUA, Espanha e Itália)',
       x="Dia da epidemia", color="",
       caption = paste0("Fonte:Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)\nElaborado por Cleiton Rocha\nÚltima atualização: ",data_att)
  )

# FORMATO CERTO PARA TWITTER
animate(let_paises, height = 422, width = 750, fps = 40, duration = 20, type ="cairo") 




