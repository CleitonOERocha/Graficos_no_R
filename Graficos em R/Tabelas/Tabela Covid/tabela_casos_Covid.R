
library(dplyr)
library(DT)
library(htmlwidgets)
library(stringr)

setwd("C:\\Users\\pc\\Desktop\\Corona\\covid19_1804\\covid19_teste")

### dataset ###
covid_BA <- read.csv("cases-brazil-cities-BA.csv", sep=";", fileEncoding = "ISO-8859-1", dec=",") %>% 
  rename(CD_GEOCMU=cd_geocmu)


### Ajuste no dataset ###

# Casos sem registro em cidades  
covid_BA$city <- str_replace(covid_BA$city,'Outra UF', 'residentes de outras UF')

# Filtrando somente os municipios com casos confirmados
covid_BA_cases <- covid_BA %>% 
  filter(totalCases != 0) %>% 
  mutate(Letalidade=round((death/totalCases*100),2)) %>% 
  mutate(Prevalencia_casos=round((totalCases/populacao_mun*100000),2)) %>%
  dplyr::select(city,totalCases, Prevalencia_casos, death, Letalidade)

# Ajustando formato decimal e acrescentado simbolo "%" na coluna letalidade
covid_BA_cases$Letalidade <- paste0(format(round(covid_BA_cases$Letalidade,2),
                                           nsmall=2,big.mark=".", decimal.mark=","),"%")

# ordenando por total de casos
covid_BA_cases <- covid_BA_cases[with(covid_BA_cases, order(-totalCases)), ]


# Tabela
datatable(head(covid_BA_cases,200),
          escape = FALSE,
          filter = "none",
          rownames = F ,
          colnames = c("Cidade", "Infectados", "Prevalência*", "Mortes", "Letalidade"),
          options = list(searching = FALSE,
                         paging = FALSE,
                         lengthChange = FALSE, 
                         pageLength = 10, autoWidth = T,
                         scrollX = TRUE,
                         columnDefs = list(list(className = 'dt-center', D = "_all")),
                         language = list(info ="",
                                         paginate = 
                                           list('next'="Próxima", 
                                                previous="Voltar")))) %>%
  formatStyle('city',  color = 'black', fontWeight = 'bold') %>%
  formatStyle('totalCases',  color = 'black', fontWeight = 'bold') %>%
  formatStyle('Prevalencia_casos',  color = 'black', fontWeight = 'bold') %>%
  formatStyle('death',  color = styleInterval(c(1, 1), c('black','red',"red")),
              fontWeight = 'bold') %>%
  formatStyle('Letalidade',  color = 'black', fontWeight = 'bold')
