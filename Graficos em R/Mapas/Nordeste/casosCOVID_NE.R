
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(RColorBrewer)


### dataset ###

# Carregando dataset do Wesley Cota
dados_pais <- read.csv2("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-total.csv", sep=",")

dados_pais <- dados_pais[dados_pais$state!="TOTAL", ]

#carregando a shape
mapa_NE <- rgdal::readOGR("Nordeste_SIR.json", use_iconv = T, encoding = "UTF-8")

### Ajustes no dataset ###

# criando variável categórica
dados_pais$categorica <- cut(dados_pais$totalCases, c(0,10,30,50,100,200,500,1000,100000), 
                             labels=c(  "1 a 10",
                                        "10 a 30",
                                        "30 a 50",
                                        "50 a 100",
                                        "100 a 200",
                                        "200 a 500",
                                        "500 a 1000",
                                        "mais de 1000"),
                             rigth=T)


#paleta 
wardpal2 <- colorFactor(brewer.pal(8, "YlOrBr"),domain=factor(dados_pais$categorica, levels=c("1 a 10",
                                                                                              "10 a 30",
                                                                                              "30 a 50",
                                                                                              "50 a 100",
                                                                                              "100 a 200",
                                                                                              "200 a 500",
                                                                                              "500 a 1000",
                                                                                              "mais de 1000")))




# criando coluna com nome dos estados
dados_pais$NM_ESTADO <- recode(dados_pais$state, AC="ACRE",	AL="ALAGOAS", AP="AMAPÁ", AM="AMAZONAS",
                               BA="BAHIA", CE="CEARÁ",DF="DISTRITO FEDERAL",ES="ESPÍRITO SANTO",
                               GO="GOIÁS", MA="MARANHÃO", PA="PARÁ", PE="PERNAMBUCO", PI="PIAUÍ",
                               RO="RONDÔNIA", RR="RORAIMA", SC="SANTA CATARINA", SP="SÃO PAULO",
                               RN="RIO GRANDE DO NORTE", PR="PARANÁ", RS="RIO GRANDE DO SUL", MT="MATO GROSSO",
                               MS="MATO GROSSO DO SUL", MG="MINAS GERAIS", RJ="RIO DE JANEIRO", SE="SERGIPE",
                               TO="TOCANTINS", PB="PARAÍBA")


# criando lista com nome dos Estados do Nordeste
SiglasUFNordeste <- c("AL","BA","CE",
                      "MA","PB","PE",
                      "PI" ,"RN","SE")

# criando dataset com dados do Nordeste
dados_Nordeste <- dados_pais %>% filter(state %in% SiglasUFNordeste)

# juntando dataset e shape
covid_NE <- merge(mapa_NE, dados_Nordeste, by="NM_ESTADO")

### Mapa ###
leaflet(covid_NE) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  setView(lat = -9.223617, lng = -40.429979, zoom = 5) %>% 
  addPolygons(stroke = T,opacity = 1, color = "black",weight = 0.8, smoothFactor = 0.0, fillOpacity = 1,
              fillColor = ~wardpal2(categorica),
              label = ~paste0(NM_ESTADO, ": ",
                              format(totalCases, big.mark = ".",decimal.mark=","),
                              " casos registrados")) %>%
  addLegend("bottomright",pal = wardpal2, values = ~categorica, opacity = 1.0, title = "Casos de Covid-19")




