
library(leaflet)
library(rgdal)
library(htmlwidgets)
library(RColorBrewer)


### dataset ###
covid_BA <- read.csv("cases-brazil-cities-BA.csv", sep=";", fileEncoding = "ISO-8859-1", dec=",") %>% 
  rename(CD_GEOCMU=cd_geocmu)

covid_BA$city <- str_replace(covid_BA$city,'Outra UF', 'residentes de outras UF')

### mapa BAHIA ###
mapa_ba <- rgdal::readOGR("29MUE250GC_SIR_REDUZ.json", use_iconv = T, encoding = "UTF-8")


### Ajustes no dataset e JSON ###

# transformando factor em num no json
mapa_ba@data[["CD_GEOCMU"]] <- as.numeric(as.character((mapa_ba@data[["CD_GEOCMU"]])))

# merge
covid_map <- merge(mapa_ba, covid_BA, by="CD_GEOCMU")

# criando variável categórica
covid_map$fxa_covid <- cut(covid_map$totalCases, c(0,0.1,2,5,10,50,1000), 
                           labels=c("Sem Casos",
                                    "1 a 2",
                                    "2 a 5",
                                    "5 a 10",
                                    "10 a 50",
                                    "mais de 50"),
                           rigth=T,exclude=NULL, include.lowest=TRUE)

# paleta
wardpal_mun <- colorFactor(c("#ffffd4","#fee391", "#fec44f","#fe9929","#ec7014","#8c2d04"),
                           domain=factor(covid_map$fxa_covid, 
                                         levels=c("Sem Casos",
                                                  "1 a 2",
                                                  "2 a 5",
                                                  "5 a 10",
                                                  "10 a 50",
                                                  "mais de 50")))

### Mapa ###
leaflet(covid_map) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  setView(lat = -13.166149, lng = -41.559343, zoom = 5.5) %>% 
  addPolygons(stroke = T,opacity = 1, color = "black",weight = 0.5, smoothFactor = 0.0, fillOpacity = 1,
              fillColor = ~wardpal_mun(fxa_covid),
              label = ~paste0(NM_MUNICIP, ": ",format(totalCases, big.mark = ".",decimal.mark=",") ,
                              " casos registrados")) %>%
  addLegend("bottomright", opacity = 1,
            title = "Casos de Covid-19", pal=wardpal_mun, values = ~fxa_covid)



