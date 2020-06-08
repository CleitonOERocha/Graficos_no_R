
################################################################################################### 
# SEIDataLab - Laboratorio de Dados da Superintendencia de Estudos Economicos e Sociais da Bahia
################################################################################################### 
#####   DESCRIÇÃO:        mapa do dashboard Demografia do InfoVis
#####   DATA DA CRIAÇÃO:  23/03/2020
#####   ESCRITO POR:      Cleiton Rocha, Jackson Conceicao, Jonatas Silva
#####   LICENÇA:          GPLv3
################################################################################################### 

library(dplyr)
library(leaflet)
library(viridis)
library(rgdal)

#dataset
territorios_geo <- rgdal::readOGR("Terri_iden_ba_v2.json") # ok
territorios_pop <- read.csv2("territorios_pop_total.csv") # ok

### Ajustes no dataset ###

  # convertendo variavel de caracter em numerica 
  territorios_geo$CD_TI <- as.numeric(territorios_geo$CD_TI)

  # criando dataset com ano filtrado
  territorios_pop_v2 <- territorios_pop %>% filter(Ano==2010)
  
  # banco unindo mapa com os dados #
  territorios_pop_v3 <- merge(territorios_geo, territorios_pop_v2, by = "CD_TI")
  
  # paleta
  wardpal <- colorFactor(viridis(7), territorios_pop_v3$faixas_pop)
  
  # removendo datasets nao utilizados
  rm(territorios_pop_v2, territorios_pop, territorios_geo)
  
### Mapa ###
  
  leaflet(territorios_pop_v3) %>%
    addTiles() %>%
    addProviderTiles("CartoDB.PositronNoLabels") %>%
    setView(lat = -12.980911, lng = -41.942723, zoom = 6) %>% 
    addPolygons(stroke = T,opacity = 1, color = "black",weight = 0.5, smoothFactor = 0.0, fillOpacity = 1,
                fillColor = ~wardpal(faixas_pop),
                label = ~paste0(NM_TI, ": ", format(total_ter, big.mark = ".",decimal.mark=",")," casos registrados")) %>%
    addLegend("bottomright",pal = wardpal, values = ~faixas_pop, opacity = 1.0, title = "Tamanho da População")

  

    
