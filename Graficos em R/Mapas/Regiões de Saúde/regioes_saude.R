
################################################################################################### 
# SEIDataLab - Laboratorio de Dados da Superintendencia de Estudos Economicos e Sociais da Bahia
################################################################################################### 
#####   DESCRIÇÃO:        mapa do coronovirus do InfoVis
#####   DATA DA CRIAÇÃO:  23/03/2020
#####   ESCRITO POR:      Cleiton Rocha, Jackson Conceicao, Jonatas Silva, Kellyene Coelho, Rodrigo Cerqueira
#####   SITE:             https://infovis.sei.ba.gov.br/covid19
#####   LICENÇA:          GPLv3
#####   PROJETO:          https://github.com/SEIDataLab/InfoVisCovid19
################################################################################################### 

library(dplyr)
library(leaflet)
library(stringr)
library(rgdal)
library(sp)

setwd("C:\\Users\\cleitonrocha\\Desktop\\Regiões de Saúde")

#####################################################################################################
##################################### dataset Bahia #################################################
#####################################################################################################

covid_BA <- read.csv("covid_BA_Municipios.csv", sep=";", fileEncoding = "ISO-8859-1", dec=",") %>% 
  dplyr::mutate(Codigo = CD_GEOCMU)


covid_BA$city <- str_replace(covid_BA$city,'BARRO PRETO', 'Governador Lomanto Júnior/Barro Preto')
covid_BA$city <- str_replace(covid_BA$city,'Outra UF', 'residentes de outras UF')

#################################################################################################
######################################### mapa BAHIA ############################################
#################################################################################################

mapa_regiaoSAUDE <- rgdal::readOGR("Shapefiles-regioesdesaude.json", use_iconv = T, encoding = "UTF-8")

# transformando factor em num no json
mapa_regiaoSAUDE@data[["Codigo"]] <- as.numeric(as.character((mapa_regiaoSAUDE@data[["Codigo"]])))

# merge
covid_map_SAUDE <- merge(mapa_regiaoSAUDE, covid_BA, by="Codigo")
#################################################################################################
#################################################################################################

#####################################################################################################
############################################## Leitos ###############################################
#####################################################################################################

LeitosReferencia <- read.csv2("Und_Referencia_Covid_BA.csv",fileEncoding = "ISO-8859-1", dec = ",") %>%
  dplyr::rename(NM_RegSau=NM_MUNICIP)

# deixando apenas a primeira letra de cada palavra em maiúscula
LeitosReferencia$NM_RegSau <- str_to_title(LeitosReferencia$NM_RegSau)

LeitosReferencia$NM_RegSau = gsub(' De ', ' de ', LeitosReferencia$NM_RegSau)
LeitosReferencia$NM_RegSau = gsub(' Do', ' do', LeitosReferencia$NM_RegSau)
LeitosReferencia$NM_RegSau = gsub(' Da', ' da', LeitosReferencia$NM_RegSau)

BDLeitoRefe_cli <- LeitosReferencia %>% 
  group_by(NM_RegSau) %>% 
  summarise(ClinicoRefe=sum(LeitosClinicos_DISPONIVEIS+LeitosPed_DISPONIVEIS))

BDLeitoRefe_UTI <- LeitosReferencia %>% 
  group_by(NM_RegSau) %>% 
  summarise(UTIRefe=sum(Leitos_Adulto_UTI_DISPONIVEIS+Leitos_Ped_UTI_DISPONIVEIS))

BDLeitoRefe <- merge(BDLeitoRefe_cli, BDLeitoRefe_UTI, by="NM_RegSau")

BDLeitoRefe$Leitos_Refe <- BDLeitoRefe$ClinicoRefe + BDLeitoRefe$UTIRefe

rm(BDLeitoRefe_cli) ; rm(BDLeitoRefe_UTI)

# remover hospitais sem leitos
LeitosReferencia$hosp_zeroleitos <- LeitosReferencia$LeitosClinicos_DISPONIVEIS + 
  LeitosReferencia$LeitosPed_DISPONIVEIS +
  LeitosReferencia$Leitos_Adulto_UTI_DISPONIVEIS +
  LeitosReferencia$Leitos_Ped_UTI_DISPONIVEIS

LeitosReferencia <- LeitosReferencia[LeitosReferencia$hosp_zeroleitos!=0, ]


# merge
covid_map_SAUDE <- sp::merge(covid_map_SAUDE, LeitosReferencia, by="NM_RegSau", duplicateGeoms = TRUE)
#######################################################################################
#######################################################################################

#merge
covid_map_SAUDE <- merge(covid_map_SAUDE, BDLeitoRefe, by="NM_RegSau", all.x=TRUE)

covid_map_SAUDE$ClinicoRefe[is.na(covid_map_SAUDE$ClinicoRefe)] <- 0
covid_map_SAUDE$UTIRefe[is.na(covid_map_SAUDE$UTIRefe)] <- 0
covid_map_SAUDE$Leitos_Refe[is.na(covid_map_SAUDE$Leitos_Refe)] <- 0

# criando variável categórica
covid_map_SAUDE$fxa_leitosCovid <- cut(covid_map_SAUDE$Leitos_Refe, c(0,0.1,9,19,29,49,99,10000),
                                 labels=c("Nenhum",
                                          "1 a 10",
                                          "10 a 20",
                                          "20 a 30",
                                          "30 a 50",
                                          "50 a 100",
                                          "mais de 100"),
                                 rigth=T,exclude=NULL, include.lowest=TRUE)

#Paleta para mapa leitos
wardpal_leito <- colorFactor(c("#ffffff","#d9f0a3","#addd8e","#78c679","#41ab5d","#238443","#005a32"),
                             domain=factor(covid_map_SAUDE$fxa_leitosCovid , 
                                           levels=c("Nenhum",
                                                    "1 a 10",
                                                    "10 a 20",
                                                    "20 a 30",
                                                    "30 a 50",
                                                    "50 a 100",
                                                    "mais de 100")))


# mapa
leaflet(covid_map_SAUDE) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  setView(lat = -13.800000, lng = -41.559343, zoom = 5.5) %>% 
  addPolygons(stroke = T,opacity = 1, color = "black",weight = 0.5, smoothFactor = 0.0, fillOpacity = 1,
              fillColor = ~wardpal_leito(fxa_leitosCovid),
              label = ~paste0(NM_RegSau, ": \n",
                              format(Leitos_Refe, big.mark = ".",decimal.mark=",") ," Leitos (",
                              format(ClinicoRefe, big.mark = ".",decimal.mark=",") ," Clínicos \\ ",
                              format(UTIRefe, big.mark = ".",decimal.mark=",") ," UTI)"
              )) %>%
  addMarkers(lng = ~Long, lat = ~Lat,
             popup = ~paste0("<b>",covid_map_SAUDE$NOME_HOSPITAL,'</b>',"<br>",
                             covid_map_SAUDE$LeitosClinicos_DISPONIVEIS," Leitos Clínicos", "<br>",
                             covid_map_SAUDE$LeitosPed_DISPONIVEIS," Leitos Pediátricos", "<br>",
                             covid_map_SAUDE$Leitos_Adulto_UTI_DISPONIVEIS, " Leitos de UTI Adulto", "<br>",
                             covid_map_SAUDE$Leitos_Ped_UTI_DISPONIVEIS, " Leitos de UTI Pediátrico")) %>%
  addLegend("bottomleft", opacity = 1,
            title = "Leitos exclusivos Covid-19", pal=wardpal_leito , values = ~fxa_leitosCovid)
