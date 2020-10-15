

library(rgdal)
library(leaflet)
library(readxl)
library(purrr)

setwd("C:\\Users\\pc\\Desktop\\Observatorio Dashboard")

# datasets
ano_2018 <- read_excel("br-capes-colsucup-curso-2018-2019-10-01 - rev SDC.xlsx")
ano_2017 <- read_excel("br-capes-colsucup-curso-2017-2018-08-01 - rev SDC.xlsx")
ano_2016 <- read_excel("br-capes-colsucup-curso-2013a2016-2017-12-02_2016 - rev SDC.xlsx")
ano_2015 <- read_excel("br-capes-colsucup-curso-2013a2016-2017-12-02_2015 - rev SDC.xlsx")
ano_2014 <- read_excel("br-capes-colsucup-curso-2013a2016-2017-12-02_2014 - rev SDC.xlsx")
ano_2013 <- read_excel("br-capes-colsucup-curso-2013a2016-2017-12-02_2013 - rev SDC.xlsx")

# renomeando colunas para fazer o merge
ano_2013 <- ano_2013 %>% rename(`CÓDIGO DO MUNÍCIPIO` = "CÓDIGO MUNICIPIO", MUNÍCIPIO = "NM_MUNICIPIO_PROGRAMA_IES")
ano_2014 <- ano_2014 %>% rename(MUNÍCIPIO = "NM_MUNICIPIO_PROGRAMA_IES")
ano_2015 <- ano_2015 %>% rename(`CÓDIGO DO MUNÍCIPIO` = "CÓDIGO MUNÍCIPIO", MUNÍCIPIO = "NM_MUNICIPIO_PROGRAMA_IES")
ano_2016 <- ano_2016 %>% rename(`CÓDIGO DO MUNÍCIPIO` = "CÓDIGO MUNÍCIPIO", MUNÍCIPIO = "NM_MUNICIPIO_PROGRAMA_IES")
ano_2017 <- ano_2017 %>% rename(`CÓDIGO DO MUNÍCIPIO` = "CÓDIGO MUNÍCIPIO", MUNÍCIPIO = "NM_MUNICIPIO_PROGRAMA_IES")

# transformando coluna em numérica
ano_2018$CD_CONCEITO_CURSO <- as.numeric(ano_2018$CD_CONCEITO_CURSO)

# Merge
capes <- list(ano_2018,ano_2017,ano_2016,ano_2015,ano_2014,ano_2013) %>%
  reduce(full_join, by = c("AN_BASE",	"NM_GRANDE_AREA_CONHECIMENTO",	"NM_AREA_CONHECIMENTO",
                           "NM_SUBAREA_CONHECIMENTO",	"NM_ESPECIALIDADE",	"CD_AREA_AVALIACAO",
                           "NM_AREA_AVALIACAO",	"SG_ENTIDADE_ENSINO",	"NM_ENTIDADE_ENSINO",	"CNPJ",
                           "CS_STATUS_JURIDICO",	"DS_DEPENDENCIA_ADMINISTRATIVA",
                           "DS_ORGANIZACAO_ACADEMICA",	"NM_REGIAO",	"SG_UF_PROGRAMA",
                           "MUNÍCIPIO",	"CÓDIGO DO MUNÍCIPIO",	"CD_PROGRAMA_IES",
                           "NM_PROGRAMA_IES",	"CD_CURSO_PPG",	"NM_CURSO",	"NM_GRAU_CURSO",
                           "CD_CONCEITO_CURSO",	"AN_INICIO_PREVISTO",	"DS_SITUACAO_CURSO",
                           "DT_SITUACAO_CURSO",	"ID_ADD_FOTO_PROGRAMA_IES",	"ID_ADD_FOTO_PROGRAMA"))



#######################################################################################################
########################################## Municípios #################################################
#######################################################################################################

# datasets
mapa_ba_mun <- rgdal::readOGR(dsn=getwd(), layer="DPA_A_GEN_2019_05_14_GCS_SIR_SEI", encoding = "UTF-8") # mapa

# renomeando codigo do municipio, filtrando Bahia, agrupando por municipio e tipo de curso
capes_grau_cursos <- capes %>%
             filter(AN_BASE == 2017) %>% 
             filter(NM_GRANDE_AREA_CONHECIMENTO =="ENGENHARIAS") %>% 
             filter(SG_UF_PROGRAMA == "BA") %>%
               rename(Codigo="CÓDIGO DO MUNÍCIPIO") %>%
                 mutate(one=1) %>%
                   group_by(MUNÍCIPIO, Codigo) %>%
                     summarise(total_cursos = sum(one))


# transformando factor em num na shapefile
mapa_ba_mun@data[["CD_GEOCMU"]] <- as.numeric(as.character((mapa_ba_mun@data[["CD_GEOCMU"]])))

# banco unindo mapa com os dados 
capes_ba_cursos <- merge(mapa_ba_mun, capes_grau_cursos, by = "Codigo",duplicateGeoms = TRUE)

# convertendo NA para zero na shape
capes_ba_cursos@data$total_cursos[is.na(capes_ba_cursos@data$total_cursos)] <- 0


# criando variável categórica
capes_ba_cursos$fxa_grau_curso <- cut(capes_ba_cursos$total_cursos, c(0,0.99,5,10,20,100000), 
                                        labels=c("Não existe",
                                                 "1 - 5",
                                                 "6 - 10",
                                                 "11 - 20",
                                                 "Mais de 20"),
                                        rigth=T,exclude=NULL, include.lowest=TRUE)

# paleta
wardpal_cursos_mun <- colorFactor(c("#fee391","#fe9929","#ec7014","#8c2d04","#461602"),
                               domain=factor(capes_ba_cursos$fxa_grau_curso, 
                                             levels=c("Não existe",
                                                      "1 - 5",
                                                      "6 - 10",
                                                      "11 - 20",
                                                      "Mais de 20")))

# mapa
leaflet(capes_ba_cursos, options = leafletOptions(zoomControl = FALSE,
                                                 minZoom = 5.5, maxZoom = 7.5, 
                                                 dragging = TRUE,
                                                 doubleClickZoom=FALSE)) %>%
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  setView(lat = -13.800000, lng = -41.559343, zoom = 5.5) %>% 
  addPolygons(stroke = T, opacity = 1, color = "grey",weight = 0.5, smoothFactor = 0.0, fillOpacity = 1,
              fillColor = ~wardpal_cursos_mun(fxa_grau_curso),
              label = ~paste0(MUNICIPIO, ": ", format(total_cursos, big.mark = ".",decimal.mark=",")," Cursos de Pós-Graduação")) %>%
  addLegend("bottomleft",pal = wardpal_cursos_mun, values = ~fxa_grau_curso, opacity = 1.0, title = "Quantidade de cursos de Pós-Graduação") 


#######################################################################################################
################################## Territórios de Identidade ##########################################
#######################################################################################################

# datasets
territorios_geo <- rgdal::readOGR("Terri_iden_ba_v2.json", use_iconv = T,encoding = "UTF-8") # mapa
terr_iden <- read.csv2("C:\\Users\\pc\\Desktop\\BI Demografia\\DemograVis-master\\Estimativa_IBGE.csv")

# selecionando colunas de interesse e renomeando coluna do merge
terr_iden <- terr_iden %>% select(NM_MUNICIP, NM_TI, CD_GEOCMU)%>% rename(`CÓDIGO DO MUNÍCIPIO` = "CD_GEOCMU")

# fazendo merge
capes <- left_join(capes,terr_iden, by="CÓDIGO DO MUNÍCIPIO")

# renomeando filtrando Bahia, agrupando por território e tipo de curso
capes_grau_cursos_TI <- capes %>%
                          filter(AN_BASE == 2017) %>% 
                          filter(NM_GRANDE_AREA_CONHECIMENTO =="ENGENHARIAS") %>% 
                          filter(SG_UF_PROGRAMA == "BA") %>%
                            mutate(one=1) %>%
                              group_by(NM_TI) %>%
                                summarise(total_cursos = sum(one))



# banco unindo mapa com os dados 
map_ba_cursos_TI <- merge(territorios_geo, capes_grau_cursos_TI, by = "NM_TI",duplicateGeoms = TRUE)

# convertendo NA para zero na shape
map_ba_cursos_TI@data$total_cursos[is.na(map_ba_cursos_TI@data$total_cursos)] <- 0

# criando variável categórica
map_ba_cursos_TI$fxa_grau_curso <- cut(map_ba_cursos_TI$total_cursos, c(0,0.99,5,10,20,100000), 
                                      labels=c("Não existe",
                                               "1 - 5",
                                               "6 - 10",
                                               "11 - 20",
                                               "Mais de 20"),
                                      rigth=T,exclude=NULL, include.lowest=TRUE)

# paleta
wardpal_cursos_TI <- colorFactor(c("#fee391","#fe9929","#ec7014","#8c2d04","#461602"),
                                  domain=factor(map_ba_cursos_TI$fxa_grau_curso, 
                                                levels=c("Não existe",
                                                         "1 - 5",
                                                         "6 - 10",
                                                         "11 - 20",
                                                         "Mais de 20")))

# mapa
leaflet(map_ba_cursos_TI, options = leafletOptions(zoomControl = FALSE,
                                                  minZoom = 5.5, maxZoom = 7.5, 
                                                  dragging = TRUE,
                                                  doubleClickZoom=FALSE)) %>%
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  setView(lat = -13.800000, lng = -41.559343, zoom = 5.5) %>% 
  addPolygons(stroke = T, opacity = 1, color = "grey",weight = 0.5, smoothFactor = 0.0, fillOpacity = 1,
              fillColor = ~wardpal_cursos_TI(fxa_grau_curso),
              label = ~paste0(NM_TI, ": ", format(total_cursos, big.mark = ".",decimal.mark=",")," Cursos de Pós-Graduação")) %>%
  addLegend("bottomleft",pal = wardpal_cursos_TI, values = ~fxa_grau_curso, opacity = 1.0, title = "Quantidade de cursos de Pós-Graduação") 








