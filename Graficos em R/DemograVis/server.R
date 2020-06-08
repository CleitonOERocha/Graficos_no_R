################################################################################################### 
# SEIDataLab - Laboratorio de Dados da Superintendencia de Estudos Economicos e Sociais da Bahia
################################################################################################### 
#####   DESCRIÇÃO:        dashboard Demografia do InfoVis
#####   DATA DA CRIAÇÃO:  23/03/2020
#####   ESCRITO POR:      Cleiton Rocha, Jackson Conceicao, Jonatas Silva
#####   LICENÇA:          GPLv3
################################################################################################### 


library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(shinydashboardPlus)
library(sp)
library(ggforce)
library(tidyr)
library(httpuv)
library(rgdal)
library(viridis)
library(tibble)

# Carregando os dataset necessarios

proj18 <- read.csv2("proj18.csv",fileEncoding = "ISO-8859-1") 
pnadCPop <- read.csv2("pnadcPopulacao.csv",fileEncoding = "ISO-8859-1") 
popfaixaquinquenal <- read.csv2("Pop_Faixa_Quiquena_BA_Proj2018.csv", dec=",", header=TRUE,fileEncoding = "ISO-8859-1") 
pnadCsexo <- read.csv2("pnadCsexo.csv", h=T, dec=",",fileEncoding = "ISO-8859-1") 
piramide_ba_pnad <- read.csv2("PNAD_piramide_etaria_BA.csv", dec=",", h=T,fileEncoding = "ISO-8859-1") 
populacao_censo <- read.csv2("populacao_censo.csv", dec=",",h=T,fileEncoding = "ISO-8859-1")
raca <- read.csv2("MUN_cor_raca_v2.csv",fileEncoding = "ISO-8859-1")
estado_civil_agreg <- read.csv2("estado_civil_agreg.csv",fileEncoding = "ISO-8859-1") 
naturalide_long <- read.csv2("naturalide_municipio.csv",fileEncoding = "ISO-8859-1") 
genero_mun <- read.csv2("genero_municipios.csv",fileEncoding = "ISO-8859-1") 
mortalidade <- read.csv2("natalidade_infantil.csv",fileEncoding = "ISO-8859-1") 
raca_long <- read.csv2("raca_long.csv",fileEncoding = "ISO-8859-1") 
fxaetaria_long <- read.csv2("faixa_etaria_tidyr.csv",fileEncoding = "ISO-8859-1") 
piramide_municipios <- read.csv2("piramide_etaria_municipal.csv",fileEncoding = "ISO-8859-1") 
urbanizacao_municipios <- read.csv2("urbanizacao_municipios.csv",fileEncoding = "ISO-8859-1")
info_mun <- read.csv2("info_pop_municipal.csv",fileEncoding = "ISO-8859-1") 
territorios_geo <- rgdal::readOGR("Terri_iden_ba_v2.json") 
territorios_pop <- read.csv2("territorios_pop_total.csv",fileEncoding = "ISO-8859-1") 
pop_total_mun <- read.csv2("pop_total_mun.csv",fileEncoding = "ISO-8859-1")
mapa_ba_mun <- rgdal::readOGR("29MUE250GC_SIR_REDUZ.json", use_iconv = T, encoding = "UTF-8")
proj_2020_gen <- read.csv2("proj_2020_gen.csv",fileEncoding = "ISO-8859-1")



#server
function(input, output, session) {
  
  ######################################################################################
  #
  # PAGINA INFORMACOES GERAIS
  #
  ######################################################################################
  
  ############# value Box - Caixa com Populacao total da Bahia - ABA 1   ############# 
  
  output$poptotalgeral <- renderValueBox({
    valueBox(
      subset(x=proj18,subset=(Ano==2020),select = c(PopTotal)),"População Total no Estado", icon = icon("users"),
      color = "blue"
    )
  })
  #############  Caixa com a posicao da Bahia no ranking do Brasil - ABA 1   ############# 
  
  output$pos_Bahia_Brasil <- renderValueBox({
    valueBox(
      paste(subset(x=pnadCPop,subset=(CodUF==29),select = c(ranking_geral)),"º do Brasil"),"em termos populacionais", icon = icon("user-friends"),
      color = "green"
    )
  })
  
  #############  Caixa com a posicao da Bahia no ranking do Nordeste - ABA 1   ############# 
  
  output$pos_Bahia_NE <- renderValueBox({
    valueBox(
      paste(subset(x=pnadCPop,subset=(CodUF==29),select = c(ranking_nordeste)),"º do NE"),"em termos populacionais", icon = icon("user-circle"),
      color = "yellow"
    )
  })
  
  #############  Caixa com a taxa de fecundidade em 2019 - ABA 1   ############# 
  
  output$tftgeral <- renderValueBox({
    valueBox(
      paste(format(subset(x=proj18,subset=(Ano==2020),select = c(TFT)), nsmall=0,  big.mark=".", decimal.mark=","),"filhos"),"por mulher em idade fertil", icon = icon("child"),
      color = "red"
    )
  })
  
######################### Grafico de Barras - Populacao ao longo dos anos - ABA 1 ####################### 
  
  # Colocando separador de milhar no Grafico de Barras do CENSO - ABA 1
  
  output$censo_barras <- renderPlot({
    ggplot(populacao_censo) + 
      geom_col(aes(x=factor(Ano),y=Populacao), show.legend = FALSE, fill="brown4",width = 0.7) +
      geom_text(aes(x=factor(Ano),y=Populacao,label=Populacao), position=position_dodge(width=0.9),
                vjust=-0.25, size=3, fontface='bold') +
      labs(x = "Ano", y = "População") +
      labs(title = "População Baiana de acordo com o CENSO Demográfico (1872 - 2010)") +
      theme_classic() +
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=10),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            plot.title = element_text(colour = "black", size = 12, hjust=0.5,face="bold"),
            axis.text.x = element_text(face="bold", color="#000000", 
                                       size=10)) 
  })
  


  ################# Grafico de Pizza - Sexo - ABA 1 #######################
  
  output$genero_bahia <- renderPlot({
    
    # filtrando apenas 2020
    proj_2020 <- proj_2020_gen %>% filter(Anos == 2020)
    
    #PERCENTUAL
    proj_2020 = mutate(proj_2020, sex_percent2020 = total_gen/sum(total_gen)*100 )
    
    # posicao da legenda
    proj_2020 <- proj_2020 %>% arrange(desc(total_gen)) %>%
      mutate(yposicao_legenda = cumsum(sex_percent2020)- 0.5*sex_percent2020 )
    
    # ordenando o eixo X
    proj_2020$genero <- factor(proj_2020$genero, levels = c("Masculino",	"Feminino"))
    
    # grafico 
    ggplot(proj_2020,aes(x="", y=sex_percent2020, fill=genero)) +
      geom_bar(width=1, stat = "identity") +
      coord_polar("y", start=0) +
      geom_text(aes(y=yposicao_legenda, label = sprintf("%1.2f%%",sex_percent2020)), size = 4,
                color = 'black',fontface='bold') +
      labs(x="",y="", fill="Sexo: ") +
      theme_minimal() +
      theme(legend.position="bottom", legend.title = element_text(colour="Black", size=10, face="bold"), legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank") )+
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=7),
            axis.text.x = element_text(face="bold",color="#000000", size=12),
            axis.text=element_text(size=6, face="bold"),
            legend.text = element_text(size=10, face="bold"),
            legend.title = element_text(size = 9, face = "bold"),
            plot.title = element_text(colour = "black", size = 12, hjust=0.5, face="bold")) +
      scale_fill_manual(values = c("#1b6db5","#b51b8f")) +
      labs(title = "Sexo da população Baiana, 2020.")
    
    
 
  })
  
  ################# Piramide etaria com dados da projeção - BA - ABA 1 ##################
  
  output$piramide_geral <- renderPlot({ 
    
  # ordenando o eixo X
  popfaixaquinquenal$Genero <- factor(popfaixaquinquenal$Genero, levels = c("Homem",	"Mulher"))
  popfaixaquinquenal <- popfaixaquinquenal %>% filter(Ano==2020)
  
  # grafico
  ggplot(popfaixaquinquenal, aes(x=FaixaEtaria, y=Populacao, fill=Genero)) +
    geom_bar(stat="identity") +
    coord_flip() +
    scale_fill_manual(values = c( "#1b6db5", "#b51b8f"))+
    labs(x="Faixa Etária", y="Total por sexo", title = "Pirâmide Etária da Bahia, 2020", fill="Sexo: ") +
    theme_classic() +
    theme(legend.position="bottom",
          legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank"))+
    theme(plot.title = element_text(hjust = 0.5, face="bold"),
          axis.title.x = element_text(size=12, colour = "black"),
          axis.title.y = element_text(size=12,  colour = "black"),
          axis.line = element_line(colour = "black", 
                                   size = 1, linetype = "solid"),
          axis.text.y = element_text(face="bold", color="#000000", 
                                     size=10),
          axis.text.x = element_text(face="bold", color="#000000", 
                                     size=10),
          legend.text = element_text(size=12, face="bold"),
          legend.title = element_text(size = 12, face = "bold")) +
    scale_y_continuous(breaks = seq(-700000,700000, 200000),
                       labels = paste0(as.character(c(seq(700, 0, -200),
                                                      seq(100, 700, 200))), "Mil"))
  
  
  })
  
  
  
  #######################################################################################
  #
  # PAGINA PROJECOES
  #
  #######################################################################################
  
  
  ######################## Value Box's - ABA 2 ############################
  
  ########## Box com populacao total - ABA 2  ########## 
  output$poptotalproj <- renderValueBox({
    valueBox(
      subset(x=proj18,subset=(Ano==input$sliderano),select = c(PopTotal)),"População baiana", icon = icon("users"),
      color = "blue"
    )
  })
  
  ########## Box com tft projetada - ABA 2  ########## 
  output$tftproj <- renderValueBox({
    valueBox(
      format(subset(x=proj18,subset=(Ano==input$sliderano),select = c(TFT)), nsmall=0,  big.mark=".", decimal.mark=","),"filhos por mulher", icon = icon("child"),
      color = "yellow"
    )
  })

  ########## Box EVN - ABA 2  ########## 
  output$EVN <- renderValueBox({
    valueBox(
      subset(x=proj18,subset=(Ano==input$sliderano),select = c(EVNAmbos)),"Expectativa de vida do baiano", icon = icon("heart"),
      color = "green"
    )
  })
  
  ########## Box com Mortalidade Infantil - ABA 2  ########## 
  output$MortInf <- renderValueBox({
    valueBox(
      subset(x=proj18,subset=(Ano==input$sliderano),select = c(Pop60Mais)),"dos baianos tem 60+ anos", icon = icon("user",lib = "glyphicon"),
      color = "maroon"
    )
  })
  
  
################ Piramide etaria da pagina projecoes - ABA 2 #######################
  
   
  output$piramide <- renderPlot({  
    
    popfaixaquinquenal <- popfaixaquinquenal %>%
    mutate(Genero=recode(Genero, 
                         "Homem"="Masculino",
                         "Mulher"="Feminino"))
    
    
    # ordenando o eixo X
    popfaixaquinquenal$Genero <- factor(popfaixaquinquenal$Genero, levels = c("Masculino",	"Feminino"))
    
    # grafico
    ggplot(subset(popfaixaquinquenal, subset= (Ano==input$sliderano)), aes(x=FaixaEtaria, y=Populacao, fill=Genero)) +
      geom_bar(stat="identity") +
      coord_flip() +
      scale_fill_manual(values = c( "#1b6db5", "#b51b8f"))+
      labs(x="Faixa Etária", y="Total por sexo", fill="Sexo: ") +
      theme_classic() +
      theme(legend.position="bottom",
            legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank"))+
      theme(plot.title = element_text(hjust = 0.5, face="bold"),
            axis.title.x = element_text(size=12, colour = "black"),
            axis.title.y = element_text(size=12,  colour = "black"),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=10),
            axis.text.x = element_text(face="bold", color="#000000", 
                                       size=10),
            legend.text = element_text(size=12, face="bold"),
            legend.title = element_text(size = 12, face = "bold")) +
      scale_y_continuous(breaks = seq(-700000,700000, 200000),
                         labels = paste0(as.character(c(seq(700, 0, -200),
                                                        seq(100, 700, 200))), "Mil")) +
      ggtitle(paste0("Pirâmide Etária, ",input$sliderano, ".")) 
    
    
    
  }) 
  
  
################### Grafico de Pizza - Genero - ABA 2 ################################  
  
  output$genero_proj <- renderPlot({  
    
  
  # filtrando e criando a coluna porcentagem
  proj_2020_2060 <- proj_2020_gen %>%
    filter(Anos==input$sliderano) %>%
    mutate(sex_percent = total_gen/sum(total_gen)*100 )
  
  # ordenando o eixo X
  proj_2020_2060$genero <- factor(proj_2020_2060$genero, levels = c("Masculino",	"Feminino"))
  
  
  # criando coluna com a posicao da legenda
  
  proj_2020_2060 <- proj_2020_2060 %>% arrange(desc(genero)) %>%
    mutate(yposicao_legenda = cumsum(sex_percent)- 0.5*sex_percent)
  
  
  # grafico 
  ggplot(proj_2020_2060,aes(x="", y=sex_percent, fill=genero)) +
    geom_bar(width=1, stat = "identity") +
    coord_polar("y", start=0) +
    geom_text(aes(y=yposicao_legenda, label = sprintf("%1.2f%%",sex_percent)), size = 4,
              color = 'black',fontface='bold') +
    labs(x="",y="", fill="Sexo: ") +
    theme_minimal() +
    theme(legend.position="bottom", legend.title = element_text(colour="Black", size=10, face="bold"), legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank") )+
    theme(axis.title.x = element_text(colour = "black"),
          axis.title.y = element_text(colour = "black"),
          axis.text.y = element_text(face="bold", color="#000000", 
                                     size=7),
          axis.text.x = element_text(face="bold",color="#000000", size=12),
          axis.text=element_text(size=6, face="bold"),
          legend.text = element_text(size=10, face="bold"),
          legend.title = element_text(size = 9, face = "bold"),
          plot.title = element_text(colour = "black", size = 12, hjust=0.5, face="bold")) +
    scale_fill_manual(values = c("#1b6db5","#b51b8f")) +
    ggtitle(paste0("Sexo da população, ",input$sliderano, ".")) 
  
  })
  
################### Expectativa de Vida ao Nascer - ABA 2 #######################
  
  output$EVNporSexo <- renderPlot({ 
    
    ggplot(proj18, aes(x=Ano)) +
      geom_area(aes(y=EVNFem),fill="violetred3",alpha=0.3) +
      geom_area(aes(y=EVNMasc),fill="royalblue4",alpha=0.8) +
      geom_line(aes(y=EVNFem,color="red"), size=1.2) +
      geom_line(aes(y=EVNMasc,color="blue"), size=1.2) +           
      labs(title = "Esperança de vida, em anos",
           x="Ano", y="Anos de Vida") +
      theme_classic() +
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=10),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            plot.title = element_text(colour = "black", size = 12, hjust=0.5, face="bold"),
            axis.text.x = element_text(face="bold", color="#000000", 
                                       size=10),  legend.text = element_text(size=12, face="bold"),
            legend.title = element_text(size = 12, face = "bold")) +
      scale_colour_manual(name = 'Sexo: ', 
                          values =c('red'='red','blue'='blue'), labels = c('Masculino','Feminino')) +
      theme(legend.position = "bottom", legend.background = element_rect(fill="ghostwhite",
                                                                         size=0.5, linetype="blank"))
    
  })
  
  
  #################### Grafico da populacao geral por ano - ABA 2 #######################
  
  # cores 
  
  paleta2 <- c("#F4A989","#F0936D","#E47658","#D35749","#C2373A",
               "#B2182B", "#d63e00", "#f7723b", "#F8BFA5","#FCD6C1",
               "#F3DDD0","#E7E0DB","#DAE2E6","#CBE1EE","#ADD1E5","#90C0DB")
  
  output$GrafPopTotal <- renderPlot({ 
    ggplot(data=subset(x=proj18,subset=(Ano %in% seq(2010,2060,by=5)))) + 
      geom_col(aes(x=as.factor(Ano),y=PopTotal,fill=as.factor(Ano)), show.legend = FALSE) + scale_fill_manual(values = paleta2) +
      labs(x = "Ano", y = "População",
           title ="População projetada pelo IBGE, Bahia, 2010-2060.") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, face="bold"),
            axis.title.x = element_text(size=12,  colour = "black"),
            axis.title.y = element_text(size=12, colour = "black"),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=10),
            axis.text.x = element_text(face="bold", color="#000000", 
                                       size=10))
  })
  
  
  
################### Grafico de TBN e TBM - ABA 2 ###################
  
  output$TaxasBrutas <- renderPlot({      
    ggplot(proj18,aes(x=Ano)) +
      geom_area(aes(y=TBM), fill="brown4", alpha=0.6) +
      geom_area(aes(y=TBN), fill="orange1",alpha=0.3) +
      geom_line(aes(y=TBM,color="red"), size=1.2) +
      geom_line(aes(y=TBN,color="orange"), size=1.2) +           
      labs(title = "Taxa Bruta de Natalidade (TBN) e\n Taxa Bruta de Mortalidade (TBM), Bahia, 2010-2060.") +
      theme_classic() +
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=10),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            plot.title = element_text(colour = "black", size = 12, hjust=0.5, face="bold"),
            axis.text.x = element_text(face="bold", color="#000000", 
                                       size=10), legend.text = element_text(size=12, face="bold"),
            legend.title = element_text(size = 12, face = "bold")) +
      scale_colour_manual(name = 'Legenda', 
                          values =c('red'='red','orange'='orange'), labels = c('TBN','TBM')) +
      theme(legend.position = "bottom", legend.background = element_rect(fill="ghostwhite",
                                                                         size=0.5, linetype="blank"))
    
  })
  
  
  ########################################################################################################
  #
  # ABA 3 - PAGINA SOBRE INFORMACOES SOBRE MUNICIPIOS 
  #
  ########################################################################################################
  
  
  ############################ Value Box's - ABA 3 ################################
  
  ############ Value Box - Pop total Mun - ABA 3 ################
  
  output$pop_tot_mun <- renderValueBox({
    info_mun %>% 
      filter(Ano==input$selectano & Codigo.do.Municipio ==input$selectmuni) %>% 
        group_by(Municipios) %>%
          summarise(cont_pop=sum(total_homens,total_mulheres)) %>%
            select(cont_pop) %>%
              as.numeric() %>%
                format(nsmall=0,  big.mark=".", decimal.mark=",") %>%
                  valueBox("População total no Município", icon = icon("users"),color = "green")
  })
  

  output$nascidos_mortos <- renderValueBox({
    valueBox(
      sprintf("%1.2f%%",subset(x=mortalidade,subset=(Codigo.do.Municipio ==input$selectmuni),
                               select = c(percentual_nascidosvivos))),"Percentual de nascidos mortos (2010)",
      icon = icon("user-minus"),
      color = "blue"
    )
  })  
  
  ################ Value Box - Urbanizacao - ABA 3 ##################
  
  output$urbanizacao_mun <- renderValueBox({
    
    # filtro para SALVADOR e pegando apenas inf sobre urbanizacao
    
    info_urban <- urbanizacao_municipios %>%
      filter(Ano==input$selectano) %>%
        filter(Codigo.do.Municipio ==input$selectmuni) %>%
          group_by(status) %>% 
            summarise(cont_total=sum(total)) 

    #PERCENTUAL
    
    info_urban = mutate(info_urban, total_percent = cont_total/sum(cont_total)*100)
    
    #filtrando apenas o valor "Urbano" 
    info_urban <- info_urban %>% filter(status=="Total_urb")
    

    valueBox(
      sprintf("%1.2f%%",subset(x=info_urban,select = c(total_percent))),
      "Taxa de Urbanizacao", icon = icon("city"),
      color = "yellow"
    )
  })
  
  ############################ Mapa - População Municipios - ABA 3 ############################
  
  
  ### Ajustes no dataset e JSON ###
  
  # transformando factor em num no json
  mapa_ba_mun@data[["CD_GEOCMU"]] <- as.numeric(as.character((mapa_ba_mun@data[["CD_GEOCMU"]])))
  
  # mapa
  output$mapa_ba_mun <- renderLeaflet({
    
    # criando dataset com ano filtrado
    pop_total_mun1 <- pop_total_mun %>% filter(Ano==input$selectano)
    
    # ordenando variaveis
    pop_total_mun1$fxa_pop_mun <- factor(pop_total_mun1$fxa_pop_mun, levels = c("Até 10 mil",
                                                                                "10 mil - 50 mil",
                                                                                "50 mil - 100 mil",
                                                                                "100 mil - 150 mil",
                                                                                "150 mil - 200 mil",
                                                                                "Mais de 200 mil"))
    
    
    # banco unindo mapa com os dados #
    pop_total_mun2 <- merge(mapa_ba_mun, pop_total_mun1, by = "CD_GEOCMU")
    
    # paleta
    wardpal_pop_mun <- colorFactor(c("#fee391", "#fec44f","#fe9929","#ec7014","#8c2d04","#461602"),
                                   domain=factor(pop_total_mun2$fxa_pop_mun, 
                                                 levels=c("Até 10 mil",
                                                          "10 mil - 50 mil",
                                                          "50 mil - 100 mil",
                                                          "100 mil - 150 mil",
                                                          "150 mil - 200 mil",
                                                          "Mais de 200 mil"),))
    
    
    leaflet(pop_total_mun2) %>%
      addTiles() %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      setView(lat = -13.591215, lng = -37.979077, zoom = 6) %>% 
      addPolygons(stroke = T, opacity = 1, color = "black",weight = 0.5, smoothFactor = 0.0, fillOpacity = 1,
                  fillColor = ~wardpal_pop_mun(fxa_pop_mun),
                  label = ~paste0(NM_MUNICIP, ": ", format(total_pop_mun, big.mark = ".",decimal.mark=",")," pessoas")) %>%
      addLegend("bottomright",pal = wardpal_pop_mun, values = ~fxa_pop_mun, opacity = 1.0, title = "Tamanho da população") 
    
  })
  
  
############################ Grafico de barras - Raca\Cor - ABA 3 ########################

  output$raca <- renderPlot({
    
    raca_long_filtrado <- raca_long %>% 
      filter(Ano==input$selectano) %>%
        filter(Codigo.do.Municipio ==input$selectmuni)
    

    
    # grafico
    ggplot(raca_long_filtrado, aes(x=reorder(Cor,+PercentCor), y=PercentCor)) + 
      geom_bar(stat="identity", fill="#40739e") +
      geom_text(aes(label=sprintf("%1.2f%%",PercentCor)),size = 3, position =position_dodge(width=0.9),
                vjust=-0.25, color = 'black',fontface='bold') +
      theme_classic() +
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=10),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            axis.text=element_text(size=6, face="bold"),
            axis.text.x = element_text(face="bold", color="#000000", size=10),
            plot.title = element_text(colour = "black", size = 12, hjust=0.5, face="bold")) +
      theme(legend.position = "bottom", legend.background = element_rect(fill="ghostwhite",
                                                                         size=0.7, linetype="blank")) +
      labs(y="Percentual", x="Cor/Raça") + 
      #scale_fill_brewer(palette = "YlGnBu") +
      guides(fill=FALSE) +
      ggtitle(paste0("Cor/Raça da população, ",input$selectmuni, "."))
              

  
  })
  
################################ Grafico de Pizza - Fxa etaria - ABA 3 ###############################
  
  
  output$fxa_etaria <- renderPlot({
    
    # filtrando e criando a coluna porcentagem
    pizza_graf_fxa <- fxaetaria_long %>%
      filter(Ano==input$selectano) %>%
        filter(Codigo.do.Municipio ==input$selectmuni) %>%
          group_by(status) %>%
            summarise(cont_fxa=sum(total))
    
    # criando coluna com a posicao da legenda
    
    pizza_graf_fxa <- pizza_graf_fxa %>% arrange(desc(status)) %>%
      mutate(yposicao_legenda = cumsum(cont_fxa)- 0.5*cont_fxa)
    
    # alterando o nome das variaveis 
    pizza_graf_fxa$status <- factor(pizza_graf_fxa$status, levels=c("fxa_0a14",
                                                                    "fxa_15a59",
                                                                    "fxa_60mais"),
                                    labels=c("0 a 14", "15 a 59", "+60"))
    
    # grafico
    ggplot(pizza_graf_fxa,aes(x="", y=cont_fxa, fill=status)) +
      geom_bar(width=1, stat = "identity") +
      coord_polar("y", start=0) +
      geom_text(aes(y=yposicao_legenda, label = sprintf("%1.2f%%",cont_fxa)), size = 4,
                color = 'black',fontface='bold') +
      scale_fill_brewer(palette = "YlOrBr") +
      labs(x="",y="", fill="Faixa etária (em anos)") +
      theme_minimal() +
      theme(legend.position="bottom", 
            legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank"))+
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=7),
            axis.text.x = element_text(face="bold",color="#000000", size=12),
            axis.text=element_text(size=6, face="bold"),
            legend.text = element_text(size=12, face="bold"),
            legend.title = element_text(size = 12, face = "bold"),
            plot.title = element_text(colour = "black", size = 12, hjust=0.5, face="bold")) +
      ggtitle(paste0("Faixa Etária da população, ",input$selectmuni, ".")) 


  })
  
########################### Grafico de Barras - Estado Civil - ABA 3 ########################### 
  
  #paleta_estadocivil <- c("#FF7F50","#BC8F8F","#B8860B","#FF0000","#8B0000")
  
 
  # ordenando eixo X
  #legenda_estadocivil <- c("Casado", "Divorciado", "Separado\n Judicialmente", "Solteiro", "Viúvo")
  
  output$estado_civil <- renderPlot({
    
    # filtrando ano e municipio
    estado_civil_v2 <- estado_civil_agreg %>%
       filter(Ano==input$selectano) %>%
         filter(Codigo.do.Municipio ==input$selectmuni) %>%
           group_by(situacao_conjugal) %>% 
             summarise(cont_civil=sum(total_civil))
    
    # calculando a porcentagem
    estado_civil_v2 = mutate(estado_civil_v2,civil_percent = cont_civil/sum(cont_civil)*100 )
    
    # Ajustando o eixo x
    estado_civil_v2 <- estado_civil_v2 %>% mutate(situacao_conjugal=recode(situacao_conjugal, 
                         "separado_jud"="Separado Jud.","Divorciado"="Divorciado",
                         "Viuvo" = "Viúvo", "casado" = "Casado", "Solteiro" = "Solteiro"))
    
    # grafico
    ggplot(estado_civil_v2, aes(x=reorder(situacao_conjugal, +civil_percent), y=civil_percent)) + 
      geom_bar(stat="identity",fill="#DC143C") +
      geom_text(aes(label=sprintf("%1.2f%%",civil_percent)),size = 4, position =position_dodge(width=0.9),
                vjust=-0.25, color = 'black',fontface='bold') +
      theme_classic() +
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=10),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            axis.text=element_text(size=6, face="bold"),
            axis.text.x = element_text(face="bold",color="#000000", size=10),
            legend.text = element_text(size=9, face="bold"),
            legend.title = element_text(size = 9, face = "bold"),
            plot.title = element_text(colour = "black", size = 12, hjust=0.5, face="bold")) +
      theme(legend.position = "bottom", legend.background = element_rect(fill="ghostwhite",
                                                                         size=0.7, linetype="blank")) +
      labs(y="Percentual", x="Estado Civil") +
      #scale_fill_manual(values = paleta_estadocivil) +
      #scale_x_discrete(labels = legenda_estadocivil) +
      ggtitle(paste0("Estado Civil da população, ",input$selectmuni, ".")) +
      guides(fill=F)

    
    
  })
  
############################# Grafico de Pizza - Naturalidade - ABA 3 #############################
  
  
  paleta_nat <- c("#E3D081", "#91C7B1")

  
  output$naturalidade <- renderPlot({
    
  
  # filtro 
  pizza_graf_nat <- naturalide_long %>%
                      filter(Ano==input$selectano) %>%
                       filter(Codigo.do.Municipio ==input$selectmuni) %>%
                        group_by(local_natural) %>%
                          summarise(cont_nat=sum(total_natural))
  
  # coluna com percentual
  pizza_graf_nat = mutate(pizza_graf_nat, nat_percent = cont_nat/sum(cont_nat)*100 )
  
  # posicao da legenda
  pizza_graf_nat <- pizza_graf_nat %>% arrange(desc(cont_nat)) %>%
    mutate(yposicao_legenda = cumsum(nat_percent)- 0.5*nat_percent)
  
  # alterando o nome das variaveis 
  pizza_graf_nat$local_natural <- factor(pizza_graf_nat$local_natural,levels = c("não_natural_mun","natural.do.mun"),
                                         labels = c("Não-Natural", "Natural"))
  
  # grafico
  ggplot(pizza_graf_nat,aes(x="", y=nat_percent, fill=local_natural)) +
    geom_bar(width=1, stat = "identity") +
    coord_polar("y", start=0) +
    geom_text(aes(y=yposicao_legenda, label = sprintf("%1.2f%%",nat_percent)), size = 4,
              color = 'black',fontface='bold') +
    labs(x="",y="", fill="Naturalidade: ") +
    theme_minimal() +
    theme(legend.position="bottom", legend.title = element_text(colour="Black", size=10, face="bold"), legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank") )+
    theme(axis.title.x = element_text(colour = "black"),
          axis.title.y = element_text(colour = "black"),
          axis.text.y = element_text(face="bold", color="#000000", 
                                     size=7),
          axis.text.x = element_text(face="bold",color="#000000", size=12),
          axis.text=element_text(size=6, face="bold"),
          legend.text = element_text(size=9, face="bold"),
          legend.title = element_text(size = 9, face = "bold"),
          plot.title = element_text(colour = "black", size = 12, hjust=0.5, face="bold")) +
    scale_fill_manual(values = paleta_nat) +
    ggtitle(paste0("Naturalidade da população em relação ao município,\n ",input$selectmuni, ".")) 


  
  })
  
################################# Grafico de pizza - Genero - ABA 3 ##################################
  

  output$genero_mun <- renderPlot({
    
    # filtro para municipio e pegando apenas inf sobre SEXO
    
    genero_mun_v2 <- genero_mun %>%
      filter(Ano==input$selectano) %>%
        filter(Codigo.do.Municipio ==input$selectmuni) %>%
          group_by(genero) %>%
            summarise(cont_sex=sum(total_genero))
    
    #PERCENTUAL
    
    genero_mun_v2 = mutate(genero_mun_v2, sex_percent = cont_sex/sum(cont_sex)*100 )
    
    # posicao da legenda
    
    genero_mun_v2 <- genero_mun_v2 %>%
      arrange(desc(cont_sex)) %>%
        mutate(yposicao_legenda = cumsum(sex_percent)- 0.5*sex_percent )
    
    # alterando o nome das variaveis 
    genero_mun_v2$genero <- factor(genero_mun_v2$genero, levels=c("total_homens","total_mulheres"),
                                   labels=c("Masculino","Feminino"))
    
    
    # grafico
    ggplot(genero_mun_v2,aes(x="", y=sex_percent, fill=genero)) +
      geom_bar(width=1, stat = "identity") +
      coord_polar("y", start=0) +
      geom_text(aes(y=yposicao_legenda, label = sprintf("%1.2f%%",sex_percent)), size = 4,
                color = 'black',fontface='bold') +
      labs(x="",y="", fill="Sexo: ") +
      theme_minimal() +
      theme(legend.position="bottom", legend.title = element_text(colour="Black", size=10, face="bold"), legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank") )+
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=7),
            axis.text.x = element_text(face="bold",color="#000000", size=12),
            axis.text=element_text(size=6, face="bold"),
            legend.text = element_text(size=10, face="bold"),
            legend.title = element_text(size = 9, face = "bold"),
            plot.title = element_text(colour = "black", size = 12, hjust=0.5, face="bold")) +
      scale_fill_manual(values = c("#1b6db5","#b51b8f")) +
      ggtitle(paste0("Sexo da população, ",input$selectmuni, ".")) 



  })
  
  

  
################################ Piramide Etaria CENSO -  ABA 3 ###################################
  
  output$piramide_censo <- renderPlot({
    
    # fazendo filtro e selecionando municipio
    
    piramide_filter <- piramide_municipios %>%
      filter(Ano==input$selectano) %>%
        filter(codigo_municipio ==input$selectmuni) %>%
          group_by(faixa_etarias,sexo) %>%
            summarise(cont_total=sum(total_faixa))
    

    # Ajustando o eixo y
    piramide_filter$faixa_etarias <- factor(piramide_filter$faixa_etarias, levels = c("Entre.0.a.4",	"Entre.5.a.9",
                                                                                      "Entre.10.a.14",	"Entre.15.a.19",
                                                                                      "Entre.20.a.24",	"Entre.25.a.29",
                                                                                      "Entre.30.a.34",	"Entre.35.a.39",
                                                                                      "Entre.40.a.44",	"Entre.45.a.49",
                                                                                      "Entre.50.a.54",	"Entre.55.a.59",
                                                                                      "Entre.60.a.64",	"Entre.65.a.69",
                                                                                      "Entre.70.a.74",	"Entre.75.a.79",
                                                                                      "Entre.80.a.84",	"Entre.85.a.89",
                                                                                      "Entre.90.a.94",	"Entre.95.a.99",
                                                                                      "Mais.de.100"),
                                            labels = c("0 a 4","5 a 9","10 a 14","15 a 19","20 a 24","25 a 29",
                                                       "30 a 34","35 a 39","40 a 44","45 a 49","50 a 54","55 a 59",
                                                       "60 a 64","65 a 69","70 a 74","75 a 79","80 a 84","85 a 89",
                                                       "90 a 94","95 a 99","+100"))
    # ordenando o eixo X
    piramide_filter$sexo <- factor(piramide_filter$sexo, levels = c("Masculino",	"Feminino"))
                                                                                      
    # grafico
    ggplot(piramide_filter, aes(x=faixa_etarias, y=cont_total, fill=sexo)) +
      geom_bar(stat="identity") +
      coord_flip() +
      scale_fill_manual(values = c("#1b6db5","#b51b8f")) +
      labs(x="Faixa Etaria", y="Total por sexo" ,fill="Sexo: ") +
      theme_classic() +
      theme(legend.position="bottom",
            legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank")) +         
      theme(plot.title = element_text(hjust = 0.5,face="bold", colour = "black",size = 12),
            axis.title.x = element_text(size=12,  colour = "black"),
            axis.title.y = element_text(size=12,  colour = "black"),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=10),
            axis.text.x = element_text(face="bold", color="#000000", 
                                       size=10), legend.text = element_text(size=12, face="bold"),
            legend.title = element_text(size = 12, face = "bold")) +
      scale_y_continuous(breaks = seq(-200000,200000, 50000),
                         labels = paste0(as.character(c(seq(200, 0, -50),
                                                        seq(50, 200, 50))), "Mil")) +
      ggtitle(paste0("Pirâmide Etária, ",input$selectmuni, "."))

    
  })
  
  
  
  #####################################################################################################
  #
  ######### ABA 4 - Pagina com informacoes sobre Territorio de Identidade
  #
  #####################################################################################################
  
  ############################# Mapa com Territorios de Identidade - ABA 4 ###########################
  
  # convertendo variavel de caracter em numerica - ABA 1
  territorios_geo$CD_TI <- as.numeric(territorios_geo$CD_TI)
  
  # mapa
  output$mapa_bahia <- renderLeaflet({
    
    # criando dataset com ano filtrado
    territorios_pop_v2 <- territorios_pop %>% filter(Ano==input$selectanov1)
    
    # banco unindo mapa com os dados #
    territorios_pop_v3 <- merge(territorios_geo, territorios_pop_v2, by = "CD_TI")
    
    # paleta
    wardpal <- colorFactor(c("#fee391", "#fec44f","#fe9929","#ec7014","#8c2d04","#461602"),
                               domain=factor(territorios_pop_v3$faixas_pop, 
                                             levels=c("100 mil - 200 mil",
                                                      "201 mil - 300 mil",
                                                      "301 mil - 400 mil",
                                                      "401 mil - 500 mil",
                                                      "501 mil - 600 mil",
                                                      "Mais que 600 mil")))
    
    # mapa final
    leaflet(territorios_pop_v3) %>%
      addTiles() %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      setView(lat = -13.591215, lng = -37.979077, zoom = 6) %>% 
      addPolygons(stroke = T, opacity = 1, color = "black",weight = 0.5, smoothFactor = 0.0, fillOpacity = 1,
                  fillColor = ~wardpal(faixas_pop),
                  label = ~paste0(NM_TI, ": ", format(total_ter, big.mark = ".",decimal.mark=",")," pessoas")) %>%
      addLegend("bottomright",pal = wardpal, values = ~faixas_pop, opacity = 1.0, title = "Tamanho da população") 


  })
  
  
  ##### Grafico de Barras - Cor/Raca - Territorio de Identidade - ABA 4 #####
  
  output$raca_ter <- renderPlot({
    
    raca_filtrado <- raca_long %>% 
      filter(Ano==input$selectanov1) %>% filter(Territorios.de.Identidade ==input$select_ter) %>%
      group_by(Cor) %>% summarise(cont_civil=mean(PercentCor))
    
    # grafico 
    ggplot(raca_filtrado, aes(x=reorder(Cor, +cont_civil), y=cont_civil)) + 
      geom_bar(stat="identity",fill="#40739e") +
      geom_text(aes(label=sprintf("%1.2f%%",cont_civil)),size = 4, position =position_dodge(width=0.9),
                vjust=-0.25, color = 'black',fontface='bold') +
      theme_classic() +
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=10),
            axis.text.x = element_text(face="bold", color="#000000", 
                                       size=10),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            axis.text=element_text(size=10, face="bold"),
            legend.text = element_text(size=10, face="bold"),
            legend.title = element_text(size = 9, face = "bold"),
            plot.title = element_text(colour = "black", size = 12, hjust=0.5, face="bold")) +
      theme(legend.position = "bottom", legend.background = element_rect(fill="ghostwhite",
                                                                         size=0.7, linetype="blank")) +
      labs(y="Percentual", x="Cor/Raça") + 
      guides(fill = F) +
      #scale_fill_brewer(palette = "YlGnBu") +
      ggtitle(paste0("Cor/Raça da população, ",input$select_ter, ".")) 
    
    
    
  })
################################ Grafico de Barras - Estado Civil - ABA 4 ##########################

  output$estado_civil_ter <- renderPlot({
    
    # Filtrando territorio de identidade e ano
    estado_civil_v2 <- estado_civil_agreg %>% filter(Ano==input$selectanov1) %>% filter(Territorios.de.Identidade ==input$select_ter) %>%
      group_by(situacao_conjugal) %>% summarise(cont_civil=sum(total_civil))
    
    # calculando a porcentagem
    estado_civil_v2 = mutate(estado_civil_v2,civil_percent = cont_civil/sum(cont_civil)*100 )
    
    # Ajustando o eixo x
    estado_civil_v2 <- estado_civil_v2 %>% mutate(situacao_conjugal=recode(situacao_conjugal, 
                                                                           "separado_jud"="Separado Jud.","Divorciado"="Divorciado",
                                                                           "Viuvo" = "Viúvo", "casado" = "Casado", "Solteiro" = "Solteiro"))
    
    # grafico
    ggplot(estado_civil_v2, aes(x=reorder(situacao_conjugal, +civil_percent), y=civil_percent)) + 
      geom_bar(stat="identity",fill="#DC143C") +
      geom_text(aes(label=sprintf("%1.2f%%",civil_percent)),size = 4, position =position_dodge(width=0.9),
                vjust=-0.25, color = 'black',fontface='bold') +
      theme_classic() +
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=10),
            axis.text.x = element_text(face="bold", color="#000000", 
                                       size=10),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            axis.text=element_text(size=6, face="bold"),
            legend.text = element_text(size=10, face="bold"),
            legend.title = element_text(size = 9, face = "bold"),
            plot.title = element_text(colour = "black", size = 12, hjust=0.5, face="bold")) +
      theme(legend.position = "bottom", legend.background = element_rect(fill="ghostwhite",
                                                                         size=0.7, linetype="blank")) +
      labs(y="Percentual", x="Estado Civil") +
      #scale_fill_manual(values = paleta_estadocivil) +
      guides(fill = guide_legend(nrow = 1)) +
      #scale_x_discrete(labels = legenda_estadocivil) +
      ggtitle(paste0("Estado Civil da população, ",input$select_ter, ".")) +
      guides(fill=F)
    

  })
  
################################### Grafico de Pizza - Genero - ABA 4 ################################
  
  output$genero_ter <- renderPlot({
    
    # filtro para Territorios e pegando apenas inf sobre SEXO
    genero_territorio <- genero_mun %>% filter(Ano==input$selectanov1) %>%
      filter(Territorios.de.Identidade ==input$select_ter) %>%
      group_by(genero) %>% summarise(cont_sex=sum(total_genero))
    
    #PERCENTUAL
    genero_territorio = mutate(genero_territorio, sex_percent = cont_sex/sum(cont_sex)*100 )
    
    # posicao da legenda
    genero_territorio <- genero_territorio %>% arrange(desc(cont_sex)) %>%
      mutate(yposicao_legenda = cumsum(sex_percent)- 0.5*sex_percent )
    
    # alterando o nome das variaveis 
    genero_territorio$genero <- factor(genero_territorio$genero, levels=c("total_homens","total_mulheres"),
                                   labels=c("Masculino","Feminino"))
    
    
    # grafico 
    ggplot(genero_territorio,aes(x="", y=sex_percent, fill=genero)) +
      geom_bar(width=1, stat = "identity") +
      coord_polar("y", start=0) +
      geom_text(aes(y=yposicao_legenda, label = sprintf("%1.2f%%",sex_percent)), size = 4,
                color = 'black',fontface='bold') +
      labs(x="",y="", fill="Sexo: ") +
      theme_minimal() +
      theme(legend.position="bottom", legend.title = element_text(colour="Black", size=10, face="bold"), legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank") )+
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=7),
            axis.text.x = element_text(face="bold",color="#000000", size=12),
            axis.text=element_text(size=6, face="bold"),
            legend.text = element_text(size=10, face="bold"),
            legend.title = element_text(size = 9, face = "bold"),
            plot.title = element_text(colour = "black", size = 12, hjust=0.5, face="bold")) +
      scale_fill_manual(values = c("#1b6db5","#b51b8f")) +
      ggtitle(paste0("Sexo da população, ",input$select_ter, ".")) 

    
    
  })
########################### Grafico de Pizza - Fxa Etaria - ABA 4 #################################
  
  output$fxa_et_terr <- renderPlot({
    
    # filtrando territorio de identidade e criando a coluna porcentagem
    
    fxa_etaria_terr <- fxaetaria_long %>% filter(Ano==input$selectanov1) %>%
      filter(Territorios.de.Identidade ==input$select_ter) %>%
      group_by(status) %>% summarise(cont_fxa=mean(total))
    
    
    # criando coluna com a posicao da legenda
    
    fxa_etaria_terr <- fxa_etaria_terr %>% arrange(desc(status)) %>%
      mutate(yposicao_legenda = cumsum(cont_fxa)- 0.5*cont_fxa)
    
    # alterando o nome das variaveis 
    fxa_etaria_terr$status <- factor(fxa_etaria_terr$status, levels=c("fxa_0a14",
                                                                    "fxa_15a59",
                                                                    "fxa_60mais"),
                                    labels=c("0 a 14", "15 a 59", "+60"))
    
    # grafico
    ggplot(fxa_etaria_terr,aes(x="", y=cont_fxa, fill=status)) +
      geom_bar(width=1, stat = "identity") +
      coord_polar("y", start=0) +
      geom_text(aes(y=yposicao_legenda, label = sprintf("%1.2f%%",cont_fxa)), size = 4,
                color = 'black',fontface='bold') +
      scale_fill_brewer(palette = "YlOrBr") +
      labs(x="",y="", fill="Faixa etária (em anos): ") +
      theme_minimal() +
      theme(legend.position="bottom", legend.title = element_text(colour="Black", size=10, face="bold"), legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank") )+
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=7),
            axis.text.x = element_text(face="bold",color="#000000", size=12),
            axis.text=element_text(size=6, face="bold"),
            legend.text = element_text(size=10, face="bold"),
            legend.title = element_text(size = 9, face = "bold"),
            plot.title = element_text(colour = "black", size = 12, hjust=0.5, face="bold")) +
      ggtitle(paste0("Faixa Etária da população, ",input$select_ter, ".")) 

  })
  
################################ Grafico de Pizza - Naturalidade - ABA 4 ###############################
  
  output$naturalidade_terr <- renderPlot({
    
    # filtro para territorio de identidade
    
    naturalidade_terr <- naturalide_long %>% filter(Ano==input$selectanov1) %>% filter(Territorios.de.Identidade ==input$select_ter) %>%
      group_by(local_natural) %>% summarise(cont_nat=sum(total_natural))
    
    # coluna com percentual
    
    naturalidade_terr = mutate(naturalidade_terr, nat_percent = cont_nat/sum(cont_nat)*100 )
    
    # posicao da legenda
    
    naturalidade_terr <- naturalidade_terr %>% arrange(desc(cont_nat)) %>%
      mutate(yposicao_legenda = cumsum(nat_percent)- 0.5*nat_percent )
    
    # alterando o nome das variaveis 
    naturalidade_terr$local_natural <- factor(naturalidade_terr$local_natural,levels = c("não_natural_mun","natural.do.mun"),
                                           labels = c("Não-Natural", "Natural"))
    
    # grafico
    ggplot(naturalidade_terr,aes(x="", y=nat_percent, fill=local_natural)) +
      geom_bar(width=1, stat = "identity") +
      coord_polar("y", start=0) +
      geom_text(aes(y=yposicao_legenda, label = sprintf("%1.2f%%",nat_percent)), size = 4,
                color = 'black',fontface='bold') +
      labs(x="",y="", fill="Naturalidade: ") +
      theme_minimal() +
      theme(legend.position="bottom", legend.title = element_text(colour="Black", size=10, face="bold"), legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank") )+
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=7),
            axis.text.x = element_text(face="bold",color="#000000", size=12),
            axis.text=element_text(size=6, face="bold"),
            legend.text = element_text(size=10, face="bold"),
            legend.title = element_text(size = 9, face = "bold"),
            plot.title = element_text(colour = "black", size = 12, hjust=0.5, face="bold")) +
      scale_fill_manual(values = paleta_nat) +
      ggtitle(paste0("Naturalidade da população em relação ao território,\n ",input$select_ter, ".")) 

  })
  
  
################### Piramide CENSO - Territorio de Identidade - ABA 4 #######################
  
  output$piramide_terr <- renderPlot({
    
    # fazendo filtro 

    piramide_filter_ter <- piramide_municipios %>% filter(Ano==input$selectanov1) %>% filter(Territorios.de.Identidade ==input$select_ter) %>%
      group_by(faixa_etarias,sexo) %>% summarise(cont_total=sum(total_faixa))
    
    
    # Ajustando o eixo y
    piramide_filter_ter$faixa_etarias <- factor(piramide_filter_ter$faixa_etarias, levels = c("Entre.0.a.4",	"Entre.5.a.9",
                                                                                      "Entre.10.a.14",	"Entre.15.a.19",
                                                                                      "Entre.20.a.24",	"Entre.25.a.29",
                                                                                      "Entre.30.a.34",	"Entre.35.a.39",
                                                                                      "Entre.40.a.44",	"Entre.45.a.49",
                                                                                      "Entre.50.a.54",	"Entre.55.a.59",
                                                                                      "Entre.60.a.64",	"Entre.65.a.69",
                                                                                      "Entre.70.a.74",	"Entre.75.a.79",
                                                                                      "Entre.80.a.84",	"Entre.85.a.89",
                                                                                      "Entre.90.a.94",	"Entre.95.a.99",
                                                                                      "Mais.de.100"),
                                            labels = c("0 a 4","5 a 9","10 a 14","15 a 19","20 a 24","25 a 29",
                                                       "30 a 34","35 a 39","40 a 44","45 a 49","50 a 54","55 a 59",
                                                       "60 a 64","65 a 69","70 a 74","75 a 79","80 a 84","85 a 89",
                                                       "90 a 94","95 a 99","+100"))
    # ordenando o eixo X
    piramide_filter_ter$sexo <- factor(piramide_filter_ter$sexo, levels = c("Masculino",	"Feminino"))
    
    
    # grafico
    ggplot(piramide_filter_ter, aes(x=faixa_etarias, y=cont_total, fill=sexo)) +
      geom_bar(stat="identity") +
      coord_flip() +
      scale_fill_manual(values = c("#1b6db5", "#b51b8f")) +
      labs(x="Faixa Etaria", y="Total por sexo") +
      theme_classic() +
      theme(legend.position="bottom",
            legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank"))+         
      theme(plot.title = element_text(hjust = 0.5,face="bold", colour = "black",size = 12),
            axis.title.x = element_text(size=12,  colour = "black"),
            axis.title.y = element_text(size=12, colour = "black"),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=10),
            axis.text.x = element_text(face="bold", color="#000000", 
                                       size=10), legend.text = element_text(size=12, face="bold"),
            legend.title = element_text(size = 12, face = "bold")) +
      scale_y_continuous(breaks = seq(-200000,200000, 50000),
                         labels = paste0(as.character(c(seq(200, 0, -50),
                                                        seq(50, 200, 50))), "Mil")) +
      ggtitle(paste0("Pirâmide Etária, ",input$select_ter, ".")) +
      labs(fill= "Sexo: ")
    
    
    
    
    
  })
  

}
