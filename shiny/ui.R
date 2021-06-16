library(shiny)
library(ggplot2)
# library(DT)
# library(rintrojs)
library(ggiraph)
library(shinythemes)

#####################
## Define settings ##
#####################

# setwd("/Users/ricard/test/autoconsum_catalunya/shiny/dades")

###############
## Load data ##
###############

comarques <- readRDS("dades/comarques.rds")
municipis <- readRDS("dades/municipis.rds")
anys <- readRDS("dades/anys.rds")

## rename stuff
comarques <- gsub("\'", " ", comarques)
municipis <- gsub("\'", " ", municipis)

##############
## Shiny UI ##
##############

shinyUI(fluidPage(
  # introjsUI(),
  theme = shinytheme("cerulean"),
          sidebarLayout(
            sidebarPanel(
              id="sidebar",
              # style = "position:fixed;width:15%;",
              width = 2,
              h3("Opcions"),
              
              #SIDEBAR INPUTS
              selectInput("comarca", "Comarca", choices = c(comarques,"Totes"), selected="Totes"),
              # checkboxGroupInput("comarca", "Comarques", choices = comarques, selected = comarques),
              # selectizeInput("comarca", "Comarques", choices = comarques, selected = comarques, multiple = TRUE),
              
              
              sliderInput(
                "anys", 
                label = "Anys", 
                min = min(as.numeric(anys)), 
                max = max(as.numeric(anys)), 
                value = c(min(as.numeric(anys)), max(as.numeric(anys))),
                sep=""
              )
              # uiOutput("municipi")
              
            
            ),
            mainPanel(
              id = "main",
              width = 10,
              titlePanel(
                "Estudi de l'autoconsum energètic a Catalunya"
              ),
              
              #put plots here
              tabsetPanel(
                id = "tabs",
                
                tabPanel("Introducció", fluid = TRUE, includeMarkdown("introduccio.md")
                ),
    
                tabPanel(
                  title = "Evolucio de l'autoconsum",
                  id = "autoconsum_evolucio",
                  br(),
                  HTML("<br>En aquest gràfic veiem el nombre d'instal·lacions per cada 1000 habitants des de 2013 fins a 2021. <br>
                  El color correspon als diferents trimestres de l'any. Pel 2021 només disposem informació del primer trimestre <br>"),
                  girafeOutput("evolucio", width = "800px", height = "1200px")
                  # plotOutput("stage_contribution", width = "900px")
                ),
                
                # tabPanel(
                #   title = "Autoconsum per municipi",
                #   id = "autoconsum_municipi",
                #   # checkboxInput("rna_vs_chromvar_pseudobulk_scatterplot_add_text", "Add cell type labels to the scatterplot"),
                #   # plotOutput("rna_vs_chromvar", width = "900px", height = "350px"),
                #   # plotOutput("rna_vs_chromvar_paga", width = "900px", height = "400px")
                #   # girafeOutput("rna_vs_chromvar", width = "900px", height = "800px")
                # ),
                
                tabPanel(
                  title = "Autoconsum per comarca",
                  id = "autoconsum_comarca",
                  HTML("<br>En aquests gràfics veiem el nombre d'instal·lacions en cada comarca per cada 1000 habitants.<br>"),
                  girafeOutput("plot_autoconsum_comarca", width = "1300px", height = "500px")
                  #second binding of data plot needed here
                  # plotOutput("gene_plot", width = "900px", height = "500px"),
                  # plotOutput("gene_violin", width = "900px")
                ),
                
                tabPanel(
                  title = "Dades socioeconomiques per comarca",
                  id = "autoconsum_vs_socioeconomia_comarca",
                  HTML("<br>En aquest apartat podem fer correlacions entre variables socials, demogràfiques i econòmiques i el nombre d'instal·lacions de plaques fotovoltaiques a nivell comarcal. <br>
                  Podeu veure com el percentatge d'habitatges unifamiliars es un factor positivament correlacionat amb el nombre d'instal·lacions. 
                  Per altra banda, és interessant veure que el PIB per càpita es troba negativament correlacionat.<br>"),
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("eix_x_comarques", "Eix X", choices = c("Percentage d'habitatges unifamiliars", "PIB per capita","Nombre total d'instalacions","Densitat de població"), selected="Percentage d'habitatges unifamiliars"),
                      selectInput("eix_y_comarques", "Eix Y", choices = c("Nombre d'instalacions per 1000 habitants","Nombre total d'instalacions","Potència instal·lada per 1000 habitants","Potència total instal·lada"), selected="Nombre d'instalacions per 1000 habitants"),
                      selectInput("colour_by_comarques", "Color", choices = c("Sense color","PIB per capita", "Potència total instal·lada", "Potència instal·lada per 1000 habitants", "Nombre d'instalacions", "Densitat de població", "Percentage d'habitatges unifamiliars", "Nombre d'instalacions per 1000 habitants"), selected="Sense color"),
                      checkboxInput("annotar_comarques", "Annotar comarques", value=TRUE),
                      checkboxInput("separar_per_any", "Separar per any", value=FALSE)
                    ),
                  mainPanel(
                    girafeOutput("Autoconsum_vs_DadesSocioEconomiques_per_comarca")
                    # plotOutput("test", width = "1200px", height = "500px")
                    )
                  )
                ), # end tabPanel
                
                tabPanel(
                  title = "Dades socioeconomiques per municipi",
                  id = "autoconsum_vs_socioeconomia_municipi",
                  HTML("<br>En aquest apartat podem fer correlacions entre variables socials, demogràfiques i econòmiques i el nombre d'instal·lacions de plaques fotovoltaiques a nivell municipal. <br>
                  Podeu veure l'associació molt clara entre el percentatge d'habitatges unifamiliars i nombre d'instal·lacions. No obstant, el PIB per càpita no està correlacionat. <br>
                  Per defecte l'aplicació mostra totes les comàrques i tots els anys. Podeu seleccionar una comarca i anys d'interès al panell esquerra. <br>"),
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("eix_x_municipis", "Eix X", choices = c("PIB per capita","Nombre total d'instalacions","Població total","Percentage d'habitatges unifamiliars"), selected="PIB"),
                      selectInput("eix_y_municipis", "Eix Y", choices = c("Nombre d'instalacions per 1000 habitants","Nombre total d'instalacions","Potència instal·lada per 1000 habitants","Potència total instal·lada"), selected="Nombre d'instalacions per 1000 habitants"),
                      selectInput("colour_by_municipis", "Color", choices = c("Sense color","PIB per capita", "Potència total instal·lada", "Potència instal·lada per 1000 habitants", "Nombre d'instalacions", "Densitat de població", "Percentage d'habitatges unifamiliars", "Nombre d'instalacions per 1000 habitants"), selected="Sense color"),
                      checkboxInput("annotar_municipis", "Annotar municipis", value=FALSE),
                      numericInput("text_size_municipis", label = "Text size", value = 7),
                      numericInput("dot_size_municipis", label = "Dot size", value = 4)
                      # checkboxInput("separar_per_any", "Separar per any", value=FALSE)
                    ),
                    mainPanel(
                      girafeOutput("Autoconsum_vs_DadesSocioEconomiques_per_municipi")
                      # plotOutput("test", width = "1200px", height = "500px")
                    )
                  )
                ), # end tabPanel
                
                tabPanel(
                  title = "Model estadístic comarcal",
                  id = "model_estadistic_comarcal",
                  HTML("<br>En aquesta secció fem servir un <a href=https://en.wikipedia.org/wiki/Regression_analysis>model de regressió lineal</a> en el que fem servir les dades socials, demogràfiques i econòmiquesper predir el nombre d'instal·lacions de plaques fotovoltaiques a nivell comarcal <br>
                  Seguidament, calculem els coeficients residuals del model per veure quines comarques ho estan fent millor o pitjor segons les seves condicions socials i econòmiques. Comarques amb índex positiu o estan fent millor donades les condicions, mentre que comarques amb index negatiu o estan fent pitjor. <br> <br>
                  Teniu en compte que aquests resultats són orientatius i el model encara es pot millorar incloent més dades de cada comarca, com per exemple d'intensitat solar o de pluviometria. <br>"),
                  girafeOutput("ModelEstadisticComarcal")
                ), # end tabPanel
                
                tabPanel(
                  title = "Model estadístic municipal",
                  id = "model_estadistic_municipal",
                  HTML("<br>En aquesta secció fem servir un <a href=https://en.wikipedia.org/wiki/Regression_analysis>model de regressió lineal</a> en el que fem servir les dades socials, demogràfiques i econòmiquesper predir el nombre d'instal·lacions de plaques fotovoltaiques a nivell municipal. <br>
                  Seguidament, calculem els coeficients residuals del model per veure quines comarques ho estan fent millor o pitjor segons les seves condicions socials i econòmiques. Comarques amb índex positiu o estan fent millor donades les condicions, mentre que comarques amb index negatiu o estan fent pitjor. <br> <br>
                  Teniu en compte que aquests resultats són orientatius i el model encara es pot millorar incloent més dades de cada municipi, com per exemple d'intensitat solar o de pluviometria. <br>"),
                  girafeOutput("ModelEstadisticMunicipal")
                ), # end tabPanel
                
                tabPanel(
                  title = "Creixement per municipi",
                  id = "creixement_per_municipi",
                  HTML("<br>En aquesta secció calculem la diferència entre el nombre d'instal·lacions per cada 1000 habitants entre 2018/2019 i 2020/2021. <br>
                  Municipis amb major valor positiu són els que més han cplaques fotovoltàiques han instalat en els últims dos anys (respecte a la quantitat que en tenien abans).<br>"),
                  girafeOutput("CreixementPerMunicipi")
                ), # end tabPanel
                
                tabPanel(
                  title = "Efecte de la bonificació de l'IBI",
                  id = "bonificacio_ibi",
                  HTML("<br>En aquesta secció calculem l'efecte que ha tingut la bonificació per IBI en l'instal·lació de plaques fotovoltaiques. <br>
                  La quantitat de bonificació varia entre municipis, però per simplificar l'ànalisi aquí ho hem discretitzat entre Sí o No. <br>
                  Veiem molt clarament com aquesta política ha tingut un efecte molt positiu en pobles (municipis amb poca densitat de població i alt percentage d'habitatges unifamiliars), però no en ciutats.<br>"),
                  plotOutput("BonificacioIBI")
                ) # end tabPanel
                
                
              ) # end tabsetPanel
              
            ) # end mainPanel
          ) # end sidebarLayout
        ) # end fluidPage
) # end shinIU