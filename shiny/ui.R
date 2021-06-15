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
              h3("Plot options"),
              
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
                tabPanel(
                  id = "landing", 
                  "Landing page",
                  HTML("Fotovoltaiques")
                ),
                
                tabPanel(
                  title = "Evolucio de l'autoconsum",
                  id = "autoconsum_evolucio",
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
                  girafeOutput("plot_autoconsum_comarca", width = "1300px", height = "500px")
                  #second binding of data plot needed here
                  # plotOutput("gene_plot", width = "900px", height = "500px"),
                  # plotOutput("gene_violin", width = "900px")
                ),
                
                tabPanel(
                  title = "Dades socioeconomiques per comarca",
                  id = "autoconsum_vs_socioeconomia_comarca",
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
                  girafeOutput("ModelEstadisticComarcal")
                ), # end tabPanel
                
                tabPanel(
                  title = "Model estadístic municipal",
                  id = "model_estadistic_municipal",
                  girafeOutput("ModelEstadisticMunicipal")
                ), # end tabPanel
                
                tabPanel(
                  title = "Creixement per municipi",
                  id = "creixement_per_municipi",
                  girafeOutput("CreixementPerMunicipi")
                ), # end tabPanel
                
                tabPanel(
                  title = "Efecte de la bonificació de l'IBI",
                  id = "bonificacio_ibi",
                  plotOutput("BonificacioIBI")
                ) # end tabPanel
                
                
              ) # end tabsetPanel
              
            ) # end mainPanel
          ) # end sidebarLayout
        ) # end fluidPage
) # end shinIU