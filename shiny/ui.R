library(shiny)
library(ggplot2)
library(DT)
# library(plotly)
library(rintrojs)
library(ggiraph)

#####################
## Define settings ##
#####################

setwd("/Users/ricard/test/autoconsum_catalunya/shiny/dades")

# "900px" = "900px"
# "500px" = "500px"
# narrower_plot_width = "650px"
# half_plot_width = "450px"
# narrower_half_plot_width = "350px"
# half_plot_height = "260px"


###############
## Load data ##
###############

comarques <- readRDS("comarques.rds")
municipi <- readRDS("municipis.rds")
anys <- readRDS("anys.rds")

##############
## Shiny UI ##
##############

fluidPage(introjsUI(),
          sidebarLayout(
            sidebarPanel(
              id="sidebar",
              # style = "position:fixed;width:15%;",
              width = 2,
              h3("Plot options"),
              
              #SIDEBAR INPUTS
              selectInput(
                "comarca",
                "Comarca",
                choices = c(comarques,"Totes"),
                selected="Totes"
              ),
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
                      selectInput("eix_x", "Eix X", choices = c("PIB per capita","Nombre d'instalacions","Densitat de població","Percentage d'habitatges unifamiliars"), selected="PIB"),
                      selectInput("eix_y", "Eix Y", choices = c("Nombre d'instalacions per 1000 habitants","Nombre total d'instalacions","Potència instal·lada per 1000 habitants","Potència total instal·lada"), selected="Nombre d'instalacions per 1000 habitants"),
                      selectInput("colour_by", "Color", choices = c("Sense color","PIB per capita", "Potència total instal·lada", "Potència instal·lada per 1000 habitants", "Nombre d'instalacions", "Densitat de població", "Percentage d'habitatges unifamiliars", "Nombre d'instalacions per 1000 habitants"), selected="Sense color"),
                      checkboxInput("annotar_comarques", "Annotar comarques", value=TRUE),
                      checkboxInput("separar_per_any", "Separar per any", value=FALSE)
                    ),
                  mainPanel(
                    girafeOutput("test")
                    # plotOutput("test", width = "1200px", height = "500px")
                    )
                  )
                ) # end tabPanel
                
              ) # end tabsetPanel
              
            ) # end mainPanel
          ) # end sidebarLayout
        ) # end fluidPage
