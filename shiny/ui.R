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
              
              # selectInput(
              #   "municipi",
              #   "Municipi",
              #   choices = municipis,
              #   selected="Sant Pere de Ribes"
              # ),
              # selectInput(
              #   "stage",
              #   "Cell subset",
              #   choices = c(
              #     "All timepoints" = "all",
              #     "E7.5" = "E7.5" ,
              #     "E7.75" ="E7.75",
              #     "E8.5" = "E8.5" 
              #   ),
              #   selected = "all"
              # ),
              # selectInput(
              #   "colourby",
              #   "Plot colour",
              #   choices = c(
              #     "Cell type" = "celltype",
              #     "Timepoint" = "stage",
              #     "Sample" = "sample"
              #   ),
              #   selected = "cluster.ann"
              # ),
              # selectizeInput("gene", "Gene", choices = NULL, selected = 26600),
              # selectizeInput("tf", "TF", choices = NULL, selected=10),
              # checkboxInput("numbers", "Annotate clusters in plot"),
              # checkboxInput("subset", "Subset cells (faster plots when many points present)"),
            
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
                  girafeOutput("evolucio", width = "800px", height = "1200px"),
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
                  girafeOutput("plot_autoconsum_comarca", width = "900px", height = "500px"),
                  #second binding of data plot needed here
                  # plotOutput("gene_plot", width = "900px", height = "500px"),
                  # plotOutput("gene_violin", width = "900px")
                )
                
              )
            )
          ))
