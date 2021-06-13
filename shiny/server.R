library(shiny)
library(ggplot2)
# library(HDF5Array)
library(data.table)
library(purrr)
library(cowplot)
library(ggrepel)
library(ggpubr)
# library(GGally)
library(ggiraph)
# library(DT)

library(rgdal)
library(broom)

setwd("/Users/ricard/test/autoconsum_catalunya/shiny/dades")


################
## Load utils ##
################

# basedir <- "/Users/ricard/test/autoconsum_catalunya/shiny/dades"

###############
## Load data ##
###############

anys <- read.table("anys.txt.gz")
municipis <- readRDS("municipis.rds")
ibi_bonificacio.dt <- fread("ibi_bonificacio.txt.gz")
autoconsum.dt <- fread("instalacions_autoconsum.txt.gz") %>%
  .[,year:=factor(year,levels=sort(unique(year)))]
pes_habitatge_unifamiliar.dt <- fread("pes_habitatge_unifamiliar.txt.gz")
pib_municipi.dt <- fread("pib_municipi.txt.gz")
poblacio_municipi.dt <- fread("poblacio_municipi.txt.gz")

catalunya_mapa_municipis.dt <- fread("catalunya_mapa.txt.gz")
catalunya_mapa_comarques.dt <- fread("catalunya_mapa_comarques.txt.gz") %>% setnames("comarca","county")


server <- function(input, output, session) {
  
  # updateSelectizeInput(session = session, inputId = 'municipi', choices = genes, server = TRUE, selected = "Barcelona") #DONT remove T, appears to be a bug that it vanishes
  # updateSelectizeInput(session = session, inputId = 'any', choices = TFs, server = TRUE, selected = "2020")
  
  #################
  ## Evolucio autoconsum ##
  #################
  
  plotAutoconsumEvolucio = reactive({
    
    # if(input$modality == "rna"){
    #   cells <- sample_metadata[pass_rnaQC==TRUE & doublet_call==FALSE & !is.na(celltype),cell]
    # } else if(input$modality == "atac"){
    #   cells <- sample_metadata[pass_atacQC==TRUE & doublet_call==FALSE & !is.na(celltype),cell]
    # }

    to.plot <- autoconsum.dt %>%
      .[,.(power=sum(power), installations=.N),by=c("quarter","year")] %>%
      .[!year==2021,c("daily_power","daily_installations"):=list(round(power/90,2),round((installations/90),2))] %>%
      .[year==2021,c("daily_power","daily_installations"):=list(round(power/30,2),round((installations/30),2))] 

    # to.plot$tooltip <- "daily_installations"
    
    p1 <- ggplot(to.plot, aes_string(x="year", fill="quarter", y="daily_installations", tooltip = "daily_installations")) +
      geom_bar_interactive(stat="identity", position = position_dodge(), color="black") +
      labs(x="", y="Number of installations (daily)") +
      theme_classic() +
      theme(
        axis.text.y = element_text(size=rel(0.95)),
        legend.title = element_blank(),
        legend.position = "top"
      )

    p2 <- ggplot(to.plot, aes_string(x="year", fill="quarter", y="daily_power", tooltip = "daily_power")) +
      geom_bar_interactive(stat="identity", position = position_dodge(), color="black") +
      labs(x="", y="Number of installations (daily)") +
      theme_classic() +
      theme(
        axis.text.y = element_text(size=rel(0.95)),
        legend.title = element_blank(),
        legend.position = "top"
      )

    p <- cowplot::plot_grid(plotlist=list(p1,p2), rel_heights = c(3/5,2/5), nrow=2)
    
    # p <- girafe(
    #   ggobj = plot_grid(p1, p2, nrow=2, rel_heights = c(3/5,2/5)),
    #   # ggobj = p1, 
    #   width_svg = 16, height_svg = 10,
    #   options = list(
    #     # opts_sizing(rescale = FALSE),
    #     opts_tooltip(offx = 20, offy = 20),
    #     opts_hover_inv(css = "opacity:0.75;")
    #     # opts_hover_inv(css = "opacity:0.75;"),
    #     # opts_hover(css = "cursor:pointer;r:14px")
    #   )
    # )
    
    return(p)
  })
  
  output$evolucio = renderPlot({
    plotAutoconsumEvolucio()
  })
  
  
  #####################################
  ## Evolucio autoconsum per comarca ##
  #####################################
  
  # Mapa catalunya
  # Seleccionar any
  # Highlighy specific county
  
  to.plot <- autoconsum.dt %>%
    .[,.(power=sum(power), installations=.N),by=c("county")] %>% 
    merge(pib_comarca.dt, by=c("county")) %>%
    merge(poblacio_comarca.dt, by=c("county")) %>%
    # merge(pluviometria_comarca.dt, by=c("county")) %>%
    .[,c("power_capita","number_installations_capita"):=list(round(power/population_size,2),round(1000*installations/population_size,2))] %>%
    setorder(number_installations_capita)# %>% .[,county:=factor(county,levels=county)]
  
  
  p1 <- ggplot(to.plot, aes(x=reorder(county,number_installations_capita), fill=number_installations_capita, y=number_installations_capita, data_id=county)) +
    geom_bar_interactive(stat="identity", position = position_dodge(), color="black") +
    scale_fill_gradientn(colours = rev(terrain.colors(10))) +
    labs(x="", y="Nombre d'instalacions per 1000 habitants") +
    guides(x = guide_axis(angle = 90)) +
    theme_classic() +
    theme(
      axis.text.y = element_text(size=rel(0.75)),
      legend.title = element_blank(),
      legend.position = "none"
    )

  to.plot <- catalunya_mapa_comarques.dt %>% 
    merge(to.plot,by="county")
  
  p2 <- ggplot(to.plot, aes(x=long, y=lat, group=group, fill=number_installations_capita, tooltip=county)) +
    # geom_polygon(aes(fill = number_installations_capita), alpha = 0.8, size = 0.05 ) +
    geom_polygon_interactive(alpha = 0.8, size = 0.05 ) +
    scale_fill_gradientn(colours = rev(terrain.colors(10))) +
    theme_void() +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      panel.background = element_rect(size= 0.5, color = "white", fill = "white")
    )
  
  p <- girafe(
    # ggobj = plot_grid(p1, p2, nrow=2, rel_heights = c(2/5,3/5)),
    ggobj = p1,
    width_svg = 12, height_svg = 14,
    options = list(
      # opts_sizing(rescale = FALSE),
      # opts_tooltip(offx = 20, offy = 20),
      opts_hover_inv(css = "opacity:0.25;")
      # opts_hover(css = "cursor:pointer;r:14px")
    )
  )
  
  #####################################
  ## Evolucio autoconsum per municipi ##
  #####################################
  
  # Mapa catalunya
  # Seleccionar any
  # Highlighy specific county
  
}
