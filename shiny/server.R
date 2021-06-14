library(shiny)
library(ggplot2)
# library(HDF5Array)
library(data.table)
library(purrr)
library(cowplot)
library(ggrepel)
library(ggpubr)
library(ggiraph)
# library(GGally)
# library(DT)

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
comarques <- readRDS("comarques.rds")
ibi_bonificacio.dt <- fread("ibi_bonificacio.txt.gz")
autoconsum.dt <- fread("instalacions_autoconsum.txt.gz") %>%
  .[,year:=factor(year,levels=sort(unique(year)))]
pes_habitatge_unifamiliar.dt <- fread("pes_habitatge_unifamiliar.txt.gz")
pib_municipi.dt <- fread("pib_municipi.txt.gz")
pib_comarca.dt <- fread("pib_comarca.txt.gz")
poblacio_comarca.dt <- fread("poblacio_comarca.txt.gz")
poblacio_municipi.dt <- fread("poblacio_municipi.txt.gz")
municipis_per_comarca.dt <- fread("municipis_per_comarca.txt.gz")


# catalunya_mapa_municipis.dt <- fread("catalunya_mapa_municipis.txt.gz")
catalunya_mapa_comarques.dt <- fread("catalunya_mapa_comarques.txt.gz") %>% setnames("comarca","county")

catalunya_mapa_comarques_centre.dt <- catalunya_mapa_comarques.dt[,.(long=mean(long), lat=mean(lat)), by="county"]

##################
## Shiny server ##
##################

server <- function(input, output, session) {
  
  # updateSelectizeInput(session = session, inputId = 'municipi', choices = genes, server = TRUE, selected = "Barcelona") #DONT remove T, appears to be a bug that it vanishes
  # updateSelectizeInput(session = session, inputId = 'any', choices = TFs, server = TRUE, selected = "2020")
  
  # output$municipi <- renderUI({
  #   if (input$comarca!="Totes") {
  #     selectInput("municipi", "Municipi", choices = municipis_per_comarca.dt[comarca==input$comarca,municipi])
  #   }
  # })
  
  
  #########################
  ## Evolucio autoconsum ##
  #########################
  
  plotAutoconsumEvolucio = reactive({
    
    # Filter data
    autoconsum_filt.dt <- autoconsum.dt[year%in%input$anys[1]:input$anys[2]]
    if (input$comarca != "Totes"){
      stopifnot(input$comarca%in%unique(autoconsum.dt$county))
      autoconsum_filt.dt <- autoconsum_filt.dt[county==input$comarca]
    }
    
    to.plot <- autoconsum_filt.dt %>%
      .[,.(power=sum(power), installations=.N),by=c("quarter","year")] %>%
      .[!year==2021,c("daily_power","daily_installations"):=list(round(power/90,2),round((installations/90),2))] %>%
      .[year==2021,c("daily_power","daily_installations"):=list(round(power/30,2),round((installations/30),2))] 

    # to.plot$tooltip <- "daily_installations"
    
    p1 <- ggplot(to.plot, aes_string(x="year", fill="quarter", y="daily_installations", tooltip = "daily_installations")) +
      geom_bar_interactive(stat="identity", position = position_dodge(), color="black") +
      scale_x_discrete(drop=FALSE) +
      labs(x="", y="Number of installations (daily)") +
      theme_classic() +
      theme(
        axis.text = element_text(size=rel(1.50)),
        axis.title = element_text(size=rel(1.50), color="black"),
        legend.title = element_blank(),
        legend.position = "top"
      )

    p2 <- ggplot(to.plot, aes_string(x="year", fill="quarter", y="daily_power", tooltip = "daily_power")) +
      geom_bar_interactive(stat="identity", position = position_dodge(), color="black") +
      scale_x_discrete(drop=FALSE) +
      labs(x="", y="Power installed (kWh, daily)") +
      theme_classic() +
      theme(
        axis.text = element_text(size=rel(1.50)),
        axis.title = element_text(size=rel(1.50), color="black"),
        legend.title = element_blank(),
        legend.position = "top"
      )

    # p <- cowplot::plot_grid(plotlist=list(p1,p2), rel_heights = c(3/5,2/5), nrow=2)
    
    p <- girafe(
      ggobj = plot_grid(p1, p2, nrow=2, rel_heights = c(3/5,2/5)),
      # ggobj = p1,
      width_svg = 16, height_svg = 10,
      options = list(
        # opts_sizing(rescale = FALSE),
        opts_tooltip(offx = 20, offy = 20),
        opts_hover_inv(css = "opacity:0.75;")
        # opts_hover_inv(css = "opacity:0.75;"),
        # opts_hover(css = "cursor:pointer;r:14px")
      )
    )
    
    return(p)
  })
  
  # output$evolucio = renderPlot({
  output$evolucio = renderGirafe({
    return(plotAutoconsumEvolucio())
  })
  
  
  #############################
  ## Comparacio de comarques ##
  #############################
  
  plotAutoconsum_per_comarca = reactive({
      
    # Filter data
    autoconsum_filt.dt <- autoconsum.dt[year%in%input$anys[1]:input$anys[2]]
    
    to.plot <- autoconsum_filt.dt %>%
      .[,.(power=sum(power), installations=.N),by=c("county")] %>% 
      merge(pib_comarca.dt, by=c("county")) %>%
      merge(poblacio_comarca.dt, by=c("county")) %>%
      # merge(pluviometria_comarca.dt, by=c("county")) %>%
      .[,c("power_capita","number_installations_capita"):=list(round(power/population_size,2),round(1000*installations/population_size,2))] %>%
      setorder(number_installations_capita)# %>% .[,county:=factor(county,levels=county)]
    
    # to.plot[, county := stri_trans_general(str = county, id = "Latin-ASCII")]
    to.plot[, county := gsub("\'", " ", county)]
    
    p1 <- ggplot(to.plot, aes(x=reorder(county,number_installations_capita), fill=number_installations_capita, y=number_installations_capita, tooltip=county, data_id=county)) +
      geom_bar_interactive(stat="identity", position = position_dodge(), color="black") +
      # scale_x_discrete(drop=FALSE) +
      scale_fill_gradientn(colours = rev(terrain.colors(10))) +
      labs(x="", y="Nombre d'instalacions per 1000 habitants") +
      guides(x = guide_axis(angle = 90)) +
      theme_classic() +
      theme(
        axis.text.y = element_text(size=rel(1.50)),
        axis.title.y = element_text(size=rel(1.75)),
        axis.text.x = element_text(size=rel(1.75)),
        axis.ticks.x = element_line(size=rel(1.50)),
        legend.title = element_blank(),
        legend.position = "none"
      )
  
    to.plot.mapa <- catalunya_mapa_comarques.dt %>% merge(to.plot,by="county")
  
    to.plot.text <- catalunya_mapa_comarques_centre.dt %>% merge(to.plot[,c("number_installations_capita","county")])
    
    p2 <- ggplot(to.plot.mapa, aes(x=long, y=lat)) +
      # geom_polygon(aes(fill = number_installations_capita), alpha = 0.8, size = 0.05 ) +
      geom_polygon_interactive(aes(group=group, fill=number_installations_capita, tooltip=county, data_id=county), alpha = 0.8, size = 0.05) +
      geom_text(aes(label=number_installations_capita), data=to.plot.text, size=4, color="black") +
      scale_fill_gradientn(colours = rev(terrain.colors(10))) +
      theme_void() +
      theme(
        legend.position = "none",
        legend.title = element_blank(),
        panel.background = element_rect(size= 0.5, color = "white", fill = "white")
      )
    
    # plot_grid(p1, p2, nrow=2, rel_heights = c(2/5,3/5)) %>% return.
    
    girafe(
      # ggobj = plot_grid(p1, p2, nrow=2, rel_heights = c(2/5,3/5)),
      ggobj = plot_grid(p1, p2, nrow=1, scale=0.9, rel_widths = c(3/5,2/5)),
      width_svg = 20, height_svg = 10,
      options = list(
        # opts_sizing(rescale = FALSE),
        # opts_tooltip(offx = 20, offy = 20),
        opts_selection(type = "single", css = "cursor:pointer;fill:darkgrey"),
        opts_hover_inv(css = "opacity:0.65;"),
        opts_hover(css = "cursor:pointer")
      )
    ) %>% return(.)
  })
    
  # output$plot_autoconsum_comarca = renderPlot({
  output$plot_autoconsum_comarca = renderGirafe({
    return(plotAutoconsum_per_comarca())
  })
  
  #############################
  ## Comparacio de municipis ##
  #############################
  
  # Mapa catalunya
  # Seleccionar any
  # Highlighy specific county
  
  ##############################################
  ## Dades socioeconomiques a nivell comarcal ##
  ##############################################
  
  # Afegir pluviometria
  # Afegir intensitat solar
  # Split by year
  
  plotAutoconsum_vs_DadesSocioEconomiques_per_comarca = reactive({
    
    # Filter data
    autoconsum_filt.dt <- autoconsum.dt[year%in%input$anys[1]:input$anys[2]]
    
    name2variable <- c(
      "PIB per capita" = "pib_capita", 
      "Potència total instal·lada" = "power", 
      "Potència instal·lada per 1000 habitants" = "power_capita", 
      "Nombre total d'instalacions" = "installations",
      "Densitat de població" = "population_density",
      # "Percentage d'habitatges unifamiliars" = "population_density",
      "Nombre d'instalacions per 1000 habitants" = "number_installations_capita"
    )
    stopifnot(input$eix_x%in%names(name2variable))
    stopifnot(input$eix_y%in%names(name2variable))
    x_axis = name2variable[input$eix_x]
    y_axis = name2variable[input$eix_y]
    
    if (input$separar_per_any) {
      to.plot <- autoconsum_filt.dt %>% .[,.(power=sum(power), installations=.N),by=c("county","year")]
    } else {
      to.plot <- autoconsum_filt.dt %>% .[,.(power=sum(power), installations=.N),by=c("county")]
    }
    
    to.plot <- to.plot %>%
      merge(pib_comarca.dt, by=c("county")) %>%
      merge(poblacio_comarca.dt, by=c("county")) %>%
      .[,c("power_capita","number_installations_capita"):=list(round(power/population_size,2),round(1000*installations/population_size,2))]
    
    to.plot[, county := gsub("\'", " ", county)]
    
    # p <- ggscatter(to.plot, x=x_axis, y=y_axis, fill=color_by, size=4, shape=21,
    #           add="reg.line", add.params = list(color="blue", fill="lightgray"), conf.int=TRUE) +
    if (input$colour_by=="Sense color") {
      p <- ggplot(to.plot, aes_string(x=x_axis, y=y_axis))
    } else {
      color_by = name2variable[input$colour_by]
      p <- ggplot(to.plot, aes_string(x=x_axis, y=y_axis, fill=color_by), shape=21)
    }
    
    p <- p +
      geom_smooth(method='lm', alpha=0.75) +
      geom_point_interactive(aes(tooltip=county, data_id=county), size=4) +
      stat_cor(method = "pearson", size=7.5) +
      scale_fill_gradientn(colours = rev(terrain.colors(10))) +
      labs(x=input$eix_x, y=input$eix_y) +
      theme_classic() +
      theme(
        # legend.position = "none",
        legend.title = element_blank(),
        legend.position = "right",
        axis.text = element_text(size=rel(2.00)),
        axis.title = element_text(size=rel(2.00))
      )
    
    if (input$annotar_comarques) {
      p <- p + ggrepel::geom_text_repel(aes(label=county), size=5, max.overlaps=Inf)
    }
    
    if (input$separar_per_any) {
      p <- p + facet_wrap(~year, ncol=2, scales="free_x") + 
        theme(strip.text = element_text(size=rel(2.0)))
    }
    
    # return(p)
    
    height_svg = ifelse(input$separar_per_any,20,10)
    width_svg = ifelse(input$separar_per_any,30,20)
    
    girafe(
      # ggobj = plot_grid(p1, p2, nrow=2, rel_heights = c(2/5,3/5)),
      ggobj = p,
      width_svg = width_svg, height_svg = height_svg,
      options = list(
        # opts_sizing(rescale = FALSE),
        opts_tooltip(offx = 20, offy = 20),
        opts_hover_inv(css = "opacity:0.75;"),
        opts_hover(css = "cursor:pointer;r:14px")
      )

    ) %>% return(.)
    
  })
  
  # output$test = renderPlot({
  output$test = renderGirafe({
    return(plotAutoconsum_vs_DadesSocioEconomiques_per_comarca())
  })
  
  # pib capita 
  # population_density
  # percentaatge_unifamiliars
  
  ###############################################
  ## Dades socioeconomiques a nivell municipal ##
  ###############################################
  
  # bonificacio IBI
  # poble/ciutat
}
