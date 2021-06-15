library(R.utils)
library(shiny)
library(ggplot2)
library(data.table)
library(purrr)
library(cowplot)
library(ggrepel)
library(ggpubr)
library(ggiraph)

# setwd("/Users/ricard/test/autoconsum_catalunya/shiny/dades")

################
## Load utils ##
################

# basedir <- "/Users/ricard/test/autoconsum_catalunya/shiny/dades"

###############
## Load data ##
###############

anys <- readRDS("dades/anys.rds")
municipis <- readRDS("dades/municipis.rds")
comarques <- readRDS("dades/comarques.rds")
ibi_bonificacio.dt <- fread("dades/ibi_bonificacio.txt.gz")# %>%
  # .[,bonificacio:=c("No bonificacio IBI","Si bonificacio IBI")[as.numeric(!is.na(percentage_bonificacio))+1]]
autoconsum.dt <- fread("dades/instalacions_autoconsum.txt.gz") %>%
  .[,year:=factor(year,levels=sort(unique(year)))]
pes_habitatge_unifamiliar_municipi.dt <- fread("dades/pes_habitatge_unifamiliar.txt.gz")# %>% setnames("municipality","municipi")
pes_habitatge_unifamiliar_comarca.dt <- fread("dades/pes_habitatge_unifamiliar_comarca.txt.gz") %>% setnames("comarca","county")
pib_municipi.dt <- fread("dades/pib_municipi.txt.gz") %>% setnames("municipality","municipi")
poblacio_municipi.dt <- fread("dades/poblacio_municipi.txt.gz") %>% setnames("municipality","municipi")
pib_comarca.dt <- fread("dades/pib_comarca.txt.gz")
poblacio_comarca.dt <- fread("dades/poblacio_comarca.txt.gz")
municipis_per_comarca.dt <- fread("dades/municipis_per_comarca.txt.gz")

catalunya_mapa_municipis.dt <- fread("dades/catalunya_mapa_municipis.txt.gz", colClasses = c("numeric","numeric","factor","factor")) %>%
  .[,long:=as.integer(long)] %>%
  .[,lat:=as.integer(lat)]
catalunya_mapa_comarques.dt <- fread("dades/catalunya_mapa_comarques.txt.gz", colClasses = c("numeric","numeric","factor","factor"))# %>%
  # .[,long:=as.integer(long)] %>%
  # .[,lat:=as.integer(lat)]

# Rename stuff
municipis <- gsub("\'", " ", municipis)
catalunya_mapa_municipis.dt[, municipi := factor(gsub("\'", " ", municipi))]
poblacio_municipi.dt[, municipi := gsub("\'", " ", municipi)]
pes_habitatge_unifamiliar_municipi.dt[, municipi := gsub("\'", " ", municipi)]
pib_municipi.dt[, municipi := gsub("\'", " ", municipi)]
municipis_per_comarca.dt[, municipi := gsub("\'", " ", municipi)]
autoconsum.dt[, municipi := gsub("\'", " ", municipi)]
ibi_bonificacio.dt[, municipi := gsub("\'", " ", municipi)]

comarques <- gsub("\'", " ", comarques)
municipis_per_comarca.dt[, comarca := gsub("\'", " ", comarca)]
poblacio_comarca.dt[, county := gsub("\'", " ", county)]
pes_habitatge_unifamiliar_comarca.dt[, county := gsub("\'", " ", county)]
pib_comarca.dt[, county := gsub("\'", " ", county)]
catalunya_mapa_comarques.dt[, county := factor(gsub("\'", " ", county))]
autoconsum.dt[, county := gsub("\'", " ", county)]

# mapes
catalunya_mapa_comarques_centre.dt <- catalunya_mapa_comarques.dt[,.(long=mean(long), lat=mean(lat)), by="county"]
catalunya_mapa_municipis_centre.dt <- catalunya_mapa_municipis.dt[,.(long=mean(long), lat=mean(lat)), by="municipi"]

# modificacions
name2variable <- c(
  "PIB per capita" = "pib_capita", 
  "Potència total instal·lada" = "power", 
  "Potència instal·lada per 1000 habitants" = "power_capita", 
  "Nombre total d'instalacions" = "installations",
  "Densitat de població" = "population_density",
  "Població total" = "population_size",
  "Percentage d'habitatges unifamiliars" = "percentage_habitatges_unifamiliars",
  "Nombre d'instalacions per 1000 habitants" = "number_installations_capita"
)

## Test ##
# input <- list()
# input$comarca <- "Totes"
# input$anys <- 2017:2021
# input$eix_x <- "PIB per capita"
# input$eix_y <- "Nombre total d'instalacions"
# input$colour_by <- "Població total"
# input$annotar_municipis <- TRUE
# input$annotar_comarques <- TRUE

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
    autoconsum_filt.dt <- autoconsum.dt[year%in%input$anys[1]:rev(input$anys)[1]]
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
    
    girafe(
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
    ) %>% return(.)
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
    autoconsum_filt.dt <- autoconsum.dt[year%in%input$anys[1]:rev(input$anys)[1]]
    
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
  
    to.plot.mapa <- catalunya_mapa_comarques.dt %>% merge(to.plot[,c("number_installations_capita","county")],by="county")
  
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
        opts_selection(type = "single", css = "cursor:pointer;fill:purple"),
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
  # Afegir mapa catalunya
  
  plotAutoconsum_vs_DadesSocioEconomiques_per_comarca = reactive({
    
    # Filter data
    autoconsum_filt.dt <- autoconsum.dt[year%in%input$anys[1]:rev(input$anys)[1]]
    
    stopifnot(input$eix_x_comarques%in%names(name2variable))
    stopifnot(input$eix_y_comarques%in%names(name2variable))
    x_axis = name2variable[input$eix_x_comarques]
    y_axis = name2variable[input$eix_y_comarques]
    
    if (input$separar_per_any) {
      to.plot <- autoconsum_filt.dt %>% .[,.(power=sum(power), installations=.N),by=c("county","year")]
    } else {
      to.plot <- autoconsum_filt.dt %>% .[,.(power=sum(power), installations=.N),by="county"]
    }
    
    to.plot <- to.plot %>%
      merge(pib_comarca.dt, by="county",all.x=TRUE) %>%
      merge(poblacio_comarca.dt, by="county",all.x=TRUE) %>%
      merge(pes_habitatge_unifamiliar_comarca.dt, by="county",all.x=TRUE) %>%
      .[,c("power_capita","number_installations_capita"):=list(round(power/population_size,2),round(1000*installations/population_size,2))]
    
    to.plot[, county := gsub("\'", " ", county)]
    
    if (input$colour_by_comarques=="Sense color") {
      p <- ggplot(to.plot, aes_string(x=x_axis, y=y_axis))
    } else {
      color_by = name2variable[input$colour_by_comarques]
      p <- ggplot(to.plot, aes_string(x=x_axis, y=y_axis, fill=color_by), shape=21)
    }
    
    if (input$annotar_comarques) {
      p <- p + ggrepel::geom_text_repel(aes(label=county), size=5, max.overlaps=Inf)
    }
    
    p <- p +
      geom_smooth(method='lm', alpha=0.25) +
      geom_point_interactive(aes(tooltip=county, data_id=county), size=4) +
      stat_cor(method = "pearson", size=7.5) +
      scale_fill_gradientn(colours = rev(terrain.colors(10))) +
      labs(x=input$eix_x_comarques, y=input$eix_y_comarques) +
      theme_classic() +
      theme(
        # legend.position = "none",
        legend.title = element_blank(),
        legend.position = "right",
        axis.text = element_text(size=rel(2.00)),
        axis.title = element_text(size=rel(2.00))
      )
    
    if (input$separar_per_any) {
      p <- p + facet_wrap(~year, ncol=2, scales="free_x") + 
        theme(strip.text = element_text(size=rel(2.0)))
    }
    
    girafe(
      # ggobj = plot_grid(p1, p2, nrow=2, rel_heights = c(2/5,3/5)),
      ggobj = p,
      width_svg = ifelse(input$separar_per_any,30,20), height_svg = ifelse(input$separar_per_any,20,10),
      options = list(
        # opts_sizing(rescale = FALSE),
        opts_tooltip(offx = 20, offy = 20),
        opts_hover_inv(css = "opacity:0.75;"),
        opts_hover(css = "cursor:pointer;r:14px")
      )) %>% return(.)
    
  })
  
  output$Autoconsum_vs_DadesSocioEconomiques_per_comarca = renderGirafe({
    return(plotAutoconsum_vs_DadesSocioEconomiques_per_comarca())
  })
  
  ###############################################
  ## Dades socioeconomiques a nivell municipal ##
  ###############################################
  
  # bonificacio IBI
  # poble/ciutat
  
  plotAutoconsum_vs_DadesSocioEconomiques_per_municipi = reactive({
    
    # Filter data
    autoconsum_filt.dt <- autoconsum.dt[year%in%input$anys[1]:rev(input$anys)[1]]
    
    if (input$comarca != "Totes"){
      stopifnot(input$comarca%in%unique(autoconsum.dt$county))
      autoconsum_filt.dt <- autoconsum_filt.dt[county==input$comarca]
    }
    
    # stopifnot(input$eix_x_municipis%in%names(name2variable))
    # stopifnot(input$eix_y_municipis%in%names(name2variable))
    x_axis = name2variable[input$eix_x_municipis]
    y_axis = name2variable[input$eix_y_municipis]
    
    to.plot <- autoconsum_filt.dt %>% 
      .[,.(power=sum(power), installations=.N),by="municipi"] %>%
      merge.data.table(pib_municipi.dt, by="municipi", all.x=TRUE) %>%
      merge.data.table(poblacio_municipi.dt, by="municipi", all.x=TRUE) %>%
      merge.data.table(pes_habitatge_unifamiliar_municipi.dt[,c("municipi","tipus_municipi","percentage_habitatges_unifamiliars")], by="municipi", all.x=TRUE) %>%
      .[,c("power_capita","number_installations_capita"):=list(round(power/population_size,2),round(1000*installations/population_size,2))]
    
    if (input$colour_by_municipis=="Sense color") {
      p <- ggplot(to.plot, aes_string(x=x_axis, y=y_axis))
    } else {
      color_by = name2variable[input$colour_by_municipis]
      p <- ggplot(to.plot, aes_string(x=x_axis, y=y_axis, fill=color_by), shape=21)
    }
    
    if (input$annotar_municipis) {
      p <- p + ggrepel::geom_text_repel(aes(label=municipi), size=input$text_size_municipis, max.overlaps=Inf)
    }
    
    p <- p +
      geom_smooth(method='lm', alpha=0.25) +
      geom_point_interactive(aes(tooltip=municipi, data_id=municipi), size=input$dot_size_municipis) +
      stat_cor(method = "pearson", size=8.5) +
      scale_fill_gradientn(colours = rev(terrain.colors(10))) +
      labs(x=input$eix_x_municipis, y=input$eix_y_municipis) +
      theme_classic() +
      theme(
        # legend.position = "none",
        legend.title = element_blank(),
        legend.position = "right",
        axis.text = element_text(size=rel(2.20)),
        axis.title = element_text(size=rel(2.50))
      )
    
    girafe(
      ggobj = p,
      width_svg = 35, height_svg = 20,
      options = list(
        # opts_sizing(rescale = FALSE),
        opts_tooltip(offx = 20, offy = 20),
        opts_hover_inv(css = "opacity:0.75;"),
        opts_hover(css = "cursor:pointer;r:14px")
      )
    ) %>% return(.)
    
  })
  
  output$Autoconsum_vs_DadesSocioEconomiques_per_municipi = renderGirafe({
    return(plotAutoconsum_vs_DadesSocioEconomiques_per_municipi())
  })
  
  ########################################
  ## Model estadístic a nivell comarcal ##
  ########################################
  
  
  plotModelEstadisticComarcal <- reactive({
    
    # Filter data
    autoconsum_filt.dt <- autoconsum.dt[year%in%input$anys[1]:rev(input$anys)[1]]
    
    to.plot <- autoconsum_filt.dt %>% 
      .[,.(installations=.N),by="county"] %>%
      merge(pib_comarca.dt, by="county") %>%
      merge(poblacio_comarca.dt, by="county") %>%
      merge(pes_habitatge_unifamiliar_comarca.dt[,c("county","percentage_habitatges_unifamiliars")], by="county") %>%
      .[,number_installations_capita:=1000*installations/population_size]
    
    linear.model <- lm(formula=number_installations_capita~percentage_habitatges_unifamiliars+population_density+pib_capita, data=to.plot)
    # summary(linear.model)
    
    to.plot[,residuals:=round(linear.model[["residuals"]],2)]
    
    p1 <- ggplot(to.plot, aes(x=reorder(county,residuals), fill=residuals, y=residuals, tooltip = county, data_id=county)) +
      geom_bar_interactive(stat="identity", position = position_dodge(), color="black") +
      scale_x_discrete(drop=FALSE) +
      scale_fill_gradient(low = "red", high = "green") +
      theme_classic() +
      labs(x="", y="Model residuals") +
      guides(x = guide_axis(angle = 90)) +
      theme(
        axis.text = element_text(size=rel(1.25), color="black"),
        axis.title = element_text(size=rel(1.5), color="black")
      )
    
    to.plot.mapa <- catalunya_mapa_comarques.dt %>% merge(to.plot[,c("residuals","county")],by="county",all.x=T)
    to.plot.text <- catalunya_mapa_comarques_centre.dt %>% merge(to.plot[,c("residuals","county")],by="county")
    
    p2 <- ggplot(to.plot.mapa, aes(x=long, y=lat)) +
      geom_polygon_interactive(aes(group=group, fill=residuals, tooltip=county, data_id=county), alpha = 0.8, size = 0.05) +
      geom_text(aes(label=residuals), data=to.plot.text, size=4, color="black") +
      scale_fill_gradient(low = "red", high = "green") +
      theme_void() +
      theme(
        legend.position = "none",
        legend.title = element_blank(),
        panel.background = element_rect(size= 0.5, color = "white", fill = "white")
      )
    
    
    girafe(
      # ggobj = plot_grid(p1, p2, nrow=2, rel_heights = c(2/5,3/5)),
      ggobj = plot_grid(p1, p2, nrow=1, scale=0.9, rel_widths = c(3/5,2/5)),
      width_svg = 23, height_svg = 10,
      options = list(
        # opts_sizing(rescale = FALSE),
        # opts_tooltip(offx = 20, offy = 20),
        opts_selection(type = "single", css = "cursor:pointer;fill:purple"),
        opts_hover_inv(css = "opacity:0.65;"),
        opts_hover(css = "cursor:pointer")
      )
    ) %>% return(.)
    

    
  })
    
  output$ModelEstadisticComarcal = renderGirafe({
    return(plotModelEstadisticComarcal())
  })
  
  
  #########################################
  ## Model estadístic a nivell municipal ##
  #########################################
  
  
  plotModelEstadisticMunicipal <- reactive({
    
    if (exists(x="p1")) rm(list = c("p1"))
    if (exists(x="p2")) rm(list = c("p2"))
    # if (exists(x="p")) rm(list = c("p"))
    if (exists(x="to.plot.mapa")) rm(list = c("to.plot.mapa"))
    
    # Filter data
    autoconsum_filt.dt <- autoconsum.dt[year%in%input$anys[1]:rev(input$anys)[1]]
    
    if (input$comarca != "Totes"){
      stopifnot(input$comarca%in%unique(autoconsum.dt$county))
      autoconsum_filt.dt <- autoconsum_filt.dt[county==input$comarca]
    }
    
    to.plot <- autoconsum_filt.dt %>% 
      .[,.(installations=.N),by="municipi"] %>%
      merge(pib_municipi.dt, by="municipi") %>%
      merge(poblacio_municipi.dt, by="municipi") %>%
      merge(pes_habitatge_unifamiliar_municipi.dt[,c("municipi","percentage_habitatges_unifamiliars")], by="municipi") %>%
      # merge(ibi_bonificacio.dt[,c("municipi","bonificacio")], by="municipi",all.x=T) %>%
       .[,number_installations_capita:=1000*installations/population_size]

    linear.model <- lm(formula=number_installations_capita~percentage_habitatges_unifamiliars+pib_capita+population_size, data=to.plot)
    # summary(linear.model)
    
    to.plot[,residuals:=round(linear.model[["residuals"]],1)]
    
    p1 <- ggplot(to.plot[abs(residuals)>2.5], aes(x=reorder(municipi,residuals), fill=residuals, y=residuals, tooltip=municipi, data_id=municipi)) +
      geom_bar_interactive(stat="identity", position = position_dodge(), color="black") +
      scale_x_discrete(drop=FALSE) +
      scale_fill_gradient(low = "red", high = "green") +
      theme_classic() +
      labs(x="", y="Model residuals") +
      guides(x = guide_axis(angle = 90)) +
      theme(
        axis.text = element_text(size=rel(1.25), color="black"),
        axis.title = element_text(size=rel(1.5), color="black")
      )
    
    to.plot.mapa <- catalunya_mapa_municipis.dt %>% merge(to.plot[,c("municipi","residuals")], by="municipi",all.x=T) %>%
      .[,municipi:=factor(municipi)]
    # to.plot.text <- catalunya_mapa_municipis_centre.dt %>% merge(to.plot[,c("residuals","municipi")], by="municipi")
    
    p2 <- ggplot(to.plot.mapa, aes(x=long, y=lat)) +
      geom_polygon_interactive(aes(group=group, fill=residuals, tooltip=municipi, data_id=municipi), alpha = 0.8, size = 0.05) +
      # geom_text(aes(label=residuals), data=to.plot.text, size=, color="black") +
      scale_fill_gradient(low = "red", high = "green") +
      theme_void() +
      theme(
        legend.position = "none",
        legend.title = element_blank(),
        panel.background = element_rect(size= 0.5, color = "white", fill = "white")
      )
    
    rm(to.plot.mapa)
    
    girafe(
      ggobj = plot_grid(p1, p2, nrow=1, scale=0.9, rel_widths = c(3/5,2/5)),
      width_svg = 23, height_svg = 10,
      options = list(
        # opts_sizing(rescale = FALSE),
        # opts_tooltip(offx = 20, offy = 20),
        opts_selection(type = "single", css = "cursor:pointer;fill:purple"),
        opts_hover_inv(css = "opacity:0.65;"),
        opts_hover(css = "cursor:pointer")
      )
    ) %>% return(.)
    
  })
  
  output$ModelEstadisticMunicipal = renderGirafe({
    return(plotModelEstadisticMunicipal())
  })

  #####################################
  ## Municipis amb millor creixement ##
  #####################################
  
  # select one and do scatterplot of year vs ninstalations
  
  plotCreixementPerMunicipi <- reactive({
    
    if (exists(x="p1")) rm(list = c("p1"))
    if (exists(x="p2")) rm(list = c("p2"))
    # if (exists(x="p")) rm(list = c("p"))
    if (exists(x="to.plot.mapa")) rm(list = c("to.plot.mapa"))
    
    # Filter data
    autoconsum_filt.dt <- autoconsum.dt[year%in%input$anys[1]:rev(input$anys)[1]]
    if (input$comarca != "Totes"){
      stopifnot(input$comarca%in%unique(autoconsum.dt$county))
      autoconsum_filt.dt <- autoconsum_filt.dt[county==input$comarca]
    }
    autoconsum_filt.dt <- autoconsum_filt.dt %>%
      .[,temps:=c("past","present")[as.numeric(as.character(year)>=2020)+1]]
    
    to.plot <- autoconsum_filt.dt %>% 
      .[,.(installations=.N),by=c("municipi","temps")] %>%
      merge(poblacio_municipi.dt, by="municipi") %>%
      .[,number_installations_capita:=1000*installations/population_size] %>%
      dcast(municipi+population_size~temps,value.var="number_installations_capita", fill = 0) %>%
      .[,diff:=present-past] %>% setorder(-diff)
    
    to.plot <- to.plot[diff>=0]
    to.plot2 <- rbind(head(to.plot[population_size>=1000],n=25), tail(to.plot[population_size>=1000],n=25))
    
    p1 <- ggplot(to.plot2, aes(x=reorder(municipi,diff), y=diff, fill=diff, tooltip=municipi, data_id=municipi)) +
      geom_bar_interactive(stat="identity", position = position_dodge(), color="black") +
      scale_x_discrete(drop=TRUE) +
      scale_fill_gradient(low = "red", high = "green") +
      theme_classic() +
      labs(x="", y="Diferencia en nombre d'instalacions per 1000 habitants") +
      guides(x = guide_axis(angle = 90)) +
      theme(
        axis.text = element_text(size=rel(1.25), color="black"),
        axis.title = element_text(size=rel(1.5), color="black")
      )
    
    to.plot.mapa <- catalunya_mapa_municipis.dt %>% merge(to.plot[,c("municipi","diff")], by="municipi",all.x=T) %>%
      .[,municipi:=factor(municipi)]
    
    # to.plot.text <- catalunya_mapa_municipis_centre.dt %>% merge(to.plot[,c("diff","municipi")], by="municipi")
    
    p2 <- ggplot(to.plot.mapa, aes(x=long, y=lat)) +
      geom_polygon_interactive(aes(group=group, fill=diff, tooltip=municipi, data_id=municipi), alpha = 0.8, size = 0.05) +
      # geom_text(aes(label=diff), data=to.plot.text, size=, color="black") +
      scale_fill_gradient(low = "red", high = "green") +
      theme_void() +
      theme(
        legend.position = "none",
        legend.title = element_blank(),
        panel.background = element_rect(size= 0.5, color = "white", fill = "white")
      )
    
    rm(to.plot.mapa)
    
    girafe(
      ggobj = plot_grid(p1, p2, nrow=1, scale=0.9, rel_widths = c(3/5,2/5)),
      width_svg = 23, height_svg = 10,
      options = list(
        # opts_sizing(rescale = FALSE),
        # opts_tooltip(offx = 20, offy = 20),
        opts_selection(type = "single", css = "cursor:pointer;fill:purple"),
        opts_hover_inv(css = "opacity:0.65;"),
        opts_hover(css = "cursor:pointer")
      )
    ) %>% return(.)
    
  })
  
  output$CreixementPerMunicipi = renderGirafe({
    return(plotCreixementPerMunicipi())
  })
  
  
  ####################################
  ## Efecte de la bonificacio d'IBI ##
  ####################################
  
  plotBonificacioIBI <- reactive({
    
    # Filter data
    autoconsum_filt.dt <- autoconsum.dt[year%in%input$anys[1]:rev(input$anys)[1]]
    if (input$comarca != "Totes"){
      stopifnot(input$comarca%in%unique(autoconsum.dt$county))
      autoconsum_filt.dt <- autoconsum.dt[county==input$comarca]
    }
    
    to.plot <- autoconsum_filt.dt %>% 
      .[,.(installations=.N),by=c("municipi")] %>%
      merge(poblacio_municipi.dt, by="municipi") %>%
      .[,number_installations_capita:=1000*installations/population_size] %>%
      merge(pes_habitatge_unifamiliar_municipi.dt, by="municipi") %>%
      merge(ibi_bonificacio.dt, by="municipi",all.x=T) %>%
      .[,bonificacio:=as.factor(c("No bonificacio IBI","Si bonificacio IBI")[as.numeric(!is.na(percentage_bonificacio))+1])]
  
    # Modificacions manuals
    to.plot[municipi=="Santa Eulàlia de Ronaçana",bonificacio:="Si bonificacio IBI"]
    to.plot[municipi=="Alella",bonificacio:="Si bonificacio IBI"]
    to.plot[municipi=="Gurb",bonificacio:="Si bonificacio IBI"]
    to.plot[municipi=="Cabrils",bonificacio:="Si bonificacio IBI"]
    to.plot[municipi=="Taradell",bonificacio:="Si bonificacio IBI"]
    to.plot[municipi=="Cardedeu",bonificacio:="Si bonificacio IBI"]
    to.plot[municipi=="Lliçà d Amunt",bonificacio:="Si bonificacio IBI"]
    to.plot[municipi=="Argentona",bonificacio:="Si bonificacio IBI"]
    to.plot[municipi=="Sant Fruitós de Bages",bonificacio:="Si bonificacio IBI"]
    
    p1 <- ggboxplot(to.plot, x="bonificacio", y="number_installations_capita", fill="bonificacio") +
      stat_compare_means(method="wilcox.test") +
      facet_wrap(~tipus_municipi, scales="free_x") +
      labs(x="", y="Number of installations per thousand people") +
      theme(
        legend.position = "none",
        axis.text.y = element_text(size=rel(1)),
        axis.title.y = element_text(size=rel(1.1)),
        strip.text = element_text(size=rel(1.25)),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
    
    to.plot.mapa <- catalunya_mapa_municipis.dt %>% merge(ibi_bonificacio.dt[,c("municipi","percentage_bonificacio")], by="municipi",all.x=T) %>%
      .[,bonificacio:=as.factor(c("No bonificacio IBI","Si bonificacio IBI")[as.numeric(!is.na(percentage_bonificacio))+1])]
    
    p2 <- ggplot(to.plot.mapa, aes(x=long, y=lat)) +
      geom_polygon(aes(group=group, fill=bonificacio), alpha = 0.8, size = 0.05) +
      scale_fill_manual(values = c("gray60","blue")) +
      theme_void() +
      theme(
        legend.position = "top",
        legend.title = element_blank(),
        panel.background = element_rect(size= 0.5, color = "white", fill = "white")
      )
    
    cowplot::plot_grid(plotlist=list(p1,p2), rel_heights = c(1/2,1/2), nrow=1) %>% return(.)
    
  })
  
  output$BonificacioIBI = renderPlot({
    return(plotBonificacioIBI())
  })
  
} # end server
