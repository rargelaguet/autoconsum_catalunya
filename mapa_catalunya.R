# https://rpubs.com/albtorval/595824

library(rgdal) # para importar archivos shapefiles
library(broom) # Para transformar los archivos shapefiles 

basedir <- "/Users/ricard/test/autoconsum_catalunya/dades"
municipi_codi_ine.dt <- fread(paste0(basedir,"/poblacio_municipi.csv.gz"), select=c(1,2)) %>%
  setnames(c("codi_ine","municipi")) %>%
  .[,codi_ine:=as.integer(substr(codi_ine,1,nchar(codi_ine)-1))]


file <- "/Users/ricard/test/rgdal/MUC_TM_SHP_ETRS89"
shapefile_cat <- readOGR(dsn = file)

# id to name
noms_municipis.dt <- data.table(
  id = as.character(seq(0, nrow(shapefile_cat)-1)),
  # municipi = shapefile_cat$MUNICIPI,
  codi_ine = as.integer(shapefile_cat$CODI_INE)
) 

# fwrite(noms_municipis.dt, "/Users/ricard/test/autoconsum_catalunya/shiny/dades/municipis_mapa.txt")
noms_municipis.dt <- fread("/Users/ricard/test/autoconsum_catalunya/shiny/dades/municipis_mapa.txt", encoding = 'Latin-1') %>% 
  .[,id:=as.character(id)] %>%
  merge(municipi_codi_ine.dt,by="codi_ine")

# Shapefile to dataframe
cat.dt <- tidy(shapefile_cat) %>% as.data.table %>% 
  merge(noms_municipis.dt, by="id")
fwrite(cat.dt, "/Users/ricard/test/autoconsum_catalunya/shiny/dades/catalunya_mapa.txt.gz")

# Plot
ggplot(cat.dt, aes( x= long, y = lat, group = group)) +
  geom_polygon(fill = "black", alpha = 0.8, size = 0.05 ) +
  theme_void() +
  theme(
    panel.background = element_rect(size= 0.5, color = "white", fill = "white")
  )

#######################
## PIB municipi mapa ##
#######################

basedir <- "/Users/ricard/test/autoconsum_catalunya/dades"

pib_municipi.dt <- fread(paste0(basedir,"/pib_municipi.csv.gz")) %>%
  .[,c(2,4)] %>% setnames(c("municipi","pib_capita")) %>%
  .[,municipi:=gsub("\\.",",",municipi)]

autoconsum.dt <- fread(paste0(basedir,"/instalacions_autoconsum.csv.gz")) %>%
  .[,c(6,7,8,10,12)] %>%
  .[Tecnologia=="FOTOVOLTAICA"] %>% .[,Tecnologia:=NULL] %>%
  setnames(c("date","power","municipi","county")) %>%
  .[power<100] %>%
  .[,date:=as.Date(date, format="%d/%m/%Y")] %>%
  .[,year:=as.factor(year(date))] %>%
  .[year%in%as.character(2013:2021)] %>% droplevels %>%
  .[,.(installations=.N),by=c("municipi")]

mean(autoconsum.dt$municipi%in%unique(cat.dt$municipi))
to.plot <- cat.dt %>% merge(autoconsum.dt,all.x=T, by="municipi")

ggplot(to.plot, aes( x= long, y = lat, group = group)) +
  geom_polygon(aes(fill = installations), alpha = 0.8, size = 0.05 ) +
  theme_void() +
  theme(
    panel.background = element_rect(size= 0.5, color = "white", fill = "white")
  )


###############
### Comarques #
###############

catalunya_mapa_comarques.shapefile <- rgdal::readOGR("/Users/ricard/test/rgdal/comarques_catalunya")

# id to name
noms_comarques.dt <- data.table(
  id = as.character(seq(0, nrow(catalunya_mapa_comarques.shapefile)-1)),
  comarca = catalunya_mapa_comarques.shapefile$nom_comar
) %>% .[comarca=="Val d'Aran",comarca:="Aran"]

catalunya_mapa_comarques.dt <- tidy(catalunya_mapa_comarques.shapefile) %>% as.data.table %>%
  merge(noms_comarques.dt,by="id")
fwrite(catalunya_mapa_comarques.dt, "/Users/ricard/test/autoconsum_catalunya/shiny/dades/catalunya_mapa_comarques.txt.gz")

centers.dt <- catalunya_mapa_comarques.dt[,.(long=mean(long), lat=mean(lat)), by="comarca"]

# Plot
ggplot(catalunya_mapa_comarques.dt, aes( x= long, y = lat)) +
  geom_polygon(aes(group = group), fill = "black", alpha = 0.8, size = 0.05 ) +
  geom_text(aes(label = comarca), data=centers.dt, size=3, color = "black") +
  theme_void() +
  theme(
    panel.background = element_rect(size= 0.5, color = "white", fill = "white")
  )

