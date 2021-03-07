library(data.table)
library(purrr)
library(ggplot2)
library(ggpubr)

autoconsum.dt <- fread("/Users/ricard/Downloads/dades_catalunya_autoconsum/instalacions_autoconsum.csv") %>%
  .[,c(6,7,8,10,12)] %>%
  .[Tecnologia=="FOTOVOLTAICA"] %>% .[,Tecnologia:=NULL] %>%
  setnames(c("date","power","municipality","county")) %>%
  .[power<100] %>%
  .[,date:=as.Date(date, format="%d/%m/%Y")] %>%
  .[,month:=months(date, abbreviate=TRUE)] %>% 
  .[,quarter:=quarters(date)] %>%
  .[,year:=as.factor(year(date))] %>%
  .[year%in%as.character(2013:2021)] %>% droplevels
unique(autoconsum.dt$year)
# autoconsum.dt[year%in%c(2018,2019,2020,2021)]


  
pib_comarca.dt <- fread("/Users/ricard/Downloads/dades_catalunya_autoconsum/pib_comarca.csv") %>%
  .[,c(2,5)] %>% setnames(c("county","pib_capita"))# %>%
  # .[,pib_capita:=as.numeric(pib_capita)]

# only avaialble for relatively citiws with >5000 hab
pib_municipi.dt <- fread("/Users/ricard/Downloads/dades_catalunya_autoconsum/pib_municipi.csv") %>%
  .[,c(2,4)] %>% setnames(c("municipality","pib_capita")) %>%
  .[,municipality:=gsub("\\.",",",municipality)]
unique(autoconsum.dt$municipality)[!unique(autoconsum.dt$municipality) %in% pib_municipi.dt$municipality]

rfdb_municipi.dt <- fread("/Users/ricard/Downloads/dades_catalunya_autoconsum/rfdb_municipi.csv") %>%
  .[,c(1,3)] %>% setnames(c("municipality","rfdb_capita")) %>%
  .[,rfdb_capita:=as.numeric(gsub("\\,","",rfdb_capita))]
unique(autoconsum.dt$municipality)[!unique(autoconsum.dt$municipality) %in% rfdb_municipi.dt$municipality]



pluviometria_comarca.dt <- fread("/Users/ricard/Downloads/dades_catalunya_autoconsum/pluviometria.txt", header=T) %>%
  .[,Estaciones:=NULL] %>%
  setnames("V1","county") %>%
  melt(id.vars="county", variable.name="month", value.name="rain") %>%
  .[,.(rain=sum(rain)),by="county"]

poblacio_municipi.dt <- fread("/Users/ricard/Downloads/dades_catalunya_autoconsum/poblacio_municipi.csv", header=T) %>%
  .[,c(2,10)] %>% setnames(c("municipality","population_size")) 
# unique(autoconsum.dt$municipality)[!unique(autoconsum.dt$municipality) %in% poblacio_municipi.dt$municipality]

poblacio_comarca.dt <- fread("/Users/ricard/Downloads/dades_catalunya_autoconsum/densitat_poblacio_comarca.txt", header=T) %>%
  setnames(c("county","population_size","surface_km2","density_km2")) %>%
  .[,log_density_km2:=log2(density_km2)]

# poblacio_comarca.dt$county[!poblacio_comarca.dt$county%in%autoconsum.dt$county]
# .[,pib_capita:=as.numeric(pib_capita)]

###############################
## Overall changes over time ##
###############################

to.plot <- autoconsum.dt %>%
  .[,.(power=sum(power), installations=.N),by=c("quarter","year")] %>%
  .[!year==2021,c("daily_power","daily_installations"):=list(power/90,(installations/90))] %>%
  .[year==2021,c("daily_power","daily_installations"):=list(power/30,(installations/30))]

# to.plot %>% melt(id.vars=c("yearhead"))
ggbarplot(to.plot, x="year", fill="quarter", y="daily_installations", position = position_dodge(width=0.75)) +
  labs(x="", y="Number of installations (daily)")

ggbarplot(to.plot, x="year", y="daily_power") +
  labs(x="", y="Power installed (kWh, daily)")

########################
## Stratify by county ##
########################

to.plot <- autoconsum.dt %>%
  .[,.(power=sum(power), installations=.N),by=c("county","year")] %>% 
  merge(pib_comarca.dt, by=c("county")) %>%
  merge(poblacio_comarca.dt, by=c("county")) %>%
  merge(pluviometria_comarca.dt, by=c("county")) %>%
  .[,c("power_capita","number_installations_capita"):=list(power/population_size,1000*installations/population_size)]


ggscatter(to.plot, x="number_installations_capita", y="pib_capita",
          add="reg.line", add.params = list(color="blue", fill="lightgray"), conf.int=TRUE) +
  stat_cor(method = "pearson") +
  facet_wrap(~year, scales="free_x") +
  labs(x="Number of installations per thousand people", y="PIB capita") +
  scale_y_continuous(limits=c(0,160)) +
  theme(
    axis.text = element_text(size=rel(0.75))
  )


ggscatter(to.plot, x="number_installations_capita", y="rain",
          add="reg.line", add.params = list(color="blue", fill="lightgray"), conf.int=TRUE) +
  stat_cor(method = "pearson") +
  facet_wrap(~year, scales="free_x") +
  labs(x="Number of installations per thousand people", y="Rain ()") +
  scale_y_continuous(limits=c(0,420)) +
  theme(
    axis.text = element_text(size=rel(0.75))
  )

ggscatter(to.plot, x="number_installations_capita", y="log_density_km2",
          add="reg.line", add.params = list(color="blue", fill="lightgray"), conf.int=TRUE) +
  stat_cor(method = "pearson") +
  facet_wrap(~year, scales="free_x") +
  scale_y_continuous(limits=c(0,15)) +
  theme(
    axis.text = element_text(size=rel(0.75))
  )

ggbarplot(to.plot[year==2020], x="county", y="number_installations_capita", sort.val = "asc") +
  labs(x="", y="Number of installations per thousand people") +
  guides(x = guide_axis(angle = 90)) +
  theme(
    axis.text = element_text(size=rel(0.75))
  )



## El següent gràfic representa la presència d’instal·lacions fotovoltaiques d’autoconsum (nombre
# d’instal·lacions d’autoconsum fotovoltaic per cada 1.000 habitants) respecte al pes dels habitatges
# unifamiliars sobre el total d’habitatges en el municipi.


# Per acabar, s’analitza la relació entre la renda familiar disponible bruta per habitant i el nombre
# d’instal·lacions d’autoconsum fotovoltaic per cada 1.000 habitants. En aquest sentit, també es pot afirmar que existeix una certa correlació entre la renda i el nombre d’instal·lacions d’autoconsum fotovoltaic,
# de manera que, com més alt és el nivell de renda, més presència d’instal·lacions per cada 1.000 habitants hi ha.

######################################################
## Stratify by municipality, aggregate across years ##
######################################################

to.plot <- autoconsum.dt %>%
  .[,.(power=sum(power), installations=.N),by=c("municipality")] %>% 
  merge(pib_municipi.dt, by=c("municipality")) %>%
  merge(rfdb_municipi.dt, by=c("municipality")) %>%
  merge(poblacio_municipi.dt, by=c("municipality")) %>%
  .[,c("power_capita","number_installations_capita"):=list(power/population_size,1000*(installations/population_size))]


ggscatter(to.plot, x="number_installations_capita", y="rfdb_capita",
          add="reg.line", add.params = list(color="blue", fill="lightgray"), conf.int=TRUE) +
  stat_cor(method = "pearson") +
  labs(x="Number of installations per thousand people (x1000)", y="RFDB capita") +
  theme(
    axis.text = element_text(size=rel(0.75))
  )

linear.model <- lm(formula=number_installations_capita~rfdb_capita, data=to.plot)
to.plot[,residuals:=linear.model[["residuals"]]]

ggbarplot(to.plot[abs(residuals)>3], x="municipality", y="residuals", fill="residuals", sort.val = "asc") +
  scale_fill_gradient(low = "red", high = "green") +
  labs(x="", y="Linear model residual") +
  guides(x = guide_axis(angle = 90)) +
  theme(
    axis.text = element_text(size=rel(0.75))
  )


#######################################
## Stratify by municipality and year ##
#######################################

to.plot <- autoconsum.dt %>%
  .[,.(power=sum(power), installations=.N),by=c("municipality","year")] %>% 
  merge(pib_municipi.dt, by=c("municipality")) %>%
  merge(rfdb_municipi.dt, by=c("municipality")) %>%
  merge(poblacio_municipi.dt, by=c("municipality")) %>%
  # merge(pluviometria_comarca.dt, by=c("municipality")) %>%
  .[,c("power_capita","number_installations_capita"):=list(power/population_size,1000*(installations/population_size))]

to.plot <- to.plot[year%in%as.character(2019:2020)]

# Seleccionem els 20 municipis amb mes instalacions
# municipalities.to.plot <- to.plot[,sum(installations),by="municipality"] %>% setorder(-V1) %>% head(n=50) %>% .$municipality
# to.plot <- to.plot[municipality%in%municipalities.to.plot]

ggscatter(to.plot, x="number_installations_capita", y="pib_capita",
          add="reg.line", add.params = list(color="blue", fill="lightgray"), conf.int=TRUE) +
  stat_cor(method = "pearson") +
  facet_wrap(~year, scales="free_x") +
  labs(x="Number of installations per thousand people", y="PIB capita") +
  # scale_y_continuous(limits=c(0,160)) +
  theme(
    axis.text = element_text(size=rel(0.75))
  )

ggscatter(to.plot, x="number_installations_capita", y="rfdb_capita",
          add="reg.line", add.params = list(color="blue", fill="lightgray"), conf.int=TRUE) +
  stat_cor(method = "pearson") +
  # coord_cartesian(ylim=c(100,300)) +
  facet_wrap(~year, scales="free_x") +
  labs(x="Number of installations per thousand people (x1000)", y="RFDB capita") +
  theme(
    axis.text = element_text(size=rel(0.75))
  )


ggbarplot(to.plot[year==2020], x="municipality", y="number_installations_capita", sort.val = "asc") +
  labs(x="", y="Number of installations per thousand people") +
  guides(x = guide_axis(angle = 90)) +
  theme(
    axis.text = element_text(size=rel(0.75))
  )


#############
## Explore ##
#############

pib_municipi.dt %>% copy %>%
  .[,ribes:=municipality=="Sant Pere de Ribes"] %>%
  setorder(-pib_capita) %>%
  .[,municipality:=factor(municipality,levels=municipality)]

ggbarplot(pib_municipi.dt, x="municipality", y="pib_capita", fill="ribes") +
  labs(x="", y="PIB capita") +
  scale_fill_manual(values=c("TRUE"="red", "FALSE"="gray50")) +
  guides(x = guide_axis(angle = 90)) +
  theme(
    legend.position = "none",
    axis.text = element_text(size=rel(0.5))
  )
