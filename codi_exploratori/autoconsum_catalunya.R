library(data.table)
library(purrr)
library(ggplot2)
library(ggpubr)

basedir <- "/Users/ricard/test/autoconsum_catalunya/dades"
outdir <- "/Users/ricard/test/autoconsum_catalunya/shiny/dades"

autoconsum.dt <- fread(paste0(basedir,"/instalacions_autoconsum.csv.gz")) %>%
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


  
pib_comarca.dt <- fread(paste0(basedir,"/pib_comarca.csv.gz")) %>%
  .[,c(2,5)] %>% setnames(c("county","pib_capita"))# %>%

  # .[,pib_capita:=as.numeric(pib_capita)]

# only avaialble for relatively citiws with >5000 hab
pib_municipi.dt <- fread(paste0(basedir,"/pib_municipi.csv.gz")) %>%
  .[,c(2,4)] %>% setnames(c("municipality","pib_capita")) %>%
  .[,municipality:=gsub("\\.",",",municipality)]
unique(autoconsum.dt$municipality)[!unique(autoconsum.dt$municipality) %in% pib_municipi.dt$municipality]

# rfdb_municipi.dt <- fread(paste0(basedir,"/rfdb_municipi.csv.gz")) %>%
#   .[,c(1,3)] %>% setnames(c("municipality","rfdb_capita")) %>%
#   .[,rfdb_capita:=as.numeric(gsub("\\,","",rfdb_capita))]
# unique(autoconsum.dt$municipality)[!unique(autoconsum.dt$municipality) %in% rfdb_municipi.dt$municipality]



pluviometria_comarca.dt <- fread(paste0(basedir,"/pluviometria.txt.gz"), header=T) %>%
  .[,Estaciones:=NULL] %>%
  setnames("V1","county") %>%
  melt(id.vars="county", variable.name="month", value.name="rain") %>%
  .[,.(rain=sum(rain)),by="county"]

poblacio_municipi.dt <- fread(paste0(basedir,"/poblacio_municipi.csv.gz"), header=T) %>%
  .[,c(2,10)] %>% setnames(c("municipality","population_size"))  %>%
  .[population_size>=500]
# unique(autoconsum.dt$municipality)[!unique(autoconsum.dt$municipality) %in% poblacio_municipi.dt$municipality]

poblacio_comarca.dt <- fread(paste0(basedir,"/densitat_poblacio_comarca.csv.gz"), header=T) %>%
  setnames(c("county","population_size","surface_km2","density_km2")) %>%
  .[,log_density_km2:=log2(density_km2)]


pes_habitatge_unifamiliar.dt <- fread(paste0(basedir,"/pes_habitatge_unifamiliar.csv.gz"), select=c(2,3,5,6)) %>%
  setnames(c("municipality","un_habitatge","multiples_habitatges","total")) %>%
  .[total>=500] %>%
  .[,percentage_habitatges_unifamiliars:=un_habitatge/total] %>%
  .[,tipus_municipi:=c("ciutat","poble")[as.numeric(percentage_habitatges_unifamiliars>0.50)+1]]

ibi_bonificacio.dt <- fread(paste0(basedir,"/bonificacions_ibi_municipis.csv"), select=c(1,2,3,4)) %>%
  setnames(c("municipality","provincia","percentage_bonificacio","anys_bonificacio")) %>%
  .[municipality=="El Masnou",municipality:="Masnou, el"] %>%
  .[municipality=="El Prat de Llobregat",municipality:="Prat de Llobregat, el"] %>%
  .[municipality%in%autoconsum.dt$municipality]

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

ggscatter(to.plot, x="number_installations_capita", y="rfdb_capita",
          add="reg.line", add.params = list(color="blue", fill="lightgray"), conf.int=TRUE) +
  stat_cor(method = "pearson") +
  # coord_cartesian(ylim=c(100,300)) +
  facet_wrap(~year, scales="free_x") +
  labs(x="Number of installations per thousand people (x1000)", y="RFDB capita") +
  theme(
    axis.text = element_text(size=rel(0.75))
  )

###############################
## Pes habitatge unifamiliar ##
###############################

to.plot <- autoconsum.dt %>%
  .[,.(installations=.N),by=c("municipality")] %>% 
  merge(poblacio_municipi.dt, by=c("municipality")) %>%
  .[,c("number_installations_capita"):=list(1000*(installations/population_size))] %>%
  merge(pes_habitatge_unifamiliar.dt, by=c("municipality"))

ggscatter(to.plot, x="percentage_habitatges_unifamiliars", y="number_installations_capita",
          add="reg.line", add.params = list(color="blue", fill="lightgray"), conf.int=TRUE) +
  stat_cor(method = "pearson") +
  labs(x="Pes d'habitatges unifamiliars (en percentatge)", y="Nombre d'instalacions per cada mil habitants") +
  theme(
    axis.text = element_text(size=rel(0.75))
  )

####################################
## Comparacio amb bonificacio IBI ##
####################################

to.plot <- autoconsum.dt %>%
  .[,.(installations=.N),by=c("municipality")] %>% 
  merge(poblacio_municipi.dt, by=c("municipality")) %>%
  .[,c("number_installations_capita"):=list(1000*(installations/population_size))] %>%
  merge(pes_habitatge_unifamiliar.dt[,c("percentage_habitatges_unifamiliars","tipus_municipi","municipality")], by="municipality") %>%
  merge(ibi_bonificacio.dt[,c("percentage_bonificacio","municipality")], by=c("municipality"),all.x=T) %>%
  .[,bonificacio:=c("No bonificacio IBI","Si bonificacio IBI")[as.numeric(!is.na(percentage_bonificacio))+1]]

ggboxplot(to.plot, x="bonificacio", y="number_installations_capita", fill="bonificacio") +
  stat_compare_means(method="wilcox.test") +
  facet_wrap(~tipus_municipi, scales="free_x") +
  labs(x="", y="Number of installations per thousand people") +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size=rel(0.75)),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


######################################
## Linear model with all statistics ##
######################################
  

to.plot <- autoconsum.dt %>%
  .[,.(installations=.N),by=c("municipality")] %>% 
  merge(poblacio_municipi.dt, by=c("municipality")) %>%
  .[,c("number_installations_capita"):=list(1000*(installations/population_size))] %>%
  merge(pes_habitatge_unifamiliar.dt[,c("percentage_habitatges_unifamiliars","tipus_municipi","municipality")], by="municipality") %>%
  merge(ibi_bonificacio.dt[,c("percentage_bonificacio","municipality")], by=c("municipality"),all.x=T) %>%
  merge(pib_municipi.dt, by="municipality") %>%
  .[,bonificacio:=c("No bonificacio IBI","Si bonificacio IBI")[as.numeric(!is.na(percentage_bonificacio))+1]]

linear.model <- lm(formula=number_installations_capita~percentage_habitatges_unifamiliars+bonificacio+pib_capita, data=to.plot)
summary(linear.model)
to.plot[,residuals:=linear.model[["residuals"]]]

ggbarplot(to.plot[abs(residuals)>3], x="municipality", y="residuals", fill="residuals", sort.val = "asc") +
  scale_fill_gradient(low = "red", high = "green") +
  labs(x="", y="Linear model residual") +
  guides(x = guide_axis(angle = 90)) +
  theme(
    axis.text = element_text(size=rel(0.75))
  )
  
#############
## Explore ##
#############

# pib_municipi.dt %>% copy %>%
#   .[,ribes:=municipality=="Sant Pere de Ribes"] %>%
#   setorder(-pib_capita) %>%
#   .[,municipality:=factor(municipality,levels=municipality)]
# 
# ggbarplot(pib_municipi.dt, x="municipality", y="pib_capita", fill="ribes") +
#   labs(x="", y="PIB capita") +
#   scale_fill_manual(values=c("TRUE"="red", "FALSE"="gray50")) +
#   guides(x = guide_axis(angle = 90)) +
#   theme(
#     legend.position = "none",
#     axis.text = element_text(size=rel(0.5))
#   )

municips_per_comarca.dt <- fread(paste0(basedir,"/municipis_per_comarca_catalunya.csv")) %>%
  .[,1:4] %>% setnames(c("codi_municipi","municipi","codi_comarca","comarca"))# %>%

# unique(municips_per_comarca.dt$comarca)[!unique(municips_per_comarca.dt$comarca)%in% comarques]
# grep("Urgell",comarques, value=T)
# grep("Urgell",unique(municips_per_comarca.dt$comarca), value=T)

###########
## Shiny ##
###########

municipis <- unique(to.plot$municipality)
saveRDS(municipis, paste0(outdir,"/municipis.rds"))

comarques <- unique(to.plot$county)
saveRDS(comarques, paste0(outdir,"/comarques.rds"))

anys <- names(which(table(autoconsum.dt$year)>25))
saveRDS(anys, paste0(outdir,"/anys.rds"))

ibi_bonificacio_to_save.dt <- ibi_bonificacio.dt %>% setnames("municipality","municipi")
fwrite(ibi_bonificacio_to_save.dt, paste0(outdir,"/ibi_bonificacio.txt.gz"))

autoconsum_to_save.dt <- autoconsum.dt %>% setnames("municipality","municipi")
fwrite(autoconsum_to_save.dt, paste0(outdir,"/instalacions_autoconsum.txt.gz"))

pes_habitatge_unifamiliar_to_save.dt <- pes_habitatge_unifamiliar.dt[municipi!="Catalunya"]
fwrite(pes_habitatge_unifamiliar_to_save.dt, paste0(outdir,"/pes_habitatge_unifamiliar.txt.gz"))

pes_habitatge_unifamiliar_comarca_to_save.dt <- fread(paste0(basedir,"/pes_habitatge_unifamiliar.csv.gz"), select=c(2,3,5,6)) %>%
  setnames(c("municipi","un_habitatge","multiples_habitatges","total")) %>%
  merge(municipis_per_comarca.dt[,c("municipi","comarca")],by="municipi") %>%
  .[,.(un_habitatge=sum(un_habitatge), multiples_habitatges=sum(multiples_habitatges), total=sum(total)),by="comarca"] %>%
  .[,percentage_habitatges_unifamiliars:=un_habitatge/total]
fwrite(pes_habitatge_unifamiliar_comarca_to_save.dt, paste0(outdir,"/pes_habitatge_unifamiliar_comarca.txt.gz"))

pib_municipi_to_save.dt <- pib_municipi.dt[municipality!="Catalunya"]
fwrite(pib_municipi_to_save.dt, paste0(outdir,"/pib_municipi.txt.gz"))

pib_comarca_to_save.dt <- pib_comarca.dt
fwrite(pib_comarca_to_save.dt, paste0(outdir,"/pib_comarca.txt.gz"))

poblacio_municipi_to_save.dt <- poblacio_municipi.dt[municipality!="Catalunya"]
fwrite(poblacio_municipi_to_save.dt, paste0(outdir,"/poblacio_municipi.txt.gz"))

# poblacio_comarca_to_save.dt <- poblacio_comarca.dt[,c(1,2,4)] %>% setnames(c("county","population_size","population_density"))
poblacio_comarca_to_save.dt <- poblacio_comarca.dt %>% setnames(c("county","population_size","population_density"))
fwrite(poblacio_comarca_to_save.dt, paste0(outdir,"/poblacio_comarca.txt.gz"))

municips_per_comarca_to_save.dt <- municips_per_comarca.dt[comarca%in%comarques]# %>% setnames(c("county","population_size","population_density"))
fwrite(municips_per_comarca_to_save.dt, paste0(outdir,"/municipis_per_comarca.txt.gz"))


