#title: "Silent cities dataset Figures"
#author: "Amandine Gasc and Jeremy Froideveaux"
#date: "2023-10-31"

# Libraries and functions
LIBList <- c("dplyr","ggplot2","countrycode","maps","mapproj","lattice","chron","lubridate","DHARMa","data.table","glmmTMB","emmeans","ggplot2","grid","gridExtra","cowplot","performance","ggeffects","MuMIn","data.table","purrr","AICcmodavg","bestNormalize","viridis","scales")
lapply(LIBList, require, character.only = TRUE)

source("https://raw.githubusercontent.com/jbryer/makeR/master/R/calendarHeat.R")



# Data Load: thoses table can be found at https://osf.io/h285u/
Site<-read.csv("Site.csv",fileEncoding="UTF-8")

Confinement<-read.csv("ConfinementMeasures.csv",fileEncoding="UTF-8")%>%
  mutate(entity_date_local_USformat=paste(entity,day_USformat,sep="_"))

csvnames_dB<-basename(list.files(path='dB',full.names=TRUE))
DATABASE_dB <- list.files(path='dB',full.names=TRUE) %>% 
						lapply(read.csv) %>% 
            setNames(.,gsub(".csv","",csvnames_dB))%>%
            bind_rows(.id = "partID")%>%
            mutate(datetime_UTC=as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S",tz="GMT"))%>%
            left_join(.,y=Site[,colnames(Site)%in%c("partID","beck2018_koppengeiger_climate_corrected","country","timezone")],by = "partID")%>%
            select(.,-c(Unnamed..0,X))
rm(csvnames_dB)

datetime_local<-DATABASE_dB%>%
		          	rowwise()%>%
                do(datetime_local=as.character(format(with_tz(.$datetime_UTC,tzone=.$timezone),format="%Y-%m-%d %H:%M:%S")))%>%
			          unlist(.$datetime_local)

DATABASE_dB$datetime_local<-datetime_local
rm(datetime_local)  

DATABASE_dB_all <-DATABASE_dB%>%
              mutate(date_local_USformat=format(as.POSIXct(datetime_local,format = "%Y-%m-%d %H:%M:%S"), format = "%m/%d/%Y"))%>%
              mutate(entity_date_local_USformat=paste(country,date_local_USformat,sep="_"))%>%
              left_join(.,Confinement[,colnames(Confinement)%in%c("stay_home_requirements","entity_date_local_USformat")], by="entity_date_local_USformat")
rm(DATABASE_dB)

csvnames_speech<-basename(list.files(path='AcousticMeasurements_renamed_withspeech',full.names=TRUE))
DATABASE_recordings_WithSpeech <- list.files(path='AcousticMeasurements_renamed_withspeech',full.names=TRUE) %>%
  lapply(read.csv) %>% 
  setNames(.,gsub(".csv","",csvnames_speech))
rm(csvnames_speech)


AveragedCompleteTable<-read.csv("AveragedCompleteTable.csv",fileEncoding="UTF-8")

AveragedCompleteTable_speech<-read.csv("AveragedCompleteTable_nospeech.csv",fileEncoding="UTF-8")

ValidTab<-read.csv("Table_Sound_Validation.csv",fileEncoding="UTF-8")
##Change to presence/absence data
ValidTab$Bio_catNum<-as.numeric(ValidTab$Bio_cat)-1
ValidTab$Bio_catPA<-ifelse(ValidTab$Bio_catNum=="0",0,1) 
ValidTab$Ant_catNum<-as.numeric(ValidTab$Ant_cat)-1
ValidTab$Ant_catPA<-ifelse(ValidTab$Ant_catNum=="0",0,1)
ValidTab$Geo_catNum<-as.numeric(ValidTab$Geo_cat)-1
ValidTab$Geo_catPA<-ifelse(ValidTab$Geo_catNum=="0",0,1)
##transform dB values
ValidTab$dBb<-10^(ValidTab$dB/20)
##Change serie as factor
ValidTab$serie<-as.factor(ValidTab$serie)



# Figures and results

## How many?

### Sites
nrow(Site)

### Participants
nrow(Participant)

### Countries
length(unique((Site$country)))

### Cities
length(unique((paste(Site$country,Site$city,sep="_"))))

### Recordings from the field
nrow(DATABASE_dB_all)

### 10-second samples after sound processing
sum(unlist(lapply(DATABASE_recordings_WithSpeech,function(x){nrow(x)})))

### 10-second samples after sound processing and exclusion of speech
sum(unlist(lapply(DATABASE_recordings_WithSpeech,function(x){nrow(filter(x,reject_speech=="0"))})))


### Sites per country (barplot)
Country.site<-as.data.frame(cbind(country=names(table(Site$country)),count=table(Site$country)))
Country.site$count<-as.numeric(Country.site$count)
Country.site$iso2 <- countrycode(Country.site$country, "country.name", "iso2c")
Country.site$continent <- countrycode(Country.site$iso2, "iso2c", "continent")

my_palette <- c("Americas" = "#99BBAD", "Asia" = "#EBD8B7", "Europe" = "#C6A9A3", "Oceania" = "#9A8194")
barplot1<-ggplot(Country.site,aes(x = reorder(country, count), y = count, fill = continent)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Sites per country ",
       x = "Country",
       y = "Number of sites")+
  coord_flip() +
  scale_fill_manual(values = my_palette)

barplot1


## When?
DATETIME2020<-DATABASE_dB_all$datetime_local

TT<-as.POSIXct(DATETIME2020, format = "%Y-%m-%d %H:%M:%S")
TT<- format(TT, format = "%Y-%m-%d")
TT2<-as.data.frame(table(TT))

### Reverse the color palette for the copied function
calendarHeat2 <- calendarHeat
body(calendarHeat2)[[23]] <- substitute(w2b <- c("#fed976", "#fc4e2a", "#b10026"))

### Plot
calendarHeat2(TT2[,1], TT2[,2],varname="Sampling effort: number of recordings", color="w2b")


## Where?
Country.site2<-Country.site
rownames(Country.site2)<-NULL
map.world <- map_data('world')
colnames(Country.site2)[1]<-"region"

Country.site2$region[Country.site2$region=="The Netherlands"]<-"Netherlands"
Country.site2$region[Country.site2$region=="United States of America"]<-"USA"
Country.site2$region[Country.site2$region=="United Kingdom"]<-"UK"
Country.site2$region[Country.site2$region=="Trinidad & Tobago"]<-"Trinidad"
Country.site2$region[Country.site2$region=="Democratic Republic of Congo"]<-"Democratic Republic of the Congo"
Country.site2$region[Country.site2$region=="Reunion Island"]<-"Reunion"

Table1<-NULL
for (i in 1:nrow(map.world))
  {
 
   if(any(Country.site2$region==map.world[i,5]))
  {
  Table1<-rbind(Table1,as.vector(as.character((Country.site2[Country.site2$region==map.world[i,5],]))))
    
  }else{
  
  Table1<-rbind(Table1,c(rep("NA",4)))
  }
}
colnames(Table1)<-c("region","count","iso2","continent")

worldSubset<-cbind(map.world,Table1[,-1])
worldSubset$count<-as.numeric(worldSubset$count)


Cities<-cbind(partID=Site$partID,
              Recorder=Site$recorder,
              Country=Site$country,
              City=Site$city)

Cities_coord<-merge(Cities,Site[,colnames(Site)%in%c("partID","city_lat","city_long")],by="partID")

### mapping

plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)


worldMAP <- ggplot()+
  
  geom_map(
    data = worldSubset, map = worldSubset,
    aes(as.numeric(long),as.numeric(lat), map_id = region, fill = count),
    color = "white", size = 0.1)+
  
  geom_point(
    data = Cities_coord,
    aes(as.numeric(city_long),as.numeric(city_lat)),colour="red",#,color=Recorder),
    alpha = 2,size=1)+
	scale_fill_viridis_c(
	name = "Number of sites",
	na.value = "grey",
	)+
  coord_map(xlim = c(-160, 160),ylim = c(-180, 180))+
  ggtitle("Country participation and site repartition") +
  plain

worldMAP 


## How?

### Protocol used
  dd<-Site$protocol
	df1 <- data.frame(value = table(dd), Field = names(table(dd)))

piechartProtocol<-	ggplot(df1, aes(x = "", y = value.Freq, fill = Field)) +
		geom_col(color = "black") +
		geom_text(aes(label = value.Freq),position = position_stack(vjust = 0.5)) +
		coord_polar(theta = "y") +
		scale_fill_manual(values=c("#1E5D62","#AACED1","#E9F2F3"))+
    theme_void()

piechartProtocol

### SM4 and audiomoth
  dd2<-Site$recorder_type
  df2 <- data.frame(value = table(dd2), Field = names(table(dd2)))

	piechartRecorder<-ggplot(df2, aes(x = "", y = value.Freq, fill = Field)) +
		geom_col(color = "black") +
		geom_text(aes(label = value.Freq),position = position_stack(vjust = 0.5)) +
		coord_polar(theta = "y") +
		scale_fill_manual(values=c("#1E5D62","#AACED1","#E9F2F3"))+
    theme_void()
	
	piechartRecorder


## Recording data with confinement data description (with total dataset)
colnames(DATABASE_dB_all)[colnames(DATABASE_dB_all)=="beck2018_koppengeiger_climate_corrected"]<-"Climat"
DATABASE_dB_all$Climat[ DATABASE_dB_all$Climat=="A"] <-"Tropical"
DATABASE_dB_all$Climat[ DATABASE_dB_all$Climat=="B"] <-"Dry"
DATABASE_dB_all$Climat[ DATABASE_dB_all$Climat=="C"] <-"Temperate"
DATABASE_dB_all$Climat[ DATABASE_dB_all$Climat=="D"] <-"Continetal"
DATABASE_dB_all$Climat[ DATABASE_dB_all$Climat=="E"] <- "Polar"

barplotconfinement<-ggplot(DATABASE_dB_all)+
geom_bar(aes(x=stay_home_requirements, fill=Climat))+
labs(x="Confinements levels", y="Number of recordings")+
scale_fill_manual(values=c("#2B4CC3", "#FDAC14", "#24889E", "#2BA932"), 
									 name="Climate")

barplotconfinement


# GLMMS: acoustic indices 

## Biophony

### Models
M1<-glmmTMB(Bio_catPA~scale(ndsi_W)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M2<-glmmTMB(Bio_catPA~scale(dBb)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M3<-glmmTMB(Bio_catPA~scale(aci_W)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M4<-glmmTMB(Bio_catPA~scale(ACT)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M5<-glmmTMB(Bio_catPA~scale(EAS_W)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M6<-glmmTMB(Bio_catPA~scale(ECV_W)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M7<-glmmTMB(Bio_catPA~scale(EPS_W)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M8<-glmmTMB(Bio_catPA~scale(BI_W)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")

## Anthropophony

### Models
M9<-glmmTMB(Ant_catPA~scale(ndsi_W)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M10<-glmmTMB(Ant_catPA~scale(dBb)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M11<-glmmTMB(Ant_catPA~scale(aci_W)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M12<-glmmTMB(Ant_catPA~scale(ACT)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M13<-glmmTMB(Ant_catPA~scale(EAS_W)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M14<-glmmTMB(Ant_catPA~scale(ECV_W)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M15<-glmmTMB(Ant_catPA~scale(EPS_W)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M16<-glmmTMB(Ant_catPA~scale(BI_W)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
```

## Geophony

### Models
M17<-glmmTMB(Geo_catPA~scale(ndsi_W)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M18<-glmmTMB(Geo_catPA~scale(dBb)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M19<-glmmTMB(Geo_catPA~scale(aci_W)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M20<-glmmTMB(Geo_catPA~scale(ACT)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M21<-glmmTMB(Geo_catPA~scale(EAS_W)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M22<-glmmTMB(Geo_catPA~scale(ECV_W)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M23<-glmmTMB(Geo_catPA~scale(EPS_W)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M24<-glmmTMB(Geo_catPA~scale(BI_W)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")

# GLMMS: tagging type

## Models Biophony
M25<-glmmTMB(Bio_catPA~scale(geophony)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M26<-glmmTMB(Bio_catPA~scale(anthropophony)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M27<-glmmTMB(Bio_catPA~scale(biophony)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")

## Models Anthropophony
M28<-glmmTMB(Ant_catPA~scale(geophony)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M29<-glmmTMB(Ant_catPA~scale(anthropophony)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M30<-glmmTMB(Ant_catPA~scale(biophony)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")

## Models Geophony
M31<-glmmTMB(Geo_catPA~scale(geophony)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M32<-glmmTMB(Geo_catPA~scale(anthropophony)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")
M33<-glmmTMB(Geo_catPA~scale(biophony)+(1|Site)+ar1(serie-1|Site),data=ValidTab,family="binomial")


# GLMMS: Output of the models

## Output of the models (to run for each model, here an example with M1)
summary(M1)

## Check model residuals (to run for each model, here an example with M1)
simulationOutput <- simulateResiduals(fittedModel = M1,quantreg=T)
plot(simulationOutput) 

# Relationship between acoustic values and confinement measures

## Data manipulation
AveragedCompleteTable2<-AveragedCompleteTable
AveragedCompleteTable2$localtime_only <- as.numeric(substr(AveragedCompleteTable2$datetime_local, 12, 13))
AveragedCompleteTable2$stay_home_requirements<-as.factor(AveragedCompleteTable2$stay_home_requirements)
AveragedCompleteTable2$recorder<-as.factor(AveragedCompleteTable2$recorder)
AveragedCompleteTable2$clim<-as.factor(AveragedCompleteTable2$beck2018_koppengeiger_climate_corrected)

datetime_UTC<-as.POSIXlt(AveragedCompleteTable2$datetime_local,format = "%Y-%m-%d %H:%M:%S",tz="GMT")

AveragedCompleteTable2$julianDay<- datetime_UTC$yday 
AveragedCompleteTable2$dBb<-10^(AveragedCompleteTable2$dB/20)
AveragedCompleteTable2$stay_home_requirements[AveragedCompleteTable2$stay_home_requirements == '3'] <- '2'
AveragedCompleteTable2$time_local<-format(as.POSIXct(AveragedCompleteTable2$datetime_local, format = "%Y-%m-%d %H:%M:%S",tz="UTC"),format="%H:%M")
AveragedCompleteTable2$time_local<-as.ITime(AveragedCompleteTable2$time_local)

## Data vizualization: focus on anthropophony and biophony   
ddf2<- AveragedCompleteTable2 %>%
  group_by(stay_home_requirements, localtime_only,partID) %>%
  summarize(min_ant = mean(anthropophony))

levels(ddf2$stay_home_requirements)<-c( "No measures","Recommended not to leave home", "Not allowed to leave home","Not allowed to leave home")

ggplot(ddf2, aes(x = localtime_only,y=min_ant)) + 
  geom_hline(yintercept = 0.10, color = "gray90") +
  geom_hline(yintercept = 0.15, color = "gray90") +
  geom_hline(yintercept = 0.20, color = "gray90") +
  geom_hline(yintercept = 0.25, color = "gray90") +
  geom_bar(aes(fill=stay_home_requirements), position = "dodge", stat = "summary",fun="mean")+ coord_polar(start = -0.12,direction=1) +
  scale_x_continuous(breaks=seq(0, 24, by=2), expand=c(0,0))+
  ylab("Anthropophony level")+xlab("")+
  scale_fill_brewer(name = expression(paste("COVID-19 \n measures")),palette = "Set2",labels = scales::label_wrap(20))+
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_blank(),
        panel.grid.major = element_blank())+
  theme(axis.text = element_text(size=10))+
  theme(axis.title.y = element_text(face="bold",size=14))+
  theme(axis.title=element_text(margin=margin(10,10,0,0)))+
  theme(legend.position="bottom")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


ddf3<- AveragedCompleteTable2 %>%
  group_by(stay_home_requirements, localtime_only,partID) %>%
  summarize(min_ant = mean(biophony))
levels(ddf3$stay_home_requirements)<-c( "No measures","Recommended not to leave home", "Not allowed to leave home","Not allowed to leave home")

ggplot(ddf3, aes(x = localtime_only,y=min_ant)) + 
  geom_hline(yintercept = 0.01, color = "gray90") +
  geom_hline(yintercept = 0.05, color = "gray90") +
  geom_hline(yintercept = 0.075, color = "gray90") +
  geom_hline(yintercept = 0.10, color = "gray90") +
  geom_hline(yintercept = 0.125, color = "gray90") +
  geom_hline(yintercept = 0.15, color = "gray90") +
  geom_bar(aes(fill=stay_home_requirements), position = "dodge", stat = "summary",fun="mean")+ coord_polar(start = -0.12,direction=1) +
  scale_x_continuous(breaks=seq(0, 24, by=2), expand=c(0,0))+
  ylab("Biophony level")+xlab("")+
  scale_fill_brewer(name = expression(paste("COVID-19 \n measures")),palette = "Set2",labels = scales::label_wrap(20))+
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_blank(),
        panel.grid.major = element_blank())+
  theme(axis.text = element_text(size=10))+
  theme(axis.title.y = element_text(face="bold",size=14))+
  theme(axis.title=element_text(margin=margin(10,10,0,0)))+
  theme(legend.position="bottom")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

## Modelling 

### Subset
dfexp<-subset(AveragedCompleteTable2,protocol=="Expert")
dfexp9<-dfexp[with(dfexp, time_local == as.ITime("08:00")), ]
summary(factor(dfexp9$time_local))


### Principal Component Analysis (PCA) to summarize the level of anthropization in the landscape
Z<-cbind(dfexp9$urban1km,dfexp9$population_density,dfexp9$footprint)
dfexp9$urban1kmL <- log(dfexp9$urban1km)
dfexp9$popdensityL <- log(dfexp9$population_density)
dfexp9$footprintL <- log(dfexp9$footprint)
dfexp92 <- data.frame(urban1km=dfexp9$urban1kmL,
                      popdensityg=dfexp9$popdensityL,
                      footprint=dfexp9$footprintL)
ir.pca <- prcomp(dfexp92,center = TRUE,scale. = TRUE) 
plot(ir.pca, type = "l")
summary(ir.pca)
ev = ir.pca$sdev^2

### Select the first axis of the PCA
dfexp9$PCA<- -ir.pca$x[,1] 


### GLMMs

#### Model on NDSI
dfexp9<-dfexp9%>%
filter(!is.na(dfexp9$NDSI))
dfexp9$NDSI<-(dfexp9$NDSI-min(dfexp9$NDSI))/(max(dfexp9$NDSI)-min(dfexp9$NDSI))
dfexp9$NDSI<-ifelse(dfexp9$NDSI=="0",0.0001,dfexp9$NDSI)
dfexp9$NDSI<-ifelse(dfexp9$NDSI=="1",0.9999,dfexp9$NDSI)

M1<-glmmTMB(NDSI~stay_home_requirements+scale(PCA)+scale(julianDay)+clim+(1|country/partID)+(1|recorder),data=dfexp9,family=beta_family(link="logit"))
summary(M1)

simulationOutputM1 <- simulateResiduals(fittedModel = M1)
plot(simulationOutputM1)
check_collinearity(M1)

M01<-glmmTMB(NDSI~1+(1|country/partID)+(1|recorder),data=dfexp9,family=beta_family(link="logit"))
AIC(M1)-AIC(M01)

hoc<-lsmeans(M1, pairwise~stay_home_requirements)
hoc
eff1<-ggeffect(M1, terms = c("stay_home_requirements"))

#### Model on engine noise
dfexp9$motor<-dfexp9$tag_motorbike+dfexp9$tag_car+dfexp9$tag_others_motors

M2<-glmmTMB(motor~stay_home_requirements+scale(PCA)+scale(julianDay)+clim+(1|country/partID)+(1|recorder),data=dfexp9,family=beta_family(link="logit"))
summary(M2)

simulationOutputM2 <- simulateResiduals(fittedModel = M2)
plot(simulationOutputM2)
check_collinearity(M2)

M02<-glmmTMB(motor~1+(1|country/partID)+(1|recorder),data=dfexp9,family=beta_family(link="logit"))
AIC(M2)-AIC(M02)

hoc<-lsmeans(M2, pairwise~stay_home_requirements)
hoc

eff2<-ggeffect(M2, terms = c("stay_home_requirements"))

#### Model on bird
M3<-glmmTMB(tag_bird~stay_home_requirements+scale(PCA)+scale(julianDay)+clim+(1|country/partID)+(1|recorder),data=dfexp9,family=beta_family(link="logit"))
summary(M3)

simulationOutputM3 <- simulateResiduals(fittedModel = M3)
plot(simulationOutputM3)
check_collinearity(M3)

M03<-glmmTMB(tag_bird~1+(1|country/partID)+(1|recorder),data=dfexp9,family=beta_family(link="logit"))
AIC(M3)-AIC(M03)

hoc<-lsmeans(M3, pairwise~stay_home_requirements)
hoc

eff3<-ggeffect(M3, terms = c("stay_home_requirements"))


### Plotting the model prediction
levels(dfexp9$stay_home_requirements)<-c( "No measures","Recommended not to leave home", "Not allowed to leave home","Not allowed to leave home")
levels(eff1$x)<-c( "No measures","Recommended not to leave home", "Not allowed to leave home")
levels(eff2$x)<-c( "No measures","Recommended not to leave home", "Not allowed to leave home")
levels(eff3$x)<-c( "No measures","Recommended not to leave home", "Not allowed to leave home")

p1<-ggplot()
p1<-p1+
  geom_point(data=eff1,aes(x=x,y=predicted),size=6)+
  geom_segment(data=eff1,aes(x=x,y = conf.low, xend = x, yend = conf.high),linewidth=1.5)+scale_y_continuous(limits = c(0.45,0.70),expand = c(0,0),breaks=c(0.45,0.50,0.55,0.60,0.65,0.70))

p1<-p1+ggtitle("Expert protocol. 08:00 am")+ylab("NDSI")+xlab("")+
  theme(panel.border = element_rect(linetype="solid",colour = "grey80", fill="NA", linewidth=1),panel.background = element_rect(fill="white"))+
  theme(axis.title.y = element_text(face="bold",size=15))+
  theme(axis.line.x= element_line(colour = "black"),axis.line.y= element_line(colour = "black"))+
  theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black"))+theme(axis.line= element_line(colour = "black"))+
  theme(axis.text = element_text(size=12))+
  theme(strip.text = element_text(size = 12, colour = "black"))+
  theme(legend.position="none")+
  theme(plot.title = element_text(size = 15, face = "bold"))+
  scale_x_discrete(labels = label_wrap(20))+
  theme(axis.title=element_text(margin=margin(20,20,20,20)))


p1<-p1+geom_segment(aes(x = 1, y = 0.65, xend =2, yend = 0.65))+
  geom_segment(aes(x = 2, y = 0.6625, xend = 3, yend = 0.6625))+
  geom_segment(aes(x = 1, y = 0.675, xend = 3, yend = 0.675))+
  annotate("text", x=1.5, y=0.652, label= "*",size=6)+
  annotate("text", x=2.5, y=0.667, label= "ns",size=3)+
  annotate("text", x=2, y=0.677, label= "**",size=6)



p2<-ggplot()
p2<-p2+
  geom_point(data=eff2,aes(x=x,y=predicted),size=6)+
  geom_segment(data=eff2,aes(x=x,y = conf.low, xend = x, yend = conf.high),linewidth=1.5)+
  scale_y_continuous(limits = c(0,0.014),expand = c(0,0),breaks=c(0,0.002,0.004,0.006,0.008,0.010,0.012,0.014))

p2<-p2+ggtitle("Expert protocol. 08:00 am")+ylab("Anthropophony level: engine noise")+xlab("")+
  theme(panel.border = element_rect(linetype="solid",colour = "grey80", fill="NA", linewidth=1),panel.background = element_rect(fill="white"))+
  theme(axis.title.y = element_text(face="bold",size=15))+
  theme(axis.line.x= element_line(colour = "black"),axis.line.y= element_line(colour = "black"))+
  theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black"))+theme(axis.line= element_line(colour = "black"))+
  theme(axis.text = element_text(size=12))+
  theme(strip.text = element_text(size = 12, colour = "black"))+
  theme(legend.position="none")+
  theme(plot.title = element_text(size = 15, face = "bold"))+
  scale_x_discrete(labels = label_wrap(20))+
  theme(axis.title=element_text(margin=margin(20,20,20,20)))


p2<-p2+geom_segment(aes(x = 1, y = 0.0112, xend =2, yend = 0.0112))+
  geom_segment(aes(x = 2, y = 0.0119, xend = 3, yend = 0.0119))+
  geom_segment(aes(x = 1, y = 0.0126, xend = 3, yend = 0.0126))+
  annotate("text", x=1.5, y=0.01134, label= "*",size=6)+
  annotate("text", x=2.5, y=0.01204, label= "***",size=6)+
  annotate("text", x=2, y=0.01274, label= "***",size=6)



p3<-ggplot()
p3<-p3+
  geom_point(data=eff3,aes(x=x,y=predicted),size=6)+
  geom_segment(data=eff3,aes(x=x,y = conf.low, xend = x, yend = conf.high),linewidth=1.5)+
  scale_y_continuous(limits = c(0,0.2),expand = c(0,0),breaks=c(0,0.05,0.10,0.15,0.2))

p3<-p3+ggtitle("Expert protocol. 08:00 am")+ylab("Biophony level: birds")+xlab("")+
  theme(panel.border = element_rect(linetype="solid",colour = "grey80", fill="NA", linewidth=1),panel.background = element_rect(fill="white"))+
  theme(axis.title.y = element_text(face="bold",size=15))+
  theme(axis.line.x= element_line(colour = "black"),axis.line.y= element_line(colour = "black"))+
  theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black"))+theme(axis.line= element_line(colour = "black"))+
  theme(axis.text = element_text(size=12))+
  theme(strip.text = element_text(size = 12, colour = "black"))+
  theme(legend.position="none")+
  theme(plot.title = element_text(size = 15, face = "bold"))+
  scale_x_discrete(labels = label_wrap(20))+
  theme(axis.title=element_text(margin=margin(20,20,20,20)))


p3<-p3+geom_segment(aes(x = 1, y = 0.16, xend =2, yend = 0.16))+
  geom_segment(aes(x = 2, y = 0.17, xend = 3, yend = 0.17))+
  geom_segment(aes(x = 1, y = 0.18, xend = 3, yend = 0.18))+
  annotate("text", x=1.5, y=0.162, label= "***",size=6)+
  annotate("text", x=2.5, y=0.172, label= "***",size=6)+
  annotate("text", x=2, y=0.182, label= "***",size=6)


ppi<-600
plot_grid(p1,p3,p2, align = "v", ncol=3, nrow = 1, rel_heights = c( 1,1,1))
