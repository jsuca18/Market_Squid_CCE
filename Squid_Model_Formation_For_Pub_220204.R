#commented out code for market squid model for (eventual) publication 

#######Reading in overall abundance data#######
#read in potential predictors 
setwd("F:/RREAS/Catch_Data/Anchovy/YOY/GAMs")
General_Preconditioning<-read.csv("General_Preconditioning.csv", header=TRUE)
Upwelling_Preconditioning<-read.csv("March_June_Upwelling_Preconditioning.csv", header=TRUE)
Lasker_Events<-read.csv("Lasker_Events_Mar_Jun.csv", header=TRUE)
Lasker_Events_NT<-read.csv("Lasker_Events_Mar_Jun_NT.csv", header=TRUE)
OEI_Index<-read.csv("OEI_Mar_Jun_Counts.csv", header=TRUE)
ROMS_Predictors<-read.csv("Full_RREAS_Predictors_CHL_2021.csv", header=TRUE)
SST_Squid<-read.csv("March_June_SST_2021.csv", header=TRUE)
CHL_Squid<-read.csv("March_June_CHL.csv", header=TRUE)
U_Squid<-read.csv("March_June_U_Currents_2021.csv", header=TRUE)
V_Squid<-read.csv("March_June_V_Currents_2021.csv", header=TRUE)
Curl_Squid<-read.csv("March_June_Wind_Curl_2021.csv", header=TRUE)
ONI<-read.csv("ONI_Values.csv", header=TRUE)
CUTI_Roll<-read.csv("CUTI_Roll.csv", header=TRUE)
#add wind curl here
setwd("F:/RREAS/Catch_Data/Squid/Envelope_Model/squid")
Squid_Spawn_by_Year<-read.csv("Squid_Spawn_by_Year.csv", header=TRUE)
#read in the full RREAS Data to set up model data
setwd("F:/RREAS/Catch_Data")
setwd("F:/RREAS/Catch_Data/Squid/Envelope_Model/Current_Code_Data")
RREAS_Data<-read.csv("rreas.data83to2020.csv", header=TRUE)
RREAS_Data$Unique_ID<-paste(RREAS_Data$cruise, RREAS_Data$station, RREAS_Data$haul_no, sep="")

RREAS_Data$Date<-as.Date(RREAS_Data$haul_date,
                         format = "%m/%d/%Y")
RREAS_Data$Latitude<-as.numeric(RREAS_Data$Latitude)
RREAS_Data$Longitude<-as.numeric(RREAS_Data$Longitude)

library(dplyr)
library(lubridate)
library(plyr)
RREAS_Data=RREAS_Data %>%
  dplyr::mutate(year = lubridate::year(Date), 
                month = lubridate::month(Date), 
                day = lubridate::day(Date),
                DOY=lubridate::yday(Date))


RREAS_Data$Lat_Deg<-round(RREAS_Data$Latitude)
RREAS_Data$Lon_Deg<-round(RREAS_Data$Longitude)

Market_Squid_Full<-RREAS_Data
Market_Squid_Full$Unique_ID<-paste(Market_Squid_Full$cruise, Market_Squid_Full$station, Market_Squid_Full$haul_no, sep=" ")
Market_Squid_Full_Station_Count<-ddply(Market_Squid_Full, .(year, strata), summarize, Count_Stations=length(Unique_ID))
#looks like there is quite a variable, especially in the south 

RREAS_Squid_Full<-merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(Market_Squid_Full, SST_Squid, by=c("year","strata")), Squid_Spawn_by_Year, by=c("year","strata")), V_Squid, by=c("year","strata")), U_Squid, by=c("year","strata")), CHL_Squid, by=c("year","strata")), ONI, by="year"), Lasker_Events, by=c("year","strata")), OEI_Index, by=c("year","strata")), Curl_Squid, by=c("year","strata")), Lasker_Events_NT, by=c("year","strata"))
RREAS_Squid_Full2<-merge(RREAS_Squid_Full, Upwelling_Preconditioning, by=c("year","Lat_Deg"))



#compile all of the data together. 
Market_Squid_Annual_Lasker_Full_Krill<-ddply(RREAS_Squid_Full2, .(year, strata), summarize, Mean_Krill_CPUE=mean(log(All_krill+1)), Mean_Squid_CPUE=mean(log(market_squid+1)), STI_BEUTI=mean(STI), STI_CUTI=mean(STI_CUTI),Mean_BEUTI=mean(Mean_BEUTI_Upwelling),Mean_CUTI=mean(Mean_CUTI_Upwelling), Mean_SST=mean(Winter_SST),  Mean_CHL=mean(Mean_CHL), Total_Spawn=mean(Total_Spawning_Area),March_Spawn_Hab=mean(March_Spawn), April_Spawn_Hab=mean(April_Spawn),May_Spawn_Hab=mean(May_Spawn), Mean_V=mean(V_Current), Mean_U=mean(U_Current), ONI_FMA=mean(FMA), ONI_MAM=mean(MAM), ONI_JFM=mean(JFM), Lasker_Events_Mar_Jun=mean(Lasker_Sum_Mar_Jun),Lasker_Events_Mar_Jun_NT=mean(Lasker_Sum_Mar_Jun_NT), OEI=mean(OEI_Sum_Mar_Jun), Curl=mean(Curl_Current) )


#separate north and south of pt conception
Market_Squid_Annual_Lasker_Full_Krill$Region<-0
Market_Squid_Annual_Lasker_Full_Krill$Region[Market_Squid_Annual_Lasker_Full_Krill$strata=="S"]<-"S"
Market_Squid_Annual_Lasker_Full_Krill$Region[Market_Squid_Annual_Lasker_Full_Krill$strata!="S"]<-"N"
Market_Squid_Annual_Lasker_Full_Krill$Region<-as.factor(Market_Squid_Annual_Lasker_Full_Krill$Region)
Market_Squid_Annual_Lasker_Full_Krill$strata<-as.factor(Market_Squid_Annual_Lasker_Full_Krill$strata)


#see if the number of stations sampled per stratum affects the mean CPUE
Market_Squid_Annual_Lasker_Full_Krill_Stations<-merge(Market_Squid_Annual_Lasker_Full_Krill,Market_Squid_Full_Station_Count, by=c("year","strata")) 
Market_Squid_Annual_Lasker_Full_Krill_Stations_Red<-Market_Squid_Annual_Lasker_Full_Krill_Stations[Market_Squid_Annual_Lasker_Full_Krill_Stations$Count_Stations>=5,]

plot(Market_Squid_Annual_Lasker_Full_Krill_Stations$Count_Stations[Market_Squid_Annual_Lasker_Full_Krill_Stations$strata=="C"], Market_Squid_Annual_Lasker_Full_Krill_Stations$Mean_Squid_CPUE[Market_Squid_Annual_Lasker_Full_Krill_Stations$strata=="C"])
plot(Market_Squid_Annual_Lasker_Full_Krill_Stations$year[Market_Squid_Annual_Lasker_Full_Krill_Stations$strata=="C"], Market_Squid_Annual_Lasker_Full_Krill_Stations$Count_Stations[Market_Squid_Annual_Lasker_Full_Krill_Stations$strata=="C"])

plot(Market_Squid_Annual_Lasker_Full_Krill_Stations$Count_Stations[Market_Squid_Annual_Lasker_Full_Krill_Stations$strata=="S"], Market_Squid_Annual_Lasker_Full_Krill_Stations$Mean_Squid_CPUE[Market_Squid_Annual_Lasker_Full_Krill_Stations$strata=="S"])
plot(Market_Squid_Annual_Lasker_Full_Krill_Stations$year[Market_Squid_Annual_Lasker_Full_Krill_Stations$strata=="S"], Market_Squid_Annual_Lasker_Full_Krill_Stations$Count_Stations[Market_Squid_Annual_Lasker_Full_Krill_Stations$strata=="S"])

plot(Market_Squid_Annual_Lasker_Full_Krill_Stations$Count_Stations[Market_Squid_Annual_Lasker_Full_Krill_Stations$strata=="SC"], Market_Squid_Annual_Lasker_Full_Krill_Stations$Mean_Squid_CPUE[Market_Squid_Annual_Lasker_Full_Krill_Stations$strata=="SC"])
plot(Market_Squid_Annual_Lasker_Full_Krill_Stations$year[Market_Squid_Annual_Lasker_Full_Krill_Stations$strata=="SC"], Market_Squid_Annual_Lasker_Full_Krill_Stations$Count_Stations[Market_Squid_Annual_Lasker_Full_Krill_Stations$strata=="SC"])

plot(Market_Squid_Annual_Lasker_Full_Krill_Stations$Count_Stations[Market_Squid_Annual_Lasker_Full_Krill_Stations$strata=="NC"], Market_Squid_Annual_Lasker_Full_Krill_Stations$Mean_Squid_CPUE[Market_Squid_Annual_Lasker_Full_Krill_Stations$strata=="NC"])
plot(Market_Squid_Annual_Lasker_Full_Krill_Stations$year[Market_Squid_Annual_Lasker_Full_Krill_Stations$strata=="NC"], Market_Squid_Annual_Lasker_Full_Krill_Stations$Count_Stations[Market_Squid_Annual_Lasker_Full_Krill_Stations$strata=="NC"])

Market_Squid_Annual_Lasker_Full_Krill_Model<-Market_Squid_Annual_Lasker_Full_Krill_Stations_Red[Market_Squid_Annual_Lasker_Full_Krill_Stations_Red$year<=2016,]

Recruit_Predictors<-Market_Squid_Annual_Lasker_Full_Krill_Stations_Red[,3:23]
library(PerformanceAnalytics)

png("Recruitment_Squid_Predictors.png", height=10, width=12,units="in", res=400 )
chart.Correlation(Recruit_Predictors)
dev.off()
#make a set of candidate GAMs
#explore if there are regional differences in the response to certain variables
#make different models with candidate predictors that are not useable
#see if there is a difference in the model residuals relative to number of stations
#and if there is an issue with the number of problems. 
library(mgcv)

Market_Annual_GAM1<-gam(Mean_Squid_CPUE~s(Mean_SST,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun,by=Region, k=6)+s(Mean_CHL, k=6)+s(Mean_Krill_CPUE,k=6)+s(Curl,by=Region, k=6)+s(OEI, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")

Market_Annual_GAM2<-gam(Mean_Squid_CPUE~s(Total_Spawn,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun,by=Region, k=6)+s(Mean_CHL, k=6)+s(Mean_Krill_CPUE,k=6)+s(Curl,by=Region, k=6)+s(OEI, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")

Market_Annual_GAM3<-gam(Mean_Squid_CPUE~s(Mean_SST,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun, k=6)+s(Mean_CHL, k=6)+s(Mean_Krill_CPUE,k=6)+s(Curl, k=6)+s(OEI, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")

Market_Annual_GAM4<-gam(Mean_Squid_CPUE~s(Total_Spawn,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun, k=6)+s(Mean_CHL, k=6)+s(Mean_Krill_CPUE,k=6)+s(Curl, k=6)+s(OEI, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")

Market_Annual_GAM5<-gam(Mean_Squid_CPUE~s(Total_Spawn, k=6)+s(Mean_V, by=Region,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun,by=Region, k=6)+s(Mean_CHL, k=6)+s(Mean_Krill_CPUE,k=6)+s(Curl,by=Region, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")


Market_Annual_GAM6<-gam(Mean_Squid_CPUE~s(Mean_SST,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun,by=Region, k=6)+s(Mean_CHL, k=6)+s(Mean_Krill_CPUE,k=6)+s(Curl,by=Region, k=6)+s(OEI, k=6)+s(Mean_U, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")

Market_Annual_GAM7<-gam(Mean_Squid_CPUE~s(Mean_SST,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun, k=6)+s(Mean_CHL, k=6)+s(Mean_Krill_CPUE,k=6)+s(Curl, k=6)+s(OEI, k=6)+s(Mean_U, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")


Market_Annual_GAM8<-gam(Mean_Squid_CPUE~s(Mean_SST,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun_NT,by=Region, k=6)+s(Mean_CHL, k=6)+s(Mean_Krill_CPUE,k=6)+s(Curl,by=Region, k=6)+s(OEI, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")

Market_Annual_GAM9<-gam(Mean_Squid_CPUE~s(Total_Spawn,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun_NT,by=Region, k=6)+s(Mean_CHL, k=6)+s(Mean_Krill_CPUE,k=6)+s(Curl,by=Region, k=6)+s(OEI, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")

Market_Annual_GAM10<-gam(Mean_Squid_CPUE~s(Mean_SST,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun_NT, k=6)+s(Mean_CHL, k=6)+s(Mean_Krill_CPUE,k=6)+s(Curl, k=6)+s(OEI, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")

Market_Annual_GAM11<-gam(Mean_Squid_CPUE~s(Total_Spawn,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun_NT, k=6)+s(Mean_CHL, k=6)+s(Mean_Krill_CPUE,k=6)+s(Curl, k=6)+s(OEI, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")

Market_Annual_GAM12<-gam(Mean_Squid_CPUE~s(Total_Spawn, k=6)+s(Mean_V, by=Region,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun_NT,by=Region, k=6)+s(Mean_CHL, k=6)+s(Mean_Krill_CPUE,k=6)+s(Curl,by=Region, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")


Market_Annual_GAM13<-gam(Mean_Squid_CPUE~s(Mean_SST,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun_NT,by=Region, k=6)+s(Mean_CHL, k=6)+s(Mean_Krill_CPUE,k=6)+s(Curl,by=Region, k=6)+s(OEI, k=6)+s(Mean_U, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")

Market_Annual_GAM14<-gam(Mean_Squid_CPUE~s(Mean_SST,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun_NT, k=6)+s(Mean_CHL, k=6)+s(Mean_Krill_CPUE,k=6)+s(Curl, k=6)+s(OEI, k=6)+s(Mean_U, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")





library(MuMIn)

Market_Annual_Dredge1<-dredge(Market_Annual_GAM1, rank="AIC",trace=5)
Market_Annual_Dredge2<-dredge(Market_Annual_GAM2, rank="AIC",trace=5)
Market_Annual_Dredge3<-dredge(Market_Annual_GAM3, rank="AIC",trace=5)
Market_Annual_Dredge4<-dredge(Market_Annual_GAM4, rank="AIC",trace=5)
Market_Annual_Dredge5<-dredge(Market_Annual_GAM5, rank="AIC",trace=5)
Market_Annual_Dredge6<-dredge(Market_Annual_GAM6, rank="AIC",trace=5)
Market_Annual_Dredge7<-dredge(Market_Annual_GAM7, rank="AIC",trace=5)
Market_Annual_Dredge8<-dredge(Market_Annual_GAM8, rank="AIC",trace=5)
Market_Annual_Dredge9<-dredge(Market_Annual_GAM9, rank="AIC",trace=5)
Market_Annual_Dredge10<-dredge(Market_Annual_GAM10, rank="AIC",trace=5)
Market_Annual_Dredge11<-dredge(Market_Annual_GAM11, rank="AIC",trace=5)
Market_Annual_Dredge12<-dredge(Market_Annual_GAM12, rank="AIC",trace=5)
Market_Annual_Dredge13<-dredge(Market_Annual_GAM13, rank="AIC",trace=5)
Market_Annual_Dredge14<-dredge(Market_Annual_GAM14, rank="AIC",trace=5)

#the following two are similarly performing models
Market_Annual_Avg<-gam(Mean_Squid_CPUE~s(Mean_SST,  k=6)+s(Lasker_Events_Mar_Jun_NT, k=6)+s(ONI_JFM, k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")
summary(Market_Annual_Avg)
#####variable importance###
Market_Annual_Avg1<-gam(Mean_Squid_CPUE~s(Mean_SST,  k=6)+s(Lasker_Events_Mar_Jun_NT, k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")
summary(Market_Annual_Avg1)
#del R2 5

Market_Annual_Avg2<-gam(Mean_Squid_CPUE~s(Lasker_Events_Mar_Jun_NT, k=6)+s(ONI_JFM, k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")
summary(Market_Annual_Avg2)
#del R2 22

Market_Annual_Avg3<-gam(Mean_Squid_CPUE~s(Mean_SST,  k=6)+s(ONI_JFM, k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")
summary(Market_Annual_Avg3)
#del R2 4.5

Market_Annual_Avg4<-gam(Mean_Squid_CPUE~s(Mean_SST,  k=6)+s(Lasker_Events_Mar_Jun, k=6)+s(Curl, k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")
summary(Market_Annual_Avg4)
# del R2 13

#compare the three models prediction
Market_Annual_Avg<-gam(Mean_Squid_CPUE~s(Mean_SST,  k=6)+s(Lasker_Events_Mar_Jun_NT, k=6)+s(ONI_JFM, k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")
#Market_Annual_Avg<-gam(Mean_Squid_CPUE~s(Mean_SST,  k=6)+s(Lasker_Events_Mar_Jun, k=6)+s(Mean_Krill_CPUE, k=6)+s(STI_BEUTI, k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")
library(mgcv)
setwd("F:/RREAS/Model_Formation/Market_Squid/Figures")
Market_Annual_Avg<-gam(Mean_Squid_CPUE~s(Mean_SST,  k=6)+s(Lasker_Events_Mar_Jun_NT, k=6)+s(ONI_JFM, k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")
png("ONI_Squid_Recruit.png",height=5, width=7, res=300, units = "in")
plot(Market_Annual_Avg, ylim=c(-3,2), shade=TRUE, ylab=c("Partial Effect"),xlab=c("Jan-Mar Ocean Niño Index"), cex.lab=1.5)
dev.off()    

Market_Annual_Avg<-gam(Mean_Squid_CPUE~s(Mean_SST,  k=6)+s(ONI_JFM, k=6)+s(Lasker_Events_Mar_Jun_NT, k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")
png("Lasker_CPUE_Effect.png",height=5, width=7, res=300, units = "in")
plot(Market_Annual_Avg, ylim=c(-3,2), shade=TRUE, ylab=c("Partial Effect"),xlab=c("Mar-Jun Relaxation Events"), cex.lab=1.5)
dev.off()    

    

Market_Annual_Avg<-gam(Mean_Squid_CPUE~s(ONI_JFM, k=6)+s(Lasker_Events_Mar_Jun_NT, k=6)+s(Mean_SST,  k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")
png("Sea_Surface_Temp.png",height=5, width=7, res=300, units = "in")
plot(Market_Annual_Avg, ylim=c(-3,2), shade=TRUE, ylab=c("Partial Effect"),xlab=expression("Mar-Jun Sea Surface Temperature ("~degree~"C )"), cex.lab=1.5)
dev.off()    


#Market_Annual_Avg<-gam(Mean_Squid_CPUE~s(Mean_SST,  k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun, k=6)+s(Mean_V, by=Region, k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")
#
#Market_Annual_Avg<-gam(Mean_Squid_CPUE~s(Mean_SST,  k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun, k=6)+s(Curl, k=6)+s(Mean_U, k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")
RREAS_Squid_Full<-merge(merge(merge(merge(merge(merge(merge(merge(Market_Squid_Full, SST_Squid, by=c("year","strata")), V_Squid, by=c("year","strata")), U_Squid, by=c("year","strata")), CHL_Squid, by=c("year","strata")), Lasker_Events, by=c("year","strata")), Curl_Squid, by=c("year","strata")), Lasker_Events_NT, by=c("year","strata")), ONI, by=c("year"))
RREAS_Squid_Full2<-merge(RREAS_Squid_Full, Upwelling_Preconditioning, by=c("year","Lat_Deg"))


Market_Squid_Annual_Lasker_Full_Krill<-ddply(RREAS_Squid_Full2, .(year, strata), summarize, Mean_Krill_CPUE=mean(log(All_krill+1)), Mean_Squid_CPUE=mean(log(market_squid+1)), STI_BEUTI=mean(STI), STI_CUTI=mean(STI_CUTI),Mean_BEUTI=mean(Mean_BEUTI_Upwelling),Mean_CUTI=mean(Mean_CUTI_Upwelling), Mean_SST=mean(Winter_SST),  Mean_CHL=mean(Mean_CHL), Mean_V=mean(V_Current), Mean_U=mean(U_Current), Lasker_Events_Mar_Jun=mean(Lasker_Sum_Mar_Jun),Lasker_Events_Mar_Jun_NT=mean(Lasker_Sum_Mar_Jun_NT), Curl=mean(Curl_Current), ONI_JFM=mean(JFM) )


#separate north and south of pt conception
Market_Squid_Annual_Lasker_Full_Krill$Region<-0
Market_Squid_Annual_Lasker_Full_Krill$Region[Market_Squid_Annual_Lasker_Full_Krill$strata=="S"]<-"S"
Market_Squid_Annual_Lasker_Full_Krill$Region[Market_Squid_Annual_Lasker_Full_Krill$strata!="S"]<-"N"
Market_Squid_Annual_Lasker_Full_Krill$Region<-as.factor(Market_Squid_Annual_Lasker_Full_Krill$Region)
Market_Squid_Annual_Lasker_Full_Krill$strata<-as.factor(Market_Squid_Annual_Lasker_Full_Krill$strata)


#see if the number of stations sampled per stratum affects the mean CPUE
Market_Squid_Annual_Lasker_Full_Krill_Stations<-merge(Market_Squid_Annual_Lasker_Full_Krill,Market_Squid_Full_Station_Count, by=c("year","strata")) 
Market_Squid_Annual_Lasker_Full_Krill_Stations_Red<-Market_Squid_Annual_Lasker_Full_Krill_Stations[Market_Squid_Annual_Lasker_Full_Krill_Stations$Count_Stations>=5,]

#Market_Annual_Avg<-gam(Mean_Squid_CPUE~s(Mean_SST,  k=6)+s(Lasker_Events_Mar_Jun_NT, k=6)+s(Mean_Krill_CPUE, k=6)+s(Mean_U, k=6)+s(Curl, k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")
Market_Annual_Avg<-gam(Mean_Squid_CPUE~s(Mean_SST,  k=6)+s(Lasker_Events_Mar_Jun_NT, k=6)+s(ONI_JFM, k=6), data=Market_Squid_Annual_Lasker_Full_Krill_Model, family=nb, na.action = "na.fail")


Market_Squid_Annual_Lasker_Full_Krill_Recent<-Market_Squid_Annual_Lasker_Full_Krill_Stations_Red[Market_Squid_Annual_Lasker_Full_Krill_Stations_Red$year>=2017,]
Market_Squid_Annual_Lasker_Full_Krill_Recent$Model_Predictions<-predict(Market_Annual_Avg,Market_Squid_Annual_Lasker_Full_Krill_Recent, type="response" )
Market_Squid_Annual_Lasker_Full_Krill_Model$Model_Predictions<-predict(Market_Annual_Avg,Market_Squid_Annual_Lasker_Full_Krill_Model, type="response" )

Market_Squid_Annual_Lasker_Full_Krill_Stations_Red$Model_Predictions<-predict(Market_Annual_Avg,Market_Squid_Annual_Lasker_Full_Krill_Stations_Red, type="response" )

library(Metrics)
cor(Market_Squid_Annual_Lasker_Full_Krill_Recent$Model_Predictions, Market_Squid_Annual_Lasker_Full_Krill_Recent$Mean_Squid_CPUE)^2
rmse(Market_Squid_Annual_Lasker_Full_Krill_Recent$Mean_Squid_CPUE,Market_Squid_Annual_Lasker_Full_Krill_Recent$Model_Predictions)
ex<-lm(Mean_Squid_CPUE~0+Model_Predictions, data=Market_Squid_Annual_Lasker_Full_Krill_Recent)
summary(ex)
#R2=0.92 for model avg1
#R2=0.61 for model avg2
#R2=0.82for model avg 3
#R2=0.89 for model avg4
#R2=0.89 for model avg
cor(Market_Squid_Annual_Lasker_Full_Krill_Model$Model_Predictions, Market_Squid_Annual_Lasker_Full_Krill_Model$Mean_Squid_CPUE)^2



rmse(Market_Squid_Annual_Lasker_Full_Krill_Model$Mean_Squid_CPUE,Market_Squid_Annual_Lasker_Full_Krill_Model$Model_Predictions)




setwd("F:/RREAS/Model_Formation/Market_Squid/Figures")

png("Squid_recruitment_Model_Predictions_thru_2016_Model.png", height=5, width=6, res=200, units="in")
plot(Market_Squid_Annual_Lasker_Full_Krill_Stations_Red$Model_Predictions[Market_Squid_Annual_Lasker_Full_Krill_Stations_Red$year<=2016], Market_Squid_Annual_Lasker_Full_Krill_Stations_Red$Mean_Squid_CPUE[Market_Squid_Annual_Lasker_Full_Krill_Stations_Red$year<=2016], ylab=c("Mean Strata ln(CPUE+1)"), xlab=c("Predicted Mean ln(CPUE+1)"), main=c("Train Set 1998-2016"), cex.lab=1.25, pch=19)
dev.off()

png("Squid_recruitment_Model_Predicitions_2021_Forecast.png", height=5, width=6, res=200, units="in")
plot(Market_Squid_Annual_Lasker_Full_Krill_Recent$Model_Predictions, Market_Squid_Annual_Lasker_Full_Krill_Recent$Mean_Squid_CPUE, ylab=c("Mean Strata ln(CPUE+1)"), xlab=c("Predicted Mean ln(CPUE+1)"), main=c("Test Set 2017-2021"), cex.lab=1.25, pch=19)
dev.off()

Market_Squid_Annual_mean_Stations<-ddply(Market_Squid_Annual_Lasker_Full_Krill_Stations_Red, .(strata), summarize, Avg_Stations=mean(Count_Stations))

Market_Squid_Annual_Lasker_Full_Krill_Stations_Red$Residuals<-Market_Squid_Annual_Lasker_Full_Krill_Stations_Red$Mean_Squid_CPUE-Market_Squid_Annual_Lasker_Full_Krill_Stations_Red$Model_Predictions
Market_Squid_Annual_Lasker_Full_Krill_Stations_Full_Red<-merge(Market_Squid_Annual_Lasker_Full_Krill_Stations_Red,Market_Squid_Annual_mean_Stations, by="strata" )
Market_Squid_Annual_Lasker_Full_Krill_Stations_Full_Red$Stations_Anomaly<-Market_Squid_Annual_Lasker_Full_Krill_Stations_Full_Red$Count_Stations-Market_Squid_Annual_Lasker_Full_Krill_Stations_Full_Red$Avg_Stations

png("Stations anomaly vs residuals.png", height=5, width=6, res=300, units="in")
plot(Market_Squid_Annual_Lasker_Full_Krill_Stations_Full_Red$Stations_Anomaly, Market_Squid_Annual_Lasker_Full_Krill_Stations_Full_Red$Residuals, pch=19, xlab=c("Station Count Anomaly"), ylab=c("Model Residuals"), cex.lab=1.25, main="Residuals vs Station Anomaly")
dev.off()

png("Stations anomaly vs abs residuals.png", height=5, width=6, res=300, units="in")
plot(Market_Squid_Annual_Lasker_Full_Krill_Stations_Full_Red$Stations_Anomaly, abs(Market_Squid_Annual_Lasker_Full_Krill_Stations_Full_Red$Residuals), pch=19, xlab=c("Station Count Anomaly"), ylab=c("Abs. Model Residuals"), cex.lab=1.25, main="Abs. Residuals vs Station Anomaly")
dev.off()

png("Abs stations anomaly vs abs residuals.png", height=5, width=6, res=300, units="in")
plot(abs(Market_Squid_Annual_Lasker_Full_Krill_Stations_Full_Red$Stations_Anomaly), abs(Market_Squid_Annual_Lasker_Full_Krill_Stations_Full_Red$Residuals), pch=19, xlab=c("Station Count Anomaly"), ylab=c("Abs. Model Residuals"), cex.lab=1.25, main="Abs. Residuals vs Abs. Station Anomaly")
dev.off()

cor.test(Market_Squid_Annual_Lasker_Full_Krill_Stations_Full_Red$Stations_Anomaly, Market_Squid_Annual_Lasker_Full_Krill_Stations_Full_Red$Residuals)

png("Squid_recruitment_Model_Predicitions_residuals_yearv2.png", height=5, width=6, res=200, units="in")
boxplot(Residuals~year, data=Market_Squid_Annual_Lasker_Full_Krill_Stations_Red, main=c("'Recruitment' Model Residuals"))
dev.off()

# it looks good!

png("Squid_recruitment_Model_Predicitions_residuals_strata.png", height=5, width=6, res=200, units="in")
boxplot(Residuals~strata, data=Market_Squid_Annual_Lasker_Full_Krill_Stations_Red, main=c("'Recruitment' Model Residuals"))
dev.off()







###########PA model#############
Market_Squid_Full$Unique_ID<-paste(Market_Squid_Full$cruise, Market_Squid_Full$station, Market_Squid_Full$haul_no, sep=" ")

Market_Squid_Full$CPUE<-log(Market_Squid_Full$market_squid+1)
Market_Squid_ROMS_Pred<-merge(Market_Squid_Full, ROMS_Predictors, by=c("Unique_ID"))
#now make model predictions for every stratum for each year
Prediction_for_All_Regions<-merge(merge( SST_Squid, Lasker_Events_NT,by=c("year","strata")), ONI, by="year")
Prediction_for_All_Regions$Mean_SST<-Prediction_for_All_Regions$Winter_SST
Prediction_for_All_Regions$ONI_JFM<-Prediction_for_All_Regions$JFM
Prediction_for_All_Regions$Lasker_Events_Mar_Jun_NT<-Prediction_for_All_Regions$Lasker_Sum_Mar_Jun_NT
Prediction_for_All_Regions$Model_Predictions<-predict(Market_Annual_Avg,Prediction_for_All_Regions, type="response" )
Market_Squid_Abund_Preds<-Prediction_for_All_Regions[,c(1,2,24)]

Market_Squid_For_Spatial_Model<-merge(Market_Squid_ROMS_Pred, Market_Squid_Abund_Preds, by=c("year","strata"))
Market_Squid_For_Spatial_Model_NA<-Market_Squid_For_Spatial_Model[is.na(Market_Squid_For_Spatial_Model$Model_Predictions),]
#model predicitons not the issue

Market_Squid_For_Spatial_Model$TKE<-0.5*((Market_Squid_For_Spatial_Model$U_Val^2+Market_Squid_For_Spatial_Model$V_Val^2)^0.5)



#ROMS Model Terms 1, includes SST and SU instead of SSH and SV


CUTI_Roll$Lat_Deg<-CUTI_Roll$latitude..degrees_north.
Market_Squid_For_Spatial_Model_Full<-merge(Market_Squid_For_Spatial_Model, CUTI_Roll, by=c("Lat_Deg", "year","DOY"), all.x=TRUE)
Market_Squid_For_Spatial_Model_Full$Latitude<-Market_Squid_For_Spatial_Model_Full$Latitude.x
Market_Squid_For_Spatial_Model_Full$CUTI<-Market_Squid_For_Spatial_Model_Full$CUTI..m2.s.1.
Vars_Used_Names<-c("Unique_ID","SST", "SSH","SST_sd", "SSH_sd","year","strata","Curl_Val",
                   "Depth_Val","TKE","Dist_Shore_Val","Rugosity_Val","CHL_Val","CUTI_3day","CUTI",
                   "Latitude","Longitude", "Model_Predictions","CPUE", "DOY")
Market_Squid_Model_Data<-Market_Squid_For_Spatial_Model_Full[, colnames(Market_Squid_For_Spatial_Model_Full) %in% Vars_Used_Names]



Market_Squid_Model_Data_Full<-Market_Squid_Model_Data[complete.cases(Market_Squid_Model_Data),]

Market_Squid_Model_Data_Full<-Market_Squid_Model_Data_Full[!duplicated((Market_Squid_Model_Data_Full)),]

for ( i in 1: nrow(Market_Squid_Model_Data_Full)){
  Market_Squid_Model_Data_Full$PA[i]<-ifelse(Market_Squid_Model_Data_Full$CPUE[i]>0,1,0)
}

RREAS_Squid_Model_Hurdle<-Market_Squid_Model_Data_Full[Market_Squid_Model_Data_Full$year<=2016,]

setwd("F:/RREAS/Catch_Data/Squid/Envelope_Model")
write.csv(RREAS_Squid_Model_Hurdle,"RREAS_Squid_Hurdle_model_Data_220120.csv")


setwd("F:/RREAS/Catch_Data/Squid/Envelope_Model")
Squid_hurdle_model_Data<-read.csv("RREAS_Squid_Hurdle_model_Data_220120.csv", header=TRUE)
library(mgcv)
Squid_Model_ROMS_PA_SST<-gam(PA~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(CUTI, k=6)+s(TKE, k=6)+s(Curl_Val,k=6)+
                               s(CHL_Val,k=6)+s(SST_sd, k=6)+s(Dist_Shore_Val, k=6)+s(SST,k=6)+s(SSH_sd, k=6)+s(DOY,k=6)+te(Longitude,Latitude, k=6), family = binomial, data=Squid_hurdle_model_Data, na.action = "na.fail")
Squid_Model_ROMS_PA_SSH<-gam(PA~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(CUTI, k=6)+s(TKE, k=6)+s(Curl_Val,k=6)+
                               s(CHL_Val,k=6)+s(SST_sd, k=6)+s(Dist_Shore_Val, k=6)+s(SSH,k=6)+s(SSH_sd, k=6)+s(DOY,k=6)+te(Longitude,Latitude, k=6), family = binomial, data=Squid_hurdle_model_Data, na.action = "na.fail")

Squid_Model_ROMS_PA_SST_No_DOY<-gam(PA~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(CUTI, k=6)+s(TKE, k=6)+s(Curl_Val,k=6)+
                               s(CHL_Val,k=6)+s(SST_sd, k=6)+s(Dist_Shore_Val, k=6)+s(SST,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = binomial, data=Squid_hurdle_model_Data, na.action = "na.fail")
Squid_Model_ROMS_PA_SSH_No_DOY<-gam(PA~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(CUTI, k=6)+s(TKE, k=6)+s(Curl_Val,k=6)+
                               s(CHL_Val,k=6)+s(SST_sd, k=6)+s(Dist_Shore_Val, k=6)+s(SSH,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = binomial, data=Squid_hurdle_model_Data, na.action = "na.fail")



Squid_hurdle_model_Data_Abund<-Squid_hurdle_model_Data[Squid_hurdle_model_Data$CPUE>0,]

hist(Squid_hurdle_model_Data_Abund$CPUE)


Squid_Model_ROMS_Abund_NB_SST<-gam(CPUE~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(CUTI, k=6)+s(TKE, k=6)+s(DOY, k=6)+s(Curl_Val,k=6)+
                                     s(CHL_Val,k=6)+s(SST_sd, k=6)+s(Dist_Shore_Val, k=6)+s(SST,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = nb, data=Squid_hurdle_model_Data_Abund, na.action = "na.fail")
Squid_Model_ROMS_Abund_NB_SSH<-gam(CPUE~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(CUTI, k=6)+s(TKE, k=6)+s(DOY,k=6)+s(Curl_Val,k=6)+
                                     s(CHL_Val,k=6)+s(SST_sd, k=6)+s(Dist_Shore_Val, k=6)+s(SSH,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = nb, data=Squid_hurdle_model_Data_Abund, na.action = "na.fail")


Squid_Model_ROMS_Abund_NB_SST_No_DOY<-gam(CPUE~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(CUTI, k=6)+s(TKE, k=6)+s(Curl_Val,k=6)+
                                     s(CHL_Val,k=6)+s(SST_sd, k=6)+s(Dist_Shore_Val, k=6)+s(SST,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = nb, data=Squid_hurdle_model_Data_Abund, na.action = "na.fail")
Squid_Model_ROMS_Abund_NB_SSH_No_DOY<-gam(CPUE~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(CUTI, k=6)+s(TKE, k=6)+s(Curl_Val,k=6)+
                                     s(CHL_Val,k=6)+s(SST_sd, k=6)+s(Dist_Shore_Val, k=6)+s(SSH,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = nb, data=Squid_hurdle_model_Data_Abund, na.action = "na.fail")

library(MuMIn)
Squid_PA_Model_Dredge_SST<-dredge(Squid_Model_ROMS_PA_SST, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)

Squid_PA_Model_Dredge_SSH<-dredge(Squid_Model_ROMS_PA_SSH, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)

Squid_PA_Model_Dredge_SST_No_DOY<-dredge(Squid_Model_ROMS_PA_SST_No_DOY, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)

Squid_PA_Model_Dredge_SSH_No_DOY<-dredge(Squid_Model_ROMS_PA_SSH_No_DOY, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)

Squid_Abund_Model_Dredge_SST<-dredge(Squid_Model_ROMS_Abund_NB_SST, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)

Squid_Abund_Model_Dredge_SSH<-dredge(Squid_Model_ROMS_Abund_NB_SSH, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)

Squid_Abund_Model_Dredge_SST_No_DOY<-dredge(Squid_Model_ROMS_Abund_NB_SST_No_DOY, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)

Squid_Abund_Model_Dredge_SSH_No_DOY<-dredge(Squid_Model_ROMS_Abund_NB_SSH_No_DOY, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)

save.image("F:/RREAS/Catch_Data/Squid/Envelope_Model/Squid_Dredge_Results_DOY_220122.RData")



#########Compare fit to new data###########
RREAS_Squid_Model_Recent<-Market_Squid_Model_Data_Full[Market_Squid_Model_Data_Full$year>=2017,]

library(mgcv)
library(MuMIn)
Squid_PA_Model_Avg<-model.avg(Squid_PA_Model_Dredge_SSH_No_DOY, delta<2, fit=TRUE)
Squid_Abund_Model_Avg<-model.avg(Squid_Abund_Model_Dredge_SSH_No_DOY, delta<2, fit=TRUE)


Squid_Model_ROMS_PA_Best<-gam(PA~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(CUTI, k=6)+s(TKE, k=6)+s(Curl_Val,k=6)+
                               s(CHL_Val,k=6)+s(SST_sd, k=6)+s(Dist_Shore_Val, k=6)+s(SST,k=6)+s(SSH_sd, k=6)+s(DOY,k=6)+te(Longitude,Latitude, k=6), family = binomial, data=Squid_hurdle_model_Data, na.action = "na.fail")
#Squid_Model_ROMS_PA_SST_Only<-gam(PA~s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(CUTI, k=6)+s(TKE, k=6)+s(Curl_Val,k=6)+
#                               s(CHL_Val,k=6)+s(SST_sd, k=6)+s(Dist_Shore_Val, k=6)+s(SST,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = binomial, data=Squid_hurdle_model_Data, na.action = "na.fail")

Market_Squid_Model_Data_Full$PA_Estimations<-predict(Squid_PA_Model_Avg,Market_Squid_Model_Data_Full, type="response" )
#Market_Squid_Model_Data_Full$PA_Estimations<-predict(Squid_Model_ROMS_PA_SST_Only,Market_Squid_Model_Data_Full, type="response" )
#Squid_Model_ROMS_Abund_NB_SST_No_DOY<-gam(CPUE~s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(CUTI, k=6)+s(TKE, k=6)+s(Curl_Val,k=6)+
#                                            s(CHL_Val,k=6)+s(SST_sd, k=6)+s(Dist_Shore_Val, k=6)+s(SST,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = nb, data=Squid_hurdle_model_Data_Abund, na.action = "na.fail")

Market_Squid_Model_Data_Full$Abund_Estimations<-predict(Squid_Abund_Model_Avg,Market_Squid_Model_Data_Full, type="response" )
#Market_Squid_Model_Data_Full$Abund_Estimations<-predict(Squid_Model_ROMS_Abund_NB_SST_No_DOY,Market_Squid_Model_Data_Full, type="response" )

Market_Squid_Model_Data_Full$Hurdle_Estimate<-Market_Squid_Model_Data_Full$PA_Estimations*Market_Squid_Model_Data_Full$Abund_Estimations


plot(Market_Squid_Model_Data_Full$Hurdle_Estimate, Market_Squid_Model_Data_Full$CPUE)
cor(Market_Squid_Model_Data_Full$Hurdle_Estimate, Market_Squid_Model_Data_Full$CPUE)^2
library(Metrics)
rmse(Market_Squid_Model_Data_Full$Hurdle_Estimate, Market_Squid_Model_Data_Full$CPUE)

Market_Squid_Model_Data_Full_Model_Only<-Market_Squid_Model_Data_Full[Market_Squid_Model_Data_Full$year<=2016,]
setwd("F:/RREAS/Model_Formation/Market_Squid/Figures")
png("Hurdle_Estimate_Comparison_Train_220124.png", res=200, height=5, width=6, units="in")
plot(Market_Squid_Model_Data_Full_Model_Only$Hurdle_Estimate, Market_Squid_Model_Data_Full_Model_Only$CPUE, xlab=c("Hurdle Estimate"), ylab=c("ln(CPUE+1)"), main=c("'Train' Hurdle Model Estimate 1998-2016"), cex.lab=1.25, pch=19)
dev.off()

Market_Squid_Model_Data_Full_Recent<-Market_Squid_Model_Data_Full[Market_Squid_Model_Data_Full$year>=2017,]

png("Hurdle_Estimate_Comparison_Recent_2021_220124.png", res=200, height=5, width=6, units="in")
plot(Market_Squid_Model_Data_Full_Recent$Hurdle_Estimate, Market_Squid_Model_Data_Full_Recent$CPUE, xlab=c("Hurdle Estimate"), ylab=c("ln(CPUE+1)"), main=c("Hurdle Model Estimate 2017-2021"), cex.lab=1.25, pch=19, xlim=c(0,10), ylim=c(0,10))
dev.off()

cor(Market_Squid_Model_Data_Full_Recent$Hurdle_Estimate, Market_Squid_Model_Data_Full_Recent$CPUE)^2
#R2=0.425
rmse( Market_Squid_Model_Data_Full_Recent$CPUE, Market_Squid_Model_Data_Full_Recent$Hurdle_Estimate)
#rmse=1.79

library(ROCR)
Predictions<-as.vector(Market_Squid_Model_Data_Full_Model_Only$PA_Estimations)
Vals<-Market_Squid_Model_Data_Full_Model_Only$PA
#predict actual p/a of test set based on the predicted values
pred=prediction(Predictions,Vals)#response variable
#measure AUC of ROC curve as a performance measure
auc=performance(pred,measure = "auc")

auc_squid_Full=as.numeric(auc@y.values)

library(verification)
tiff("Market_ROC_Full_220204.tif", height=5, width=5, units="in", res=300)
roc.plot(Market_Squid_Model_Data_Full_Model_Only$PA,Predictions, show.thres=FALSE, main=c("Market Squid P/A 1998-2016"), ylab=c("True Positive Rate"), xlab="False Positive Rate")
dev.off()

Predictions<-as.vector(Market_Squid_Model_Data_Full_Recent$PA_Estimations)
Vals<-Market_Squid_Model_Data_Full_Recent$PA
#predict actual p/a of test set based on the predicted values
pred=prediction(Predictions,Vals)#response variable
#measure AUC of ROC curve as a performance measure
auc=performance(pred,measure = "auc")

auc_squid_Recent=as.numeric(auc@y.values)

library(verification)
tiff("Market_ROC_Recent_220204.tif", height=5, width=5, units="in", res=300)
roc.plot(Market_Squid_Model_Data_Full_Recent$PA,Predictions, show.thres=FALSE, main=c("Market Squid P/A 2017-2021"), ylab=c("True Positive Rate"), xlab="False Positive Rate")
dev.off()

Squid_Model_ROMS_PA_Best<-gam(PA~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(TKE, k=6)+s(Curl_Val,k=6)+
                               s(CHL_Val,k=6)+s(SST_sd, k=6)+s(Dist_Shore_Val, k=6)+s(SST,k=6)+s(DOY,k=6)+te(Longitude,Latitude, k=6), family = binomial, data=Squid_hurdle_model_Data, na.action = "na.fail")
plot(Squid_Model_ROMS_PA_Best)
###########abundance comparison now############
Market_Squid_Model_Data_Pres<-Market_Squid_Model_Data_Full[Market_Squid_Model_Data_Full$CPUE>0,]

cor(Market_Squid_Model_Data_Pres$Abund_Estimations, Market_Squid_Model_Data_Pres$CPUE)^2
plot(Market_Squid_Model_Data_Pres$Abund_Estimations, Market_Squid_Model_Data_Pres$CPUE)

library(Metrics)
rmse(Market_Squid_Model_Data_Pres$Abund_Estimations, Market_Squid_Model_Data_Pres$CPUE)
Market_Squid_Model_Data_Pres_Model_Only<-Market_Squid_Model_Data_Pres[Market_Squid_Model_Data_Pres$year<=2016,]

setwd("F:/RREAS/Model_Formation/Market_Squid/Figures")
png("Abund_Estimate_Comparison_Full_2016_220124.png", res=200, height=5, width=6, units="in")
plot(Market_Squid_Model_Data_Pres_Model_Only$Abund_Estimations, Market_Squid_Model_Data_Pres_Model_Only$CPUE, xlab=c("Abund. Estimate"), ylab=c("ln(CPUE+1)"), main=c("'Train' Abundance Model Estimate 1998-2016"), cex.lab=1.25, pch=19)
dev.off()

Market_Squid_Model_Data_Pres_Recent<-Market_Squid_Model_Data_Pres[Market_Squid_Model_Data_Pres$year>=2017,]

cor(Market_Squid_Model_Data_Pres_Recent$Abund_Estimations, Market_Squid_Model_Data_Pres_Recent$CPUE)^2
plot(Market_Squid_Model_Data_Pres_Recent$Abund_Estimations,Market_Squid_Model_Data_Pres_Recent$CPUE)

library(Metrics)
rmse(Market_Squid_Model_Data_Pres_Recent$Abund_Estimations, Market_Squid_Model_Data_Pres_Recent$CPUE)

setwd("F:/RREAS/Model_Formation/Market_Squid/Figures")
png("Abund_Estimate_Comparison_Recent_2021_220204.png", res=200, height=5, width=6, units="in")
plot(Market_Squid_Model_Data_Pres_Recent$Abund_Estimations, Market_Squid_Model_Data_Pres_Recent$CPUE, xlab=c("Abund. Estimate"), ylab=c("ln(CPUE+1)"), main=c("Abundance Model Estimate 2017-2021"), cex.lab=1.25, pch=19, ylim=c(0,10), xlim=c(0,10))
dev.off()

Squid_Model_ROMS_Abund_NB_Best<-gam(CPUE~s(Model_Predictions,k=3)+s(DOY, k=6)+
                                     s(CHL_Val,k=6)+s(SST_sd, k=6)+s(Dist_Shore_Val, k=6)+s(SST,k=6)+te(Longitude,Latitude, k=6), family = nb, data=Squid_hurdle_model_Data_Abund, na.action = "na.fail")
plot(Squid_Model_ROMS_Abund_NB_Best)
#############landings comparison################
setwd("F:/RREAS/Catch_Data/Squid/Envelope_Model")

Landings<-read.csv("Squid_Landings.csv", header=TRUE)


Market_Squid_Annual_Catch_North_Conception<-Market_Squid_Annual_Lasker_Full_Krill_Stations_Full_Red[Market_Squid_Annual_Lasker_Full_Krill_Stations_Full_Red$strata!="N" & Market_Squid_Annual_Lasker_Full_Krill_Stations_Full_Red$Region=="N",]
Market_Squid_Annual_Catch_South_Conception<-Market_Squid_Annual_Lasker_Full_Krill_Stations_Full_Red[Market_Squid_Annual_Lasker_Full_Krill_Stations_Full_Red$strata=="S",]

library(plyr)
Market_Squid_Annual_Catch_North_Conception_Comp<-ddply(Market_Squid_Annual_Catch_North_Conception,.(year), summarize, Total_N_CPUE=sum(Mean_Squid_CPUE), Est_total_N_CPUE=sum(Model_Predictions), Mean_N_CPUE=mean(Mean_Squid_CPUE), Est_mean_N_CPUE=mean(Model_Predictions))

Market_Squid_Annual_Catch_South_Conception_Comp<-ddply(Market_Squid_Annual_Catch_South_Conception,.(year), summarize, Total_S_CPUE=sum(Mean_Squid_CPUE), Est_total_S_CPUE=sum(Model_Predictions))

Landings_Comparison<-merge(merge(Landings,Market_Squid_Annual_Catch_North_Conception_Comp, by=c("year")), Market_Squid_Annual_Catch_South_Conception_Comp, by=c("year")) 

Landings_Comparison$Total_CPUE_Proportion<-Landings_Comparison$Total_N_CPUE /(Landings_Comparison$Total_N_CPUE+Landings_Comparison$Total_S_CPUE)
Landings_Comparison$Total_Est_CPUE_Proportion<-Landings_Comparison$Est_total_N_CPUE /(Landings_Comparison$Est_total_N_CPUE+Landings_Comparison$Est_total_S_CPUE)
Landings_Comparison$Mean_Est_CPUE_Proportion<-Landings_Comparison$Est_mean_N_CPUE /(Landings_Comparison$Est_mean_N_CPUE+Landings_Comparison$Est_total_S_CPUE)
Landings_Comparison$Mean_CPUE_Proportion<-Landings_Comparison$Mean_N_CPUE /(Landings_Comparison$Mean_N_CPUE+Landings_Comparison$Total_S_CPUE)

png("Squid_Landings_Total_annual_Q3.png", res=200, units="in", height=5, width=6)
plot(Landings_Comparison$Total_CPUE_Proportion, Landings_Comparison$Portion_of_Landings_Q3, xlab=c("Proportion Total CPUE"), ylab=c("Prop. Landings through Q3"), main=c("Observations"), pch=19, cex.lab=1.25)
dev.off()

png("Squid_Landings_mean_annual_Q3.png", res=200, units="in", height=5, width=6)
plot(Landings_Comparison$Mean_CPUE_Proportion, Landings_Comparison$Portion_of_Landings_Q3, xlab=c("Proportion Mean CPUE"), ylab=c("Prop. Landings through Q3"), main=c("Observations"), pch=19, cex.lab=1.25)
dev.off()


png("Squid_Landings_Total_est_Q3.png", res=200, units="in", height=5, width=6)
plot(Landings_Comparison$Total_Est_CPUE_Proportion, Landings_Comparison$Portion_of_Landings_Q3, xlab=c("Proportion Total CPUE"), ylab=c("Prop. Landings during Q3"), main=c("Model Estimates"), pch=19, cex.lab=1.25)
dev.off()

png("Squid_Landings_Mean_est_Q3.png", res=200, units="in", height=5, width=6)
plot(Landings_Comparison$Mean_Est_CPUE_Proportion, Landings_Comparison$Portion_of_Landings_Q3, xlab=c("Proportion Mean CPUE"), ylab=c("Prop. Landings during Q3"), main=c("Model Estimates"), pch=19, cex.lab=1.25)
dev.off()

cor(Landings_Comparison$Total_Est_CPUE_Proportion, Landings_Comparison$Portion_of_Landings_Q3)
cor.test(Landings_Comparison$Total_Est_CPUE_Proportion, Landings_Comparison$Portion_of_Landings_Q3)

Land_Mod<-lm(Landings_Comparison$Portion_of_Landings_Q3~Landings_Comparison$Total_Est_CPUE_Proportion)
Landings_Comparison$Residuals<-resid(Land_Mod)

plot(Landings_Comparison$Total_S_CPUE, Landings_Comparison$Residuals)
plot(Landings_Comparison$South_Conception3, Landings_Comparison$Residuals)

##########compare with sea lion diet data###############
setwd("F:/RREAS/Model_Formation/Market_Squid")
CSL_Diet_Data<-read.csv("CSLdiet_Do_quarterly.csv", header=TRUE)
CSL_Diet_Data$year<-CSL_Diet_Data$altYear
CSL_SCI_Summer<-CSL_Diet_Data[CSL_Diet_Data$Season=="3-Su" & CSL_Diet_Data$Island=="SCI",]
CSL_SNI_Summer<-CSL_Diet_Data[CSL_Diet_Data$Season=="3-Su" & CSL_Diet_Data$Island=="SNI",]

Market_Squid_Annual_South<-Market_Squid_Annual_Lasker_Full_Krill_Stations_Full_Red[Market_Squid_Annual_Lasker_Full_Krill_Stations_Full_Red$strata=="S",]
Market_Squid_Annual_South_SC<-Market_Squid_Annual_Lasker_Full_Krill_Stations_Full_Red[Market_Squid_Annual_Lasker_Full_Krill_Stations_Full_Red$strata=="S" | Market_Squid_Annual_Lasker_Full_Krill_Stations_Full_Red$strata=="SC",]
Market_Squid_Annual_Mean_Catch<-ddply(Market_Squid_Annual_South_SC, .(year), summarize, Mean_CPUE=mean(Mean_Squid_CPUE), Mean_Model=mean(Model_Predictions))


CSL_SCI_Summer_Comparison<-merge(CSL_SCI_Summer, Market_Squid_Annual_South, by="year")


plot(CSL_SCI_Summer_Comparison$Mean_Squid_CPUE, CSL_SCI_Summer_Comparison$FO, pch=19,  ylab="Number of Prey", main="SCI Summer", xlab="S CPUE")
plot(CSL_SCI_Summer_Comparison$Model_Predictions, CSL_SCI_Summer_Comparison$FO, pch=19, ylab="Number of Prey", main="SCI Summer", xlab="S Estimations")

CSL_SNI_Summer_Comparison<-merge(CSL_SNI_Summer, Market_Squid_Annual_Mean_Catch, by="year")

plot(CSL_SNI_Summer_Comparison$Mean_CPUE, CSL_SNI_Summer_Comparison$FO, pch=19, ylab="FO", main="SNI Summer", xlab="S/SC CPUE")
plot(CSL_SNI_Summer_Comparison$Mean_Model, CSL_SNI_Summer_Comparison$FO, pch=19, ylab="FO", main="SNI Summer", xlab="S/SC Estimations")

CSL_SNI_Summer_Comparison<-merge(CSL_SNI_Summer, Market_Squid_Annual_South_SC, by="year")

plot(CSL_SNI_Summer_Comparison$Mean_Squid_CPUE[CSL_SNI_Summer_Comparison$strata=="SC"], CSL_SNI_Summer_Comparison$FO[CSL_SNI_Summer_Comparison$strata=="SC"], pch=19,  ylab="FO", main="SNI Summer", xlab="SC CPUE")
plot(CSL_SNI_Summer_Comparison$Model_Predictions[CSL_SNI_Summer_Comparison$strata=="SC"], CSL_SNI_Summer_Comparison$FO[CSL_SNI_Summer_Comparison$strata=="SC"], pch=19, ylab="FO", main="SNI Summer", xlab="SC Estimations")

plot(CSL_SNI_Summer_Comparison$Mean_Squid_CPUE[CSL_SNI_Summer_Comparison$strata=="S"], CSL_SNI_Summer_Comparison$FO[CSL_SNI_Summer_Comparison$strata=="S"], pch=19, ylab="Num of Prey", main="SNI Summer", xlab="S CPUE")
plot(CSL_SNI_Summer_Comparison$Model_Predictions[CSL_SNI_Summer_Comparison$strata=="S"], CSL_SNI_Summer_Comparison$FO[CSL_SNI_Summer_Comparison$strata=="S"], pch=19, ylab="Num of Prey", main="SNI Summer", xlab="S Estimations")


##########estimate total CPUE in the survey#######
library(plyr)
Market_squid_total_CPUE<-ddply(Market_Squid_Annual_Lasker_Full_Krill_Stations_Full_Red, .(year), summarize, Total_CPUE=sum(Mean_Squid_CPUE))

setwd("F:/RREAS/Model_Formation/Market_Squid")

png("Squid_total_mean_CPUE_2004_2021.png", res=300, units="in", width=8, height=5)
plot(Market_squid_total_CPUE$year[Market_squid_total_CPUE$year>=2004],Market_squid_total_CPUE$Total_CPUE[Market_squid_total_CPUE$year>=2004], type="l", lwd=3, xlab=c("Year"), ylab=c("Total CPUE"), cex.lab=1.25, xlim=c(2004, 2021), ylim=c(0,20))
par(new=T)
plot(Market_squid_total_CPUE$year[Market_squid_total_CPUE$year>=2017],Market_squid_total_CPUE$Total_CPUE[Market_squid_total_CPUE$year>=2017], type="l", lwd=3,col="red", xlab=c("Year"), ylab=c("Total CPUE"), cex.lab=1.25, xlim=c(2004, 2021), ylim=c(0,20))
legend("topleft", c("Train", "Test"), col=c("black","red"), lty=1, lwd=3, bty="n", cex=1.25)
par(new=T)
plot(Market_squid_total_CPUE$year[Market_squid_total_CPUE$year>=2004],Market_squid_total_CPUE$Total_CPUE[Market_squid_total_CPUE$year>=2004], pch=19, xlab=c("Year"), ylab=c("Total CPUE"), cex.lab=1.25, xlim=c(2004, 2021), ylim=c(0,20))
par(new=T)
plot(Market_squid_total_CPUE$year[Market_squid_total_CPUE$year>=2017],Market_squid_total_CPUE$Total_CPUE[Market_squid_total_CPUE$year>=2017], pch=19, lwd=3,col="red", xlab=c("Year"), ylab=c("Total CPUE"), cex.lab=1.25, xlim=c(2004, 2021), ylim=c(0,20))
dev.off()

#########plot map of coastline and area#############

library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(viridis)
setwd("F:/RREAS/Predictors/Static")
Depth1<-raster("z_1.grd")
Depth<-as.data.frame(Depth1,xy=TRUE)
colnames(Depth)<-c("Longitude","Latitude", "Depth")
#read in the country coastline you are interested in 
RREAS_Station_Only_1$strata<- factor(RREAS_Station_Only_1$strata, levels = c("N","NC","C","SC","S"))
RREAS_Data$Unq_Station<-paste(RREAS_Data$strata, RREAS_Data$station)
RREAS_Station_Only<-RREAS_Data[,c(14,9,32,17)]
RREAS_Station_Only$Latitude<-round(RREAS_Station_Only$Latitude, digits=1)
RREAS_Station_Only$Longitude<-round(RREAS_Station_Only$Longitude, digits=1)
RREAS_Station_Only$Strata<-RREAS_Station_Only$strata

RREAS_Station_Only_1<-RREAS_Station_Only[!duplicated(RREAS_Station_Only$Unq_Station),]
RREAS_Station_Only_1$Strata<- factor(RREAS_Station_Only_1$Strata, levels = c("N","NC","C","SC","S"))

setwd("F:/RREAS/Model_Formation/Market_Squid/Figures")
world<-ne_countries(scale="medium", returnclass = "sf")
png("RREAS_Station_Figures_update.png", width=6, height=5, units="in", res=300)
ggplot(data=world) + geom_raster(data = Depth,aes(x=Longitude, y = Latitude, fill=Depth),linejoin = "mitre") + scale_fill_gradient(low = "cornflowerblue", high = "white",limits=c(-5000,0))+
  coord_fixed(ratio = 1)+geom_point(data=RREAS_Station_Only_1, aes(x=Longitude, y=Latitude, color=Strata), size=2)+scale_color_manual(values=c("red", "green","orange","hotpink","darkgreen"))+
  theme_bw()+geom_segment(aes(x = -121.5, y = 35, xend = -121.5, yend = 33.2), size=1.2,linetype=4)+geom_segment(aes(x = -121.5, y = 35, xend = -121, yend = 35), size=1.2,linetype=4)+geom_segment(aes(x = -121.5, y = 35, xend = -121.5, yend = 33.2), size=1.2,linetype=4)+geom_segment(aes(x = -121.5, y = 33.2, xend = -117, yend = 33.2), size=1.2,linetype=4)+
geom_segment(aes(x = -120.5, y = 33.2, xend = -120.5, yend = 32), size=1.2,linetype=3)+geom_segment(aes(x = -120.5, y = 32, xend = -116, yend = 32), size=1.2,linetype=3)+
labs(fill = "Depth (m)")+geom_sf()+coord_sf(xlim=c(-125.5,-116.5), ylim=c(32, 44))+
theme(legend.title=element_text(size=16),legend.text=element_text(size=14),legend.direction = "vertical", legend.box = "horizontal")
dev.off()


Market_Squid_Annual_Lasker_Full_Krill_Stations_Red$Strata<-factor(Market_Squid_Annual_Lasker_Full_Krill_Stations_Red$strata, levels=c("N","NC","C","SC","S"))
Market_Squid_Annual_Lasker_Full_Krill_Stations_N<-Market_Squid_Annual_Lasker_Full_Krill_Stations_Red[Market_Squid_Annual_Lasker_Full_Krill_Stations_Red$strata=="N"|Market_Squid_Annual_Lasker_Full_Krill_Stations_Red$strata=="NC",]

png("North_Trend_Predictions.png", height=5, width=7, units="in", res=300)
P<-ggplot(Market_Squid_Annual_Lasker_Full_Krill_Stations_N, aes(x=year, y=Mean_Squid_CPUE, group=Strata)) +geom_line(aes(color=Strata), size=1.5)+geom_point(aes( color=Strata),size=3)+theme_cowplot()+  geom_line( aes(x=year, y=Model_Predictions,  color=Strata),linetype="twodash", size=1.5)+geom_point(aes(x=year, y=Model_Predictions,  color=Strata), shape=17, size=3)+ylim(0,8)+ylab("Mean ln(CPUE+1)")+xlab("Year")+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"), legend.title=element_text(size=18,face="bold"), legend.text=element_text(size=16,face="bold"))+geom_vline(xintercept = 2016, linetype="dotted", size=1.5)+theme(legend.position = c(0.85,0.9))
P+scale_color_manual(values=c("red", "green"))
dev.off()

Market_Squid_Annual_Lasker_Full_Krill_Stations_C<-Market_Squid_Annual_Lasker_Full_Krill_Stations_Red[Market_Squid_Annual_Lasker_Full_Krill_Stations_Red$strata=="C",]

png("Central_Trend_Predictions.png", height=5, width=7, units="in", res=300)
P<-ggplot(Market_Squid_Annual_Lasker_Full_Krill_Stations_C, aes(x=year, y=Mean_Squid_CPUE, group=Strata)) +geom_line(aes(color=Strata), size=1.5)+geom_point(aes( color=Strata),size=3)+theme_cowplot()+  geom_line( aes(x=year, y=Model_Predictions,  color=Strata),linetype="twodash", size=1.5)+geom_point(aes(x=year, y=Model_Predictions,  color=Strata), shape=17, size=3)+ylim(0,8)+ylab("Mean ln(CPUE+1)")+xlab("Year")+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"), legend.title=element_text(size=18,face="bold"), legend.text=element_text(size=16,face="bold"))+geom_vline(xintercept = 2016, linetype="dotted", size=1.5)+theme(legend.position = c(0.85,0.9))
P+scale_color_manual(values=c("orange"))
dev.off()

Market_Squid_Annual_Lasker_Full_Krill_Stations_S<-Market_Squid_Annual_Lasker_Full_Krill_Stations_Red[Market_Squid_Annual_Lasker_Full_Krill_Stations_Red$strata=="S"|Market_Squid_Annual_Lasker_Full_Krill_Stations_Red$strata=="SC",]

png("South_Trend_Predictions.png", height=5, width=7, units="in", res=300)
P<-ggplot(Market_Squid_Annual_Lasker_Full_Krill_Stations_S, aes(x=year, y=Mean_Squid_CPUE, group=Strata)) +geom_line(aes(color=Strata), size=1.5)+geom_point(aes( color=Strata),size=3)+theme_cowplot()+  geom_line( aes(x=year, y=Model_Predictions,  color=Strata),linetype="twodash", size=1.5)+geom_point(aes(x=year, y=Model_Predictions,  color=Strata), shape=17, size=3)+ylim(0,8)+ylab("Mean ln(CPUE+1)")+xlab("Year")+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"), legend.title=element_text(size=18,face="bold"), legend.text=element_text(size=16,face="bold"))+geom_vline(xintercept = 2016, linetype="dotted", size=1.5)+theme(legend.position = c(0.85,0.9))
P+scale_color_manual(values=c("hotpink","darkgreen"))
dev.off()


