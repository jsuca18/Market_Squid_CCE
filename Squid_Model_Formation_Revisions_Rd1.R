###Generating market squid models: 'Recruitment', PA, Abundance, Hurdle####

#Suca, J.J. 220426, subsequently revised 220716


library(dplyr)
library(lubridate)
library(plyr)
library(mgcv)
library(MuMIn)
library(Metrics)

#######Recruitment Model#######
#read in potential predictors 
setwd("F:/RREAS/Catch_Data/Anchovy/YOY/GAMs")
General_Preconditioning<-read.csv("General_Preconditioning.csv", header=TRUE)
Upwelling_Preconditioning<-read.csv("March_June_Upwelling_Preconditioning.csv", header=TRUE)
Lasker_Events_NT<-read.csv("Lasker_Events_Mar_Jun_NT.csv", header=TRUE)
OEI_Index<-read.csv("OEI_Mar_Jun_Counts.csv", header=TRUE)
ROMS_Predictors<-read.csv("Full_RREAS_Predictors_CHL_2021.csv", header=TRUE)
SST_Squid<-read.csv("March_June_SST_2021_DL.csv", header=TRUE)
SST_JF_Squid<-read.csv("Jan_Feb_SST_2021_DL.csv", header=TRUE)
CHL_Squid<-read.csv("March_June_CHL_DL.csv", header=TRUE)
CHL_JF_Squid<-read.csv("Jan_Feb_CHL_DL.csv", header=TRUE)
U_Squid<-read.csv("March_June_U_Currents_2021_DL.csv", header=TRUE)
V_Squid<-read.csv("March_June_V_Currents_2021_DL.csv", header=TRUE)
Curl_Squid<-read.csv("March_June_Wind_Curl_2021_DL.csv", header=TRUE)
ONI<-read.csv("ONI_Values.csv", header=TRUE)
CUTI_Roll<-read.csv("CUTI_Roll.csv", header=TRUE)

#read in the full RREAS Data to set up model data
setwd("F:/RREAS/Catch_Data")
setwd("F:/RREAS/Catch_Data/Squid/Envelope_Model/Current_Code_Data")
RREAS_Data<-read.csv("rreas.data83to2020.csv", header=TRUE)
#create a unique identifier
RREAS_Data$Unique_ID<-paste(RREAS_Data$cruise, RREAS_Data$station, RREAS_Data$haul_no, sep="")

#convert to date format
RREAS_Data$Date<-as.Date(RREAS_Data$haul_date,
                         format = "%m/%d/%Y")
RREAS_Data$Latitude<-as.numeric(RREAS_Data$Latitude)
RREAS_Data$Longitude<-as.numeric(RREAS_Data$Longitude)

#parse dates to easily compare at different time scales
RREAS_Data=RREAS_Data %>%
  dplyr::mutate(year = lubridate::year(Date), 
                month = lubridate::month(Date), 
                day = lubridate::day(Date),
                DOY=lubridate::yday(Date))

#round to nearest degree to allow for comparison with CUTI data
RREAS_Data$Lat_Deg<-round(RREAS_Data$Latitude)
RREAS_Data$Lon_Deg<-round(RREAS_Data$Longitude)

#look at station count per stratum to remove strata that were undersampled
Market_Squid_Full<-RREAS_Data
Market_Squid_Full_Station_Count<-ddply(Market_Squid_Full, .(year, strata), summarize, Count_Stations=length(Unique_ID))

#compile all the predictor data
RREAS_Squid_Full<-merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(Market_Squid_Full, SST_Squid, by=c("year","strata")), V_Squid, by=c("year","strata")), U_Squid, by=c("year","strata")), CHL_Squid, by=c("year","strata")), ONI, by="year"), OEI_Index, by=c("year","strata")), Curl_Squid, by=c("year","strata")), Lasker_Events_NT, by=c("year","strata")), SST_JF_Squid, by=c("year","strata")), CHL_JF_Squid, by=c("year", "strata")), Upwelling_Preconditioning, by=c("year","Lat_Deg"))

#reduce these to the scale of unique response values
Market_Squid_Annual_Full<-ddply(RREAS_Squid_Full, .(year, strata), summarize, Mean_Squid_CPUE=mean(log(market_squid+1)), STI_BEUTI=mean(STI), STI_CUTI=mean(STI_CUTI),Mean_BEUTI=mean(Mean_BEUTI_Upwelling),Mean_JF_BEUTI=mean(Mean_JF_BEUTI), Mean_CUTI=mean(Mean_CUTI_Upwelling), Mean_SST=mean(Winter_SST),JF_SST=mean(JF_SST),JF_CHL=mean(JF_CHL),  Mean_CHL=mean(Mean_CHL), Mean_V=mean(V_Current), Mean_U=mean(U_Current), ONI_FMA=mean(FMA), ONI_MAM=mean(MAM), ONI_JFM=mean(JFM),Lasker_Events_Mar_Jun_NT=mean(Lasker_Sum_Mar_Jun_NT), OEI=mean(OEI_Sum_Mar_Jun), Curl=mean(Curl_Current) )


#separate north and south of pt conception based on hydrographic shift (e.g. Checkley Barth 2009)
Market_Squid_Annual_Full$Region<-0
Market_Squid_Annual_Full$Region[Market_Squid_Annual_Full$strata=="S"]<-"S"
Market_Squid_Annual_Full$Region[Market_Squid_Annual_Full$strata!="S"]<-"N"

#make these delimiters factors for potential by-smooths
Market_Squid_Annual_Full$Region<-as.factor(Market_Squid_Annual_Full$Region)
Market_Squid_Annual_Full$strata<-as.factor(Market_Squid_Annual_Full$strata)

#remove any strata with less than 5 stations in a given year
Market_Squid_Annual_Full_Stations<-merge(Market_Squid_Annual_Full,Market_Squid_Full_Station_Count, by=c("year","strata")) 
Market_Squid_Annual_Full_Stations_Red<-Market_Squid_Annual_Full_Stations[Market_Squid_Annual_Full_Stations$Count_Stations>=5,]

#split into a training and test set by 1998-2016 and 2017 onwards
Market_Squid_Annual_Full_Model<-Market_Squid_Annual_Full_Stations_Red[Market_Squid_Annual_Full_Stations_Red$year<=2016,]

Recruit_Predictors<-Market_Squid_Annual_Full_Model[,3:17]

#assess collinearity in predictors
library(PerformanceAnalytics)
png("Recruitment_Squid_Predictors.png", height=10, width=12,units="in", res=400 )
chart.Correlation(Recruit_Predictors)
dev.off()

#make a set of candidate GAMs
#explore if there are regional differences in the response to certain variables
#make different models with candidate predictors that are not useable
#see if there is a difference in the model residuals relative to number of stations
Market_Annual_GAM1<-gam(Mean_Squid_CPUE~s(Mean_SST,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun_NT, k=6)+s(Mean_CHL, k=6)+s(Curl, k=6)+s(OEI, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Full_Model, family=tw, na.action = "na.fail")

Market_Annual_GAM2<-gam(Mean_Squid_CPUE~s(Mean_SST,by=Region,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun_NT, k=6)+s(Mean_CHL, k=6)+s(Curl, k=6)+s(OEI, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Full_Model, family=tw, na.action = "na.fail")

Market_Annual_GAM3<-gam(Mean_Squid_CPUE~s(Mean_SST,by=Region, k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun_NT, k=6)+s(Mean_CHL, k=6)+s(Curl,by=Region, k=6)+s(OEI, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Full_Model, family=tw, na.action = "na.fail")

Market_Annual_GAM4<-gam(Mean_Squid_CPUE~s(Mean_SST,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun_NT,k=6)+s(Mean_CHL, k=6)+s(Curl, k=6)+s(OEI, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Full_Model, family=tw, na.action = "na.fail")

Market_Annual_GAM5<-gam(Mean_Squid_CPUE~s(Mean_BEUTI,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun_NT, k=6)+s(Mean_CHL, k=6)+s(Curl, k=6)+s(OEI, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Full_Model, family=tw, na.action = "na.fail")

Market_Annual_GAM6<-gam(Mean_Squid_CPUE~s(Mean_BEUTI,by=Region,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun_NT, k=6)+s(Mean_CHL, k=6)+s(Curl, k=6)+s(OEI, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Full_Model, family=tw, na.action = "na.fail")

Market_Annual_GAM7<-gam(Mean_Squid_CPUE~s(Mean_BEUTI,by=Region, k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun_NT, k=6)+s(Mean_CHL, k=6)+s(Curl,by=Region, k=6)+s(OEI, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Full_Model, family=tw, na.action = "na.fail")

Market_Annual_GAM8<-gam(Mean_Squid_CPUE~s(Mean_BEUTI,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun_NT, by=Region,k=6)+s(Mean_CHL, k=6)+s(Curl,by=Region, k=6)+s(OEI, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Full_Model, family=tw, na.action = "na.fail")


#'Dredge' each full candidate set to identify best fit
Market_Annual_Dredge1<-dredge(Market_Annual_GAM1, rank="AIC",trace=5)
Market_Annual_Dredge2<-dredge(Market_Annual_GAM2, rank="AIC",trace=5)
Market_Annual_Dredge3<-dredge(Market_Annual_GAM3, rank="AIC",trace=5)
Market_Annual_Dredge4<-dredge(Market_Annual_GAM4, rank="AIC",trace=5)
Market_Annual_Dredge5<-dredge(Market_Annual_GAM5, rank="AIC",trace=5)
Market_Annual_Dredge6<-dredge(Market_Annual_GAM6, rank="AIC",trace=5)
Market_Annual_Dredge7<-dredge(Market_Annual_GAM7, rank="AIC",trace=5)
Market_Annual_Dredge8<-dredge(Market_Annual_GAM8, rank="AIC",trace=5)

#the following it the best fit model of the group
Market_Annual_1<-gam(Mean_Squid_CPUE~s(Mean_SST,  k=6)+s(Lasker_Events_Mar_Jun_NT, k=6)+s(ONI_JFM, k=6), data=Market_Squid_Annual_Full_Model, family=tw, na.action = "na.fail")

#but these two are similar fit (del AIC<2)
Market_Annual_2<-gam(Mean_Squid_CPUE~s(Mean_SST,  k=6)+s(Lasker_Events_Mar_Jun_NT, k=6)+s(ONI_JFM, k=6)+s(Curl,k=6), data=Market_Squid_Annual_Full_Model, family=tw, na.action = "na.fail")

Market_Annual_3<-gam(Mean_Squid_CPUE~s(Mean_SST,by=Region,  k=6)+s(Lasker_Events_Mar_Jun_NT, k=6)+s(ONI_JFM, k=6), data=Market_Squid_Annual_Full_Model, family=tw, na.action = "na.fail")

AIC(Market_Annual_1)
AIC(Market_Annual_2)
AIC(Market_Annual_3)

#make a model average of these three similarly fit models
Market_Squid_Recruitment_Models<-list(Market_Annual_1, Market_Annual_2, Market_Annual_3)

Market_Annual_Avg<-model.avg(Market_Squid_Recruitment_Models)


#separate the 'test' set of data and evaluate fit, in addition to 'full' model
Market_Squid_Annual_Full_Recent<-Market_Squid_Annual_Full_Stations_Red[Market_Squid_Annual_Full_Stations_Red$year>=2017,]
#predict over test set
Market_Squid_Annual_Full_Recent$Model_Predictions<-predict(Market_Annual_Avg,Market_Squid_Annual_Full_Recent, type="response" )
#predict over training set
Market_Squid_Annual_Full_Model$Model_Predictions<-predict(Market_Annual_Avg,Market_Squid_Annual_Full_Model, type="response" )
#predict over full dataset
Market_Squid_Annual_Full_Stations_Red$Model_Predictions<-predict(Market_Annual_Avg,Market_Squid_Annual_Full_Stations_Red, type="response" )
#look at simple R2 of values and RMSE for test set
cor(Market_Squid_Annual_Full_Recent$Model_Predictions, Market_Squid_Annual_Full_Recent$Mean_Squid_CPUE)^2
rmse(Market_Squid_Annual_Full_Recent$Mean_Squid_CPUE,Market_Squid_Annual_Full_Recent$Model_Predictions)

#look at simple R2 of values and RMSE for train set
cor(Market_Squid_Annual_Full_Model$Model_Predictions, Market_Squid_Annual_Full_Model$Mean_Squid_CPUE)^2
rmse(Market_Squid_Annual_Full_Model$Mean_Squid_CPUE,Market_Squid_Annual_Full_Model$Model_Predictions)

#make figures showing model fit to train and test sets.
setwd("F:/RREAS/Model_Formation/Market_Squid/Figures")
png("Squid_recruitment_Model_Predictions_thru_2016.png", height=5, width=6, res=200, units="in")
plot(Market_Squid_Annual_Full_Model$Model_Predictions, Market_Squid_Annual_Full_Model$Mean_Squid_CPUE, ylab=c("Mean Strata ln(CPUE+1)"), xlab=c("Predicted Mean ln(CPUE+1)"), main=c("Train Set 1998-2016"), cex.lab=1.25, pch=19)
dev.off()

png("Squid_recruitment_Model_Predicitions_2021_Forecast.png", height=5, width=6, res=200, units="in")
plot(Market_Squid_Annual_Full_Recent$Model_Predictions, Market_Squid_Annual_Full_Recent$Mean_Squid_CPUE, ylab=c("Mean Strata ln(CPUE+1)"), xlab=c("Predicted Mean ln(CPUE+1)"), main=c("Test Set 2017-2021"), cex.lab=1.25, pch=19)
dev.off()


#compare the residuals with time and by stratum to see if there are issues
png("Squid_recruitment_Model_Predicitions_residuals_yearv2.png", height=5, width=6, res=200, units="in")
boxplot(Residuals~year, data=Market_Squid_Annual_Full_Stations_Red, main=c("'Recruitment' Model Residuals"))
dev.off()

png("Squid_recruitment_Model_Predicitions_residuals_strata.png", height=5, width=6, res=200, units="in")
boxplot(Residuals~strata, data=Market_Squid_Annual_Full_Stations_Red, main=c("'Recruitment' Model Residuals"))
dev.off()

###########PA model#############
Market_Squid_Full$Unique_ID<-paste(Market_Squid_Full$cruise, Market_Squid_Full$station, Market_Squid_Full$haul_no, sep=" ")

Market_Squid_Full$CPUE<-log(Market_Squid_Full$market_squid+1)
Market_Squid_ROMS_Pred<-merge(Market_Squid_Full, ROMS_Predictors, by=c("Unique_ID"))
#now make model predictions for every stratum for each year
Prediction_for_All_Regions<-merge(merge(merge( SST_Squid, Lasker_Events_NT,by=c("year","strata")),Curl_Squid, by=c("year","strata")), ONI, by="year")
Prediction_for_All_Regions$Mean_SST<-Prediction_for_All_Regions$Winter_SST
Prediction_for_All_Regions$ONI_JFM<-Prediction_for_All_Regions$JFM
Prediction_for_All_Regions$Lasker_Events_Mar_Jun_NT<-Prediction_for_All_Regions$Lasker_Sum_Mar_Jun_NT
Prediction_for_All_Regions$Curl<-Prediction_for_All_Regions$Curl_Current
Prediction_for_All_Regions$Region<-0
Prediction_for_All_Regions$Region[Prediction_for_All_Regions$strata=="S"]<-"S"
Prediction_for_All_Regions$Region[Prediction_for_All_Regions$strata!="S"]<-"N"
Prediction_for_All_Regions$Region<-as.factor(Prediction_for_All_Regions$Region)

#generate 'Recruitment' estimates
Prediction_for_All_Regions$Model_Predictions<-predict(Market_Annual_Avg,Prediction_for_All_Regions, type="response" )
Market_Squid_Abund_Preds<-Prediction_for_All_Regions[,c(1,2,29)]


Market_Squid_For_Spatial_Model<-merge(Market_Squid_ROMS_Pred, Market_Squid_Abund_Preds, by=c("year","strata"))
Market_Squid_For_Spatial_Model_NA<-Market_Squid_For_Spatial_Model[is.na(Market_Squid_For_Spatial_Model$Model_Predictions),]

#calculate TKE from U and V
Market_Squid_For_Spatial_Model$TKE<-0.5*((Market_Squid_For_Spatial_Model$U_Val^2+Market_Squid_For_Spatial_Model$V_Val^2)^0.5)


#merge in daily CUTI values 
CUTI_Roll$Lat_Deg<-CUTI_Roll$latitude..degrees_north.
Market_Squid_For_Spatial_Model_Full<-merge(Market_Squid_For_Spatial_Model, CUTI_Roll, by=c("Lat_Deg", "year","DOY"), all.x=TRUE)
Market_Squid_For_Spatial_Model_Full$Latitude<-Market_Squid_For_Spatial_Model_Full$Latitude.x
Market_Squid_For_Spatial_Model_Full$CUTI<-Market_Squid_For_Spatial_Model_Full$CUTI..m2.s.1.

#select the set of candidate variables from the larger dataframe
Vars_Used_Names<-c("Unique_ID","SST", "SSH","SST_sd", "SSH_sd","year","strata","Curl_Val",
                   "Depth_Val","TKE","Rugosity_Val","CHL_Val","CUTI_3day","CUTI","Lunar",
                   "Latitude","Longitude", "Model_Predictions","CPUE", "DOY")
Market_Squid_Model_Data<-Market_Squid_For_Spatial_Model_Full[, colnames(Market_Squid_For_Spatial_Model_Full) %in% Vars_Used_Names]

#get rid of stations with empty predictors 
Market_Squid_Model_Data_Full<-Market_Squid_Model_Data[complete.cases(Market_Squid_Model_Data),]

#remove any duplicate rows
Market_Squid_Model_Data_Full<-Market_Squid_Model_Data_Full[!duplicated((Market_Squid_Model_Data_Full)),]
Market_Squid_Model_Data_Full$Date<-as.Date(Market_Squid_Model_Data_Full$DOY-1, origin=paste(Market_Squid_Model_Data_Full$year,"01-01",sep="-"))

for ( i in 1: nrow(Market_Squid_Model_Data_Full)){
  Market_Squid_Model_Data_Full$PA[i]<-ifelse(Market_Squid_Model_Data_Full$CPUE[i]>0,1,0)
}

RREAS_Squid_Model_Hurdle<-Market_Squid_Model_Data_Full[Market_Squid_Model_Data_Full$year<=2016,]

setwd("F:/RREAS/Catch_Data/Squid/Envelope_Model")
Squid_hurdle_model_Data<-RREAS_Squid_Model_Hurdle

PA_Predictors<-Squid_hurdle_model_Data[,9:20]

#look for collinearity among predictors 
setwd("F:/RREAS/Model_Formation/Market_Squid/Figures")
library(PerformanceAnalytics)
png("PA_Abund_Squid_Predictors_Pub.png", height=10, width=12,units="in", res=400 )
chart.Correlation(PA_Predictors)
dev.off()


###Running candidate PA models
library(mgcv)

Squid_Model_ROMS_PA_SST_SSH_sd<-gam(PA~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(Curl_Val,k=6)+s(Lunar, k=6)+
                               s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SST,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = binomial, data=Squid_hurdle_model_Data, na.action = "na.fail")
Squid_Model_ROMS_PA_SSH_SSH_sd<-gam(PA~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(Curl_Val,k=6)+s(Lunar, k=6)+
                               s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SSH,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = binomial, data=Squid_hurdle_model_Data, na.action = "na.fail")
Squid_Model_ROMS_PA_SST_TKE<-gam(PA~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(TKE, k=6)+s(Curl_Val,k=6)+s(Lunar, k=6)+
                                      s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SST,k=6)+te(Longitude,Latitude, k=6), family = binomial, data=Squid_hurdle_model_Data, na.action = "na.fail")
Squid_Model_ROMS_PA_SSH_TKE<-gam(PA~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(TKE, k=6)+s(Curl_Val,k=6)+s(Lunar, k=6)+
                                      s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SSH,k=6)+te(Longitude,Latitude, k=6), family = binomial, data=Squid_hurdle_model_Data, na.action = "na.fail")

Squid_Model_ROMS_PA_SST_SSH_sd_CUTI<-gam(PA~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(CUTI,k=6)+s(Lunar, k=6)+
                                             s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SST,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = binomial, data=Squid_hurdle_model_Data, na.action = "na.fail")
Squid_Model_ROMS_PA_SSH_SSH_sd_CUTI<-gam(PA~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(CUTI,k=6)+s(Lunar, k=6)+
                                             s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SSH,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = binomial, data=Squid_hurdle_model_Data, na.action = "na.fail")
Squid_Model_ROMS_PA_SST_TKE_CUTI<-gam(PA~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(TKE, k=6)+s(CUTI,k=6)+s(Lunar, k=6)+
                                          s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SST,k=6)+te(Longitude,Latitude, k=6), family = binomial, data=Squid_hurdle_model_Data, na.action = "na.fail")
Squid_Model_ROMS_PA_SSH_TKE_CUTI<-gam(PA~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(TKE, k=6)+s(CUTI,k=6)+s(Lunar, k=6)+
                                          s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SSH,k=6)+te(Longitude,Latitude, k=6), family = binomial, data=Squid_hurdle_model_Data, na.action = "na.fail")

Squid_Model_ROMS_PA_SST_SSH_sd_Dredge<-dredge(Squid_Model_ROMS_PA_SST_SSH_sd, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_PA_SSH_SSH_sd_Dredge<-dredge(Squid_Model_ROMS_PA_SSH_SSH_sd, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_PA_SST_TKE_Dredge<-dredge(Squid_Model_ROMS_PA_SST_TKE, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_PA_SSH_TKE_Dredge<-dredge(Squid_Model_ROMS_PA_SSH_TKE, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_PA_SST_SSH_sd_CUTI_Dredge<-dredge(Squid_Model_ROMS_PA_SST_SSH_sd_CUTI, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_PA_SSH_SSH_sd_CUTI_Dredge<-dredge(Squid_Model_ROMS_PA_SSH_SSH_sd_CUTI, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_PA_SST_TKE_CUTI_Dredge<-dredge(Squid_Model_ROMS_PA_SST_TKE_CUTI, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_PA_SSH_TKE_CUTI_Dredge<-dredge(Squid_Model_ROMS_PA_SSH_TKE_CUTI, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)


#now generate the abundance models for presence only 
Squid_hurdle_model_Data_Abund<-Squid_hurdle_model_Data[Squid_hurdle_model_Data$Catch>0,]

#have a look at the distribution of catch (raw counts)
hist(Squid_hurdle_model_Data_Abund$Catch)

Squid_Model_ROMS_Abund_SST_SSH_sd<-gam(Catch~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(Curl_Val,k=6)+s(Lunar, k=6)+
                                      s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SST,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = nb, data=Squid_hurdle_model_Data_Abund, na.action = "na.fail")
Squid_Model_ROMS_Abund_SSH_SSH_sd<-gam(Catch~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(Curl_Val,k=6)+s(Lunar, k=6)+
                                      s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SSH,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = nb, data=Squid_hurdle_model_Data_Abund, na.action = "na.fail")
Squid_Model_ROMS_Abund_SST_TKE<-gam(Catch~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(TKE, k=6)+s(Curl_Val,k=6)+s(Lunar, k=6)+
                                   s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SST,k=6)+te(Longitude,Latitude, k=6), family = nb, data=Squid_hurdle_model_Data_Abund, na.action = "na.fail")
Squid_Model_ROMS_Abund_SSH_TKE<-gam(Catch~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(TKE, k=6)+s(Curl_Val,k=6)+s(Lunar, k=6)+
                                   s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SSH,k=6)+te(Longitude,Latitude, k=6), family = nb, data=Squid_hurdle_model_Data_Abund, na.action = "na.fail")

Squid_Model_ROMS_Abund_SST_SSH_sd_CUTI<-gam(Catch~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(CUTI,k=6)+s(Lunar, k=6)+
                                           s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SST,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = nb, data=Squid_hurdle_model_Data_Abund, na.action = "na.fail")
Squid_Model_ROMS_Abund_SSH_SSH_sd_CUTI<-gam(Catch~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(CUTI,k=6)+s(Lunar, k=6)+
                                           s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SSH,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = nb, data=Squid_hurdle_model_Data_Abund, na.action = "na.fail")
Squid_Model_ROMS_Abund_SST_TKE_CUTI<-gam(Catch~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(TKE, k=6)+s(CUTI,k=6)+s(Lunar, k=6)+
                                        s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SST,k=6)+te(Longitude,Latitude, k=6), family = nb, data=Squid_hurdle_model_Data_Abund, na.action = "na.fail")
Squid_Model_ROMS_Abund_SSH_TKE_CUTI<-gam(Catch~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(TKE, k=6)+s(CUTI,k=6)+s(Lunar, k=6)+
                                        s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SSH,k=6)+te(Longitude,Latitude, k=6), family = nb, data=Squid_hurdle_model_Data_Abund, na.action = "na.fail")

Squid_Model_ROMS_Abund_SST_SSH_sd_Dredge<-dredge(Squid_Model_ROMS_Abund_SST_SSH_sd, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_Abund_SSH_SSH_sd_Dredge<-dredge(Squid_Model_ROMS_Abund_SSH_SSH_sd, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_Abund_SST_TKE_Dredge<-dredge(Squid_Model_ROMS_Abund_SST_TKE, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_Abund_SSH_TKE_Dredge<-dredge(Squid_Model_ROMS_Abund_SSH_TKE, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_Abund_SST_SSH_sd_CUTI_Dredge<-dredge(Squid_Model_ROMS_Abund_SST_SSH_sd_CUTI, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_Abund_SSH_SSH_sd_CUTI_Dredge<-dredge(Squid_Model_ROMS_Abund_SSH_SSH_sd_CUTI, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_Abund_SST_TKE_CUTI_Dredge<-dredge(Squid_Model_ROMS_Abund_SST_TKE_CUTI, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_Abund_SSH_TKE_CUTI_Dredge<-dredge(Squid_Model_ROMS_Abund_SSH_TKE_CUTI, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)



save.image("F:/RREAS/Catch_Data/Squid/Envelope_Model/Squid_Dredge_Results_220712_Catch.RData")

load("F:/RREAS/Catch_Data/Squid/Envelope_Model/Squid_Dredge_Results_220712_Catch.RData")


#########Compare fit to new data###########
RREAS_Squid_Model_Recent<-Market_Squid_Model_Data_Full[Market_Squid_Model_Data_Full$year>=2017,]

#take model averages for similarly well fit model
Squid_PA_Model_Avg_Lunar<-model.avg(Squid_Model_ROMS_PA_SSH_SSH_sd_Dredge, delta<2, fit=TRUE)
Squid_Abund_Model_Avg_Catch<-model.avg(Squid_Model_ROMS_Abund_SSH_TKE_Dredge, delta<2, fit=TRUE)
#predict prob occurrence and haul specific abundance
Market_Squid_Model_Data_Full$PA_Estimations<-predict(Squid_PA_Model_Avg_Lunar,Market_Squid_Model_Data_Full, type="response" )

Market_Squid_Model_Data_Full$Abund_Estimations<-predict(Squid_Abund_Model_Avg_Catch,Market_Squid_Model_Data_Full, type="response")

Market_Squid_Model_Data_Full$Hurdle_Estimate<-Market_Squid_Model_Data_Full$PA_Estimations*Market_Squid_Model_Data_Full$Abund_Estimations


#plot and look at the quality of fit to the training set
library(Metrics)
Market_Squid_Model_Data_Full_Model_Only<-Market_Squid_Model_Data_Full[Market_Squid_Model_Data_Full$year<=2016,]

cor(log(Market_Squid_Model_Data_Full_Model_Only$Hurdle_Estimate+1), Market_Squid_Model_Data_Full_Model_Only$CPUE)^2

rmse(log(Market_Squid_Model_Data_Full_Model_Only$Hurdle_Estimate+1), Market_Squid_Model_Data_Full_Model_Only$CPUE)

#plot and look at the quality of fit for the test set
Market_Squid_Model_Data_Full_Recent<-Market_Squid_Model_Data_Full[Market_Squid_Model_Data_Full$year>=2017,]
cor(log(Market_Squid_Model_Data_Full_Recent$Hurdle_Estimate+1), Market_Squid_Model_Data_Full_Recent$CPUE)^2
rmse(log(Market_Squid_Model_Data_Full_Recent$Hurdle_Estimate+1), Market_Squid_Model_Data_Full_Recent$CPUE)

plot(log(Market_Squid_Model_Data_Full_Recent$Hurdle_Estimate+1), log(Market_Squid_Model_Data_Full_Recent$Catch+1), xlab=c("Hurdle Estimate"), ylab=c("ln(CPUE+1)"), main=c("Hurdle Model Estimate 2017-2021"), cex.lab=1.25, pch=19, xlim=c(0,10), ylim=c(0,10))
plot(log(Market_Squid_Model_Data_Full_Model_Only$Hurdle_Estimate+1), log(Market_Squid_Model_Data_Full_Model_Only$Catch+1), xlab=c("Hurdle Estimate"), ylab=c("ln(CPUE+1)"), main=c("Hurdle Model Estimate 2017-2021"), cex.lab=1.25, pch=19, xlim=c(0,10), ylim=c(0,10))

#fit a ROC and calculate AUC as an a model fit metric for PA
library(ROCR)
Predictions<-as.vector(Market_Squid_Model_Data_Full_Model_Only$PA_Estimations)
Vals<-Market_Squid_Model_Data_Full_Model_Only$PA
#predict actual p/a of test set based on the predicted values
pred=prediction(Predictions,Vals)#response variable
#measure AUC of ROC curve as a performance measure
auc=performance(pred,measure = "auc")

auc_squid_train=as.numeric(auc@y.values)

library(verification)
tiff("Market_ROC_Full_220310.tif", height=5, width=5, units="in", res=300)
roc.plot(Market_Squid_Model_Data_Full_Model_Only$PA,Predictions, show.thres=FALSE, main=c("Market Squid P/A 1998-2016"), ylab=c("True Positive Rate"), xlab="False Positive Rate")
dev.off()


#now do the same thing for the test set
Predictions<-as.vector(Market_Squid_Model_Data_Full_Recent$PA_Estimations)
Vals<-Market_Squid_Model_Data_Full_Recent$PA
#predict actual p/a of test set based on the predicted values
pred=prediction(Predictions,Vals)#response variable
#measure AUC of ROC curve as a performance measure
auc=performance(pred,measure = "auc")

auc_squid_Recent=as.numeric(auc@y.values)

library(verification)
tiff("Market_ROC_Recent_220310.tif", height=5, width=5, units="in", res=300)
roc.plot(Market_Squid_Model_Data_Full_Recent$PA,Predictions, show.thres=FALSE, main=c("Market Squid P/A 2017-2021"), ylab=c("True Positive Rate"), xlab="False Positive Rate")
dev.off()


###########Compare with abundance only model############

#subset full dataframe to present only
Market_Squid_Model_Data_Pres<-Market_Squid_Model_Data_Full[Market_Squid_Model_Data_Full$Catch>0,]

cor(Market_Squid_Model_Data_Pres$Abund_Estimations, Market_Squid_Model_Data_Pres$Catch)^2
cor(log(Market_Squid_Model_Data_Pres$Abund_Estimations+1), log(Market_Squid_Model_Data_Pres$Catch+1))^2
rmse(log(Market_Squid_Model_Data_Pres$Abund_Estimations+1), log(Market_Squid_Model_Data_Pres$Catch+1))


#look at the abundance model fit to the training set 
Market_Squid_Model_Data_Pres_Model_Only<-Market_Squid_Model_Data_Pres[Market_Squid_Model_Data_Pres$year<=2016,]

cor(log(Market_Squid_Model_Data_Pres_Model_Only$Abund_Estimations+1), log(Market_Squid_Model_Data_Pres_Model_Only$Catch+1))^2
rmse(log(Market_Squid_Model_Data_Pres_Model_Only$Abund_Estimations+1), log(Market_Squid_Model_Data_Pres_Model_Only$Catch+1))


#compare abundance model fit to the test set
Market_Squid_Model_Data_Pres_Recent<-Market_Squid_Model_Data_Pres[Market_Squid_Model_Data_Pres$year>=2017,]

cor(log(Market_Squid_Model_Data_Pres_Recent$Abund_Estimations+1), log(Market_Squid_Model_Data_Pres_Recent$Catch+1))^2
rmse(log(Market_Squid_Model_Data_Pres_Recent$Abund_Estimations+1), log(Market_Squid_Model_Data_Pres_Recent$Catch+1))



