###Generating market squid models: 'Recruitment', PA, Abundance, Hurdle####

#Suca, J.J. 220426


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

#make these delimitors factors for potential by-smooths
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
png("Recruitment_Squid_Predictors_Pub.png", height=10, width=12,units="in", res=400 )
chart.Correlation(Recruit_Predictors)
dev.off()

#make a set of candidate GAMs
#explore if there are regional differences in the response to certain variables
#make different models with candidate predictors that are not useable
#see if there is a difference in the model residuals relative to number of stations

Market_Annual_GAM1<-gam(Mean_Squid_CPUE~s(Mean_SST,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun_NT,by=Region, k=6)+s(Mean_CHL, k=6)+s(Curl,by=Region, k=6)+s(OEI, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Full_Model, family=nb, na.action = "na.fail")

Market_Annual_GAM2<-gam(Mean_Squid_CPUE~s(Mean_SST,by=Region,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun_NT, k=6)+s(Mean_CHL, k=6)+s(Curl, k=6)+s(OEI, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Full_Model, family=nb, na.action = "na.fail")

Market_Annual_GAM3<-gam(Mean_Squid_CPUE~s(Mean_SST,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun_NT, k=6)+s(Mean_CHL, k=6)+s(Curl, k=6)+s(OEI, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Full_Model, family=nb, na.action = "na.fail")

Market_Annual_GAM4<-gam(Mean_Squid_CPUE~s(Mean_SST,by=Region,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun_NT, by=Region,k=6)+s(Mean_CHL, k=6)+s(Curl,by=Region, k=6)+s(OEI, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Full_Model, family=nb, na.action = "na.fail")

Market_Annual_GAM5<-gam(Mean_Squid_CPUE~s(Mean_BEUTI,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun_NT,by=Region, k=6)+s(Mean_CHL, k=6)+s(Curl,by=Region, k=6)+s(OEI, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Full_Model, family=nb, na.action = "na.fail")

Market_Annual_GAM6<-gam(Mean_Squid_CPUE~s(Mean_BEUTI,by=Region,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun_NT, k=6)+s(Mean_CHL, k=6)+s(Curl, k=6)+s(OEI, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Full_Model, family=nb, na.action = "na.fail")

Market_Annual_GAM7<-gam(Mean_Squid_CPUE~s(Mean_BEUTI,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun_NT, k=6)+s(Mean_CHL, k=6)+s(Curl, k=6)+s(OEI, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Full_Model, family=nb, na.action = "na.fail")

Market_Annual_GAM8<-gam(Mean_Squid_CPUE~s(Mean_BEUTI,by=Region,k=6)+s(STI_BEUTI, k=6)+s(Lasker_Events_Mar_Jun_NT, by=Region,k=6)+s(Mean_CHL, k=6)+s(Curl,by=Region, k=6)+s(OEI, k=6)+s(ONI_JFM,k=6), data=Market_Squid_Annual_Full_Model, family=nb, na.action = "na.fail")


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
Market_Annual_1<-gam(Mean_Squid_CPUE~s(Mean_SST,  k=6)+s(Lasker_Events_Mar_Jun_NT, k=6)+s(ONI_JFM, k=6), data=Market_Squid_Annual_Full_Model, family=nb, na.action = "na.fail")

#but these two are similar fit (del AIC<2)
Market_Annual_2<-gam(Mean_Squid_CPUE~s(Mean_SST,  k=6)+s(Lasker_Events_Mar_Jun_NT, k=6)+s(ONI_JFM, k=6)+s(Curl,k=6), data=Market_Squid_Annual_Full_Model, family=nb, na.action = "na.fail")

Market_Annual_3<-gam(Mean_Squid_CPUE~s(Mean_SST,by=Region,  k=6)+s(Lasker_Events_Mar_Jun_NT, k=6)+s(ONI_JFM, k=6), data=Market_Squid_Annual_Full_Model, family=nb, na.action = "na.fail")

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
png("Squid_recruitment_Model_Predictions_thru_2016_Model.png", height=5, width=6, res=200, units="in")
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

Prediction_for_All_Regions$Model_Predictions<-predict(Market_Annual_Avg,Prediction_for_All_Regions, type="response" )
Market_Squid_Abund_Preds<-Prediction_for_All_Regions[,c(1,2,29)]

plot(Market_Squid_Abund_Preds$year[Market_Squid_Abund_Preds$strata=="S"], Market_Squid_Abund_Preds$Model_Predictions[Market_Squid_Abund_Preds$strata=="S"])
plot(Market_Squid_Abund_Preds$year[Market_Squid_Abund_Preds$strata=="SC"], Market_Squid_Abund_Preds$Model_Predictions[Market_Squid_Abund_Preds$strata=="SC"])



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
                   "Depth_Val","TKE","Rugosity_Val","CHL_Val","CUTI_3day","CUTI",
                   "Latitude","Longitude", "Model_Predictions","CPUE", "DOY")
Market_Squid_Model_Data<-Market_Squid_For_Spatial_Model_Full[, colnames(Market_Squid_For_Spatial_Model_Full) %in% Vars_Used_Names]



Market_Squid_Model_Data_Full<-Market_Squid_Model_Data[complete.cases(Market_Squid_Model_Data),]

Market_Squid_Model_Data_Full<-Market_Squid_Model_Data_Full[!duplicated((Market_Squid_Model_Data_Full)),]
library(lunar)
Market_Squid_Model_Data_Full$Date<-as.Date(Market_Squid_Model_Data_Full$DOY-1, origin=paste(Market_Squid_Model_Data_Full$year,"01-01",sep="-"))
Market_Squid_Model_Data_Full$Lunar<-lunar.illumination(Market_Squid_Model_Data_Full$Date, shift= -8 )
for ( i in 1: nrow(Market_Squid_Model_Data_Full)){
  Market_Squid_Model_Data_Full$PA[i]<-ifelse(Market_Squid_Model_Data_Full$CPUE[i]>0,1,0)
}

RREAS_Squid_Model_Hurdle<-Market_Squid_Model_Data_Full[Market_Squid_Model_Data_Full$year<=2016,]

setwd("F:/RREAS/Catch_Data/Squid/Envelope_Model")
write.csv(RREAS_Squid_Model_Hurdle,"RREAS_Squid_Hurdle_model_Data_220309.csv")


setwd("F:/RREAS/Catch_Data/Squid/Envelope_Model")
Squid_hurdle_model_Data<-read.csv("RREAS_Squid_Hurdle_model_Data_220309.csv", header=TRUE)

PA_Predictors<-Squid_hurdle_model_Data[,9:20]

setwd("F:/RREAS/Model_Formation/Market_Squid/Figures")
library(PerformanceAnalytics)
png("PA_Abund_Squid_Predictors_Pub.png", height=10, width=12,units="in", res=400 )
chart.Correlation(PA_Predictors)
dev.off()



###try adding lunar illumination to see if it improves matters. 
library(mgcv)

Squid_Model_ROMS_PA_SST_SSH_sd<-gam(PA~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(Curl_Val,k=6)+
                               s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SST,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = binomial, data=Squid_hurdle_model_Data, na.action = "na.fail")
Squid_Model_ROMS_PA_SSH_SSH_sd<-gam(PA~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(Curl_Val,k=6)+
                               s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SSH,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = binomial, data=Squid_hurdle_model_Data, na.action = "na.fail")
Squid_Model_ROMS_PA_SST_TKE<-gam(PA~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(TKE, k=6)+s(Curl_Val,k=6)+
                                      s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SST,k=6)+te(Longitude,Latitude, k=6), family = binomial, data=Squid_hurdle_model_Data, na.action = "na.fail")
Squid_Model_ROMS_PA_SSH_TKE<-gam(PA~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(TKE, k=6)+s(Curl_Val,k=6)+
                                      s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SSH,k=6)+te(Longitude,Latitude, k=6), family = binomial, data=Squid_hurdle_model_Data, na.action = "na.fail")

Squid_Model_ROMS_PA_SST_SSH_sd_CUTI<-gam(PA~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(CUTI,k=6)+
                                             s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SST,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = binomial, data=Squid_hurdle_model_Data, na.action = "na.fail")
Squid_Model_ROMS_PA_SSH_SSH_sd_CUTI<-gam(PA~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(CUTI,k=6)+
                                             s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SSH,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = binomial, data=Squid_hurdle_model_Data, na.action = "na.fail")
Squid_Model_ROMS_PA_SST_TKE_CUTI<-gam(PA~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(TKE, k=6)+s(CUTI,k=6)+
                                          s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SST,k=6)+te(Longitude,Latitude, k=6), family = binomial, data=Squid_hurdle_model_Data, na.action = "na.fail")
Squid_Model_ROMS_PA_SSH_TKE_CUTI<-gam(PA~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(TKE, k=6)+s(CUTI,k=6)+
                                          s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SSH,k=6)+te(Longitude,Latitude, k=6), family = binomial, data=Squid_hurdle_model_Data, na.action = "na.fail")

Squid_Model_ROMS_PA_SST_SSH_sd_Dredge<-dredge(Squid_Model_ROMS_PA_SST_SSH_sd, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_PA_SSH_SSH_sd_Dredge<-dredge(Squid_Model_ROMS_PA_SSH_SSH_sd, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_PA_SST_TKE_Dredge<-dredge(Squid_Model_ROMS_PA_SST_TKE, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_PA_SSH_TKE_Dredge<-dredge(Squid_Model_ROMS_PA_SSH_TKE, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_PA_SST_SSH_sd_CUTI_Dredge<-dredge(Squid_Model_ROMS_PA_SST_SSH_sd_CUTI, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_PA_SSH_SSH_sd_CUTI_Dredge<-dredge(Squid_Model_ROMS_PA_SSH_SSH_sd_CUTI, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_PA_SST_TKE_CUTI_Dredge<-dredge(Squid_Model_ROMS_PA_SST_TKE_CUTI, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_PA_SSH_TKE_CUTI_Dredge<-dredge(Squid_Model_ROMS_PA_SSH_TKE_CUTI, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)

Squid_Model_ROMS_PA_SSH_SSH_sd_Lunar<-gam(PA~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(Curl_Val,k=6)+s(Lunar, k=6)+
                                      s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SSH,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = binomial, data=Squid_hurdle_model_Data, na.action = "na.fail")

plot(Squid_Model_ROMS_PA_SSH_SSH_sd_Lunar)
Squid_Model_ROMS_PA_SSH_SSH_sd_Lunar_Dredge<-dredge(Squid_Model_ROMS_PA_SSH_SSH_sd_Lunar, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)


Squid_hurdle_model_Data_Abund<-Squid_hurdle_model_Data[Squid_hurdle_model_Data$CPUE>0,]

hist(Squid_hurdle_model_Data_Abund$CPUE)


Squid_Model_ROMS_Abund_SST_SSH_sd<-gam(CPUE~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(Curl_Val,k=6)+
                                      s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SST,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = nb, data=Squid_hurdle_model_Data_Abund, na.action = "na.fail")
Squid_Model_ROMS_Abund_SSH_SSH_sd<-gam(CPUE~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(Curl_Val,k=6)+
                                      s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SSH,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = nb, data=Squid_hurdle_model_Data_Abund, na.action = "na.fail")
Squid_Model_ROMS_Abund_SST_TKE<-gam(CPUE~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(TKE, k=6)+s(Curl_Val,k=6)+
                                   s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SST,k=6)+te(Longitude,Latitude, k=6), family = nb, data=Squid_hurdle_model_Data_Abund, na.action = "na.fail")
Squid_Model_ROMS_Abund_SSH_TKE<-gam(CPUE~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(TKE, k=6)+s(Curl_Val,k=6)+
                                   s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SSH,k=6)+te(Longitude,Latitude, k=6), family = nb, data=Squid_hurdle_model_Data_Abund, na.action = "na.fail")

Squid_Model_ROMS_Abund_SST_SSH_sd_CUTI<-gam(CPUE~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(CUTI,k=6)+
                                           s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SST,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = nb, data=Squid_hurdle_model_Data_Abund, na.action = "na.fail")
Squid_Model_ROMS_Abund_SSH_SSH_sd_CUTI<-gam(CPUE~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(CUTI,k=6)+
                                           s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SSH,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = nb, data=Squid_hurdle_model_Data_Abund, na.action = "na.fail")
Squid_Model_ROMS_Abund_SST_TKE_CUTI<-gam(CPUE~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(TKE, k=6)+s(CUTI,k=6)+
                                        s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SST,k=6)+te(Longitude,Latitude, k=6), family = nb, data=Squid_hurdle_model_Data_Abund, na.action = "na.fail")
Squid_Model_ROMS_Abund_SSH_TKE_CUTI<-gam(CPUE~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(TKE, k=6)+s(CUTI,k=6)+
                                        s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SSH,k=6)+te(Longitude,Latitude, k=6), family = nb, data=Squid_hurdle_model_Data_Abund, na.action = "na.fail")

Squid_Model_ROMS_Abund_SST_SSH_sd_Dredge<-dredge(Squid_Model_ROMS_Abund_SST_SSH_sd, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_Abund_SSH_SSH_sd_Dredge<-dredge(Squid_Model_ROMS_Abund_SSH_SSH_sd, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_Abund_SST_TKE_Dredge<-dredge(Squid_Model_ROMS_Abund_SST_TKE, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_Abund_SSH_TKE_Dredge<-dredge(Squid_Model_ROMS_Abund_SSH_TKE, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_Abund_SST_SSH_sd_CUTI_Dredge<-dredge(Squid_Model_ROMS_Abund_SST_SSH_sd_CUTI, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_Abund_SSH_SSH_sd_CUTI_Dredge<-dredge(Squid_Model_ROMS_Abund_SSH_SSH_sd_CUTI, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_Abund_SST_TKE_CUTI_Dredge<-dredge(Squid_Model_ROMS_Abund_SST_TKE_CUTI, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)
Squid_Model_ROMS_Abund_SSH_TKE_CUTI_Dredge<-dredge(Squid_Model_ROMS_Abund_SSH_TKE_CUTI, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)


Squid_Model_ROMS_Abund_SSH_SSH_sd_Lunar<-gam(CPUE~s(Model_Predictions,k=3)+s(Depth_Val, k=6)+s(Rugosity_Val, k=6)+s(Curl_Val,k=6)+s(Lunar,k=6)+
                                         s(CHL_Val,k=6)+s(SST_sd, k=6)+s(SSH,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = nb, data=Squid_hurdle_model_Data_Abund, na.action = "na.fail")

Squid_Model_ROMS_Abund_SSH_SSH_sd_Dredge_Lunar<-dredge(Squid_Model_ROMS_Abund_SSH_SSH_sd_Lunar, fixed=~te(Longitude, Latitude,k=6),rank="AIC",trace=5)

save.image("F:/RREAS/Catch_Data/Squid/Envelope_Model/Squid_Dredge_Results_220309_Lunar.RData")



#########Compare fit to new data###########
RREAS_Squid_Model_Recent<-Market_Squid_Model_Data_Full[Market_Squid_Model_Data_Full$year>=2017,]

library(mgcv)
library(MuMIn)
#take model averages for similarly well fit model
Squid_PA_Model_Avg_Lunar<-model.avg(Squid_Model_ROMS_PA_SSH_SSH_sd_Lunar_Dredge, delta<2, fit=TRUE)
Squid_Abund_Model_Avg_Lunar<-model.avg(Squid_Model_ROMS_Abund_SSH_SSH_sd_Dredge_Lunar, delta<2, fit=TRUE)

#predict prob occurrence and haul specific abundance
Market_Squid_Model_Data_Full$PA_Estimations<-predict(Squid_PA_Model_Avg,Market_Squid_Model_Data_Full, type="response" )

Market_Squid_Model_Data_Full$Abund_Estimations<-predict(Squid_Abund_Model_Avg,Market_Squid_Model_Data_Full, type="response" )

Market_Squid_Model_Data_Full$Hurdle_Estimate<-Market_Squid_Model_Data_Full$PA_Estimations*Market_Squid_Model_Data_Full$Abund_Estimations


#plot and look at the quality of fit to the training set
library(Metrics)
Market_Squid_Model_Data_Full_Model_Only<-Market_Squid_Model_Data_Full[Market_Squid_Model_Data_Full$year<=2016,]
setwd("F:/RREAS/Model_Formation/Market_Squid/Figures")
png("Hurdle_Estimate_Comparison_Train_220310.png", res=200, height=5, width=6, units="in")
plot(Market_Squid_Model_Data_Full_Model_Only$Hurdle_Estimate, Market_Squid_Model_Data_Full_Model_Only$CPUE, xlab=c("Hurdle Estimate"), ylab=c("ln(CPUE+1)"), main=c("'Train' Hurdle Model Estimate 1998-2016"), cex.lab=1.25, pch=19)
dev.off()
cor(Market_Squid_Model_Data_Full_Model_Only$Hurdle_Estimate, Market_Squid_Model_Data_Full_Model_Only$CPUE)^2
#R2=0.425
rmse( Market_Squid_Model_Data_Full_Model_Only$CPUE, Market_Squid_Model_Data_Full_Model_Only$Hurdle_Estimate)
#rmse=1.79

#plot and look at the quality of fit for the test set
Market_Squid_Model_Data_Full_Recent<-Market_Squid_Model_Data_Full[Market_Squid_Model_Data_Full$year>=2017,]

png("Hurdle_Estimate_Comparison_Recent_2021_220310.png", res=200, height=5, width=6, units="in")
plot(Market_Squid_Model_Data_Full_Recent$Hurdle_Estimate, Market_Squid_Model_Data_Full_Recent$CPUE, xlab=c("Hurdle Estimate"), ylab=c("ln(CPUE+1)"), main=c("Hurdle Model Estimate 2017-2021"), cex.lab=1.25, pch=19, xlim=c(0,10), ylim=c(0,10))
dev.off()

cor(Market_Squid_Model_Data_Full_Recent$Hurdle_Estimate, Market_Squid_Model_Data_Full_Recent$CPUE)^2
#R2=0.425
rmse( Market_Squid_Model_Data_Full_Recent$CPUE, Market_Squid_Model_Data_Full_Recent$Hurdle_Estimate)
#rmse=1.79

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
Market_Squid_Model_Data_Pres<-Market_Squid_Model_Data_Full[Market_Squid_Model_Data_Full$CPUE>0,]

cor(Market_Squid_Model_Data_Pres$Abund_Estimations, Market_Squid_Model_Data_Pres$CPUE)^2


#look at the abundance model fit to the training set 
Market_Squid_Model_Data_Pres_Model_Only<-Market_Squid_Model_Data_Pres[Market_Squid_Model_Data_Pres$year<=2016,]

cor(Market_Squid_Model_Data_Pres_Model_Only$Abund_Estimations, Market_Squid_Model_Data_Pres_Model_Only$CPUE)^2
rmse(Market_Squid_Model_Data_Pres_Model_Only$Abund_Estimations, Market_Squid_Model_Data_Pres_Model_Only$CPUE)

setwd("F:/RREAS/Model_Formation/Market_Squid/Figures")
png("Abund_Estimate_Comparison_Full_2016_220310.png", res=200, height=5, width=6, units="in")
plot(Market_Squid_Model_Data_Pres_Model_Only$Abund_Estimations, Market_Squid_Model_Data_Pres_Model_Only$CPUE, xlab=c("Abund. Estimate"), ylab=c("ln(CPUE+1)"), main=c("'Train' Abundance Model Estimate 1998-2016"), cex.lab=1.25, pch=19)
dev.off()


#compare abundance model fit to the test set
Market_Squid_Model_Data_Pres_Recent<-Market_Squid_Model_Data_Pres[Market_Squid_Model_Data_Pres$year>=2017,]

plot(Market_Squid_Model_Data_Pres_Recent$Abund_Estimations,Market_Squid_Model_Data_Pres_Recent$CPUE)

cor(Market_Squid_Model_Data_Pres_Recent$Abund_Estimations, Market_Squid_Model_Data_Pres_Recent$CPUE)^2
rmse(Market_Squid_Model_Data_Pres_Recent$Abund_Estimations, Market_Squid_Model_Data_Pres_Recent$CPUE)


setwd("F:/RREAS/Model_Formation/Market_Squid/Figures")
png("Abund_Estimate_Comparison_Recent_2021_220310.png", res=200, height=5, width=6, units="in")
plot(Market_Squid_Model_Data_Pres_Recent$Abund_Estimations, Market_Squid_Model_Data_Pres_Recent$CPUE, xlab=c("Abund. Estimate"), ylab=c("ln(CPUE+1)"), main=c("Abundance Model Estimate 2017-2021"), cex.lab=1.25, pch=19, ylim=c(0,10), xlim=c(0,10))
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


Market_Squid_Annual_Full_Stations_Red$Strata<-factor(Market_Squid_Annual_Full_Stations_Red$strata, levels=c("N","NC","C","SC","S"))
Market_Squid_Annual_Full_Stations_Red_N<-Market_Squid_Annual_Full_Stations_Red[Market_Squid_Annual_Full_Stations_Red$strata=="N"|Market_Squid_Annual_Full_Stations_Red$strata=="NC",]

png("North_Trend_Predictions_220301.png", height=5, width=7, units="in", res=300)
P<-ggplot(Market_Squid_Annual_Full_Stations_Red_N, aes(x=year, y=Mean_Squid_CPUE, group=Strata)) +geom_line(aes(color=Strata), size=1.5)+geom_point(aes( color=Strata),size=3)+theme_cowplot()+  geom_line( aes(x=year, y=Model_Predictions,  color=Strata),linetype="twodash", size=1.5)+geom_point(aes(x=year, y=Model_Predictions,  color=Strata), shape=17, size=3)+ylim(0,8)+ylab("Mean ln(CPUE+1)")+xlab("Year")+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"), legend.title=element_text(size=18,face="bold"), legend.text=element_text(size=16,face="bold"))+geom_vline(xintercept = 2016, linetype="dotted", size=1.5)+theme(legend.position = c(0.85,0.9))+xlim(1998,2021)
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


