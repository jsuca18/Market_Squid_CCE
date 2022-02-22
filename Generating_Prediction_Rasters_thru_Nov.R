#generate rasters for prediction

########ssh ############
setwd('F:/RREAS/Predictors/ROMS/SSH')
library(ncdf4)
SSH_ROMs_2010=nc_open('wcra31_ssh_daily_1980_2010.nc')
SSH_Daily_2010<-ncvar_get(SSH_ROMs_2010, "ssh")
Lat<-ncvar_get(SSH_ROMs_2010, "lat")
Lon<-ncvar_get(SSH_ROMs_2010, "lon")
Year_2010<-ncvar_get(SSH_ROMs_2010, "year")
Month_2010<-ncvar_get(SSH_ROMs_2010, "month")
Day_2010<-ncvar_get(SSH_ROMs_2010, "day")

SSH_ROMs_2017=nc_open('wcnrt_ssh_daily_20110102_20170419.nc')

library(abind)
SSH_Daily_2017<-ncvar_get(SSH_ROMs_2017, "ssh")
Year_2017<-ncvar_get(SSH_ROMs_2017, "year")
Month_2017<-ncvar_get(SSH_ROMs_2017, "month")
Day_2017<-ncvar_get(SSH_ROMs_2017, "day")

SSH_ROMs_2018=nc_open('wcnrt_ssh_daily_20170420_20180731.nc')

library(abind)
SSH_Daily_2018<-ncvar_get(SSH_ROMs_2018, "ssh")
Year_2018<-ncvar_get(SSH_ROMs_2018, "year")
Month_2018<-ncvar_get(SSH_ROMs_2018, "month")
Day_2018<-ncvar_get(SSH_ROMs_2018, "day")

SSH_ROMs_2019=nc_open('wcnrt_ssh_daily_20180801_20190815.nc')

library(abind)
SSH_Daily_2019<-ncvar_get(SSH_ROMs_2019, "ssh")
Year_2019<-ncvar_get(SSH_ROMs_2019, "year")
Month_2019<-ncvar_get(SSH_ROMs_2019, "month")
Day_2019<-ncvar_get(SSH_ROMs_2019, "day")

Month<-c(Month_2010, Month_2017, Month_2018,Month_2019)
Year<-c(Year_2010, Year_2017, Year_2018, Year_2019)
Day<-c(Day_2010, Day_2017, Day_2018, Day_2019)

SSH_Daily<-abind(SSH_Daily_2010, SSH_Daily_2017, SSH_Daily_2018, SSH_Daily_2019)

setwd("F:/RREAS/Predictors/ROMS_Output/ROMS_Output")

#separate the raster brick by year and take the mean for each year 
# of June-August. 

I<-which(Month>6 & Month<11 & Year>=1998)
Summer_SSH<-SSH_Daily[1:185,1:180,I]
library(raster)
library(rgdal)
Summer_SSH1<-brick(Summer_SSH)

Summer_SSH<-flip(t(Summer_SSH1), direction='y')
plot(Summer_SSH[[1]])

library(raster)
setwd("F:/RREAS/Predictors/Static")
Distance_to_shore<-raster("Distance_from_Land.grd")
#identify areas close to shore 
Distance_to_shore[Distance_to_shore> 100000]<-NA
plot(Distance_to_shore)
Ex<-extent(Distance_to_shore@extent)
extent(Summer_SSH) <- Ex
Summer_SSH<- setExtent(Summer_SSH, Ex)
Summer_SSH[is.na(Distance_to_shore)]<-NA

SSH_Summer_Daily<-crop(Summer_SSH, extent(-134,-115.5,32,45))
SSH_Summer_Daily<-trim(SSH_Summer_Daily)
plot(SSH_Summer_Daily[[600]])
SSH_Summer_SD_Daily=   multiFocal( SSH_Summer_Daily,w=matrix(1,nrow=3,ncol = 3),fun=sd,na.rm=T)

#create the equaivalent for each of the predictors and then convert them into a dataframe for a daily value
#then predict over those values each day 
 rm(SSH_Daily, SSH_Daily_2010, SSH_Daily_2017, SSH_Daily_2018, SSH_Daily_2019)
 
####sst ####
 setwd('F:/RREAS/Predictors/ROMS/SST')
 library(ncdf4)
 SST_ROMs_2010=nc_open('wcra31_sst_daily_1980_2010.nc')
 SST_Daily_2010<-ncvar_get(SST_ROMs_2010, "sst")
 Lat<-ncvar_get(SST_ROMs_2010, "lat")
 Lon<-ncvar_get(SST_ROMs_2010, "lon")
 Year_2010<-ncvar_get(SST_ROMs_2010, "year")
 Month_2010<-ncvar_get(SST_ROMs_2010, "month")
 Day_2010<-ncvar_get(SST_ROMs_2010, "day")
 
 SST_ROMs_2017=nc_open('wcnrt_sst_daily_20110102_20170419.nc')
 
 library(abind)
 SST_Daily_2017<-ncvar_get(SST_ROMs_2017, "sst")
 Year_2017<-ncvar_get(SST_ROMs_2017, "year")
 Month_2017<-ncvar_get(SST_ROMs_2017, "month")
 
 SST_ROMs_2018=nc_open('wcnrt_sst_daily_20170420_20180731.nc')
 
 library(abind)
 SST_Daily_2018<-ncvar_get(SST_ROMs_2018, "sst")
 Year_2018<-ncvar_get(SST_ROMs_2018, "year")
 Month_2018<-ncvar_get(SST_ROMs_2018, "month")
 
 SST_ROMs_2019=nc_open('wcnrt_sst_daily_20180801_20190815.nc')
 
 library(abind)
 SST_Daily_2019<-ncvar_get(SST_ROMs_2019, "sst")
 Year_2019<-ncvar_get(SST_ROMs_2019, "year")
 Month_2019<-ncvar_get(SST_ROMs_2019, "month")
 
 Month<-c(Month_2010, Month_2017, Month_2018,Month_2019)
 Year<-c(Year_2010, Year_2017, Year_2018, Year_2019)
 SST_Daily<-abind(SST_Daily_2010, SST_Daily_2017, SST_Daily_2018, SST_Daily_2019)
 
 setwd("F:/RREAS/Predictors/ROMS_Output/ROMS_Output")
 
 #separate the raster brick by year and take the mean for each year 
 # of June-August. 
 
 I<-which(Month>6 & Month<11 & Year>=1998)
 Summer_SST<-SST_Daily[1:185,1:180,I]
 library(raster)
 library(rgdal)
 Summer_SST1<-brick(Summer_SST)
 
 Summer_SST<-flip(t(Summer_SST1), direction='y')
 plot(Summer_SST[[1]])
 
 library(raster)
 setwd("F:/RREAS/Predictors/Static")
 Distance_to_shore<-raster("Distance_from_Land.grd")
 #identify areas close to shore 
 Distance_to_shore[Distance_to_shore> 100000]<-NA
 plot(Distance_to_shore)
 Ex<-extent(Distance_to_shore@extent)
 extent(Summer_SST) <- Ex
 Summer_SST<- setExtent(Summer_SST, Ex)
 Summer_SST[is.na(Distance_to_shore)]<-NA
 
 SST_Summer_Daily<-crop(Summer_SST, extent(-134,-115.5,32,45))
 SST_Summer_Daily<-trim(SST_Summer_Daily)
 plot(SST_Summer_Daily[[600]])
 
 SST_Summer_SD_Daily=   multiFocal( SST_Summer_Daily,w=matrix(1,nrow=3,ncol = 3),fun=sd,na.rm=T)
 
 rm(SST_Daily, SST_Daily_2010, SST_Daily_2017, SST_Daily_2018, SST_Daily_2019)
 
 
 ###########V current ####
 setwd('F:/RREAS/Predictors/ROMS/Currents')
 library(ncdf4)
 SV_ROMs_2010=nc_open('wcra31_sv_daily_1980_2010.nc')
 SV_Daily_2010<-ncvar_get(SV_ROMs_2010, "sv")
 Lat<-ncvar_get(SV_ROMs_2010, "lat")
 Lon<-ncvar_get(SV_ROMs_2010, "lon")
 Year_2010<-ncvar_get(SV_ROMs_2010, "year")
 Month_2010<-ncvar_get(SV_ROMs_2010, "month")
 Day_2010<-ncvar_get(SV_ROMs_2010, "day")
 
 SV_ROMs_2017=nc_open('wcnrt_sv_daily_20110102_20170419.nc')
 
 library(abind)
 SV_Daily_2017<-ncvar_get(SV_ROMs_2017, "sv")
 Year_2017<-ncvar_get(SV_ROMs_2017, "year")
 Month_2017<-ncvar_get(SV_ROMs_2017, "month")
 
 SV_ROMs_2018=nc_open('wcnrt_sv_daily_20170420_20180731.nc')
 
 library(abind)
 SV_Daily_2018<-ncvar_get(SV_ROMs_2018, "sv")
 Year_2018<-ncvar_get(SV_ROMs_2018, "year")
 Month_2018<-ncvar_get(SV_ROMs_2018, "month")
 
 SV_ROMs_2019=nc_open('wcnrt_sv_daily_20180801_20190815.nc')
 
 library(abind)
 SV_Daily_2019<-ncvar_get(SV_ROMs_2019, "sv")
 Year_2019<-ncvar_get(SV_ROMs_2019, "year")
 Month_2019<-ncvar_get(SV_ROMs_2019, "month")
 
 Month<-c(Month_2010, Month_2017, Month_2018,Month_2019)
 Year<-c(Year_2010, Year_2017, Year_2018, Year_2019)
 SV_Daily<-abind(SV_Daily_2010, SV_Daily_2017, SV_Daily_2018, SV_Daily_2019)
 
 setwd("F:/RREAS/Predictors/ROMS_Output/ROMS_Output")
 
 #separate the raster brick by year and take the mean for each year 
 # of June-August. 
 
 I<-which(Month>6 & Month<11 & Year>=1998)
 Summer_SV<-SV_Daily[1:185,1:180,I]
 library(raster)
 library(rgdal)
 Summer_SV1<-brick(Summer_SV)
 
 Summer_SV<-flip(t(Summer_SV1), direction='y')
 plot(Summer_SV[[1]])
 
 library(raster)
 setwd("F:/RREAS/Predictors/Static")
 Distance_to_shore<-raster("Distance_from_Land.grd")
 #identify areas close to shore 
 Distance_to_shore[Distance_to_shore> 100000]<-NA
 plot(Distance_to_shore)
 Ex<-extent(Distance_to_shore@extent)
 extent(Summer_SV) <- Ex
 Summer_SV<- setExtent(Summer_SV, Ex)
 Summer_SV[is.na(Distance_to_shore)]<-NA
 
 SV_Summer_Daily<-crop(Summer_SV, extent(-134,-115.5,32,45))
 SV_Summer_Daily<-trim( SV_Summer_Daily)
 plot(SV_Summer_Daily[[600]])
 rm(SV_Daily, SV_Daily_2010, SV_Daily_2017, SV_Daily_2018, SV_Daily_2019)
 #######U current ############
 setwd('F:/RREAS/Predictors/ROMS/Currents')
 library(ncdf4)
 SU_ROMs_2010=nc_open('wcra31_su_daily_1980_2010.nc')
 SU_Daily_2010<-ncvar_get(SU_ROMs_2010, "su")
 Lat<-ncvar_get(SU_ROMs_2010, "lat")
 Lon<-ncvar_get(SU_ROMs_2010, "lon")
 Year_2010<-ncvar_get(SU_ROMs_2010, "year")
 Month_2010<-ncvar_get(SU_ROMs_2010, "month")
 Day_2010<-ncvar_get(SU_ROMs_2010, "day")
 
 SU_ROMs_2017=nc_open('wcnrt_su_daily_20110102_20170419.nc')
 
 library(abind)
 SU_Daily_2017<-ncvar_get(SU_ROMs_2017, "su")
 Year_2017<-ncvar_get(SU_ROMs_2017, "year")
 Month_2017<-ncvar_get(SU_ROMs_2017, "month")
 
 SU_ROMs_2018=nc_open('wcnrt_su_daily_20170420_20180731.nc')
 
 library(abind)
 SU_Daily_2018<-ncvar_get(SU_ROMs_2018, "su")
 Year_2018<-ncvar_get(SU_ROMs_2018, "year")
 Month_2018<-ncvar_get(SU_ROMs_2018, "month")
 
 SU_ROMs_2019=nc_open('wcnrt_su_daily_20180801_20190815.nc')
 
 library(abind)
 SU_Daily_2019<-ncvar_get(SU_ROMs_2019, "su")
 Year_2019<-ncvar_get(SU_ROMs_2019, "year")
 Month_2019<-ncvar_get(SU_ROMs_2019, "month")
 
 Month<-c(Month_2010, Month_2017, Month_2018,Month_2019)
 Year<-c(Year_2010, Year_2017, Year_2018, Year_2019)
 SU_Daily<-abind(SU_Daily_2010, SU_Daily_2017, SU_Daily_2018, SU_Daily_2019)
 
 setwd("F:/RREAS/Predictors/ROMS_Output/ROMS_Output")
 
 #separate the raster brick by year and take the mean for each year 
 # of June-August. 
 
 I<-which(Month>6 & Month<11 & Year>=1998)
 Summer_SU<-SU_Daily[1:185,1:180,I]
 library(raster)
 library(rgdal)
 Summer_SU1<-brick(Summer_SU)
 
 Summer_SU<-flip(t(Summer_SU1), direction='y')
 plot(Summer_SU[[1]])
 
 library(raster)
 setwd("F:/RREAS/Predictors/Static")
 Distance_to_shore<-raster("Distance_from_Land.grd")
 #identify areas close to shore 
 Distance_to_shore[Distance_to_shore> 100000]<-NA
 plot(Distance_to_shore)
 Ex<-extent(Distance_to_shore@extent)
 extent(Summer_SU) <- Ex
 Summer_SU<- setExtent(Summer_SU, Ex)
 Summer_SU[is.na(Distance_to_shore)]<-NA
 
 SU_Summer_Daily<-crop(Summer_SU, extent(-134,-115.5,32,45))
 SU_Summer_Daily<-trim(SU_Summer_Daily)
 plot(SU_Summer_Daily[[600]])
 rm(SU_Daily, SU_Daily_2010, SU_Daily_2017, SU_Daily_2018, SU_Daily_2019)
 #########wind stress curl#####
 setwd('F:/RREAS/Predictors/ROMS/Wind/Curl')
 library(ncdf4)
 Curl_ROMs_2010=nc_open('wcra31_curl_daily_1980_2010.nc')
 Curl_Daily_2010<-ncvar_get(Curl_ROMs_2010, "curl")
 Lat<-ncvar_get(Curl_ROMs_2010, "lat")
 Lon<-ncvar_get(Curl_ROMs_2010, "lon")
 Year_2010<-ncvar_get(Curl_ROMs_2010, "year")
 Month_2010<-ncvar_get(Curl_ROMs_2010, "month")
 Day_2010<-ncvar_get(Curl_ROMs_2010, "day")
 
 
 Curl_ROMs_2017=nc_open('wcnrt_curl_daily_20110102_20170419.nc')
 
 library(abind)
 Curl_Daily_2017<-ncvar_get(Curl_ROMs_2017, "curl")
 Year_2017<-ncvar_get(Curl_ROMs_2017, "year")
 Month_2017<-ncvar_get(Curl_ROMs_2017, "month")
 Day_2017<-ncvar_get(Curl_ROMs_2017, "day")
 
 Curl_ROMs_2018=nc_open('wcnrt_curl_daily_20170420_20180731.nc')
 
 library(abind)
 Curl_Daily_2018<-ncvar_get(Curl_ROMs_2018, "curl")
 Year_2018<-ncvar_get(Curl_ROMs_2018, "year")
 Month_2018<-ncvar_get(Curl_ROMs_2018, "month")
 
 Curl_ROMs_2019=nc_open('wcnrt_curl_daily_20180801_20190815.nc')
 
 library(abind)
 Curl_Daily_2019<-ncvar_get(Curl_ROMs_2019, "curl")
 Year_2019<-ncvar_get(Curl_ROMs_2019, "year")
 Month_2019<-ncvar_get(Curl_ROMs_2019, "month")
 
 Month<-c(Month_2010, Month_2017, Month_2018, Month_2019)
 Year<-c(Year_2010, Year_2017, Year_2018,Year_2019)
 Curl_Daily<-abind(Curl_Daily_2010, Curl_Daily_2017, Curl_Daily_2018, Curl_Daily_2019)
 
 I<-which(Month>6 & Month<11 & Year>=1998)
 Summer_Curl<-Curl_Daily[1:185,1:180,I]
 
 Summer_Curl1<-brick( Summer_Curl)
 
 Summer_Curl<-flip(t( Summer_Curl1), direction='y')
 plot( Summer_Curl[[700]])
 
 multiFocal <- function(x, w=matrix(1, nr=5, nc=5), ...) {
   
   if(is.character(x)) {
     x <- brick(x)
   }
   # The function to be applied to each individual layer
   fun <- function(ind, x, w, ...){
     focal(x[[ind]], w=w, ...)
   }
   
   n <- seq(nlayers(x))
   list <- lapply(X=n, FUN=fun, x=x, w=w, ...)
   
   out <- stack(list)
   return(out)
 }
 
 
 Summer_Curl_Smoothed<-multiFocal( Summer_Curl,w=matrix(1,nrow=5,ncol = 5),fun=mean,na.rm=T)
 
 Ex<-extent(Distance_to_shore@extent)
 extent( Summer_Curl_Smoothed) <- Ex
 Summer_Curl_Smoothed<- setExtent( Summer_Curl_Smoothed, Ex)
 Summer_Curl_Smoothed[is.na(Distance_to_shore)]<-NA
 
 plot(  Summer_Curl_Smoothed[[300]])
 
 Summer_Curl_Smoothed_Daily<-crop(Summer_Curl_Smoothed, extent(-134,-115.5,32,45))
 Summer_Curl_Smoothed_Daily<-trim( Summer_Curl_Smoothed_Daily)
 #plot(SV_Winter_Daily[[1:30]])
 rm(Curl_Daily, Curl_Daily_2010, Curl_Daily_2017, Curl_Daily_2018, Curl_Daily_2019)
 ##########chl monthly output###############
 setwd("F:/RREAS/Predictors/CHL/Monthly")
 
 CHL_Monthly<-list.files(pattern='*CHL1_MO_00.nc')
 CHL_Monthly<-as.data.frame(CHL_Monthly)
 colnames(CHL_Monthly)<-"FileName"
 CHL_Monthly$Date<-substr(CHL_Monthly$FileName, 5,12)
 
 CHL_Monthly$Month<-as.numeric(substr(CHL_Monthly$Date, 5,6))
 CHL_Monthly$Year<-as.numeric(substr(CHL_Monthly$Date, 1,4))
 CHL_Monthly<-CHL_Monthly[grep("GSM", CHL_Monthly$FileName), ]
 
 library(ncdf4)
 
 
 K<-which(CHL_Monthly$Month>6 & CHL_Monthly$Month<11 & CHL_Monthly$Year<=2019 & CHL_Monthly$Year>=1998)
 Summer_CHL<-stack(CHL_Monthly$FileName[K])
 Summer_CHL_Close<-crop(Summer_CHL, Distance_to_shore)
 Summer_CHL_Close<-trim(Summer_CHL_Close)
 r.new = resample(Summer_CHL,Distance_to_shore, "bilinear")
 ex = extent(r.new)
 
 r2 = crop(Distance_to_shore, ex)
 
 CHL_Close_to_Shore= mask(r.new, r2)
 plot(CHL_Close_to_Shore[[50]])
 
 plot(r.new[[50]])
 plot(r2)
 plot( CHL_Close_to_Shore[[1]])
 

 CHL_Close_to_Shore<-crop(CHL_Close_to_Shore, extent(-134,-115.5,32,45))
 CHL_Close_to_Shore<-trim( CHL_Close_to_Shore)
 plot(  CHL_Close_to_Shore[[20]])
 
 
 
 #####static data########
 setwd("F:/RREAS/Predictors/Static")
 Depth_Grid<-raster("z.grd")
 Rugosity_Grid<-raster("z_sd.grd")
 Distance_to_Land<-raster("Distance_from_Land.grd")
 
 
 Ex<-extent(Distance_to_shore@extent)
 extent(  Distance_to_Land) <- Ex
 Distance_to_Land<- setExtent(  Distance_to_Land, Ex)
 Distance_to_Land[is.na(Distance_to_shore)]<-NA 
 Distance_to_Land<-crop(Distance_to_Land, extent(-134,-115.5,32,45))
 Distance_to_Land<-trim(Distance_to_Land)
 plot(Distance_to_Land)
 
 Distance_to_Land_DF<-as.data.frame(Distance_to_Land, xy=TRUE)
 colnames(Distance_to_Land_DF)<-c("Lon","Lat","Dist_Shore_Val")
   
   
 Ex<-extent(Distance_to_shore@extent)
 extent(   Rugosity_Grid) <- Ex
 Rugosity_Grid<- setExtent(  Rugosity_Grid, Ex)
 Rugosity_Grid[is.na( Distance_to_shore)]<-NA 
 Rugosity_Grid<-crop( Rugosity_Grid, extent(-134,-115.5,32,45))
 Rugosity_Grid<-trim( Rugosity_Grid)
 plot( Rugosity_Grid)
 
 Rugosity_Grid_DF<-as.data.frame(Rugosity_Grid, xy=TRUE)
 colnames(Rugosity_Grid_DF)<-c("Lon","Lat","Rugosity_Val")
 
 Ex<-extent(Distance_to_shore@extent)
 extent(    Depth_Grid) <- Ex
 Depth_Grid<- setExtent(  Depth_Grid, Ex)
 Depth_Grid[is.na( Distance_to_shore)]<-NA 
 Depth_Grid<-crop(  Depth_Grid, extent(-134,-115.5,32,45))
 Depth_Grid<-trim(  Depth_Grid)
 plot(  Depth_Grid)
 
 Depth_Grid_DF<-as.data.frame( Depth_Grid, xy=TRUE)
 colnames( Depth_Grid_DF)<-c("Lon","Lat","Depth_Val")
 
 ########upwelling indicies##################
 #load in file from maket squid folder from the model formation folder
 setwd("F:/RREAS/Catch_Data/Anchovy/YOY/GAMs")
 CUTI_Roll<-read.csv("CUTI_Roll.csv", header=TRUE)
 
 CUTI_Roll$CUTI<-CUTI_Roll$CUTI..m2.s.1.
 CUTI_Roll$Lat_Deg<-CUTI_Roll$latitude..degrees_north.
 
 ###setup predictions####
 #now merge these values all together
 #the following names are the dataframes that need to be used 
 #SSH_Summer_Daily
 #SSH_Summer_SD_Daily
 # SST_Summer_SD_Daily
 #SV_Summer_Daily
 #SU_Summer_Daily
 #Summer_Curl_Smoothed_Daily
 #CHL_Close_to_Shore, note this is monthly 
 #Depth_Grid_DF, already DF
 #Rugosity_Grid_DF
 #Distance_to_Land_DF
 ########make general rasters for climatology######
 
 SSH_Summer_SD_Climatology<-mean(SSH_Summer_SD_Daily, na.rm=T)
plot(SSH_Summer_SD_Climatology) 
SSH_Summer_Climatology<-mean(SSH_Summer_Daily,na.rm=T)
plot(SSH_Summer_Climatology)
SST_Summer_SD_Climatology<-mean(SST_Summer_SD_Daily, na.rm=T)
plot(SST_Summer_SD_Climatology)
TKE_Raster_Daily<-0.5*((SV_Summer_Daily)^2+(SU_Summer_Daily)^2)
plot(TKE_Raster_Daily[[2]])
TKE_Summer_Climatology<-mean(TKE_Raster_Daily, na.rm=T)
plot(TKE_Summer_Climatology)
Summer_Curl_Climatology<-mean(Summer_Curl_Smoothed_Daily, na.rm=T)
plot(Summer_Curl_Climatology)
CHL_Climatology<-mean(CHL_Close_to_Shore, na.rm=T)
plot(CHL_Climatology)

######read in model predictions####
Market_Squid_Abund_Preds
Market_Squid_Abund_Preds_Clim<-Market_Squid_Abund_Preds[Market_Squid_Abund_Preds$year>=1998 & Market_Squid_Abund_Preds$year<=2019,]
Market_Squid_Climatology<-ddply(Market_Squid_Abund_Preds_Clim,.(strata), summarize, Model_Predictions=mean(Model_Predictions))

###make a dataframe of all data####
CHL_Climatology_DF<-as.data.frame(CHL_Climatology, xy=TRUE)
colnames(CHL_Climatology_DF)<-c("Lon","Lat","CHL_Val")
SSH_Summer_Climatology_DF<-as.data.frame(SSH_Summer_Climatology, xy=TRUE)
colnames(SSH_Summer_Climatology_DF)<-c("Lon","Lat","SSH")
SSH_Summer_SD_Climatology_DF<-as.data.frame(SSH_Summer_SD_Climatology, xy=TRUE)
colnames(SSH_Summer_SD_Climatology_DF)<-c("Lon","Lat","SSH_sd")
SST_Summer_SD_Climatology_DF<-as.data.frame(SST_Summer_SD_Climatology, xy=TRUE)
colnames(SST_Summer_SD_Climatology_DF)<-c("Lon","Lat","SST_sd")
Summer_Curl_Climatology_DF<-as.data.frame(Summer_Curl_Climatology, xy=TRUE)
colnames(Summer_Curl_Climatology_DF)<-c("Lon","Lat","Curl_Val")
TKE_Summer_Climatology_DF<-as.data.frame(TKE_Summer_Climatology, xy=TRUE)
colnames(TKE_Summer_Climatology_DF)<-c("Longitude","Latitude","TKE")
TKE_Summer_Climatology_DF$Lon<-SSH_Summer_Climatology_DF$Lon
TKE_Summer_Climatology_DF$Lat<-SSH_Summer_Climatology_DF$Lat

#TKE_Summer_Climatology_DF$Longitude<-TKE_Summer_Climatology_DF$Lon
#TKE_Summer_Climatology_DF$Latitude<-TKE_Summer_Climatology_DF$Lat


Climatology_DF<-merge(merge(merge(merge(merge(merge(merge(merge(SSH_Summer_Climatology_DF, SSH_Summer_SD_Climatology_DF, by=c("Lon","Lat")), Summer_Curl_Climatology_DF, by=c("Lon","Lat")),TKE_Summer_Climatology_DF, by=c("Lon","Lat")), CHL_Climatology_DF, by=c("Lon","Lat")), Depth_Grid_DF, by=c("Lon","Lat")), Distance_to_Land_DF, by=c("Lon","Lat")), Rugosity_Grid_DF, by=c("Lon","Lat")) ,SST_Summer_SD_Climatology_DF, by=c("Lon","Lat") )
Climatology_DF$CUTI<-mean(Squid_hurdle_model_Data$CUTI)
Climatology_DF$Model_Predictions<-NA
Climatology_DF$Model_Predictions[Climatology_DF$Latitude>=32 & Climatology_DF$Latitude<34.5]<-Market_Squid_Climatology$Model_Predictions[Market_Squid_Climatology$strata=="S"]
Climatology_DF$Model_Predictions[Climatology_DF$Latitude>=34.5 & Climatology_DF$Latitude<=34.7]<-(Market_Squid_Climatology$Model_Predictions[Market_Squid_Climatology$strata=="SC"]+Market_Squid_Climatology$Model_Predictions[Market_Squid_Climatology$strata=="S"])/2

Climatology_DF$Model_Predictions[Climatology_DF$Latitude>34.7 & Climatology_DF$Latitude<36.3]<-Market_Squid_Climatology$Model_Predictions[Market_Squid_Climatology$strata=="SC"]
Climatology_DF$Model_Predictions[Climatology_DF$Latitude>=36.3 & Climatology_DF$Latitude<=36.5]<-(Market_Squid_Climatology$Model_Predictions[Market_Squid_Climatology$strata=="C"]+Market_Squid_Climatology$Model_Predictions[Market_Squid_Climatology$strata=="SC"])/2

Climatology_DF$Model_Predictions[Climatology_DF$Latitude>36.5 & Climatology_DF$Latitude<38.2]<-Market_Squid_Climatology$Model_Predictions[Market_Squid_Climatology$strata=="C"]
Climatology_DF$Model_Predictions[Climatology_DF$Latitude>=38.2 & Climatology_DF$Latitude<=38.4]<-(Market_Squid_Climatology$Model_Predictions[Market_Squid_Climatology$strata=="C"]+Market_Squid_Climatology$Model_Predictions[Market_Squid_Climatology$strata=="NC"])/2

Climatology_DF$Model_Predictions[Climatology_DF$Latitude>38.4 & Climatology_DF$Latitude<40.2]<-Market_Squid_Climatology$Model_Predictions[Market_Squid_Climatology$strata=="NC"]
Climatology_DF$Model_Predictions[Climatology_DF$Latitude>=40.2 & Climatology_DF$Latitude<=40.3]<-(Market_Squid_Climatology$Model_Predictions[Market_Squid_Climatology$strata=="NC"]+Market_Squid_Climatology$Model_Predictions[Market_Squid_Climatology$strata=="N"])/2
Climatology_DF$Model_Predictions[Climatology_DF$Latitude>40.3 & Climatology_DF$Latitude<=43]<-Market_Squid_Climatology$Model_Predictions[Market_Squid_Climatology$strata=="N"]
Squid_Model_ROMS_PA_Best<-gam(PA~s(Model_Predictions,k=3)+s(Rugosity_Val, k=6)+s(CUTI, k=6)+s(Curl_Val,k=6)+
                                     s(SST_sd, k=6)+s(Depth_Val, k=6)+s(SSH,k=6)+s(SSH_sd, k=6)+te(Longitude,Latitude, k=6), family = binomial, data=Squid_hurdle_model_Data, na.action = "na.fail")

Climatology_DF$PA_Est<-predict(Squid_PA_Model_Avg,Climatology_DF, type="response")
Climatology_DF$Abund_Est<-predict(Squid_Abund_Model_Avg,Climatology_DF, type="response" )
Climatology_DF$Hurdle_Est<-Climatology_DF$PA_Est*Climatology_DF$Abund_Est

library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(viridis)
library(ggplot2)
world<-ne_countries(scale="medium", returnclass = "sf")
setwd("F:/RREAS/Model_Formation/Market_Squid/Figures")
png("Market_Squid_Pa_Climatology_220217.png", width=6, height=5, units="in", res=300)
ggplot(data=world) + geom_tile(data = Climatology_DF, aes(x=Longitude, y = Latitude, fill=PA_Est),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = 1, na.value="white")+
  theme_bw()+ggtitle("PA Climatology")+labs(fill = "Prob. Occurr.")+geom_sf()+coord_sf(xlim=c(-128,-117), ylim=c(32, 43))
dev.off()

png("Market_Squid_Hurdle_Abund_Climatology_220217.png", width=6, height=5, units="in", res=300)
ggplot(data=world) + geom_raster(data = Climatology_DF, aes(x=Longitude, y = Latitude, fill=Hurdle_Est),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = 1, na.value="white")+
  theme_bw()+ggtitle("Hurdle Climatology")+labs(fill = "Log(CPUE)")+geom_sf()+coord_sf(xlim=c(-128,-117), ylim=c(32, 43))
dev.off()

png("Market_Squid_Abund_Climatology_220217.png", width=6, height=5, units="in", res=300)
ggplot(data=world) + geom_raster(data = Climatology_DF, aes(x=Longitude, y = Latitude, fill=Abund_Est),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = 1, na.value="white")+
  theme_bw()+ggtitle("Abund. Climatology")+labs(fill = "Log(CPUE)")+geom_sf()+coord_sf(xlim=c(-128,-117), ylim=c(32, 43))
dev.off()

####select CUTI values###
CUTI_Summer<-CUTI_Roll[CUTI_Roll$year>=1998 & CUTI_Roll$year<=2019 &CUTI_Roll$month>=7 & CUTI_Roll$month<=10, ]
CUTI_Summer$Unique_Date<-paste(CUTI_Summer$year, CUTI_Summer$month, CUTI_Summer$day, sep=" ")
CUTI_Summer$CUTI<-CUTI_Summer$CUTI..m2.s.1.
CUTI_Summer$Lat_Deg<-CUTI_Summer$latitude..degrees_north.

CHL_Summer<-CHL_Monthly[CHL_Monthly$Year>=1998 & CHL_Monthly$Year<=2019 & CHL_Monthly$Month>=7 & CHL_Monthly$Month<=10,]
#### now curate it to daily values #####

J<-which(Month>6 & Month<11 & Year>=1998)
YR_ID<-Year[J]
Month_ID<-Month[J]
Day_ID<-Day[J]
Unique_Date<-paste(YR_ID, Month_ID, Day_ID, sep=" ")

PA_Output<-matrix(,nrow=nrow(Climatology_DF), ncol=length(YR_ID))
Abund_Output<-matrix(,nrow=nrow(Climatology_DF), ncol=length(YR_ID))

for (i in 885:length(YR_ID)){
  SSH_Day<-SSH_Summer_Daily[[i]]
  SSH_Day_DF<-as.data.frame(  SSH_Day, xy=TRUE)
  colnames( SSH_Day_DF)<-c("Lon","Lat","SSH")
  SSH_SD_Day<-SSH_Summer_SD_Daily[[i]]
  SSH_SD_Day_DF<-as.data.frame(  SSH_Day_DF, xy=TRUE)
  colnames(SSH_SD_Day_DF)<-c("Lon","Lat","SSH_sd")
  SST_SD_Day<-SST_Summer_SD_Daily[[i]]
  SST_SD_Day_DF<-as.data.frame(  SSH_Day_DF, xy=TRUE)
  colnames(SST_SD_Day_DF)<-c("Lon","Lat","SST_sd")
  TKE_Day<-TKE_Raster_Daily[[i]]
  TKE_Day_DF<-as.data.frame(  TKE_Day, xy=TRUE)
  colnames( TKE_Day_DF)<-c("Longitude","Latitude","TKE")
  TKE_Day_DF$Lon<-SST_SD_Day_DF$Lon
  TKE_Day_DF$Lat<-SST_SD_Day_DF$Lat
  Curl_Day<-Summer_Curl_Smoothed_Daily[[i]]
  Curl_Day_DF<-as.data.frame(  Curl_Day, xy=TRUE)
  colnames( Curl_Day_DF)<-c("Lon","Lat","Curl_Val")
  CUTI_Day<-CUTI_Summer[CUTI_Summer$Unique_Date==Unique_Date[i],]
  K<-which(CHL_Summer$Year==YR_ID[i] & CHL_Summer$Month==Month_ID[i])
  CHL_Day<-CHL_Close_to_Shore[[K]]
  CHL_DF<-as.data.frame(CHL_Day, xy=TRUE)
  colnames(CHL_DF)<-c("Lon","Lat","CHL_Val")
  TKE_Day_DF$Longitude<-TKE_Day_DF$Lon
  TKE_Day_DF$Latitude<-TKE_Day_DF$Lat
  Daily_DF<-merge(merge(merge(merge(merge(merge(merge(merge(SSH_Day_DF, SSH_SD_Day_DF, by=c("Lon","Lat")),SST_SD_Day_DF, by=c("Lon","Lat")), Curl_Day_DF, by=c("Lon","Lat")),TKE_Day_DF, by=c("Lon","Lat")), CHL_DF, by=c("Lon","Lat")), Depth_Grid_DF, by=c("Lon","Lat")), Distance_to_Land_DF, by=c("Lon","Lat")), Rugosity_Grid_DF, by=c("Lon","Lat"))
  Daily_DF$Lat_Deg<-round(Daily_DF$Latitude)
  Daily_DF$CUTI<-NA
  for (ii in 1:nrow(Daily_DF)){
  Daily_DF$CUTI[ii]<-CUTI_Day$CUTI[CUTI_Day$Lat_Deg==Daily_DF$Lat_Deg[ii]]
  }
  Model_Est<-Market_Squid_Abund_Preds[Market_Squid_Abund_Preds$year==YR_ID[i],]
  Daily_DF$Model_Predictions<-NA
  Daily_DF$Model_Predictions[Daily_DF$Latitude>=32 & Daily_DF$Latitude<34.5]<-Model_Est$Model_Predictions[Model_Est$strata=="S"]
  Daily_DF$Model_Predictions[Daily_DF$Latitude>=34.5 & Daily_DF$Latitude<=34.7]<-(Model_Est$Model_Predictions[Model_Est$strata=="SC"]+Model_Est$Model_Predictions[Model_Est$strata=="S"])/2
  
  Daily_DF$Model_Predictions[Daily_DF$Latitude>34.7 & Daily_DF$Latitude<36.3]<-Model_Est$Model_Predictions[Market_Squid_Climatology$strata=="SC"]
  Daily_DF$Model_Predictions[Daily_DF$Latitude>=36.3 & Daily_DF$Latitude<=36.5]<-(Model_Est$Model_Predictions[Model_Est$strata=="C"]+Model_Est$Model_Predictions[Model_Est$strata=="SC"])/2
  
  Daily_DF$Model_Predictions[Daily_DF$Latitude>36.5 & Daily_DF$Latitude<38.2]<-Model_Est$Model_Predictions[Model_Est$strata=="C"]
  Daily_DF$Model_Predictions[Daily_DF$Latitude>=38.2 & Daily_DF$Latitude<=38.4]<-(Model_Est$Model_Predictions[Model_Est$strata=="C"]+Model_Est$Model_Predictions[Model_Est$strata=="NC"])/2
  
  Daily_DF$Model_Predictions[Daily_DF$Latitude>38.4 & Daily_DF$Latitude<40.2]<-Model_Est$Model_Predictions[Market_Squid_Climatology$strata=="NC"]
  Daily_DF$Model_Predictions[Daily_DF$Latitude>=40.2 & Daily_DF$Latitude<=40.3]<-(Model_Est$Model_Predictions[Model_Est$strata=="NC"]+Model_Est$Model_Predictions[Model_Est$strata=="N"])/2
  Daily_DF$Model_Predictions[Daily_DF$Latitude>40.3 & Daily_DF$Latitude<=43]<-Model_Est$Model_Predictions[Model_Est$strata=="N"]
 PA_Output[,i]<-predict(Squid_PA_Model_Avg,Daily_DF, type="response" )
  Abund_Output[,i]<-predict(Squid_Abund_Model_Avg,Daily_DF, type="response" )
  
  
}
#save.image("F:/RREAS/Model_Formation/Market_Squid/Annual_Rasters/Market_Squid_Daily_Output_220206_V2.RData", safe=FALSE)
#left off at 1646

Hurdle_Output<-PA_Output*Abund_Output

setwd("F:/RREAS/Model_Formation/Market_Squid/Annual_Rasters/Daily_Output")


Hurdle_Output<-PA_Output*Abund_Output
PA_Output_IDs<-cbind(PA_Output, Daily_DF[,7:8])
Abund_Output_IDs<-cbind(Abund_Output, Daily_DF[,7:8])
Hurdle_Output_IDs<-cbind(Abund_Output, Daily_DF[,7:8])

save.image("F:/RREAS/Model_Formation/Market_Squid/Annual_Rasters/Daily_Output/New_Daily_Output.RData", safe=FALSE)



write.csv(PA_Output_IDs,"PA_Daily_Output_220218.csv")
write.csv(Abund_Output_IDs,"Abund_Daily_Output_220218.csv")
write.csv(Hurdle_Output_IDs,"Hurdle_Daily_Output_220218.csv")







######now compare annual trends 
Yrsss<-1998:2019
Annual_PA_Mean<-matrix(,nrow=nrow(PA_Output), ncol=length(Yrsss))
Annual_Hurdle_Mean<-matrix(,nrow=nrow(PA_Output), ncol=length(Yrsss))
library(fame)
for (q in 1: length(Yrsss)){
  U<-which(YR_ID==Yrsss[q] & Month_ID<10)
  Annual_PA_Mean[,q]<-rowMeans(PA_Output[,U], na.rm=TRUE)
  Annual_Hurdle_Mean[,q]<-rowMeans(Hurdle_Output[,U], na.rm=TRUE)
  
}

Annual_PA_Mean_LL<-as.data.frame(cbind(Annual_PA_Mean, Daily_DF[,7:8]))
Annual_Hurdle_Mean_LL<-as.data.frame(cbind(Annual_Hurdle_Mean, Daily_DF[,7:8]))

ZX<-which(Annual_PA_Mean_LL$Latitude<=35 & Annual_PA_Mean_LL$Latitude>=33.2 & Annual_PA_Mean_LL$Longitude>= -121.5)
Annual_SNI_PA<-colMeans(Annual_PA_Mean[ZX,1:22], na.rm=TRUE)
Annual_SNI_Hurdle<-colMeans(Annual_Hurdle_Mean[ZX,1:22], na.rm=TRUE)

plot(Annual_SNI_Hurdle, Annual_SNI_PA)

SNI_Model_Est<-cbind(Yrsss, Annual_SNI_PA, Annual_SNI_Hurdle)

XZ<-which(Annual_PA_Mean_LL$Latitude<=33.2 & Annual_PA_Mean_LL$Latitude>=32 & Annual_PA_Mean_LL$Longitude>= -120.5)
Annual_SCI_PA<-colMeans(Annual_PA_Mean[XZ,1:22], na.rm=TRUE)
Annual_SCI_Hurdle<-colMeans(Annual_Hurdle_Mean[XZ,1:22], na.rm=TRUE)

SCI_Model_Est<-cbind(Yrsss, Annual_SCI_PA, Annual_SCI_Hurdle)


#####read in CASL diet####
setwd("F:/RREAS/Model_Formation/Market_Squid")
CSL_Diet_Data<-read.csv("CSLdiet_Do_quarterly.csv", header=TRUE)
CSL_Diet_Data$year<-CSL_Diet_Data$altYear
CSL_SCI_Summer<-CSL_Diet_Data[CSL_Diet_Data$Season=="3-Su" & CSL_Diet_Data$Island=="SCI",]
CSL_SNI_Summer<-CSL_Diet_Data[CSL_Diet_Data$Season=="3-Su" & CSL_Diet_Data$Island=="SNI",]



SCI_Diet_Comp<-merge(SCI_Model_Est, CSL_SCI_Summer, by.x=c("Yrsss"), by.y=c("year"))


plot(SCI_Diet_Comp$Annual_SCI_PA, SCI_Diet_Comp$FO)
plot(SCI_Diet_Comp$Annual_SCI_Hurdle, SCI_Diet_Comp$FO)
plot(SCI_Diet_Comp$Annual_SCI_Hurdle, SCI_Diet_Comp$SSFO)
plot(SCI_Diet_Comp$Annual_SCI_Hurdle, SCI_Diet_Comp$nprey)
cor.test(SCI_Diet_Comp$Annual_SCI_Hurdle, SCI_Diet_Comp$nprey)
cor.test(SCI_Diet_Comp$Annual_SCI_PA, SCI_Diet_Comp$nprey)


SNI_Diet_Comp<-merge(SNI_Model_Est, CSL_SNI_Summer, by.x=c("Yrsss"), by.y=c("year"))

plot(SNI_Diet_Comp$Annual_SNI_PA, SNI_Diet_Comp$FO)
plot(SNI_Diet_Comp$Annual_SNI_Hurdle, SNI_Diet_Comp$FO)

plot(SNI_Diet_Comp$Annual_SNI_PA, SNI_Diet_Comp$nprey)
plot(SNI_Diet_Comp$Annual_SNI_Hurdle, SNI_Diet_Comp$nprey, xlab=c(""))
cor.test(SNI_Diet_Comp$Annual_SNI_Hurdle, SNI_Diet_Comp$nprey)
cor.test(SNI_Diet_Comp$Annual_SNI_PA, SNI_Diet_Comp$nprey)
plot(SNI_Diet_Comp$Annual_SNI_Hurdle[SNI_Diet_Comp$Yrsss>=2004], SNI_Diet_Comp$nprey[SNI_Diet_Comp$Yrsss>=2004])
plot(SNI_Diet_Comp$Annual_SNI_PA[SNI_Diet_Comp$Yrsss>=2004], SNI_Diet_Comp$nprey[SNI_Diet_Comp$Yrsss>=2004])

png("SNI_N_prey_Model_Estimates_New.png", height=5, width=6, units="in", res=300)
 ggplot(SNI_Diet_Comp, aes(x=Annual_SNI_Hurdle, y=nprey)) +
  geom_point() +
  geom_smooth(method=lm , color="black", fill="grey", se=TRUE) + theme_classic()+ylab("Num. of Squid")+xlab("Hurdle Estimate ln(CPUE+1)")+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold"))+ggtitle("SNI")
dev.off()

png("SNI_N_prey_PA_Estimates_New.png", height=5, width=6, units="in", res=300)
ggplot(SNI_Diet_Comp, aes(x=Annual_SNI_PA, y=nprey)) +
  geom_point() +
  geom_smooth(method=lm , color="black", fill="grey", se=TRUE) + theme_classic()+ylab("Num. of Squid")+xlab("Mean Prob. Occurr.")+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold"))+ggtitle("SNI")
dev.off()



png("SNI_FO_Model_Estimates_New.png", height=5, width=6, units="in", res=300)
ggplot(SNI_Diet_Comp, aes(x=Annual_SNI_Hurdle, y=(FO/100))) +
  geom_point()  + theme_classic()+ylab("Freq. Occurr.")+xlab("Hurdle Estimate ln(CPUE+1)")+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold"))+ggtitle("SNI")
dev.off()

geom_smooth(method = "glm", 
            method.args = list(family = "binomial"), 
            se = FALSE) 

png("SCI_N_prey_Model_Estimates_New.png", height=5, width=6, units="in", res=300)
ggplot(SCI_Diet_Comp, aes(x=Annual_SCI_Hurdle, y=nprey)) +
  geom_point() +
  geom_smooth(method=lm , color="black", fill="grey", se=TRUE) + theme_classic()+ylab("Num. of Squid")+xlab("Hurdle Estimate ln(CPUE+1)")+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold"))+ggtitle("SCI")
dev.off()

png("SCI_N_prey_PA_Estimates_New.png", height=5, width=6, units="in", res=300)
ggplot(SCI_Diet_Comp, aes(x=Annual_SCI_PA, y=nprey)) +
  geom_point() +
  geom_smooth(method=lm , color="black", fill="grey", se=TRUE) + theme_classic()+ylab("Num. of Squid")+xlab("Mean Prob. Occurr.")+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold"))+ggtitle("SCI")
dev.off()

png("SCI_FO_PA_Estimates_New.png", height=5, width=6, units="in", res=300)
ggplot(SCI_Diet_Comp, aes(x=Annual_SCI_PA, y=(FO/100))) +
  geom_point() + theme_classic()+ylab("Freq. Occurr.")+xlab("Mean Prob. Occurr.")+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold"))+ggtitle("SCI")
dev.off()

png("SCI_FO_Hurd_Estimates_New.png", height=5, width=6, units="in", res=300)
ggplot(SCI_Diet_Comp, aes(x=Annual_SCI_Hurdle, y=(FO/100))) +
  geom_point() + theme_classic()+ylab("Freq. Occurr.")+xlab("Hurdle Estimate ln(CPUE+1)")+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold"))+ggtitle("SCI")
dev.off()

library(betareg)
SNI_Diet_Comp$FO_Frac<-(SNI_Diet_Comp$FO/100)+0.00001
SNI_Beta_Reg<-betareg(FO_Frac~Annual_SNI_Hurdle, data=SNI_Diet_Comp)
summary(SNI_Beta_Reg)

png("SNI_FO_Model_Log_Estimates_New.png", height=5, width=6, units="in", res=300)
ggplot(SNI_Diet_Comp, aes(x=Annual_SNI_Hurdle, y=(FO/100))) +
  geom_point() + geom_smooth(method=lm , color="black", fill="grey",formula='y~log(x)', se=TRUE) + theme_classic()+ylab("Freq. Occurr.")+xlab("Hurdle Estimate ln(CPUE+1)")+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold"))+ggtitle("SNI")
dev.off()

Example<-lm(SNI_Diet_Comp$FO_Frac~SNI_Diet_Comp$Annual_SNI_Hurdle)
summary(Example)


SCI_Diet_Comp$FO_Frac<-(SCI_Diet_Comp$FO/100)+0.00001

png("SCI_FO_Model_Log_Estimates_New.png", height=5, width=6, units="in", res=300)
ggplot(SCI_Diet_Comp, aes(x=Annual_SCI_Hurdle, y=(FO/100))) +
  geom_point() + geom_smooth(method=lm , color="black", fill="grey",formula='y~log(x)', se=TRUE,linetype = "dashed") + theme_classic()+ylab("Freq. Occurr.")+xlab("Hurdle Estimate ln(CPUE+1)")+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"),
                                                                                                                                                                                  plot.title = element_text(size = 20, face = "bold"))+ggtitle("SCI")
dev.off()

Example<-lm(SCI_Diet_Comp$FO_Frac~SCI_Diet_Comp$Annual_SCI_Hurdle)
summary(Example)


#######compare with landings#####
#conception is 34.4 but I ma playing around here.... 
ZZ<-which(Annual_PA_Mean_LL$Latitude>35.67)
Annual_NConc_PA<-colSums(Annual_PA_Mean[ZZ,1:22], na.rm=TRUE)
Annual_NConc_Hurdle<-colSums(Annual_Hurdle_Mean[ZZ,1:22], na.rm=TRUE)

ZY<-which(Annual_PA_Mean_LL$Latitude<=35.67)
Annual_South_PA<-colSums(Annual_PA_Mean[ZY,1:22], na.rm=TRUE)
Annual_South_Hurdle<-colSums(Annual_Hurdle_Mean[ZY,1:22], na.rm=TRUE)

plot(Annual_NConc_PA, Annual_NConc_Hurdle)

Annual_Totals<-as.data.frame(cbind(Yrsss, Annual_NConc_Hurdle, Annual_NConc_PA, Annual_South_Hurdle, Annual_South_PA))

Annual_Totals$Prop_PA<-Annual_Totals$Annual_NConc_PA/(Annual_Totals$Annual_South_PA+Annual_Totals$Annual_NConc_PA)
Annual_Totals$Prop_Hurdle<-Annual_Totals$Annual_NConc_Hurdle/(Annual_Totals$Annual_South_Hurdle+Annual_Totals$Annual_NConc_Hurdle)
setwd("F:/RREAS/Catch_Data/Squid/Envelope_Model")

Landings<-read.csv("Squid_Landings.csv", header=TRUE)
Landings_Comp<-merge(Annual_Totals, Landings, by.x=c("Yrsss"), by.y=c("year"))

plot(Landings_Comp$Prop_Hurdle, Landings_Comp$Portion_of_Landings_Q3)
plot(Landings_Comp$Prop_PA, Landings_Comp$Portion_of_Landings_Q3)
cor.test(Landings_Comp$Prop_Hurdle, Landings_Comp$Portion_of_Landings_Q3)
cor.test(Landings_Comp$Prop_PA, Landings_Comp$Portion_of_Landings_Q3)
library(betareg)
Landings_Comp$Portion_of_Landings_Q3<-Landings_Comp$Portion_of_Landings_Q3+0.00000001
PA_beta<-betareg(Portion_of_Landings_Q3~Prop_PA, data=Landings_Comp)
Landings_Comp_Red<-Landings_Comp[Landings_Comp$Yrsss!=2009,]
Hurdle_beta<-betareg(Portion_of_Landings_Q3~Prop_Hurdle, data=Landings_Comp)
PA_beta<-betareg(Portion_of_Landings_Q3~Prop_PA, data=Landings_Comp)

summary(Hurdle_beta)
summary(PA_beta)
setwd("F:/RREAS/Model_Formation/Market_Squid")
Data<-as.data.frame(matrix(,nrow=4001,ncol=2))
colnames(Data)<-c("Prop_PA","Y")
Data$Prop_PA<-seq(0,0.4, by=0.0001)
png("Landings_Estimates_Hurdle_MB.png", height=5, width=6, units="in", res=300)
ggplot(Landings_Comp, aes(x=boxcox(Prop_Hurdle), y=Portion_of_Landings_Q3)) +
  geom_point()+geom_line(y=predict(Hurdle_beta,Landings_Comp)) + theme_classic()+ylab("Prop. Landings N. Concep.")+xlab("Prop. Model Estimate N. Morro Bay")+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold"))+ggtitle("Q3 Landings")
dev.off()
png("Landings_Estimates_PA_MB.png", height=5, width=6, units="in", res=300)
ggplot(Landings_Comp, aes(x=Prop_PA, y=Portion_of_Landings_Q3)) +
  geom_point() + theme_classic()+ylab("Prop. Landings N. Concep.")+xlab("Prop. PA Estimate N. Morro Bay")+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold"))+ggtitle("Q3 Landings")
dev.off()


######comparison with biomass###########
ZZ<-which(Annual_PA_Mean_LL$Latitude<=33)
Annual_Region_3_PA<-colMeans(Annual_PA_Mean[ZZ,1:22], na.rm=TRUE)
Annual_Region_3_Hurdle<-colSums(Annual_Hurdle_Mean[ZZ,1:22], na.rm=TRUE)

ZY<-which(Annual_PA_Mean_LL$Latitude<=35.67 & Annual_PA_Mean_LL$Latitude>33)
Annual_Region_2_PA<-colMeans(Annual_PA_Mean[ZY,1:22], na.rm=TRUE)
Annual_Region_2_Hurdle<-colSums(Annual_Hurdle_Mean[ZY,1:22], na.rm=TRUE)

YY<-which(Annual_PA_Mean_LL$Latitude<=42& Annual_PA_Mean_LL$Latitude>36)
Annual_Region_1_PA<-colMeans(Annual_PA_Mean[YY,1:22], na.rm=TRUE)
Annual_Region_1_Hurdle<-colSums(Annual_Hurdle_Mean[YY,1:22], na.rm=TRUE)

plot(Annual_NConc_PA, Annual_NConc_Hurdle)

Annual_Region_Totals<-as.data.frame(cbind(Yrsss, Annual_Region_3_PA, Annual_Region_3_Hurdle, Annual_Region_2_PA, Annual_Region_2_Hurdle,Annual_Region_1_PA, Annual_Region_1_Hurdle))
setwd("F:/RREAS/Model_Formation/Market_Squid/Catch_Data_Biomass")
write.csv(Annual_Region_Totals,"Annual_Region_Model_Est.csv")
MSq_Biomass<-read.csv("Market_Squid_Biomass_Estimates_Dorval_Ralston.csv", header=TRUE)


MSq_Biomass_Region3<-MSq_Biomass[MSq_Biomass$Region==3,]
MSq_Biomass_Region2<-MSq_Biomass[MSq_Biomass$Region==2,]
MSq_Biomass_Region1<-MSq_Biomass[MSq_Biomass$Region==1,]


MSq_Biomass_Region3_Comp<-merge(MSq_Biomass_Region3, Annual_Region_Totals, by.x=c("Year"), by.y=c("Yrsss"))
MSq_Biomass_Region2_Comp<-merge(MSq_Biomass_Region2, Annual_Region_Totals, by.x=c("Year"), by.y=c("Yrsss"))
MSq_Biomass_Region1_Comp<-merge(MSq_Biomass_Region1, Annual_Region_Totals, by.x=c("Year"), by.y=c("Yrsss"))

plot(MSq_Biomass_Region3_Comp$Annual_Region_3_Hurdle, MSq_Biomass_Region3_Comp$MT)
cor.test(MSq_Biomass_Region3_Comp$Annual_Region_3_Hurdle, MSq_Biomass_Region3_Comp$MT)

plot(MSq_Biomass_Region3_Comp$Annual_Region_3_PA, MSq_Biomass_Region3_Comp$MT)
plot(MSq_Biomass_Region2_Comp$Annual_Region_2_Hurdle, MSq_Biomass_Region2_Comp$MT)
cor.test(MSq_Biomass_Region2_Comp$Annual_Region_2_Hurdle, MSq_Biomass_Region2_Comp$MT)

plot(MSq_Biomass_Region2_Comp$Annual_Region_2_PA, MSq_Biomass_Region2_Comp$MT)

plot(MSq_Biomass_Region1_Comp$Annual_Region_1_Hurdle, MSq_Biomass_Region1_Comp$MT)
cor.test(MSq_Biomass_Region1_Comp$Annual_Region_1_Hurdle, MSq_Biomass_Region1_Comp$MT)

plot(MSq_Biomass_Region1_Comp$Annual_Region_1_PA, MSq_Biomass_Region1_Comp$MT)

plot(MSq_Biomass$PA, MSq_Biomass$MT)
plot(MSq_Biomass$Hurdle, MSq_Biomass$MT)

cor.test(MSq_Biomass$PA, MSq_Biomass$MT)
cor.test(MSq_Biomass$Hurdle, MSq_Biomass$MT)

png("Region_1_biomass_Comparison_Hurdle_New.png", height=5, width=6, units="in", res=300)
ggplot(MSq_Biomass_Region1_Comp, aes(x=Annual_Region_1_Hurdle, y=MT)) +
  geom_point() +
  geom_smooth(method=lm , color="black", fill="grey",formula= 'y~x', se=TRUE) + theme_classic()+ylab("Metric Tons")+xlab("Total Hurdle Estimate ln(CPUE+1)")+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold"))+ggtitle("Region 1")
dev.off()

png("Region_2_biomass_Comparison_Hurdle_New.png", height=5, width=6, units="in", res=300)
ggplot(MSq_Biomass_Region2_Comp, aes(x=Annual_Region_2_Hurdle, y=MT)) +
  geom_point() +
  geom_smooth(method=lm , color="black", fill="grey",formula= 'y~x', se=TRUE) + theme_classic()+ylab("Metric Tons")+xlab("Total Hurdle Estimate ln(CPUE+1)")+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold"))+ggtitle("Region 2")
dev.off()

png("Region_3_biomass_Comparison_Hurdle_New.png", height=5, width=6, units="in", res=300)
ggplot(MSq_Biomass_Region3_Comp, aes(x=Annual_Region_3_Hurdle, y=MT)) +
  geom_point() +
  geom_smooth(method=lm , color="black", fill="grey",formula= 'y~x', se=TRUE) + theme_classic()+ylab("Metric Tons")+xlab("Total Hurdle Estimate ln(CPUE+1)")+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold"))+ggtitle("Region 3")
dev.off()

Msq_Catches<-read.csv("Catch_by_region_Oct.csv", header=TRUE)

MSq_Catches_Comp<-merge(Msq_Catches, Annual_Region_Totals, by.x=c("Year"), by.y=c("Yrsss"))


MSq_Catches_Comp$Model_Portion_R1<-MSq_Catches_Comp$Annual_Region_1_Hurdle/(MSq_Catches_Comp$Annual_Region_1_Hurdle+MSq_Catches_Comp$Annual_Region_2_Hurdle)
MSq_Catches_Comp$Model_Portion_R1_PA<-MSq_Catches_Comp$Annual_Region_1_Hurdle/(MSq_Catches_Comp$Annual_Region_1_Hurdle+MSq_Catches_Comp$Annual_Region_2_Hurdle)

plot(MSq_Catches_Comp$Annual_Region_2_Hurdle, MSq_Catches_Comp$Region2)
plot(MSq_Catches_Comp$Annual_Region_1_Hurdle, MSq_Catches_Comp$Region1)
plot(MSq_Catches_Comp$Model_Portion_R1, MSq_Catches_Comp$Portion_R1)
cor.test(MSq_Catches_Comp$Model_Portion_R1, MSq_Catches_Comp$Portion_R1)

MSq_Catches_Comp_Red<-MSq_Catches_Comp[MSq_Catches_Comp$Year!=2000 & MSq_Catches_Comp$Year!=2001 &MSq_Catches_Comp$Year!=2004 &MSq_Catches_Comp$Year!=2005,]
plot(MSq_Catches_Comp_Red$Model_Portion_R1, MSq_Catches_Comp_Red$Portion_R1)

ggplot(MSq_Catches_Comp_Red, aes(x=Model_Portion_R1, y=Portion_R1)) +
  geom_point() + theme_classic()+ylab("Portion Landings")+xlab("Portion Model")+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold"))+ggtitle("Region 3")
library(betareg)

MSq_Catches_Comp$Portion_R1<-MSq_Catches_Comp$Portion_R1+0.00000001
MSq_Catches_Comp_Red$Portion_R1<-MSq_Catches_Comp_Red$Portion_R1+0.00000001

MSQ_Full_Beta<-betareg(Portion_R1~Model_Portion_R1,data=MSq_Catches_Comp )
summary(MSQ_Full_Beta)

plot



MSq_Catches_Comp$Residuals<-resid(MSQ_Full_Beta)
plot(MSq_Catches_Comp$Sardine_Landings, MSq_Catches_Comp$Residuals)

cor.test(MSq_Catches_Comp$Sardine_Landings, MSq_Catches_Comp$Residuals)

png("Landings_Comparisons_Res_Sardines.png", height=5, width=6, res=300, units="in")
ggplot(MSq_Catches_Comp, aes(x=Sardine_Landings, y=Residuals)) +
  geom_point() +
  geom_smooth(method=lm , color="black", fill="grey",formula= 'y~x', se=TRUE) + theme_classic()+xlab("Residuals")+xlab("Sardine Landings (MT)")+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold"))
dev.off()
library(mgcv)
Model_Portion_R1<-seq(0.14,0.41, by=0.001)
Model_Portion_R1<-as.data.frame(Model_Portion_R1)
Beta_Landings_Est<-predict(MSQ_Full_Beta, Model_Portion_R1, se.fit=TRUE, type='response')
MSQ_Full_Beta = gam(Portion_R1 ~ Model_Portion_R1, family=betar(link="logit"), data = MSq_Catches_Comp)
summary(MSQ_Full_Beta)
plot(MSQ_Full_Beta)
Beta_Landings_Est<-cbind(as.data.frame(predict(MSQ_Full_Beta, Model_Portion_R1, se.fit=TRUE, type='response')), Model_Portion_R1)
Beta_Landings_Est <- mutate(Beta_Landings_Est, lwr = fit - 2.1 * se.fit, upr = fit + 2.1 * se.fit) # calculating the 95% confidence interval
Beta_Landings_Est$Portion_R1<-Beta_Landings_Est$fit

Beta_Landings_Est$upr[Beta_Landings_Est$upr>=1]<-1
png("Landings_Comparisons_R1_R2_New.png", height=5, width=6, res=300, units="in")
ggplot(MSq_Catches_Comp, aes(x=Model_Portion_R1, y=Portion_R1)) +
  geom_point() +
  geom_smooth(data = Beta_Landings_Est, aes(ymin = lwr, ymax = upr), stat = 'identity') + theme_classic()+ylab("Portion of Landings")+xlab("Portion of Model Estimates")+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold"))
dev.off()

png("Landings_Comparisons_R1_R2_Reduced.png", height=5, width=6, res=300, units="in")
ggplot(MSq_Catches_Comp_Red, aes(x=Model_Portion_R1, y=Portion_R1)) +
  geom_point()+ theme_classic()+ylab("Portion of Landings")+xlab("Portion of Model Estimates")+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold"))+ggtitle("2000,2001,2004,2005 Removed")
dev.off()


png("Region_1_biomass_Comparison_Hurdle.png", height=5, width=6, units="in", res=300)
ggplot(MSq_Biomass_Region1_Comp, aes(x=Annual_Region_1_Hurdle, y=MT)) +
  geom_point() +
  geom_smooth(method=lm , color="black", fill="grey",formula= 'y~x', se=TRUE) + theme_classic()+ylab("Metric Tons")+xlab("Total Hurdle Estimate ln(CPUE+1)")+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold"))+ggtitle("Region 1")
dev.off()

png("Region_2_landings_Comparison_Hurdle_New.png", height=5, width=6, units="in", res=300)
ggplot(MSq_Catches_Comp, aes(x=Annual_Region_2_Hurdle, y=Region2)) +
  geom_point(size=2) +
  geom_smooth(method=lm , color="black", fill="grey",formula= 'y~x', se=TRUE) + theme_classic()+ylab("Landings")+xlab("Total Hurdle Estimate ln(CPUE+1)")+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold"))+ggtitle("Region 2")
dev.off()

png("Region_1_Landings_Comparison_Hurdle_New.png", height=5, width=6, units="in", res=300)
ggplot(MSq_Catches_Comp, aes(x=Annual_Region_1_Hurdle, y=Region1)) +
  geom_point(size=2) +
  geom_smooth(method=lm , color="black", fill="grey",formula= 'y~x', se=TRUE) + theme_classic()+ylab("Landings")+xlab("Total Hurdle Estimate ln(CPUE+1)")+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"), plot.title = element_text(size = 20, face = "bold"))+ggtitle("Region 1")
dev.off()
#######annual figure generation#######

for (i in 1:22){
  
  Sq_Data<-Annual_PA_Mean_LL[,i]
  LL<-Annual_PA_Mean_LL[,23:24]
  LL_SQ<-as.data.frame(cbind(LL,Sq_Data))
  world<-ne_countries(scale="medium", returnclass = "sf")
  png(paste("Squid_PA_Fig_New",Yrsss[i],".png"), width=6, height=5, units="in", res=300)
  final.plot<-ggplot(data=world) + geom_raster(data =  LL_SQ, aes(x=Longitude, y = Latitude, fill=Sq_Data),linejoin = "mitre") + 
    coord_fixed(ratio = 1) +
    scale_fill_viridis(direction = 1, na.value="white", limits=c(0,1))+
    theme_bw()+ggtitle(paste("Jul-Sep",Yrsss[i]))+labs(fill = "Prob. Occurr.")+geom_sf()+coord_sf(xlim=c(-128,-117), ylim=c(32, 43))
  print(final.plot)
  dev.off()
}

for (i in 1:22){
  
  Sq_Data<-Annual_Hurdle_Mean_LL[,i]
  LL<-Annual_Hurdle_Mean_LL[,23:24]
  LL_SQ<-as.data.frame(cbind(LL,Sq_Data))
  world<-ne_countries(scale="medium", returnclass = "sf")
  png(paste("Squid_Hurdle_Fig_New",Yrsss[i],".png"), width=6, height=5, units="in", res=300)
  final.plot<-ggplot(data=world) + geom_raster(data =  LL_SQ, aes(x=Longitude, y = Latitude, fill=Sq_Data),linejoin = "mitre") + 
    coord_fixed(ratio = 1) +
    scale_fill_viridis(direction = 1, na.value="white", limits=c(0, 8.1))+
    theme_bw()+ggtitle(paste("Jul-Sep",Yrsss[i]))+labs(fill = "Log(CPUE)")+geom_sf()+coord_sf(xlim=c(-128,-117), ylim=c(32, 43))
  print(final.plot)
  dev.off()
}


for (i in 1:22){
  
  Sq_Data<-Annual_PA_Mean_LL[,i]
  LL<-Annual_PA_Mean_LL[,23:24]
  LL_SQ<-as.data.frame(cbind(LL,Sq_Data))
  world<-ne_countries(scale="medium", returnclass = "sf")
  png(paste("Squid_PA_Fig_New",Yrsss[i],".png"), width=6, height=5, units="in", res=300)
  final.plot<-ggplot(data=world) + geom_raster(data =  LL_SQ, aes(x=Longitude, y = Latitude, fill=Sq_Data),linejoin = "mitre") + 
    coord_fixed(ratio = 1) +
    scale_fill_viridis(direction = 1, na.value="white", limits=c(0,1))+
    theme_bw()+ggtitle(paste("Jul-Sep",Yrsss[i]))+labs(fill = "Prob. Occurr.")+geom_sf()+coord_sf(xlim=c(-128,-117), ylim=c(32, 43))
  print(final.plot)
  dev.off()
}


#anomaly calculator

Nina=c(2000,2008,2011)
Nino=c(1998,2010,2016)
qqq=which(Yrsss %in% Nina)
  Sq_Data<-rowMeans(Annual_Hurdle_Mean_LL[,qqq], na.rm = TRUE)
  LL<-Annual_Hurdle_Mean_LL[,23:24]
  LL_SQ<-as.data.frame(cbind(LL,Sq_Data))
  world<-ne_countries(scale="medium", returnclass = "sf")
  png("Squid_Hurdle_Fig_Nina.png", width=4.5, height=7, units="in", res=300)
  final.plot<-ggplot(data=world) + geom_raster(data =  LL_SQ, aes(x=Longitude, y = Latitude, fill=Sq_Data),linejoin = "mitre") + 
    coord_fixed(ratio = 1) +
    scale_fill_viridis(direction = 1, na.value="white", limits=c(0, 5))+
    theme_bw()+ggtitle("Jul-Sep La Niña")+labs(fill = "Log(CPUE)")+geom_sf()+coord_sf(xlim=c(-128,-117), ylim=c(32, 43))
  print(final.plot)
  dev.off()

  
  qqq=which(Yrsss %in% Nino)
  Sq_Data<-rowMeans(Annual_Hurdle_Mean_LL[,qqq], na.rm = TRUE)
  LL<-Annual_Hurdle_Mean_LL[,23:24]
  LL_SQ<-as.data.frame(cbind(LL,Sq_Data))
  world<-ne_countries(scale="medium", returnclass = "sf")
  png("Squid_Hurdle_Fig_Nino.png", width=4.5, height=7, units="in", res=300)
  final.plot<-ggplot(data=world) + geom_raster(data =  LL_SQ, aes(x=Longitude, y = Latitude, fill=Sq_Data),linejoin = "mitre") + 
    coord_fixed(ratio = 1) +
    scale_fill_viridis(direction = 1, na.value="white", limits=c(0, 5))+
    theme_bw()+ggtitle("Jul-Sep El Niño")+labs(fill = "Log(CPUE)")+geom_sf()+coord_sf(xlim=c(-128,-117), ylim=c(32, 43))
  print(final.plot)
  dev.off()
  
Boom=c(2012:2014)
  Bust=c(2010,2011)
  qqq=which(Yrsss %in% Boom)
  Sq_Data<-rowMeans(Annual_Hurdle_Mean_LL[,qqq], na.rm = TRUE)
  LL<-Annual_Hurdle_Mean_LL[,23:24]
  LL_SQ<-as.data.frame(cbind(LL,Sq_Data))
  world<-ne_countries(scale="medium", returnclass = "sf")
  png("Squid_Hurdle_Fig_Boom.png", width=4.5, height=7, units="in", res=300)
  final.plot<-ggplot(data=world) + geom_raster(data =  LL_SQ, aes(x=Longitude, y = Latitude, fill=Sq_Data),linejoin = "mitre") + 
    coord_fixed(ratio = 1) +
    scale_fill_viridis(direction = 1, na.value="white", limits=c(0, 8))+
    theme_bw()+ggtitle("Jul-Sep 2012-2014")+labs(fill = "Log(CPUE)")+geom_sf()+coord_sf(xlim=c(-128,-117), ylim=c(32, 43))
  print(final.plot)
  dev.off()
  
  
  qqq=which(Yrsss %in% Bust)
  Sq_Data<-rowMeans(Annual_Hurdle_Mean_LL[,qqq], na.rm = TRUE)
  LL<-Annual_Hurdle_Mean_LL[,23:24]
  LL_SQ<-as.data.frame(cbind(LL,Sq_Data))
  world<-ne_countries(scale="medium", returnclass = "sf")
  png("Squid_Hurdle_Fig_Bust.png", width=4.5, height=7, units="in", res=300)
  final.plot<-ggplot(data=world) + geom_raster(data =  LL_SQ, aes(x=Longitude, y = Latitude, fill=Sq_Data),linejoin = "mitre") + 
    coord_fixed(ratio = 1) +
    scale_fill_viridis(direction = 1, na.value="white", limits=c(0, 8))+
    theme_bw()+ggtitle("Jul-Sep 2010-2011")+labs(fill = "Log(CPUE)")+geom_sf()+coord_sf(xlim=c(-128,-117), ylim=c(32, 43))
  print(final.plot)
  dev.off()
  

png("2016_Survey_Map.png", height=5, width=6, units="in", res=300)
ggplot(data=world) + geom_point(data =RREAS_Data[RREAS_Data$year==2016,] , aes(x=Longitude, y = Latitude)) + 
    coord_fixed(ratio = 1) +
    theme_bw()+geom_sf()+coord_sf(xlim=c(-128,-117), ylim=c(32, 43))+ggtitle(2016)
dev.off()


RREAS_Data$Longitude<-as.numeric(RREAS_Data$Longitude)
  RREAS_Data$Latitude<-as.numeric(RREAS_Data$Latitude)
  



#######make daily plots#############

setwd ("F:/RREAS/Model_Formation/Market_Squid/Annual_Rasters/Daily_Output/Daily_Figs")
for (i in 1:length(Unique_Date)){
  
  Sq_Data<-PA_Output[,i]
  LL<-Annual_PA_Mean_LL[,23:24]
  LL_SQ<-as.data.frame(cbind(LL,Sq_Data))
  world<-ne_countries(scale="medium", returnclass = "sf")
  png(paste("Squid_PA_Fig_New",Unique_Date[i],".png"), width=6, height=5, units="in", res=300)
  final.plot<-ggplot(data=world) + geom_raster(data =  LL_SQ, aes(x=Longitude, y = Latitude, fill=Sq_Data),linejoin = "mitre") + 
    coord_fixed(ratio = 1) +
    scale_fill_viridis(direction = 1, na.value="white", limits=c(0,1))+
    theme_bw()+ggtitle(Unique_Date[i])+labs(fill = "Prob. Occurr.")+geom_sf()+coord_sf(xlim=c(-128,-117), ylim=c(32, 43))
  print(final.plot)
  dev.off()
}

for (i in 1:length(Unique_Date)){
  Sq_Data<-Hurdle_Output[,i]
  LL<-Annual_Hurdle_Mean_LL[,23:24]
  LL_SQ<-as.data.frame(cbind(LL,Sq_Data))
  world<-ne_countries(scale="medium", returnclass = "sf")
  png(paste("Squid_Hurdle_Fig_New",Unique_Date[i],".png"), width=6, height=5, units="in", res=300)
  final.plot<-ggplot(data=world) + geom_raster(data =  LL_SQ, aes(x=Longitude, y = Latitude, fill=Sq_Data),linejoin = "mitre") + 
    coord_fixed(ratio = 1) +
    scale_fill_viridis(direction = 1, na.value="white", limits=c(0, 8.1))+
    theme_bw()+ggtitle(Unique_Date[i])+labs(fill = "Log(CPUE)")+geom_sf()+coord_sf(xlim=c(-128,-117), ylim=c(32, 43))
  print(final.plot)
  dev.off()
}

save.image("F:/RREAS/Model_Formation/Market_Squid/Annual_Rasters/Market_Squid_Daily_Output_220208.RData", safe=FALSE)
