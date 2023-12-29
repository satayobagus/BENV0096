library(tidyverse)
library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(ncdf4)
library(chron)
library(stringr)

library(MASS)
library(Metrics)
library(forecast)
library(sigmoid)

#Set working directory
wd = "C:/Users/Lenovo/Documents/Buku/BENV0096 Dissertation"

#----------------------------------------------------------------------------------------------
#1. Clean disaggregated data

clean_append_disagg("/RCP 2.6")
clean_append_disagg("/RCP 4.5")
clean_append_disagg("/RCP 8.5")

clean_append_disagg <- function(main_folder){
  
work_folder <- paste0(wd,main_folder)
dirs <- list.dirs(work_folder,recursive = FALSE)

#Initialize empty dataframes
df_init_jkt <- data.frame()
df_loop_jkt <- data.frame()
df_total_jkt <- data.frame()

df_init_smg <- data.frame()
df_loop_smg <- data.frame()
df_total_smg <- data.frame()


#Clean the time columns and combine all variables into one dataframe
for (i in 1:length(dirs)){
  jkt_files <- list.files(path=dirs[i], pattern="*jkt_disagg.csv", full.names=TRUE, recursive=FALSE)
  smg_files <- list.files(path=dirs[i], pattern="*smg_disagg.csv", full.names=TRUE, recursive=FALSE)
  
  for (j in 1:length(jkt_files)) {
    #Load dataframe
    var_name_jkt <- sub(".*/" , "", sub("_jkt.*", "", jkt_files[j]))
    df_init_jkt <- read.csv(jkt_files[j]) %>%
      subset(select = c(1,2)) %>%
      mutate(time = as.POSIXct(trunc(as_datetime(time), units = "hours")))
    colnames(df_init_jkt)[2] = var_name_jkt
    
    #Adjust time for csv files that begins at BST
    if (hour(df_init_jkt$time[1])>0){
      df_init_jkt$time <- df_init_jkt$time-3600
    }
    else{}
    
    #Adjust time to handle skipped hour or duplicate hour of day
    for (k in 1:nrow(df_init_jkt)){
      
      b = k-1
      if (b==0) {}
      
      else {
        if (as.numeric(df_init_jkt$time[k]-df_init_jkt$time[b]) == 1){}
        else if (as.numeric(df_init_jkt$time[k]-df_init_jkt$time[b]) == 2){
            df_init_jkt$time[k] <- df_init_jkt$time[k]-3600
        }
        else if (as.numeric(df_init_jkt$time[k]-df_init_jkt$time[b]) == 0){
          df_init_jkt$time[k] <- df_init_jkt$time[b]+3600
        }
        else {}
        }
      }
    
    #Create combined dataframe
    if(j == 1){
      df_loop_jkt <- df_init_jkt
    }
    
    else{
      df_loop_jkt <- df_loop_jkt %>%
        left_join(df_init_jkt, by=join_by(time))
    }
  }
  
  #Write csv for each interval
  attr(df_loop_jkt$time, "tzone") <- "Asia/Jakarta"
  #write.csv(df_loop_jkt, paste0(work_folder, "/", sub(paste0(".*",main_folder,"/"), "", dirs[i]), "_jkt_", gsub("\\.", "", sub("/.*", "", sub(".*RCP ","", main_folder))), ".csv"), row.names=FALSE)
  
  #Create total dataframe
  if(i == 1){
    df_total_jkt <- df_loop_jkt
  }
  
  else{
    df_total_jkt <- rbind(df_total_jkt, df_loop_jkt)
  }
  
  #Write csv for total
  write.csv(df_total_jkt, paste0(work_folder, "/", "total", "_jkt_", gsub("\\.", "", sub("/.*", "", sub(".*RCP ","", main_folder))), ".csv"), row.names=FALSE)
  
  
  for (m in 1:length(smg_files)) {
    #Load dataframe
    var_name_smg <- sub(".*/" , "", sub("_smg.*", "", smg_files[m]))
    df_init_smg <- read.csv(smg_files[m]) %>%
      subset(select = c(1,2)) %>%
      mutate(time = as.POSIXct(trunc(as_datetime(time), units = "hours")))
    colnames(df_init_smg)[2] = var_name_smg
    
    #Adjust time for csv files that begins at BST
    if (hour(df_init_smg$time[1])>0){
      df_init_smg$time <- df_init_smg$time-3600
    }
    else{}
    
    #Adjust time to handle skipped hour or duplicate hour of day
    for (n in 1:nrow(df_init_smg)){
      
      c = n-1
      if (c==0) {}
      
      else {
        if (as.numeric(df_init_smg$time[n]-df_init_smg$time[c]) == 1){}
        else if (as.numeric(df_init_smg$time[n]-df_init_smg$time[c]) == 2){
          df_init_smg$time[n] <- df_init_smg$time[n]-3600
        }
        else if (as.numeric(df_init_smg$time[n]-df_init_smg$time[c]) == 0){
          df_init_smg$time[n] <- df_init_smg$time[n]+3600
        }
        else {}
      }
    }
    
    #Create combined dataframe
    if(m == 1){
      df_loop_smg <- df_init_smg
    }
    
    else{
      df_loop_smg <- df_loop_smg %>%
        left_join(df_init_smg, by=join_by(time))
    }
  }
  
  #Write csv for each interval
  attr(df_loop_smg$time, "tzone") <- "Asia/Jakarta"
  #write.csv(df_loop_smg, paste0(work_folder, "/", sub(paste0(".*",main_folder,"/"), "", dirs[i]), "_smg_", gsub("\\.", "", sub("/.*", "", sub(".*RCP ","", main_folder))), ".csv"), row.names=FALSE)
  
  #Create total dataframe
  if(i == 1){
    df_total_smg <- df_loop_smg
  }
  
  else{
    df_total_smg <- rbind(df_total_smg, df_loop_smg)
  }
  
  #Write csv for total
  write.csv(df_total_smg, paste0(work_folder, "/", "total", "_smg_", gsub("\\.", "", sub("/.*", "", sub(".*RCP ","", main_folder))), ".csv"), row.names=FALSE)
  
  
}
}

#-----------------------------------------------------------------------------------------
#3. Prepare historical hourly weather data for pattern synchronization
#Load weather data

history_append("/Jakarta")
history_append("/Semarang")


history_append <- function(loc_folder) {

loc_files <- list.files(path=paste0(wd,loc_folder), pattern="*hour weather.nc", full.names=TRUE, recursive=FALSE)

loc_df <- data.frame()

for (j in 1:length(loc_files)) {
  
  w_loc <- nc_open(loc_files[j])
  
  #Extract dimensions
  lon_loc <- ncvar_get(w_loc, "longitude")
  lat_loc <- ncvar_get(w_loc, "latitude")
  time_loc <- ncvar_get(w_loc, "time")
  
  #Convert time from string to date-time
  tunits_loc <- ncatt_get(w_loc,"time","units")
  tustr_loc <- strsplit(tunits_loc$value, " ") 
  tdstr_loc <- strsplit(unlist(tustr_loc)[3], "-")
  tyear_loc <- as.integer(unlist(tdstr_loc)[1]) 
  tmonth_loc <- as.integer(unlist(tdstr_loc)[2])
  tday_loc <- as.integer(unlist(tdstr_loc)[3])
  
  #Create dataframe out of the weather data
  df_loc <- as.matrix((expand.grid(lon_loc, lat_loc, time_loc)))
  
  #Extract variables
  for (i in 1:length(w_loc[[15]])) {
    
    var_name <- w_loc[[15]][[i]]$name
    var_get <- ncvar_get(w_loc, var_name)
    var_vec <- if(length(dim(var_get))==3) {
                  as.vector(var_get[,,])
                }
               else if (length(dim(var_get))==2){ 
                 as.vector(var_get[,])
               }
               else{}
    df_loc <- data.frame(cbind(df_loc,var_vec)) %>%
      na.omit()
  }
  
  #Rename columns
  colnames(df_loc) <- c("lon", "lat", "time","u10_wind","v10_wind","dewpoint_temp","temp","ssrd","precip")
  
  #Change time column format
  df_loc$time <- as.POSIXct(chron(df_loc$time/24, origin=c(tmonth_loc, tday_loc, tyear_loc)))
  attr(df_loc$time, "tzone") <- "Asia/Jakarta"
  
  #Obtain hourly average values of all measurement coordinates
  master_loc <- df_loc %>%
    group_by(time) %>%
    summarise_all(mean) %>%
    dplyr::select(-c(lon, lat))
  
  #Create total dataframe
  if(j == 1){
    loc_df <- master_loc
  }
  
  else{
    loc_df <- rbind(loc_df, master_loc)
  }
 
  #Adjust time to handle skipped hour or duplicate hour of day
  for (k in 1:nrow(loc_df)){
    
    b = k-1
    if (b==0) {}
    
    else {
      if (as.numeric(loc_df$time[k]-loc_df$time[b]) == 1){}
      else if (as.numeric(loc_df$time[k]-loc_df$time[b]) == 2){
        loc_df$time[k] <- loc_df$time[k]-3600
      }
      else if (as.numeric(loc_df$time[k]-loc_df$time[b]) == 0){
        loc_df$time[k] <- loc_df$time[b]+3600
      }
      else {}
    }
  }
  
}

#Calculate wind speed using u and v-components
#Drop u and v-component columns
loc_df$wind_speed <- sqrt(loc_df$u10_wind**2 + loc_df$v10_wind**2)
loc_df <- loc_df %>%
  subset(select=-c(u10_wind, v10_wind))

#Write csv for total
write.csv(loc_df, paste0(wd, loc_folder, "/", "weather_", gsub("*/","", loc_folder), ".csv"), row.names=FALSE)

}

#Identify climate pattern using historical weather data for the last 30 years
#Do it for every weather variable

#Read historical weather data csv for both locations
hist_jkt <- read_csv(paste0(wd, "/Jakarta/weather_Jakarta.csv")) %>%
  subset(year(time)<=2021 & year(time)>=1992) %>%
  mutate(ssrd_adjust = ifelse(ssrd<0,0,ssrd), precip_adjust = ifelse(precip<0,0,precip)) %>%
  dplyr::select(-c(ssrd,precip))
hist_smg <- read_csv(paste0(wd, "/Semarang/weather_Semarang.csv")) %>%
  subset(year(time)<=2020 & year(time)>=1991) %>%
  mutate(ssrd_adjust = ifelse(ssrd<0,0,ssrd), precip_adjust = ifelse(precip<0,0,precip)) %>%
  dplyr::select(-c(ssrd,precip))

#Rename adjusted columns
hist_jkt <- rename(hist_jkt, ssrd = ssrd_adjust, precip = precip_adjust)
hist_smg <- rename(hist_smg, ssrd = ssrd_adjust, precip = precip_adjust)

#Adjust units
#Temperatures to Celsius
hist_smg$dewpoint_temp <- hist_smg$dewpoint_temp-273.15
hist_smg$temp <- hist_smg$temp-273.15

hist_jkt$dewpoint_temp <- hist_jkt$dewpoint_temp-273.15
hist_jkt$temp <- hist_jkt$temp-273.15

#SSRD to W/m2
hist_smg$ssrd <- hist_smg$ssrd/3600
hist_jkt$ssrd <- hist_jkt$ssrd/3600

#Precipitation in mm
hist_smg$precip <- hist_smg$precip*10^3
hist_jkt$precip <- hist_jkt$precip*10^3

#Wind speed in km/h
hist_smg$wind_speed <- hist_smg$wind_speed*3600/10^3
hist_jkt$wind_speed <- hist_jkt$wind_speed*3600/10^3

#Prepare year index to "normalize" the years
year_index_jkt <- hist_jkt %>%
  distinct(year(time)) %>%
  rename("year" = "year(time)") %>%
  mutate(year_proj = year+30, year_index = order(year))

year_index_smg <- hist_smg %>%
  distinct(year(time)) %>%
  rename("year" = "year(time)") %>%
  mutate(year_proj = year+30, year_index = order(year))

#Method 1 (daily mean ratio disaggregation pattern)
#Calculate daily mean pattern for 30 years for each weather variable
mean_jkt <- hist_jkt %>%
  mutate(year=year(time), month=month(time), day=day(time)) %>%
  left_join(year_index_jkt, by = "year") %>%
  mutate(group_index = paste0(year_index,"-",month,"-",day)) %>%
  group_by(group_index) %>%
  mutate(mean_dewpoint = mean(dewpoint_temp),mean_temp = mean(temp),mean_wind_speed = mean(wind_speed),mean_ssrd = mean(ssrd), mean_precip = mean(precip)) %>%
  mutate(ratio_dewpoint = dewpoint_temp/mean_dewpoint,ratio_temp = temp/mean_temp,ratio_wind_speed = wind_speed/mean_wind_speed,ratio_ssrd = ssrd/mean_ssrd,ratio_precip = precip/mean_precip) %>%
  dplyr::select(-c(year, month, day, year_index, year_proj)) %>%
  imputeTS::na_replace(0)

mean_smg <- hist_smg %>%
  mutate(year=year(time), month=month(time), day=day(time)) %>%
  left_join(year_index_smg, by = "year") %>%
  mutate(group_index = paste0(year_index,"-",month,"-",day)) %>%
  group_by(group_index) %>%
  mutate(mean_dewpoint = mean(dewpoint_temp),mean_temp = mean(temp),mean_wind_speed = mean(wind_speed),mean_ssrd = mean(ssrd), mean_precip = mean(precip)) %>%
  mutate(ratio_dewpoint = dewpoint_temp/mean_dewpoint,ratio_temp = temp/mean_temp,ratio_wind_speed = wind_speed/mean_wind_speed,ratio_ssrd = ssrd/mean_ssrd,ratio_precip = precip/mean_precip) %>%
  dplyr::select(-c(year, month, day, year_index, year_proj)) %>%
  imputeTS::na_replace(0)

#Prepare mean dataframe for join with projection daily data
mean_jkt_ratio <- mean_jkt %>%
  mutate(time=time+30*365.25*24*60*60+12*60*60, year=year(time)) %>%
  left_join(year_index_jkt, by = c("year"="year_proj")) %>%
  mutate(group_index = paste0(year_index,"-",month(time),"-",day(time))) %>%
  dplyr::select(c(time,group_index, contains('ratio')))

mean_smg_ratio <- mean_smg %>%
  mutate(time=time+30*365.25*24*60*60+12*60*60, year=year(time)) %>%
  left_join(year_index_smg, by = c("year"="year_proj")) %>%
  mutate(group_index = paste0(year_index,"-",month(time),"-",day(time))) %>%
  dplyr::select(c(time,group_index, contains('ratio')))

#Append daily data of projections
clean_append_daily("/RCP 2.6")
clean_append_daily("/RCP 4.5")
clean_append_daily("/RCP 8.5")

clean_append_daily <- function(main_folder){
  
  work_folder <- paste0(wd,main_folder)
  dirs <- list.dirs(work_folder,recursive = FALSE)
  
  #Initialize empty dataframes
  df_init_jkt <- data.frame()
  df_loop_jkt <- data.frame()
  df_total_jkt <- data.frame()
  
  df_init_smg <- data.frame()
  df_loop_smg <- data.frame()
  df_total_smg <- data.frame()
  
  
  #Clean the time columns and combine all variables into one dataframe
  for (i in 1:length(dirs)){
    jkt_files <- list.files(path=dirs[i], pattern="*jkt_daily.csv", full.names=TRUE, recursive=FALSE)
    smg_files <- list.files(path=dirs[i], pattern="*smg_daily.csv", full.names=TRUE, recursive=FALSE)
    
    for (j in 1:length(jkt_files)) {
      #Load dataframe
      var_name_jkt <- sub(".*/" , "", sub("_jkt.*", "", jkt_files[j]))
      df_init_jkt <- read.csv(jkt_files[j]) %>%
        subset(select = c(1,2)) %>%
        mutate(time = as.POSIXct(trunc(as_datetime(time), units = "hours")))
      colnames(df_init_jkt)[2] = var_name_jkt
      
      #Adjust time for csv files that begins at BST
      if (hour(df_init_jkt$time[1])>0){
        df_init_jkt$time <- df_init_jkt$time-3600
      }
      else{}
      
      #Create combined dataframe
      if(j == 1){
        df_loop_jkt <- df_init_jkt
      }

      else{
        df_loop_jkt <- df_loop_jkt %>%
          left_join(df_init_jkt, by=join_by(time))
      }
    }
    
    #Write csv for each interval
    attr(df_loop_jkt$time, "tzone") <- "Asia/Jakarta"
    
    #Create total dataframe
    if(i == 1){
      df_total_jkt <- df_loop_jkt
    }
    
    else{
      df_total_jkt <- rbind(df_total_jkt, df_loop_jkt)
    }
    
    #Write csv for total
    write.csv(df_total_jkt, paste0(work_folder, "/", "total", "_jkt_", gsub("\\.", "", sub("/.*", "", sub(".*RCP ","", main_folder))), "_daily.csv"), row.names=FALSE)
    
    
    for (m in 1:length(smg_files)) {
      #Load dataframe
      var_name_smg <- sub(".*/" , "", sub("_smg.*", "", smg_files[m]))
      df_init_smg <- read.csv(smg_files[m]) %>%
        subset(select = c(1,2)) %>%
        mutate(time = as.POSIXct(trunc(as_datetime(time), units = "hours")))
      colnames(df_init_smg)[2] = var_name_smg
      
      #Adjust time for csv files that begins at BST
      if (hour(df_init_smg$time[1])>0){
        df_init_smg$time <- df_init_smg$time-3600
      }
      else{}
      
      #Create combined dataframe
      if(m == 1){
        df_loop_smg <- df_init_smg
      }
      
      else{
        df_loop_smg <- df_loop_smg %>%
          left_join(df_init_smg, by=join_by(time))
      }
    }
    
    #Write csv for each interval
    attr(df_loop_smg$time, "tzone") <- "Asia/Jakarta"
    
    #Create total dataframe
    if(i == 1){
      df_total_smg <- df_loop_smg
    }
    
    else{
      df_total_smg <- rbind(df_total_smg, df_loop_smg)
    }
    
    #Write csv for total
    write.csv(df_total_smg, paste0(work_folder, "/", "total", "_smg_", gsub("\\.", "", sub("/.*", "", sub(".*RCP ","", main_folder))), "_daily.csv"), row.names=FALSE)
    
    
  }
}

#Create synthetic hourly weather data from daily projection data
#Utilize the ratio between hourly and daily mean weather values to obtain the 30-year weather pattern
#Use the obtained ratio with the daily mean projected weather values to obtain hourly weather values
#that followed the exact same pattern with the appropriate daily average

mean_of_project("/RCP 2.6")
main_folder <- "/RCP 2.6"

mean_of_project("/RCP 4.5")
main_folder <- "/RCP 4.5"

mean_of_project("/RCP 8.5")
main_folder <- "/RCP 8.5"

mean_of_project <- function(main_folder){
  
  work_folder <<- paste0(wd,main_folder)
  
#Load projection daily data
jkt_daily_mean_proj <- read.csv(paste0(work_folder,"/",list.files(path=work_folder, pattern="jkt.+daily.csv"))) %>%
  mutate(time=as_datetime(time), year=year(time)) %>%
  subset(year(time)<=2051 & year(time)>=2022) %>%
  left_join(year_index_jkt, by = c("year"="year_proj")) %>%
  mutate(group_index = paste0(year_index,"-",month(time),"-",day(time))) %>%
  dplyr::select(c(time,hurs,pr,rsds,sfcWind,tas,group_index))

smg_daily_mean_proj <- read.csv(paste0(work_folder,"/",list.files(path=work_folder, pattern="smg.+daily.csv"))) %>%
  mutate(time=as_datetime(time), year=year(time)) %>%
  subset(year(time)<=2050 & year(time)>=2021) %>%
  left_join(year_index_smg, by = c("year"="year_proj")) %>%
  mutate(group_index = paste0(year_index,"-",month(time),"-",day(time))) %>%
  dplyr::select(c(time,hurs,pr,rsds,sfcWind,tas,group_index))

#Adjust units
#Temperatures to Celsius
if(main_folder == "/RCP 2.6"){
  jkt_daily_mean_proj$temp <- jkt_daily_mean_proj$tas-273.15+1
  smg_daily_mean_proj$temp <- smg_daily_mean_proj$tas-273.15+1
}
else if(main_folder == "/RCP 4.5"){
  jkt_daily_mean_proj$temp <- jkt_daily_mean_proj$tas-273.15+1.4
  smg_daily_mean_proj$temp <- smg_daily_mean_proj$tas-273.15+1.4
}
else{
  jkt_daily_mean_proj$temp <- jkt_daily_mean_proj$tas-273.15+2
  smg_daily_mean_proj$temp <- smg_daily_mean_proj$tas-273.15+2
}



#Relative humidity to dewpoint temperature (Celsius)
#Formula used by Lawrence (2005); https://doi.org/10.1175/BAMS-86-2-225 
jkt_daily_mean_proj$dewpoint_temp <- (243.04*(log(jkt_daily_mean_proj$hurs/100)+(17.625*jkt_daily_mean_proj$temp)/(243.04+jkt_daily_mean_proj$temp)))/(17.625-log(jkt_daily_mean_proj$hurs/100)-(17.625*jkt_daily_mean_proj$temp)/(243.04+jkt_daily_mean_proj$temp))
smg_daily_mean_proj$dewpoint_temp <- (243.04*(log(smg_daily_mean_proj$hurs/100)+(17.625*smg_daily_mean_proj$temp)/(243.04+smg_daily_mean_proj$temp)))/(17.625-log(smg_daily_mean_proj$hurs/100)-(17.625*smg_daily_mean_proj$temp)/(243.04+smg_daily_mean_proj$temp))

#Rename SSRD column already in W/m2
jkt_daily_mean_proj <- rename(jkt_daily_mean_proj, "ssrd" = "rsds")
smg_daily_mean_proj <- rename(smg_daily_mean_proj, "ssrd" = "rsds")

#Precipitation in mm/hour
jkt_daily_mean_proj$precip <- jkt_daily_mean_proj$pr*3600
smg_daily_mean_proj$precip <- smg_daily_mean_proj$pr*3600

#Wind speed in km/h
jkt_daily_mean_proj$wind_speed <- jkt_daily_mean_proj$sfcWind*3600/10^3
smg_daily_mean_proj$wind_speed <- smg_daily_mean_proj$sfcWind*3600/10^3

#Select adjusted variables
jkt_daily_mean_proj <- jkt_daily_mean_proj %>%
  dplyr::select(c(dewpoint_temp, temp, ssrd, precip, wind_speed, group_index))

smg_daily_mean_proj <- smg_daily_mean_proj %>%
  dplyr::select(c(dewpoint_temp, temp, ssrd, precip, wind_speed, group_index))

#Join ratio and projected weather values tables
jkt_disagg_ratio <<- mean_jkt_ratio %>%
  left_join(jkt_daily_mean_proj, by = "group_index") %>%
  mutate(proj_dewpoint_temp = dewpoint_temp*ratio_dewpoint, proj_temp = temp*ratio_temp, proj_wind_speed = wind_speed*ratio_wind_speed, proj_ssrd = ssrd*ratio_ssrd, proj_precip = precip*ratio_precip) %>%
  dplyr::select(c(time,contains("proj")))

smg_disagg_ratio <<- mean_smg_ratio %>%
  left_join(smg_daily_mean_proj, by = "group_index") %>%
  mutate(proj_dewpoint_temp = dewpoint_temp*ratio_dewpoint, proj_temp = temp*ratio_temp, proj_wind_speed = wind_speed*ratio_wind_speed, proj_ssrd = ssrd*ratio_ssrd, proj_precip = precip*ratio_precip) %>%
  dplyr::select(c(time,contains("proj"))) 

}


#Order dataframe
jkt_disagg_ratio <- jkt_disagg_ratio[order(jkt_disagg_ratio$time),]
smg_disagg_ratio <- smg_disagg_ratio[order(smg_disagg_ratio$time),]

#Write csv for total
write.csv(jkt_disagg_ratio, paste0(work_folder, "/", "mean_disagg_jkt_", gsub("\\.", "", sub("/.*", "", sub(".*RCP ","", main_folder))), ".csv"), row.names=FALSE)
write.csv(smg_disagg_ratio, paste0(work_folder, "/", "mean_disagg_smg_", gsub("\\.", "", sub("/.*", "", sub(".*RCP ","", main_folder))), ".csv"), row.names=FALSE)



# #Method 2 (rank pattern with tempdisagg data)
# #Rank values for each day to obtain pattern
# rank_smg <- hist_smg  %>%
#   mutate(year=year(time), month=month(time), day=day(time)) %>%
#   left_join(year_index_smg, by = "year") %>%
#   mutate(group_index = paste0(year_index,"-",month,"-",day)) %>%
#   group_by(group_index) %>%
#   mutate(rank_dew = rank(-dewpoint_temp), rank_temp = rank(-temp), rank_pr = rank(-precip), rank_wind=rank(-wind_speed), rank_ssrd=rank(-ssrd)) %>%
#   mutate(join_key_dew=paste0(group_index,"-",rank_dew),join_key_temp=paste0(group_index,"-",rank_temp),join_key_pr=paste0(group_index,"-",rank_pr),join_key_wind=paste0(group_index,"-",rank_wind),join_key_ssrd=paste0(group_index,"-",rank_ssrd)) %>%
#   dplyr::select(-c(year,month,day))
# 
# rank_jkt <- hist_jkt  %>%
#   mutate(year=year(time), month=month(time), day=day(time)) %>%
#   left_join(year_index_jkt, by = "year") %>%
#   mutate(group_index = paste0(year_index,"-",month,"-",day)) %>%
#   group_by(group_index) %>%
#   mutate(rank_dew = rank(-dewpoint_temp), rank_temp = rank(-temp), rank_pr = rank(-precip), rank_wind=rank(-wind_speed), rank_ssrd=rank(-ssrd)) %>%
#   mutate(join_key_dew=paste0(group_index,"-",rank_dew),join_key_temp=paste0(group_index,"-",rank_temp),join_key_pr=paste0(group_index,"-",rank_pr),join_key_wind=paste0(group_index,"-",rank_wind),join_key_ssrd=paste0(group_index,"-",rank_ssrd)) %>%
#   dplyr::select(-c(year,month,day))


