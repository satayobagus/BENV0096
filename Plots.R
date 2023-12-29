library(tidyverse)
library(tidymodels)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ncdf4)
library(chron)
library(stringr)

library(ggcorrplot)
library(MASS)
library(Metrics)
library(forecast)
library(sigmoid)

library(ggsci)
library(ggcorrplot)
library(plotly)

#Set working directory
wd = "C:/Users/Lenovo/Documents/Buku/BENV0096 Dissertation"

#Plot original daily weather variables
#Jakarta
#Read
daily_proj_26_jkt <- read.csv(paste0(wd, "/RCP 2.6/total_jkt_26_daily.csv")) %>%
  mutate(time=as_datetime(time, tz="Asia/Jakarta")) %>%
  subset(year(time)>=2022 & year(time)<=2051) %>%
  dplyr::select(-c(sfcWind))

daily_proj_45_jkt <- read.csv(paste0(wd, "/RCP 4.5/total_jkt_45_daily.csv")) %>%
  mutate(time=as_datetime(time, tz="Asia/Jakarta")) %>%
  subset(year(time)>=2022 & year(time)<=2051) %>%
  dplyr::select(-c(sfcWind))

daily_proj_85_jkt <- read.csv(paste0(wd, "/RCP 8.5/total_jkt_85_daily.csv")) %>%
  mutate(time=as_datetime(time, tz="Asia/Jakarta")) %>%
  subset(year(time)>=2022 & year(time)<=2051) %>%
  dplyr::select(-c(sfcWind))

#Convert
daily_proj_26_jkt$temp <- daily_proj_26_jkt$tas-273.15+1
daily_proj_26_jkt$dewpoint_temp <- (243.04*(log(daily_proj_26_jkt$hurs/100)+(17.625*daily_proj_26_jkt$temp)/(243.04+daily_proj_26_jkt$temp)))/(17.625-log(daily_proj_26_jkt$hurs/100)-(17.625*daily_proj_26_jkt$temp)/(243.04+daily_proj_26_jkt$temp))
daily_proj_26_jkt <- rename(daily_proj_26_jkt, "ssrd" = "rsds")
daily_proj_26_jkt$precip <- daily_proj_26_jkt$pr*3600

daily_proj_45_jkt$temp <- daily_proj_45_jkt$tas-273.15+1.4
daily_proj_45_jkt$dewpoint_temp <- (243.04*(log(daily_proj_45_jkt$hurs/100)+(17.625*daily_proj_45_jkt$temp)/(243.04+daily_proj_45_jkt$temp)))/(17.625-log(daily_proj_45_jkt$hurs/100)-(17.625*daily_proj_45_jkt$temp)/(243.04+daily_proj_45_jkt$temp))
daily_proj_45_jkt <- rename(daily_proj_45_jkt, "ssrd" = "rsds")
daily_proj_45_jkt$precip <- daily_proj_45_jkt$pr*3600

daily_proj_85_jkt$temp <- daily_proj_85_jkt$tas-273.15+2
daily_proj_85_jkt$dewpoint_temp <- (243.04*(log(daily_proj_85_jkt$hurs/100)+(17.625*daily_proj_85_jkt$temp)/(243.04+daily_proj_85_jkt$temp)))/(17.625-log(daily_proj_85_jkt$hurs/100)-(17.625*daily_proj_85_jkt$temp)/(243.04+daily_proj_85_jkt$temp))
daily_proj_85_jkt <- rename(daily_proj_85_jkt, "ssrd" = "rsds")
daily_proj_85_jkt$precip <- daily_proj_85_jkt$pr*3600

#Select
daily_proj_26_jkt <- daily_proj_26_jkt %>%
  dplyr::select(c(time, dewpoint_temp, temp, ssrd, precip))

daily_proj_45_jkt <- daily_proj_45_jkt %>%
  dplyr::select(c(time, dewpoint_temp, temp, ssrd, precip))

daily_proj_85_jkt <- daily_proj_85_jkt %>%
  dplyr::select(c(time, dewpoint_temp, temp, ssrd, precip))

#Bind/Join
daily_proj_26_jkt <- daily_proj_26_jkt %>%
  cbind(dewpoint_temp_45 = daily_proj_45_jkt$dewpoint_temp, temp_45 = daily_proj_45_jkt$temp, ssrd_45 = daily_proj_45_jkt$ssrd, precip_45 = daily_proj_45_jkt$precip) %>%
  cbind(dewpoint_temp_85 = daily_proj_85_jkt$dewpoint_temp, temp_85 = daily_proj_85_jkt$temp, ssrd_85 = daily_proj_85_jkt$ssrd, precip_85 = daily_proj_85_jkt$precip)

monthly_proj_26_jkt <- daily_proj_26_jkt %>%
  mutate(year=year(time),month=month(time)) %>%
  group_by(year,month) %>%
  summarise_all(mean) %>%
  ungroup() %>%
  mutate(time=as.POSIXct(trunc(as_datetime(time), units = "month")))

bquote(bold('SSRD'~(W/m^2)))
#Plot
ggplot(data=monthly_proj_26_jkt %>%
         dplyr::select(time,starts_with("ssrd")) %>%
         pivot_longer(!c(time), names_to = "category", values_to = "values"),
       aes(x=time,y=values))+
  geom_line(aes(colour=category),linewidth=1)+
  scale_colour_manual(name = "Scenario", labels = c("RCP 2.6", "RCP 4.5", "RCP 8.5"), values=c("chartreuse4","deepskyblue3","red"))+
  labs(title=paste0("Surface Solar Radiation Downwards, Jakarta"), subtitle = "Monthly average, 2022-2051", x = "Time", y = bquote(bold('SSRD'~(W/m^2))))+ 
  theme(plot.title=element_text(size=25, face="bold", colour="black"),
        plot.subtitle=element_text(size=18, face="italic", color="black"))+
  theme(axis.title.x=element_text(size=14, face="bold", color="black"),
        axis.title.y=element_text(size=14, face="bold", color="black"),
        axis.text.x=element_text(size=12, color="black"),
        axis.text.y=element_text(size=12, color="black"))+
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=10)) 


#Semarang
#Read
daily_proj_26_smg <- read.csv(paste0(wd, "/RCP 2.6/total_smg_26_daily.csv")) %>%
  mutate(time=as_datetime(time, tz="Asia/Jakarta")) %>%
  subset(year(time)>=2021 & year(time)<=2050) %>%
  dplyr::select(-c(sfcWind))

daily_proj_45_smg <- read.csv(paste0(wd, "/RCP 4.5/total_smg_45_daily.csv")) %>%
  mutate(time=as_datetime(time, tz="Asia/Jakarta")) %>%
  subset(year(time)>=2021 & year(time)<=2050) %>%
  dplyr::select(-c(sfcWind))

daily_proj_85_smg <- read.csv(paste0(wd, "/RCP 8.5/total_smg_85_daily.csv")) %>%
  mutate(time=as_datetime(time, tz="Asia/Jakarta")) %>%
  subset(year(time)>=2021 & year(time)<=2050) %>%
  dplyr::select(-c(sfcWind))

#Convert
daily_proj_26_smg$temp <- daily_proj_26_smg$tas-273.15+1
daily_proj_26_smg$dewpoint_temp <- (243.04*(log(daily_proj_26_smg$hurs/100)+(17.625*daily_proj_26_smg$temp)/(243.04+daily_proj_26_smg$temp)))/(17.625-log(daily_proj_26_smg$hurs/100)-(17.625*daily_proj_26_smg$temp)/(243.04+daily_proj_26_smg$temp))
daily_proj_26_smg <- rename(daily_proj_26_smg, "ssrd" = "rsds")
daily_proj_26_smg$precip <- daily_proj_26_smg$pr*3600

daily_proj_45_smg$temp <- daily_proj_45_smg$tas-273.15+1.4
daily_proj_45_smg$dewpoint_temp <- (243.04*(log(daily_proj_45_smg$hurs/100)+(17.625*daily_proj_45_smg$temp)/(243.04+daily_proj_45_smg$temp)))/(17.625-log(daily_proj_45_smg$hurs/100)-(17.625*daily_proj_45_smg$temp)/(243.04+daily_proj_45_smg$temp))
daily_proj_45_smg <- rename(daily_proj_45_smg, "ssrd" = "rsds")
daily_proj_45_smg$precip <- daily_proj_45_smg$pr*3600

daily_proj_85_smg$temp <- daily_proj_85_smg$tas-273.15+2
daily_proj_85_smg$dewpoint_temp <- (243.04*(log(daily_proj_85_smg$hurs/100)+(17.625*daily_proj_85_smg$temp)/(243.04+daily_proj_85_smg$temp)))/(17.625-log(daily_proj_85_smg$hurs/100)-(17.625*daily_proj_85_smg$temp)/(243.04+daily_proj_85_smg$temp))
daily_proj_85_smg <- rename(daily_proj_85_smg, "ssrd" = "rsds")
daily_proj_85_smg$precip <- daily_proj_85_smg$pr*3600

#Select
daily_proj_26_smg <- daily_proj_26_smg %>%
  dplyr::select(c(time, dewpoint_temp, temp, ssrd, precip))

daily_proj_45_smg <- daily_proj_45_smg %>%
  dplyr::select(c(time, dewpoint_temp, temp, ssrd, precip))

daily_proj_85_smg <- daily_proj_85_smg %>%
  dplyr::select(c(time, dewpoint_temp, temp, ssrd, precip))

#Bind/Join
daily_proj_26_smg <- daily_proj_26_smg %>%
  cbind(dewpoint_temp_45 = daily_proj_45_smg$dewpoint_temp, temp_45 = daily_proj_45_smg$temp, ssrd_45 = daily_proj_45_smg$ssrd, precip_45 = daily_proj_45_smg$precip) %>%
  cbind(dewpoint_temp_85 = daily_proj_85_smg$dewpoint_temp, temp_85 = daily_proj_85_smg$temp, ssrd_85 = daily_proj_85_smg$ssrd, precip_85 = daily_proj_85_smg$precip)

monthly_proj_26_smg <- daily_proj_26_smg %>%
  mutate(year=year(time),month=month(time)) %>%
  group_by(year,month) %>%
  summarise_all(mean) %>%
  ungroup() %>%
  mutate(time=as.POSIXct(trunc(as_datetime(time), units = "month")))

#Plot
ggplot(data=monthly_proj_26_smg %>%
         dplyr::select(time,starts_with("ssrd")) %>%
         pivot_longer(!c(time), names_to = "category", values_to = "values"),
       aes(x=time,y=values))+
  geom_line(aes(colour=category),linewidth=1)+
  scale_colour_manual(name = "Scenario", labels = c("RCP 2.6", "RCP 4.5", "RCP 8.5"), values=c("chartreuse4","deepskyblue3","red"))+
  labs(title=paste0("Surface Solar Radiation Downwards, Semarang"), subtitle = "Monthly average, 2021-2050", x = "Time", y = bquote(bold('SSRD'~(W/m^2))))+ 
  theme(plot.title=element_text(size=25, face="bold", colour="black"),
        plot.subtitle=element_text(size=18, face="italic", color="black"))+
  theme(axis.title.x=element_text(size=14, face="bold", color="black"),
        axis.title.y=element_text(size=14, face="bold", color="black"),
        axis.text.x=element_text(size=12, color="black"),
        axis.text.y=element_text(size=12, color="black"))+
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=10)) 

#--------------------------------------------------------------------------------------
#PLOTS

#Original average cooling load
t <- ggplot(data=cooling_data_smg %>% 
              mutate(hour=hour(time),month=month(time,label=TRUE)) %>%
              pivot_longer(-c(time,hour,month), names_to = "category", values_to = "values") %>%
              filter(category=="avg_cooling_load"),
            aes(x=hour,y=values))+
  geom_line(aes(colour=month),linewidth=1)+
  scale_color_igv(name = "Month")+
  labs(title=paste0("Hotel Cooling Load, Semarang"), subtitle = "Monthly hour average, 2020", x = "Hour", y = "Cooling Load (kW)")+ 
  theme(plot.title=element_text(size=25, face="bold", colour="black"),
        plot.subtitle=element_text(size=18, face="italic", color="black"))+
  theme(axis.title.x=element_text(size=14, face="bold", color="black"),
        axis.title.y=element_text(size=14, face="bold", color="black"),
        axis.text.x=element_text(size=12, color="black"),
        axis.text.y=element_text(size=12, color="black"))+
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=10)) 
t


#Weather variables
q <- ggplot(data=semarang_2020_2 %>% 
              mutate(hour=hour(time),month=month(time,label=TRUE)) %>%
              pivot_longer(-c(time,hour,month), names_to = "category", values_to = "values") %>%
              filter(category=="precip"),
            aes(x=hour,y=values))+
  geom_line(aes(colour=month),linewidth=1)+
  scale_color_igv(name = "Month")+
  labs(title=paste0("Total Precipitation, Semarang"), subtitle = "Monthly hour average, 2020", x = "Hour", y = "Total Precipitation (mm)")+ 
  theme(plot.title=element_text(size=25, face="bold", colour="black"),
        plot.subtitle=element_text(size=18, face="italic", color="black"))+
  theme(axis.title.x=element_text(size=14, face="bold", color="black"),
        axis.title.y=element_text(size=14, face="bold", color="black"),
        axis.text.x=element_text(size=12, color="black"),
        axis.text.y=element_text(size=12, color="black"))+
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=10)) 
q

#Plot in comparison to original test data
ggplot(data=prediction_df_jkt_3 %>% pivot_longer(!hour, names_to = "category", values_to = "values"), aes(x=hour,y=values))+
  geom_line(aes(colour=category),linewidth=1)+
  scale_colour_discrete(name = "Model", labels = c("ANN", "Original", "DNN"))+
  labs(title=paste0("Test Evaluation Results, Jakarta"), x = "Hour", y = "Cooling Load (kW)")+
  theme(plot.title=element_text(size=25, face="bold", colour="black"),
        plot.subtitle=element_text(size=18, face="italic", color="black"))+
  theme(axis.title.x=element_text(size=14, face="bold", color="black"),
        axis.title.y=element_text(size=14, face="bold", color="black"),
        axis.text.x=element_text(size=12, color="black"),
        axis.text.y=element_text(size=12, color="black"))+
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=10)) 

ggplot(data=prediction_df_smg_3 %>% pivot_longer(!hour, names_to = "category", values_to = "values"), aes(x=hour,y=values))+
  geom_line(aes(colour=category),linewidth=1)+
  scale_colour_discrete(name = "Model", labels = c("ANN", "Original", "DNN"))+
  labs(title=paste0("Test Evaluation Results, Semarang"), x = "Hour", y = "Cooling Load (kW)")+
  theme(plot.title=element_text(size=25, face="bold", colour="black"),
        plot.subtitle=element_text(size=18, face="italic", color="black"))+
  theme(axis.title.x=element_text(size=14, face="bold", color="black"),
        axis.title.y=element_text(size=14, face="bold", color="black"),
        axis.text.x=element_text(size=12, color="black"),
        axis.text.y=element_text(size=12, color="black"))+
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=10)) 

#R2 plot
ggplot(data=prediction_df_jkt_3, aes(x=avg_cooling_load,y=dnn_cooling_load))+
  geom_point(size=2.5,colour="blue") +
  geom_smooth(method = "lm", se = FALSE, colour="red")+
  labs(title=paste0("Predicted vs Actual Cooling Load, Jakarta"), subtitle = "DNN, 4 layers", x = "Actual Cooling Load (kW)", y = "Predicted Cooling Load (kW)")+
  theme(plot.title=element_text(size=25, face="bold", colour="black"),
        plot.subtitle=element_text(size=18, face="italic", color="black"))+
  theme(axis.title.x=element_text(size=14, face="bold", color="black"),
        axis.title.y=element_text(size=14, face="bold", color="black"),
        axis.text.x=element_text(size=12, color="black"),
        axis.text.y=element_text(size=12, color="black"))+
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=10))+
  annotate("text", x = 1600, y = 1300, size = 8, label = paste0("italic(R) ^ 2 ==", round(rsquare_dnn_jkt_3,2)),
           parse = TRUE)

ggplot(data=prediction_df_smg_3, aes(x=avg_cooling_load,y=ann_cooling_load))+
  geom_point(size=2.5,colour="darkred") +
  geom_smooth(method = "lm", se = FALSE, colour="blue")+
  labs(title=paste0("Predicted vs Actual Cooling Load, Semarang"), subtitle = "ANN, 21 neurons", x = "Actual Cooling Load (kW)", y = "Predicted Cooling Load (kW)")+
  theme(plot.title=element_text(size=25, face="bold", colour="black"),
        plot.subtitle=element_text(size=18, face="italic", color="black"))+
  theme(axis.title.x=element_text(size=14, face="bold", color="black"),
        axis.title.y=element_text(size=14, face="bold", color="black"),
        axis.text.x=element_text(size=12, color="black"),
        axis.text.y=element_text(size=12, color="black"))+
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=10))+
  annotate("text", x = 1800, y = 1450, size = 8, label = paste0("italic(R) ^ 2 ==", round(rsquare_ann_smg_3,2)),
           parse = TRUE) 


#Plot to compare projections of cooling load (month)
ggplot(data=proj_cooling_loads_jkt_month_3 %>% 
         dplyr::select(month_index,contains("dnn")) %>%
         pivot_longer(!c(month_index), names_to = "category", values_to = "values"), aes(x=month_index,y=values))+
  geom_line(aes(colour=category),linewidth=1)+
  scale_colour_manual(name = "Scenario", labels = c("RCP 2.6", "RCP 4.5", "RCP 8.5"), values=c("chartreuse4","deepskyblue3","red"))+
  labs(title=paste0("Climate Projection Prediction Results, Jakarta"), subtitle = "DNN, 2022-2051", x = "Month", y = "Avg. Cooling Load (kW)")+
  theme(plot.title=element_text(size=25, face="bold", colour="black"),
        plot.subtitle=element_text(size=18, face="italic", color="black"))+
  theme(axis.title.x=element_text(size=14, face="bold", color="black"),
        axis.title.y=element_text(size=14, face="bold", color="black"),
        axis.text.x=element_text(size=12, color="black"),
        axis.text.y=element_text(size=12, color="black"))+
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=10)) 

ggplot(data=proj_cooling_loads_smg_month_3 %>% 
         dplyr::select(month_index,contains("dnn")) %>%
         pivot_longer(!c(month_index), names_to = "category", values_to = "values"), aes(x=month_index,y=values))+
  geom_line(aes(colour=category),linewidth=1)+
  scale_colour_manual(name = "Scenario", labels = c("RCP 2.6", "RCP 4.5", "RCP 8.5"), values=c("chartreuse4","deepskyblue3","red"))+
  labs(title=paste0("Climate Projection Prediction Results, Semarang"), subtitle = "DNN, 2021-2050", x = "Month", y = "Avg. Cooling Load (kW)")+
  theme(plot.title=element_text(size=25, face="bold", colour="black"),
        plot.subtitle=element_text(size=18, face="italic", color="black"))+
  theme(axis.title.x=element_text(size=14, face="bold", color="black"),
        axis.title.y=element_text(size=14, face="bold", color="black"),
        axis.text.x=element_text(size=12, color="black"),
        axis.text.y=element_text(size=12, color="black"))+
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=10)) 

#Plot to compare projections of cooling load (year)
ggplot(data=cooling_loads_jkt_year %>% 
         dplyr::select(year,contains("dnn")) %>%
         pivot_longer(!c(year), names_to = "category", values_to = "values"), aes(x=year,y=values))+
  geom_line(aes(colour=category),linewidth=1)+
  scale_colour_manual(name = "Scenario", labels = c("RCP 2.6", "RCP 4.5", "RCP 8.5"), values=c("chartreuse4","deepskyblue3","red"))+
  labs(title=paste0("Hotel Cooling Load, Jakarta"), subtitle = "Hourly average by year, 2021-2051", x = "Year", y = "Avg. Cooling Load (kW)")+
  theme(plot.title=element_text(size=25, face="bold", colour="black"),
        plot.subtitle=element_text(size=18, face="italic", color="black"))+
  theme(axis.title.x=element_text(size=14, face="bold", color="black"),
        axis.title.y=element_text(size=14, face="bold", color="black"),
        axis.text.x=element_text(size=12, color="black"),
        axis.text.y=element_text(size=12, color="black"))+
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=10)) 

ggplot(data=cooling_loads_smg_year %>% 
         dplyr::select(year,contains("dnn")) %>%
         pivot_longer(!c(year), names_to = "category", values_to = "values"), aes(x=year,y=values))+
  geom_line(aes(colour=category),linewidth=1)+
  scale_colour_manual(name = "Scenario", labels = c("RCP 2.6", "RCP 4.5", "RCP 8.5"), values=c("chartreuse4","deepskyblue3","red"))+
  labs(title=paste0("Hotel Cooling Load, Semarang"), subtitle = "Hourly average by year, 2020-2050", x = "Year", y = "Avg. Cooling Load (kW)")+
  theme(plot.title=element_text(size=25, face="bold", colour="black"),
        plot.subtitle=element_text(size=18, face="italic", color="black"))+
  theme(axis.title.x=element_text(size=14, face="bold", color="black"),
        axis.title.y=element_text(size=14, face="bold", color="black"),
        axis.text.x=element_text(size=12, color="black"),
        axis.text.y=element_text(size=12, color="black"))+
  theme(legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=10)) 

