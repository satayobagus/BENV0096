library(tensorflow)
library(keras)
library(kerastuneR)
install_tensorflow()
install_keras()
install_kerastuner()

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

#Set random seed
set.seed(500)

#----------------------------------------------------------------------------------------------
#1. Weather data
master_data_jkt <- data.frame()
master_data_smg <- data.frame()

load_weather <- function(location){
  
  #Load weather data
  w_data <- nc_open(paste0(wd, location, "/data.nc"))
  
  #Extract dimensions
  lon_data <- ncvar_get(w_data, "longitude")
  lat_data <- ncvar_get(w_data, "latitude")
  time_data <- ncvar_get(w_data, "time")
  
  #Convert time from string to date-time
  tunits_data <- ncatt_get(w_data,"time","units")
  tustr_data <- strsplit(tunits_data$value, " ") 
  tdstr_data <- strsplit(unlist(tustr_data)[3], "-")
  tyear_data <- as.integer(unlist(tdstr_data)[1]) 
  tmonth_data <- as.integer(unlist(tdstr_data)[2])
  tday_data <- as.integer(unlist(tdstr_data)[3])
  
  #Extract variables
  u10_data <- ncvar_get(w_data,"u10")
  v10_data <- ncvar_get(w_data,"v10")
  d2m_data <- ncvar_get(w_data,"d2m")
  t2m_data <- ncvar_get(w_data,"t2m")
  ssrd_data <- ncvar_get(w_data,"ssrd")
  tp_data <- ncvar_get(w_data,"tp")
  
  #Create dataframe out of the weather data
  df_data <- as.matrix((expand.grid(lon_data, lat_data, time_data)))
  vec_u10_data <- as.vector(u10_data[,,])
  vec_v10_data <- as.vector(v10_data[,,])
  vec_d2m_data <- as.vector(d2m_data[,,])
  vec_t2m_data <- as.vector(t2m_data[,,])
  vec_ssrd_data <- as.vector(ssrd_data[,,])
  vec_tp_data <- as.vector(tp_data[,,])
  df_data <- data.frame(cbind(df_data,vec_u10_data,vec_v10_data,vec_d2m_data,vec_t2m_data,vec_ssrd_data,vec_tp_data)) %>%
    na.omit()
  
  #Rename columns
  colnames(df_data) <- c("lon", "lat", "time","u10_wind","v10_wind","dewpoint_temp","temp","ssrd","precip")
  
  #Change column formats
  #Time
  df_data$time <- as.POSIXct(chron(df_data$time/24, origin=c(tmonth_data, tday_data, tyear_data)))
  attr(df_data$time, "tzone") <- "Asia/Jakarta"
  
  #Temperatures to Celsius
  df_data$dewpoint_temp <- df_data$dewpoint_temp-273.15
  df_data$temp <- df_data$temp-273.15
  
  #SSRD to W/m2
  df_data$ssrd <- df_data$ssrd/3600
  
  #Precipitation in mm
  df_data$precip <- df_data$precip*10^3
  
  #Wind speed in km/h
  df_data$wind_speed <- sqrt(df_data$u10_wind**2 + df_data$v10_wind**2) * 3600/10^3
  
  #Drop wind speed
  df_data <- df_data %>%
    dplyr::select(-c(u10_wind, v10_wind))
  
  #Group by time column to obtain hourly average data
  master_data <- df_data %>%
    group_by(time) %>%
    summarise_all(mean) %>%
    dplyr::select(-c(lon, lat))
  
  if(location == "/Jakarta"){
    master_data_jkt <<- master_data
  }
  else{
    master_data_smg <<- master_data
  }
  
}

load_weather("/Jakarta")
load_weather("/Semarang")

#SSRD and precipitation are displayed in accumulation values
#Change it into hourly values 
master_data_smg$hour_ssrd <- ifelse(hour(master_data_smg$time)==8, master_data_smg$ssrd, master_data_smg$ssrd-lag(master_data_smg$ssrd))
master_data_smg$hour_precip <- ifelse(hour(master_data_smg$time)==8, master_data_smg$precip, master_data_smg$precip-lag(master_data_smg$precip))
master_data_smg$ssrd <- master_data_smg$hour_ssrd
master_data_smg$precip <- master_data_smg$hour_precip
master_data_smg <- master_data_smg %>%
  dplyr::select(-c(hour_ssrd,hour_precip))

master_data_jkt$hour_ssrd <- ifelse(hour(master_data_jkt$time)==8, master_data_jkt$ssrd, master_data_jkt$ssrd-lag(master_data_jkt$ssrd))
master_data_jkt$hour_precip <- ifelse(hour(master_data_jkt$time)==8, master_data_jkt$precip, master_data_jkt$precip-lag(master_data_jkt$precip))
master_data_jkt$ssrd <- master_data_jkt$hour_ssrd
master_data_jkt$precip <- master_data_jkt$hour_precip
master_data_jkt <- master_data_jkt %>%
  dplyr::select(-c(hour_ssrd,hour_precip))


#----------------------------------------------------------------------------------------------
#2. Cooling load data
cooling_data_jkt <- data.frame()
cooling_data_smg <- data.frame()

#Number of weekdays and weekends each month
Nweekdays <- Vectorize(function(a, b) 
  sum(!weekdays(seq(a, b, "days")) %in% c("Saturday", "Sunday")))

load_cooling <- function(location) {
  
  #Load cooling load data
  cl_data <- readxl::read_excel(paste0(wd,"/Hotel Jakarta.xlsx"), sheet = paste0("CL_", location)) %>%
    na.omit()
  
  #Fix column names
  names(cl_data) <- str_replace_all(names(cl_data), stringr::fixed(c(" ("= "_", " " ="_", ")" = "")))
  
  #Create date-time column
  #Order by date-time column
  cl_data <- cl_data %>% 
    mutate(time = ifelse(Hour >= 8 & Hour <=23, dmy_h(paste(1,Month,Year, Hour)),dmy_h(paste(2,Month,Year, Hour))))
  cl_data$time <- unname(as.POSIXct(cl_data$time, origin='1970-01-01',tz='Asia/Jakarta'))-7*60*60
  cl_data <- cl_data[order(cl_data$time),]
  
  
  #Summarize to obtain one value of cooling load per hour
  #Average cooling load obtained through monthly average
  
  #Calculate monthly average cooling load
  avg_data <- cl_data %>%
    dplyr::select(c(time,Weekday, Cooling_Load_kW)) %>%
    pivot_wider(names_from = Weekday, values_from = Cooling_Load_kW) %>%
    mutate(first_day = floor_date(time,'month'), last_day = ceiling_date(time,'month')) %>%
    mutate(no_weekdays = Nweekdays(first_day, last_day)) %>%
    mutate(no_weekends = days_in_month(time)-no_weekdays) %>%
    mutate(avg_cooling_load = (Weekday*no_weekdays + Weekend*no_weekends)/(no_weekdays+no_weekends)) %>%
    dplyr::select(c(time,Weekend,Weekday,avg_cooling_load))
  
  
  if(location == "Jakarta"){
    cooling_data_jkt <<- avg_data
  }
  else{
    cooling_data_smg <<- avg_data
  }
  
}

load_cooling("Jakarta")
load_cooling("Semarang")

#----------------------------------------------------------------------------------------------
#3. Composite dataframe
#Join both dataframes
#Add previous hour values for weather data
#Select 2020 for Semarang and 2021 for Jakarta
semarang_2020_2 <- master_data_smg %>%
  mutate(ph_dewpoint_temp = lag(dewpoint_temp), ph_temp = lag(temp), ph_ssrd = lag(ssrd), ph_precip = lag(precip), ph_wind_speed = lag(wind_speed)) %>%
  subset(year(time) == 2020) %>%
  left_join(cooling_data_smg, by = join_by(time)) %>%
  dplyr::select(-c(Weekday, Weekend, wind_speed, ph_wind_speed)) %>%
  mutate(dry = ifelse(month(time) >= 5 & month(time) <= 10, 1, 0)) %>%
  mutate(xhour = sin(2*pi*hour(time)/24),yhour= cos(2*pi*hour(time)/24)) %>%
  relocate(avg_cooling_load, .after = last_col())

jakarta_2021_2 <- master_data_jkt %>%
  mutate(ph_dewpoint_temp = lag(dewpoint_temp), ph_temp = lag(temp), ph_ssrd = lag(ssrd), ph_precip = lag(precip), ph_wind_speed = lag(wind_speed)) %>%
  subset(year(time) == 2021) %>%
  left_join(cooling_data_jkt, by = join_by(time))%>%
  dplyr::select(-c(Weekday, Weekend, wind_speed, ph_wind_speed)) %>%
  mutate(dry= ifelse(month(time) >= 5 & month(time) <= 10, 1, 0)) %>%
  mutate(xhour = sin(2*pi*hour(time)/24),yhour= cos(2*pi*hour(time)/24)) %>%
  relocate(avg_cooling_load, .after = last_col())

#------------------------------------------------------------------------------------------------
#4. Build ANN and DNN

#Train-test split and normalize
train_test_split_2 <- function(location){
  
  #Select required regressors data
  if (location == "Jakarta") {
    ann_feature_data <- jakarta_2021_2 %>%
      dplyr::select(!c(time)) %>%
      na.omit()
  }
  else {
    ann_feature_data <- semarang_2020_2 %>%
      dplyr::select(!c(time)) %>%
      na.omit()
  }
  
  #Split the data into training and testing set
  index <- floor(nrow(ann_feature_data)*2/3)
  train_ <- ann_feature_data[1:index,]
  test_ <- ann_feature_data[(index+1):nrow(ann_feature_data),]
  
  #Normalize the data
  maxs_train <- apply(train_, 2, max) 
  mins_train <- apply(train_, 2, min)
  scaled_train <- as.data.frame(scale(train_, center = mins_train, 
                                      scale = maxs_train - mins_train))
  scaled_test <- as.data.frame(scale(test_, center = mins_train, 
                                     scale = maxs_train - mins_train))
  
  #Split features from target variable
  train_features <- scaled_train %>% dplyr::select(-avg_cooling_load)
  test_features <- scaled_test %>% dplyr::select(-avg_cooling_load)
  
  train_labels <- scaled_train %>% dplyr::select(avg_cooling_load)
  test_labels <- scaled_test %>% dplyr::select(avg_cooling_load)
  
  #Store variables
  if (location == "Jakarta") {
    jkt_train_2 <<- train_
    jkt_train_features_2 <<- train_features
    jkt_train_labels_2 <<- train_labels
    jkt_test_features_2 <<- test_features
    jkt_test_labels_2 <<- test_labels
  }
  else {
    smg_train_2 <<- train_
    smg_train_features_2 <<- train_features
    smg_train_labels_2 <<- train_labels
    smg_test_features_2 <<- test_features
    smg_test_labels_2 <<- test_labels
  }
}

train_test_split_2("Jakarta")
train_test_split_2("Semarang")

#Initialize early stopping
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

#Calculate R-square
rsquare <- function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}


corr_plot_smg_2 <- smg_train_2 %>%
  cor() %>%
  round(2) %>%
  ggcorrplot(title = "Correlation plot, Semarang",
             ggtheme = ggplot2::theme_gray, type = "upper", lab = TRUE)+
  theme(plot.title=element_text(size=25, face="bold", colour="black"))

corr_plot_jkt_2 <- jkt_train_2 %>%
  cor() %>%
  round(2) %>%
  ggcorrplot(title = "Correlation plot, Jakarta",
             ggtheme = ggplot2::theme_gray, type = "upper", lab = TRUE)+
  theme(plot.title=element_text(size=25, face="bold", colour="black"))


#Create model
ann_model_jkt_3 <- keras$models$load_model("best_ann_model_jkt_15.h5")
dnn_model_jkt_3 <- keras$models$load_model("best_dnn_model_jkt_15.h5")

ann_model_smg_3 <- keras$models$load_model("best_ann_model_smg_15.h5")
dnn_model_smg_3 <- keras$models$load_model("best_dnn_model_smg_15.h5")

#Fit the model
history_ann_jkt_3 <- ann_model_jkt_3 %>% fit(
  as.matrix(jkt_train_features_2),
  as.matrix(jkt_train_labels_2),
  batch_size = 256,
  epochs = 250,
  verbose = 1,
  callbacks = list(early_stop),
  # Calculate validation results on 20% of the training data.
  validation_split = 0.2
)

history_dnn_jkt_3 <- dnn_model_jkt_3 %>% fit(
  as.matrix(jkt_train_features_2),
  as.matrix(jkt_train_labels_2),
  batch_size = 256,
  epochs = 250,
  verbose = 1,
  callbacks = list(early_stop),
  # Calculate validation results on 20% of the training data.
  validation_split = 0.2
)

history_ann_smg_3 <- ann_model_smg_3 %>% fit(
  as.matrix(smg_train_features_2),
  as.matrix(smg_train_labels_2),
  batch_size = 256,
  epochs = 250,
  verbose = 1,
  callbacks = list(early_stop),
  # Calculate validation results on 20% of the training data.
  validation_split = 0.2
)

history_dnn_smg_3 <- dnn_model_smg_3 %>% fit(
  as.matrix(smg_train_features_2),
  as.matrix(smg_train_labels_2),
  batch_size = 256,
  epochs = 250,
  verbose = 1,
  callbacks = list(early_stop),
  # Calculate validation results on 20% of the training data.
  validation_split = 0.2
)

plot(history_ann_jkt_3)
plot(history_dnn_jkt_3)
plot(history_ann_smg_3)
plot(history_dnn_smg_3)

#Predict and calculate mean squared error
predictions_nn_3 <- function(location) {
  
  if (location == "Jakarta") {
    jkt_ann_test_predictions_3 <<- predict(ann_model_jkt_3, as.matrix(jkt_test_features_2))
    jkt_dnn_test_predictions_3 <<- predict(dnn_model_jkt_3, as.matrix(jkt_test_features_2))
    
    prediction_df_jkt_3 <<- data.frame(hour=seq(1,nrow(jkt_test_features_2)), 
                                       ann_cooling_load = as.numeric(jkt_ann_test_predictions_3), 
                                       dnn_cooling_load = as.numeric(jkt_dnn_test_predictions_3), 
                                       avg_cooling_load = jkt_test_labels_2$avg_cooling_load) %>%
      mutate(ann_cooling_load = ann_cooling_load * (max(jkt_train_2$avg_cooling_load) - min(jkt_train_2$avg_cooling_load)) + min(jkt_train_2$avg_cooling_load),
             dnn_cooling_load = dnn_cooling_load * (max(jkt_train_2$avg_cooling_load) - min(jkt_train_2$avg_cooling_load)) + min(jkt_train_2$avg_cooling_load),
             avg_cooling_load = avg_cooling_load * (max(jkt_train_2$avg_cooling_load) - min(jkt_train_2$avg_cooling_load)) + min(jkt_train_2$avg_cooling_load))
    
    MSE_ann_jkt_3 <<- as.numeric(tf$keras$losses$mean_squared_error(prediction_df_jkt_3$avg_cooling_load,prediction_df_jkt_3$ann_cooling_load))
    MSE_dnn_jkt_3 <<- as.numeric(tf$keras$losses$mean_squared_error(prediction_df_jkt_3$avg_cooling_load,prediction_df_jkt_3$dnn_cooling_load))
    
    RMSE_ann_jkt_3 <<- sqrt(MSE_ann_jkt_3)
    RMSE_dnn_jkt_3 <<- sqrt(MSE_dnn_jkt_3)
    
    CV_RMSE_ann_jkt_3 <<- 100*RMSE_ann_jkt_3/mean(prediction_df_jkt_3$avg_cooling_load)
    CV_RMSE_dnn_jkt_3 <<- 100*RMSE_dnn_jkt_3/mean(prediction_df_jkt_3$avg_cooling_load) 
    
    MAPE_ann_jkt_3 <<- as.numeric(tf$keras$losses$mean_absolute_percentage_error(prediction_df_jkt_3$avg_cooling_load,prediction_df_jkt_3$ann_cooling_load))
    MAPE_dnn_jkt_3 <<- as.numeric(tf$keras$losses$mean_absolute_percentage_error(prediction_df_jkt_3$avg_cooling_load,prediction_df_jkt_3$dnn_cooling_load))
    
    rsquare_ann_jkt_3 <<- rsquare(prediction_df_jkt_3$avg_cooling_load,prediction_df_jkt_3$ann_cooling_load)
    rsquare_dnn_jkt_3 <<- rsquare(prediction_df_jkt_3$avg_cooling_load,prediction_df_jkt_3$dnn_cooling_load)
    
  }
  else {
    
    smg_ann_test_predictions_3 <<- predict(ann_model_smg_3, as.matrix(smg_test_features_2))
    smg_dnn_test_predictions_3 <<- predict(dnn_model_smg_3, as.matrix(smg_test_features_2))
    
    prediction_df_smg_3<<- data.frame(hour=seq(1,nrow(smg_test_features_2)), 
                                       ann_cooling_load = as.numeric(smg_ann_test_predictions_3), 
                                       dnn_cooling_load = as.numeric(smg_dnn_test_predictions_3), 
                                       avg_cooling_load = smg_test_labels_2$avg_cooling_load) %>%
      mutate(ann_cooling_load = ann_cooling_load * (max(smg_train_2$avg_cooling_load) - min(smg_train_2$avg_cooling_load)) + min(smg_train_2$avg_cooling_load),
             dnn_cooling_load = dnn_cooling_load * (max(smg_train_2$avg_cooling_load) - min(smg_train_2$avg_cooling_load)) + min(smg_train_2$avg_cooling_load),
             avg_cooling_load = avg_cooling_load * (max(smg_train_2$avg_cooling_load) - min(smg_train_2$avg_cooling_load)) + min(smg_train_2$avg_cooling_load))
    
    MSE_ann_smg_3 <<- as.numeric(tf$keras$losses$mean_squared_error(prediction_df_smg_3$avg_cooling_load,prediction_df_smg_3$ann_cooling_load))
    MSE_dnn_smg_3 <<- as.numeric(tf$keras$losses$mean_squared_error(prediction_df_smg_3$avg_cooling_load,prediction_df_smg_3$dnn_cooling_load))
    
    RMSE_ann_smg_3 <<- sqrt(MSE_ann_smg_3)
    RMSE_dnn_smg_3 <<- sqrt(MSE_dnn_smg_3)
    
    CV_RMSE_ann_smg_3 <<- 100*RMSE_ann_smg_3/mean(prediction_df_smg_3$avg_cooling_load)
    CV_RMSE_dnn_smg_3 <<- 100*RMSE_dnn_smg_3/mean(prediction_df_smg_3$avg_cooling_load) 
    
    MAPE_ann_smg_3 <<- as.numeric(tf$keras$losses$mean_absolute_percentage_error(prediction_df_smg_3$avg_cooling_load,prediction_df_smg_3$ann_cooling_load))
    MAPE_dnn_smg_3 <<- as.numeric(tf$keras$losses$mean_absolute_percentage_error(prediction_df_smg_3$avg_cooling_load,prediction_df_smg_3$dnn_cooling_load))
    
    rsquare_ann_smg_3 <<- rsquare(prediction_df_smg_3$avg_cooling_load,prediction_df_smg_3$ann_cooling_load)
    rsquare_dnn_smg_3 <<- rsquare(prediction_df_smg_3$avg_cooling_load,prediction_df_smg_3$dnn_cooling_load)
    
  }
}

predictions_nn_3("Jakarta")
predictions_nn_3("Semarang")

#-------------------------------------------------------------------------------------------------
#5. Hyperparameter tuning

#ANN model
#Jakarta
build_model_ann_jkt_3 <- function(hp) {
  
  model = keras_model_sequential()
  model %>% layer_dense(units = hp$Int('units',
                                       min_value = 1,
                                       max_value = 22,
                                       step=1),input_shape = dim(jkt_train_features_2)[2],
                        activation =  'relu') %>%
    layer_dense(units = 1, activation = 'linear') %>%
    compile(
      optimizer = tf$keras$optimizers$Adam(
        hp$Choice('learning_rate',
                  values=c(1e-2, 1e-3, 1e-4))),
      loss = 'mean_squared_error')
  return(model)
}

#Semarang
build_model_ann_smg_3 <- function(hp) {
  
  model = keras_model_sequential()
  model %>% layer_dense(units = hp$Int('units',
                                       min_value = 1,
                                       max_value = 22,
                                       step=1),input_shape = dim(smg_train_features_2)[2],
                        activation =  'relu') %>%
    layer_dense(units = 1, activation = 'linear') %>%
    compile(
      optimizer = tf$keras$optimizers$Adam(
        hp$Choice('learning_rate',
                  values=c(1e-2, 1e-3, 1e-4))),
      loss = 'mean_squared_error')
  return(model)
}

#DNN model
#Jakarta
build_model_dnn_jkt_3 <- function(hp) {
  
  model_2 = keras_model_sequential() %>% 
    layer_dense(units = hp$Int('units',
                               min_value = 1,
                               max_value = 22,
                               step=1),input_shape = dim(jkt_train_features_2)[2],
                activation ='relu')
  for (i in 1:(hp$Int('num_layers', 1, 10)) ) {
    model_2 %>% layer_dense(units = hp$Int(paste0('unit',i),
                                           min_value = 1,
                                           max_value = 22,
                                           step=1),
                            activation ='relu') %>%
      layer_dropout(rate = hp$Choice(paste0('rate',i),values=c(0,0.2,0.4)))
  } %>% 
    layer_dense(units = 1, activation = 'linear') %>%
    compile(
      optimizer = tf$keras$optimizers$Adam(hp$Choice('learning_rate',
                                                     values=c(1e-2, 1e-3, 1e-4))),
      loss = 'mean_squared_error') 
  return(model_2)
  
}

#Semarang
build_model_dnn_smg_3 <- function(hp) {
  
  model_2 = keras_model_sequential() %>% 
    layer_dense(units = hp$Int('units',
                               min_value = 1,
                               max_value = 22,
                               step=1),input_shape = dim(smg_train_features_2)[2],
                activation ='relu')
  for (i in 1:(hp$Int('num_layers', 1, 10)) ) {
    model_2 %>% layer_dense(units = hp$Int(paste0('unit',i),
                                           min_value = 1,
                                           max_value = 22,
                                           step=1),
                            activation ='relu') %>%
      layer_dropout(rate = hp$Choice(paste0('rate',i),values=c(0,0.2,0.4)))
  } %>% 
    layer_dense(units = 1, activation = 'linear') %>%
    compile(
      optimizer = tf$keras$optimizers$Adam(hp$Choice('learning_rate',
                                                     values=c(1e-2, 1e-3, 1e-4))),
      loss = 'mean_squared_error') 
  return(model_2)
  
}

#Tune
ann_tuning_3 <- function(location){
  
  
  if (location == "Jakarta"){
    
    #Create ANN tuner
    tuner_ann_jkt_3 <<- RandomSearch(
      build_model_ann_jkt_3,
      objective = 'val_loss',
      max_trials = 10,
      executions_per_trial = 3,
      directory = 'howu',
      project_name = 'ann_jktu')
    
    #Fit the ANN tuner
    tuner_ann_jkt_3 %>% fit_tuner(jkt_train_features_2,jkt_train_labels_2, batch_size = 128, 
                                  epochs = 250, validation_split = 0.2)
    
    #Create DNN tuner
    tuner_dnn_jkt_3 <<- RandomSearch(
      build_model_dnn_jkt_3,
      objective = 'val_loss',
      max_trials = 10,
      executions_per_trial = 3,
      directory = 'howu',
      project_name = 'dnn_jktu')
    
    #Fit the DNN tuner
    tuner_dnn_jkt_3 %>% fit_tuner(jkt_train_features_2,jkt_train_labels_2, batch_size = 128, 
                                  epochs = 250, validation_split = 0.2)
    
    #Store result and best model
    tuner_ann_jkt_result_3 <<- kerastuneR::plot_tuner(tuner_ann_jkt_3)
    tuner_dnn_jkt_result_3 <<- kerastuneR::plot_tuner(tuner_dnn_jkt_3)
    
    best_ann_model_jkt_3 <<- tuner_ann_jkt_3 %>% get_best_models(1)
    best_dnn_model_jkt_3 <<- tuner_dnn_jkt_3 %>% get_best_models(1)
    
    save_model_hdf5(best_ann_model_jkt_3[[1]], 'best_ann_model_jkt_15.h5')
    save_model_hdf5(best_dnn_model_jkt_3[[1]], 'best_dnn_model_jkt_15.h5')
  }
  
  else {
    
    #Create ANN tuner
    tuner_ann_smg_3 <<- RandomSearch(
      build_model_ann_smg_3,
      objective = 'val_loss',
      max_trials = 10,
      executions_per_trial = 3,
      directory = 'howu',
      project_name = 'ann_smgu')
    
    #Fit the ANN tuner
    tuner_ann_smg_3 %>% fit_tuner(smg_train_features_2,smg_train_labels_2, batch_size = 128,
                                  epochs = 250, validation_split = 0.2)
    
    #Create DNN tuner
    tuner_dnn_smg_3 <<- RandomSearch(
      build_model_dnn_smg_3,
      objective = 'val_loss',
      max_trials = 10,
      executions_per_trial = 3,
      directory = 'howu',
      project_name = 'dnn_smgu')
    
    #Fit the DNN tuner
    tuner_dnn_smg_3 %>% fit_tuner(smg_train_features_2,smg_train_labels_2, batch_size = 128, 
                                  epochs = 250, validation_split = 0.2)
    
    #Store result and best model
    tuner_ann_smg_result_3 <<- kerastuneR::plot_tuner(tuner_ann_smg_3)
    tuner_dnn_smg_result_3 <<- kerastuneR::plot_tuner(tuner_dnn_smg_3)
    
    best_ann_model_smg_3 <<- tuner_ann_smg_3 %>% get_best_models(1)
    best_dnn_model_smg_3 <<- tuner_dnn_smg_3 %>% get_best_models(1)
    
    save_model_hdf5(best_ann_model_smg_3[[1]], 'best_ann_model_smg_15.h5')
    save_model_hdf5(best_dnn_model_smg_3[[1]], 'best_dnn_model_smg_15.h5')
  }
  
}

ann_tuning_3("Jakarta")
ann_tuning_3("Semarang")

#------------------------------------------------------------------------------------------------
#6. Predict using climate projection data


clean_arrange <- function(location){
  
  #Select required regressors data
  if (location == "Jakarta") {
    
    #Summarize by monthly hour average and add features
    #2.6
    jkt_feature_data_26 <- read.csv(paste0(wd,"/","RCP 2.6/mean_disagg_jkt_26.csv")) %>%
      rename_all(~str_replace(.,"proj_","")) %>%
      dplyr::select(-c(wind_speed)) %>%
      mutate(time=as.POSIXct(as_datetime(time)))
    attr(jkt_feature_data_26$time, "tzone") <- "Asia/Jakarta"
    
    jkt_feature_data_26 <- jkt_feature_data_26  %>%
      mutate(time=time-7*60*60) %>%
      mutate(group_index=paste0(year(time),"-",month(time),"-",hour(time)),year=year(time),month=month(time),hour=hour(time))
    
    jkt_feature_data_26 <- jkt_feature_data_26 %>%
      group_by(group_index) %>%
      summarise_all(mean) 
    
    jkt_feature_data_26 <- jkt_feature_data_26 %>%
      mutate(time=as_datetime(ifelse(hour>=8, ymd_h(paste0(year,"-",month,"-",1," ",hour),tz="Asia/Jakarta"),ymd_h(paste0(year,"-",month,"-",2," ",hour),tz="Asia/Jakarta"))))
    attr(jkt_feature_data_26$time, "tzone") <- "Asia/Jakarta"  
    
    jkt_feature_data_26 <- jkt_feature_data_26[order(jkt_feature_data_26$time),] %>%
      mutate(ph_dewpoint_temp = lag(dewpoint_temp), ph_temp = lag(temp), ph_ssrd = lag(ssrd), ph_precip = lag(precip), dry = ifelse(month(time) >= 5 & month(time) <= 10, 1, 0), xhour = sin(2*pi*hour(time)/24),yhour= cos(2*pi*hour(time)/24))
    
    jkt_feature_data_26[1,"ph_dewpoint_temp"] <- jkt_feature_data_26[24,"dewpoint_temp"]
    jkt_feature_data_26[1,"ph_temp"] <- jkt_feature_data_26[24,"temp"]
    jkt_feature_data_26[1,"ph_ssrd"] <- jkt_feature_data_26[24,"ssrd"]
    jkt_feature_data_26[1,"ph_precip"] <- jkt_feature_data_26[24,"precip"]
    
    jkt_feature_data_26 <- jkt_feature_data_26 %>%
      na.omit()
    
    #4.5
    jkt_feature_data_45 <- read.csv(paste0(wd,"/","RCP 4.5/mean_disagg_jkt_45.csv")) %>%
      rename_all(~str_replace(.,"proj_","")) %>%
      dplyr::select(-c(wind_speed)) %>%
      mutate(time=as.POSIXct(as_datetime(time)))
    attr(jkt_feature_data_45$time, "tzone") <- "Asia/Jakarta"
    
    jkt_feature_data_45 <- jkt_feature_data_45  %>%
      mutate(time=time-7*60*60) %>%
      mutate(group_index=paste0(year(time),"-",month(time),"-",hour(time)),year=year(time),month=month(time),hour=hour(time))
    
    jkt_feature_data_45 <- jkt_feature_data_45 %>%
      group_by(group_index) %>%
      summarise_all(mean) 
    
    jkt_feature_data_45 <- jkt_feature_data_45 %>%
      mutate(time=as_datetime(ifelse(hour>=8, ymd_h(paste0(year,"-",month,"-",1," ",hour),tz="Asia/Jakarta"),ymd_h(paste0(year,"-",month,"-",2," ",hour),tz="Asia/Jakarta"))))
    attr(jkt_feature_data_45$time, "tzone") <- "Asia/Jakarta"  
    
    jkt_feature_data_45 <- jkt_feature_data_45[order(jkt_feature_data_45$time),] %>%
      mutate(ph_dewpoint_temp = lag(dewpoint_temp), ph_temp = lag(temp), ph_ssrd = lag(ssrd), ph_precip = lag(precip), dry = ifelse(month(time) >= 5 & month(time) <= 10, 1, 0), xhour = sin(2*pi*hour(time)/24),yhour= cos(2*pi*hour(time)/24))
    
    jkt_feature_data_45[1,"ph_dewpoint_temp"] <- jkt_feature_data_45[24,"dewpoint_temp"]
    jkt_feature_data_45[1,"ph_temp"] <- jkt_feature_data_45[24,"temp"]
    jkt_feature_data_45[1,"ph_ssrd"] <- jkt_feature_data_45[24,"ssrd"]
    jkt_feature_data_45[1,"ph_precip"] <- jkt_feature_data_45[24,"precip"]
    
    jkt_feature_data_45 <- jkt_feature_data_45 %>%
      na.omit()    
    
    #8.5
    jkt_feature_data_85 <- read.csv(paste0(wd,"/","RCP 8.5/mean_disagg_jkt_85.csv")) %>%
      rename_all(~str_replace(.,"proj_","")) %>%
      dplyr::select(-c(wind_speed)) %>%
      mutate(time=as.POSIXct(as_datetime(time)))
    attr(jkt_feature_data_85$time, "tzone") <- "Asia/Jakarta"
    
    jkt_feature_data_85 <- jkt_feature_data_85  %>%
      mutate(time=time-7*60*60) %>%
      mutate(group_index=paste0(year(time),"-",month(time),"-",hour(time)),year=year(time),month=month(time),hour=hour(time))
    
    jkt_feature_data_85 <- jkt_feature_data_85 %>%
      group_by(group_index) %>%
      summarise_all(mean) 
    
    jkt_feature_data_85 <- jkt_feature_data_85 %>%
      mutate(time=as_datetime(ifelse(hour>=8, ymd_h(paste0(year,"-",month,"-",1," ",hour),tz="Asia/Jakarta"),ymd_h(paste0(year,"-",month,"-",2," ",hour),tz="Asia/Jakarta"))))
    attr(jkt_feature_data_85$time, "tzone") <- "Asia/Jakarta"  
    
    jkt_feature_data_85 <- jkt_feature_data_85[order(jkt_feature_data_85$time),] %>%
      mutate(ph_dewpoint_temp = lag(dewpoint_temp), ph_temp = lag(temp), ph_ssrd = lag(ssrd), ph_precip = lag(precip), dry = ifelse(month(time) >= 5 & month(time) <= 10, 1, 0), xhour = sin(2*pi*hour(time)/24),yhour= cos(2*pi*hour(time)/24))
    
    jkt_feature_data_85[1,"ph_dewpoint_temp"] <- jkt_feature_data_85[24,"dewpoint_temp"]
    jkt_feature_data_85[1,"ph_temp"] <- jkt_feature_data_85[24,"temp"]
    jkt_feature_data_85[1,"ph_ssrd"] <- jkt_feature_data_85[24,"ssrd"]
    jkt_feature_data_85[1,"ph_precip"] <- jkt_feature_data_85[24,"precip"]
    
    jkt_feature_data_85 <- jkt_feature_data_85 %>%
      na.omit() 
    
    #Save time only
    proj_time <<- jkt_feature_data_26[order(jkt_feature_data_26$time),] %>%
      ungroup() %>%
      dplyr::select(time)
    
    #Reorder rows
    jkt_feature_data_26 <<- jkt_feature_data_26[order(jkt_feature_data_26$time),] %>%
      ungroup() %>%
      dplyr::select(!c(group_index,time, year, month, hour))
    
    jkt_feature_data_45 <<- jkt_feature_data_45[order(jkt_feature_data_45$time),] %>%
      ungroup() %>%
      dplyr::select(!c(group_index,time, year, month, hour))
    
    jkt_feature_data_85 <<- jkt_feature_data_85[order(jkt_feature_data_85$time),] %>%
      ungroup() %>%
      dplyr::select(!c(group_index,time, year, month, hour))
    
  }
  
  else {
    
    #Summarize by monthly hour average and add features
    #2.6
    smg_feature_data_26 <- read.csv(paste0(wd,"/","RCP 2.6/mean_disagg_smg_26.csv")) %>%
      rename_all(~str_replace(.,"proj_","")) %>%
      dplyr::select(-c(wind_speed)) %>%
      mutate(time=as.POSIXct(as_datetime(time)))
    attr(smg_feature_data_26$time, "tzone") <- "Asia/Jakarta"
    
    smg_feature_data_26 <- smg_feature_data_26  %>%
      mutate(time=time-7*60*60) %>%
      mutate(group_index=paste0(year(time),"-",month(time),"-",hour(time)),year=year(time),month=month(time),hour=hour(time))
    
    smg_feature_data_26 <- smg_feature_data_26 %>%
      group_by(group_index) %>%
      summarise_all(mean) 
    
    smg_feature_data_26 <- smg_feature_data_26 %>%
      mutate(time=as_datetime(ifelse(hour>=8, ymd_h(paste0(year,"-",month,"-",1," ",hour),tz="Asia/Jakarta"),ymd_h(paste0(year,"-",month,"-",2," ",hour),tz="Asia/Jakarta"))))
    attr(smg_feature_data_26$time, "tzone") <- "Asia/Jakarta"  
    
    smg_feature_data_26 <- smg_feature_data_26[order(smg_feature_data_26$time),] %>%
      mutate(ph_dewpoint_temp = lag(dewpoint_temp), ph_temp = lag(temp), ph_ssrd = lag(ssrd), ph_precip = lag(precip), dry = ifelse(month(time) >= 5 & month(time) <= 10, 1, 0), xhour = sin(2*pi*hour(time)/24),yhour= cos(2*pi*hour(time)/24))
    
    smg_feature_data_26[1,"ph_dewpoint_temp"] <- smg_feature_data_26[24,"dewpoint_temp"]
    smg_feature_data_26[1,"ph_temp"] <- smg_feature_data_26[24,"temp"]
    smg_feature_data_26[1,"ph_ssrd"] <- smg_feature_data_26[24,"ssrd"]
    smg_feature_data_26[1,"ph_precip"] <- smg_feature_data_26[24,"precip"]
    
    smg_feature_data_26 <- smg_feature_data_26 %>%
      na.omit()
    
    #4.5
    smg_feature_data_45 <- read.csv(paste0(wd,"/","RCP 4.5/mean_disagg_smg_45.csv")) %>%
      rename_all(~str_replace(.,"proj_","")) %>%
      dplyr::select(-c(wind_speed)) %>%
      mutate(time=as.POSIXct(as_datetime(time)))
    attr(smg_feature_data_45$time, "tzone") <- "Asia/Jakarta"
    
    smg_feature_data_45 <- smg_feature_data_45  %>%
      mutate(time=time-7*60*60) %>%
      mutate(group_index=paste0(year(time),"-",month(time),"-",hour(time)),year=year(time),month=month(time),hour=hour(time))
    
    smg_feature_data_45 <- smg_feature_data_45 %>%
      group_by(group_index) %>%
      summarise_all(mean) 
    
    smg_feature_data_45 <- smg_feature_data_45 %>%
      mutate(time=as_datetime(ifelse(hour>=8, ymd_h(paste0(year,"-",month,"-",1," ",hour),tz="Asia/Jakarta"),ymd_h(paste0(year,"-",month,"-",2," ",hour),tz="Asia/Jakarta"))))
    attr(smg_feature_data_45$time, "tzone") <- "Asia/Jakarta"  
    
    smg_feature_data_45 <- smg_feature_data_45[order(smg_feature_data_45$time),] %>%
      mutate(ph_dewpoint_temp = lag(dewpoint_temp), ph_temp = lag(temp), ph_ssrd = lag(ssrd), ph_precip = lag(precip), dry = ifelse(month(time) >= 5 & month(time) <= 10, 1, 0), xhour = sin(2*pi*hour(time)/24),yhour= cos(2*pi*hour(time)/24))
    
    smg_feature_data_45[1,"ph_dewpoint_temp"] <- smg_feature_data_45[24,"dewpoint_temp"]
    smg_feature_data_45[1,"ph_temp"] <- smg_feature_data_45[24,"temp"]
    smg_feature_data_45[1,"ph_ssrd"] <- smg_feature_data_45[24,"ssrd"]
    smg_feature_data_45[1,"ph_precip"] <- smg_feature_data_45[24,"precip"]
    
    smg_feature_data_45 <- smg_feature_data_45 %>%
      na.omit()    
    
    #8.5
    smg_feature_data_85 <- read.csv(paste0(wd,"/","RCP 8.5/mean_disagg_smg_85.csv")) %>%
      rename_all(~str_replace(.,"proj_","")) %>%
      dplyr::select(-c(wind_speed)) %>%
      mutate(time=as.POSIXct(as_datetime(time)))
    attr(smg_feature_data_85$time, "tzone") <- "Asia/Jakarta"
    
    smg_feature_data_85 <- smg_feature_data_85  %>%
      mutate(time=time-7*60*60) %>%
      mutate(group_index=paste0(year(time),"-",month(time),"-",hour(time)),year=year(time),month=month(time),hour=hour(time))
    
    smg_feature_data_85 <- smg_feature_data_85 %>%
      group_by(group_index) %>%
      summarise_all(mean) 
    
    smg_feature_data_85 <- smg_feature_data_85 %>%
      mutate(time=as_datetime(ifelse(hour>=8, ymd_h(paste0(year,"-",month,"-",1," ",hour),tz="Asia/Jakarta"),ymd_h(paste0(year,"-",month,"-",2," ",hour),tz="Asia/Jakarta"))))
    attr(smg_feature_data_85$time, "tzone") <- "Asia/Jakarta"  
    
    smg_feature_data_85 <- smg_feature_data_85[order(smg_feature_data_85$time),] %>%
      mutate(ph_dewpoint_temp = lag(dewpoint_temp), ph_temp = lag(temp), ph_ssrd = lag(ssrd), ph_precip = lag(precip), dry = ifelse(month(time) >= 5 & month(time) <= 10, 1, 0), xhour = sin(2*pi*hour(time)/24),yhour= cos(2*pi*hour(time)/24))
    
    smg_feature_data_85[1,"ph_dewpoint_temp"] <- smg_feature_data_85[24,"dewpoint_temp"]
    smg_feature_data_85[1,"ph_temp"] <- smg_feature_data_85[24,"temp"]
    smg_feature_data_85[1,"ph_ssrd"] <- smg_feature_data_85[24,"ssrd"]
    smg_feature_data_85[1,"ph_precip"] <- smg_feature_data_85[24,"precip"]
    
    smg_feature_data_85 <- smg_feature_data_85 %>%
      na.omit() 
    
    #Save time only
    proj_time <<- smg_feature_data_26[order(smg_feature_data_26$time),] %>%
      ungroup() %>%
      dplyr::select(time)
    
    #Reorder rows
    smg_feature_data_26 <<- smg_feature_data_26[order(smg_feature_data_26$time),] %>%
      ungroup() %>%
      dplyr::select(!c(group_index,time, year, month, hour))
    
    smg_feature_data_45 <<- smg_feature_data_45[order(smg_feature_data_45$time),] %>%
      ungroup() %>%
      dplyr::select(!c(group_index,time, year, month, hour))
    
    smg_feature_data_85 <<- smg_feature_data_85[order(smg_feature_data_85$time),] %>%
      ungroup() %>%
      dplyr::select(!c(group_index,time, year, month, hour))
    
  }
  
  
  
  
}
clean_arrange("Jakarta")
clean_arrange("Semarang")


#Train features for scales
jkt_train_features <- jkt_train_2 %>% dplyr::select(-c(avg_cooling_load))
smg_train_features <- smg_train_2 %>% dplyr::select(-c(avg_cooling_load))


norm_pred_3 <- function(location){
  
  #Select required regressors data
  if (location == "Jakarta") {
    
    #Normalize the data
    maxs_train <- apply(jkt_train_features, 2, max) 
    mins_train <- apply(jkt_train_features, 2, min)
    scaled_feature_26 <- as.data.frame(scale(jkt_feature_data_26, center = mins_train, 
                                             scale = maxs_train - mins_train))
    
    scaled_feature_45 <- as.data.frame(scale(jkt_feature_data_45, center = mins_train, 
                                             scale = maxs_train - mins_train))
    
    scaled_feature_85 <- as.data.frame(scale(jkt_feature_data_85, center = mins_train, 
                                             scale = maxs_train - mins_train))
    
    #Predict
    jkt_ann_test_predictions_26_3 <<- predict(ann_model_jkt_3, as.matrix(scaled_feature_26))
    jkt_dnn_test_predictions_26_3 <<- predict(dnn_model_jkt_3, as.matrix(scaled_feature_26))
    
    jkt_ann_test_predictions_45_3 <<- predict(ann_model_jkt_3, as.matrix(scaled_feature_45))
    jkt_dnn_test_predictions_45_3 <<- predict(dnn_model_jkt_3, as.matrix(scaled_feature_45))
    
    jkt_ann_test_predictions_85_3 <<- predict(ann_model_jkt_3, as.matrix(scaled_feature_85))
    jkt_dnn_test_predictions_85_3 <<- predict(dnn_model_jkt_3, as.matrix(scaled_feature_85))
    
    prediction_df_jkt_26_3 <<- data.frame(hour=seq(1,nrow(scaled_feature_26)), 
                                        ann_cooling_load = as.numeric(jkt_ann_test_predictions_26_3), 
                                        dnn_cooling_load = as.numeric(jkt_dnn_test_predictions_26_3)) %>%
      mutate(ann_cooling_load = ann_cooling_load * (max(jkt_train_2$avg_cooling_load) - min(jkt_train_2$avg_cooling_load)) + min(jkt_train_2$avg_cooling_load),
             dnn_cooling_load = dnn_cooling_load * (max(jkt_train_2$avg_cooling_load) - min(jkt_train_2$avg_cooling_load)) + min(jkt_train_2$avg_cooling_load))
    
    prediction_df_jkt_45_3 <<- data.frame(hour=seq(1,nrow(scaled_feature_45)), 
                                        ann_cooling_load = as.numeric(jkt_ann_test_predictions_45_3), 
                                        dnn_cooling_load = as.numeric(jkt_dnn_test_predictions_45_3)) %>%
      mutate(ann_cooling_load = ann_cooling_load * (max(jkt_train_2$avg_cooling_load) - min(jkt_train_2$avg_cooling_load)) + min(jkt_train_2$avg_cooling_load),
             dnn_cooling_load = dnn_cooling_load * (max(jkt_train_2$avg_cooling_load) - min(jkt_train_2$avg_cooling_load)) + min(jkt_train_2$avg_cooling_load))
    
    prediction_df_jkt_85_3 <<- data.frame(hour=seq(1,nrow(scaled_feature_85)), 
                                        ann_cooling_load = as.numeric(jkt_ann_test_predictions_85_3), 
                                        dnn_cooling_load = as.numeric(jkt_dnn_test_predictions_85_3)) %>%
      mutate(ann_cooling_load = ann_cooling_load * (max(jkt_train_2$avg_cooling_load) - min(jkt_train_2$avg_cooling_load)) + min(jkt_train_2$avg_cooling_load),
             dnn_cooling_load = dnn_cooling_load * (max(jkt_train_2$avg_cooling_load) - min(jkt_train_2$avg_cooling_load)) + min(jkt_train_2$avg_cooling_load))
    
    
  }
  
  else {
    
    #Normalize the data
    maxs_train <- apply(smg_train_features, 2, max) 
    mins_train <- apply(smg_train_features, 2, min)
    scaled_feature_26 <- as.data.frame(scale(smg_feature_data_26, center = mins_train, 
                                             scale = maxs_train - mins_train))
    
    scaled_feature_45 <- as.data.frame(scale(smg_feature_data_45, center = mins_train, 
                                             scale = maxs_train - mins_train))
    
    scaled_feature_85 <- as.data.frame(scale(smg_feature_data_85, center = mins_train, 
                                             scale = maxs_train - mins_train))
    
    #Predict
    smg_ann_test_predictions_26_3 <<- predict(ann_model_smg_3, as.matrix(scaled_feature_26))
    smg_dnn_test_predictions_26_3 <<- predict(dnn_model_smg_3, as.matrix(scaled_feature_26))
    
    smg_ann_test_predictions_45_3 <<- predict(ann_model_smg_3, as.matrix(scaled_feature_45))
    smg_dnn_test_predictions_45_3 <<- predict(dnn_model_smg_3, as.matrix(scaled_feature_45))
    
    smg_ann_test_predictions_85_3 <<- predict(ann_model_smg_3, as.matrix(scaled_feature_85))
    smg_dnn_test_predictions_85_3 <<- predict(dnn_model_smg_3, as.matrix(scaled_feature_85))
    
    prediction_df_smg_26_3 <<- data.frame(hour=seq(1,nrow(scaled_feature_26)), 
                                        ann_cooling_load = as.numeric(smg_ann_test_predictions_26_3), 
                                        dnn_cooling_load = as.numeric(smg_dnn_test_predictions_26_3)) %>%
      mutate(ann_cooling_load = ann_cooling_load * (max(smg_train_2$avg_cooling_load) - min(smg_train_2$avg_cooling_load)) + min(smg_train_2$avg_cooling_load),
             dnn_cooling_load = dnn_cooling_load * (max(smg_train_2$avg_cooling_load) - min(smg_train_2$avg_cooling_load)) + min(smg_train_2$avg_cooling_load))
    
    prediction_df_smg_45_3 <<- data.frame(hour=seq(1,nrow(scaled_feature_45)), 
                                        ann_cooling_load = as.numeric(smg_ann_test_predictions_45_3), 
                                        dnn_cooling_load = as.numeric(smg_dnn_test_predictions_45_3)) %>%
      mutate(ann_cooling_load = ann_cooling_load * (max(smg_train_2$avg_cooling_load) - min(smg_train_2$avg_cooling_load)) + min(smg_train_2$avg_cooling_load),
             dnn_cooling_load = dnn_cooling_load * (max(smg_train_2$avg_cooling_load) - min(smg_train_2$avg_cooling_load)) + min(smg_train_2$avg_cooling_load))
    
    prediction_df_smg_85_3 <<- data.frame(hour=seq(1,nrow(scaled_feature_85)), 
                                        ann_cooling_load = as.numeric(smg_ann_test_predictions_85_3), 
                                        dnn_cooling_load = as.numeric(smg_dnn_test_predictions_85_3)) %>%
      mutate(ann_cooling_load = ann_cooling_load * (max(smg_train_2$avg_cooling_load) - min(smg_train_2$avg_cooling_load)) + min(smg_train_2$avg_cooling_load),
             dnn_cooling_load = dnn_cooling_load * (max(smg_train_2$avg_cooling_load) - min(smg_train_2$avg_cooling_load)) + min(smg_train_2$avg_cooling_load))  
  }
  
  
  
  
}

norm_pred_3("Jakarta")
norm_pred_3("Semarang")

#Combine all cooling load projection data
proj_cooling_loads_jkt_3 <- proj_time %>%
  mutate(hour=seq(1:nrow(proj_time))) %>%
  cbind(ann_cooling_load_26 = prediction_df_jkt_26_3$ann_cooling_load,dnn_cooling_load_26 = prediction_df_jkt_26_3$dnn_cooling_load) %>%
  cbind(ann_cooling_load_45 = prediction_df_jkt_45_3$ann_cooling_load,dnn_cooling_load_45 = prediction_df_jkt_45_3$dnn_cooling_load) %>%
  cbind(ann_cooling_load_85 = prediction_df_jkt_85_3$ann_cooling_load,dnn_cooling_load_85 = prediction_df_jkt_85_3$dnn_cooling_load)

proj_cooling_loads_smg_3 <- proj_time %>%
  mutate(hour=seq(1:nrow(proj_time))) %>%
  cbind(ann_cooling_load_26 = prediction_df_smg_26_3$ann_cooling_load,dnn_cooling_load_26 = prediction_df_smg_26_3$dnn_cooling_load) %>%
  cbind(ann_cooling_load_45 = prediction_df_smg_45_3$ann_cooling_load,dnn_cooling_load_45 = prediction_df_smg_45_3$dnn_cooling_load) %>%
  cbind(ann_cooling_load_85 = prediction_df_smg_85_3$ann_cooling_load,dnn_cooling_load_85 = prediction_df_smg_85_3$dnn_cooling_load)


#Calculate average monthly cooling load
proj_cooling_loads_jkt_month_3 <- proj_cooling_loads_jkt_3 %>%
  mutate(year=year(time),month=month(time)) %>%
  dplyr::select(-c(time,hour)) %>%
  group_by(year,month) %>%
  summarise_all(mean) %>%
  ungroup()

proj_cooling_loads_jkt_month_3 <- proj_cooling_loads_jkt_month_3 %>%
  mutate(month_index=seq(1:nrow(proj_cooling_loads_jkt_month_3)))

cooling_loads_jkt_2021 <- jakarta_2021_2 %>%
  mutate(year=year(time), month_days=days_in_month(time)) %>%
  mutate(dnn_cooling_load_26=avg_cooling_load*month_days, dnn_cooling_load_45=avg_cooling_load*month_days, dnn_cooling_load_85=avg_cooling_load*month_days) %>%
  dplyr::select(c('year', contains('dnn'))) %>%
  group_by(year) %>%
  summarise_all(sum) %>%
  ungroup() %>%
  mutate(dnn_cooling_load_26=dnn_cooling_load_26/(24*yday(as.Date(paste(year,"12","31",sep="-"), '%Y-%m-%d'))),
         dnn_cooling_load_45=dnn_cooling_load_45/(24*yday(as.Date(paste(year,"12","31",sep="-"), '%Y-%m-%d'))),
         dnn_cooling_load_85=dnn_cooling_load_85/(24*yday(as.Date(paste(year,"12","31",sep="-"), '%Y-%m-%d'))))

cooling_loads_jkt_year <- proj_cooling_loads_jkt_month_3 %>%
  mutate(year=year+1, month_days=days_in_month(as.Date(paste(year,month,"1",sep="-"), '%Y-%m-%d'))) %>%
  dplyr::select(-c(contains('ann'))) %>%
  mutate(dnn_cooling_load_26=dnn_cooling_load_26*24*month_days/(24*yday(as.Date(paste(year,"12","31",sep="-"), '%Y-%m-%d'))),
         dnn_cooling_load_45=dnn_cooling_load_45*24*month_days/(24*yday(as.Date(paste(year,"12","31",sep="-"), '%Y-%m-%d'))),
         dnn_cooling_load_85=dnn_cooling_load_85*24*month_days/(24*yday(as.Date(paste(year,"12","31",sep="-"), '%Y-%m-%d')))) %>%
  group_by(year) %>%
  summarise_all(sum) %>%
  ungroup() %>%
  dplyr::select(-c(contains('month'))) %>%
  rbind(cooling_loads_jkt_2021)



proj_cooling_loads_smg_month_3 <- proj_cooling_loads_smg_3 %>%
  mutate(year=year(time),month=month(time)) %>%
  dplyr::select(-c(time,hour)) %>%
  group_by(year,month) %>%
  summarise_all(mean) %>%
  ungroup()

proj_cooling_loads_smg_month_3 <- proj_cooling_loads_smg_month_3 %>%
  mutate(month_index=seq(1:nrow(proj_cooling_loads_smg_month_3))) 

cooling_loads_smg_2020 <- semarang_2020_2 %>%
  mutate(year=year(time), month_days=days_in_month(time)) %>%
  mutate(dnn_cooling_load_26=avg_cooling_load*month_days, dnn_cooling_load_45=avg_cooling_load*month_days, dnn_cooling_load_85=avg_cooling_load*month_days) %>%
  dplyr::select(c('year', contains('dnn'))) %>%
  group_by(year) %>%
  summarise_all(sum) %>%
  ungroup() %>%
  mutate(dnn_cooling_load_26=dnn_cooling_load_26/(24*yday(as.Date(paste(year,"12","31",sep="-"), '%Y-%m-%d'))),
         dnn_cooling_load_45=dnn_cooling_load_45/(24*yday(as.Date(paste(year,"12","31",sep="-"), '%Y-%m-%d'))),
         dnn_cooling_load_85=dnn_cooling_load_85/(24*yday(as.Date(paste(year,"12","31",sep="-"), '%Y-%m-%d'))))

cooling_loads_smg_year <- proj_cooling_loads_smg_month_3 %>%
  mutate(month_days=days_in_month(as.Date(paste(year,month,"1",sep="-"), '%Y-%m-%d'))) %>%
  dplyr::select(-c(contains('ann'))) %>%
  mutate(dnn_cooling_load_26=dnn_cooling_load_26*24*month_days/(24*yday(as.Date(paste(year,"12","31",sep="-"), '%Y-%m-%d'))),
         dnn_cooling_load_45=dnn_cooling_load_45*24*month_days/(24*yday(as.Date(paste(year,"12","31",sep="-"), '%Y-%m-%d'))),
         dnn_cooling_load_85=dnn_cooling_load_85*24*month_days/(24*yday(as.Date(paste(year,"12","31",sep="-"), '%Y-%m-%d')))) %>%
  group_by(year) %>%
  summarise_all(sum) %>%
  ungroup() %>%
  dplyr::select(-c(contains('month'))) %>%
  rbind(cooling_loads_smg_2020)

#------------------------------------------------------------------------------------------------
