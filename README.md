# BENV0096
Dissertation on Prediction of Cooling Loads for Commercial Buildings under Different Climate Projection Scenarios in Indonesia

A master's degree dissertation aimed to predict future cooling load trends of commercial buildings in two provinces of Jakarta and Semarang, Indonesia based on future climate projection scenarios
using artificial neural network and deep neural network methods. The features used are cooling load of the buildings, along with weather variables such as temperature, solar irradiation, precipitaiton, dew point temperature, wind speed, as well as temporal and seasonal variables such as time of day, month, and its season.

As the future weather data are only avaliable in daily average granularity and the cooling load model required data with hour by day average granularity, previous 30 years weather variables with hour by day granularity was used to create a psuedo-climate pattern based on its hour by day ratio to its daily average values.

The models were evaluated using CV-RMSE, RMSE, MAPE, and R-square values.
