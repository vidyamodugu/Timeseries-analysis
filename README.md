# Timeseries-analysis
This  analysis discovers and delivers new insights about factors affecting revenue generating potential of a financial representative.
An ARIMA model was developed to predict revenue for all active reps with more than 3 years of tenure with the firm.
The model reports forecasts along with important predictors driving revenue.
Deviation between actual and forecasted revenue is small.
The overall error rate (----) of the model, represented by Mean Absolute Error (MAE), is low.

DATA PREPROCESSING
PREPROCESSING: Creation of response variable "Attrition" and predicting its missing values.
Ordering data year wise 
Removing low-variance predictors: This removes attributes with a near zero variance i.e. the variables that had close to the same value of variance. 
Removing highly correlated variables :A correlation matrix was created from the given attributes and highly correlated attributes were identified and removed. Centering and Scaling: This made the data uniform and unit-independent which helped in further processing. 
Yeo-Johnson Transformation : This method shifted the distribution of the attribute to reduce the skewness and made them more normally distributed.

SOLUTIONING 
Discover and deliver new insights about factors affecting revenue generating potential of a financial representative.
Model factors driving revenue  at individual representative level


The data used for analysis is a time series data.
We used Auto Regressive Integrated Moving Average (ARIMA)  to model the data. 
Since the data was non-stationary, an initial differencing step was applied to make the data stationary for application of the model 
The model had a two-fold effect : 
It helped better understand of the data 
Predict 2107 revenue forecast in the series. 

 
 
Splittling Dataset
The data was split into two timelines. 
The ARIMA model was run on the “First timeline” to predict 2016 Revenues. 
The predicted results obtained for 2016 was compared to the actual 2016 observations from the “Second timeline” and the efficiency of the model prediction was confirmed. 
2017 Revenues were predicted by running the ARIMA model on the whole dataset (2010 to 2016 observations) 
