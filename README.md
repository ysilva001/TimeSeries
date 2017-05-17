# TimeSeries
Built a model to to predict the future values of time series,
based upon the census Median and Average Sale Price of Houses Sold in the US dataset.

1. Plotted Data
2. Transformed to Stationary Data
  a. Removed trends and periodic effects
  b. Elimated tren when there is no seasnality 
3. Linear Filtering
  a. Created a second time series by smoothing the ireregular parts of the time series.
  b. Moving average is used for smoothing the series of mathematic means over time (weight equally). 
  c. Exponential smoothing is weighted asymmetric moving average, in which weights decline exponentially where most recent data is weighted more.
4. Used the exponential smoothing for forecasting, by using 𝑌𝑡 to predict 𝑌𝑡+1.

