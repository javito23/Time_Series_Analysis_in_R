# Time_Series_Analysis_in_R

This project is a simle time series analysis with R. I'll be using the libraries 'fpp2', 'tseries', 'forecast', 'seasonal', 'descomponer' and 'TSA'

The project it's divided into two parts:
- In the first one we will decompose our time series, look for the presence of a seasonality component, study it if it exists and, finally, look 
for the exponential smoothing technique which best replicates the behaviour of our time series.
- Second, we will test if our time serie is stationary and, in case it isn't, try to transform it into a stationary serie. Then, we will look for the ARIMA model
which best captures the behaviour of our data and evaluate its capability to predict the evolution of our time series. For this, we will consider two approaches,
1) choosing the model manually 2) and letting an automatic algorithm figure out which model is the best for our case. Finally, we will compare their performance
and choose the best approach.
