# Energy Consumption

## general description
Here is the analysis of energy consumption in a household in France in order to know the patterns of the use of electricity.
Knowing the pattern and the visualization of energy consumption are the keys to reduce their use of energy. Saving energy has some advantages such that the tenants can save utility cost and it is also environment friendly. Because of these reasons, Here time series analysis of energy consumption was conducted.

## Data description of energy consumption
47 months energy consumption data was given. 
The data was observed energy 1 minute. 
The data includes
- date
- time
- Global active power (in kilowatt-minute-averaged)
- Global reactive power (in kilowatt-minute-averaged)
- Voltage (in volt-minute-averaged)
- Global intensity (in ampere-minute-averaged)
- Submeter 1 (Corresponding to Kitchen in watt-hour)
- Submeter 2 (Corresponding to Laundry in watt-hour)
- Submeter 3 (Corresponding to Water heater and Air conditioner in watt-hour)


## Data description of temperature 
After some analysis, some relationship between energy consumption and the temperature was found. In order to know the exact relation between the energy consumption and the temperature, the temperature information was also added into the analysis. 47 months temperature data was given for every 3 hours. 
The data includes
- date
- time
- temperature


## Methods of analysis
Time series analysis was conducted. 
To make a prediction, three different algorythms which were linear model, HoltWinter(triple exponential smoothing), Arima were used. In the end, the prediction based on liner model gave the best values of accuracy. And therefore, linear model was used for the final prediction.
