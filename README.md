# Energy Consumption

## general description
Knowing the pattern and the visualization of energy consumption is the key to reduce their energy consumption. Saving energy has some advantages such that the user can save money and it is also environment friendly. Because of these reasons, Here time series analysis of energy consumption was conducted. As an example, energy consumption data in a household for 47 months in Sceaux was used. 
After some analysis, some relationship between energy consumption and the temperature was found. To be more accurate, the temperature information was also added into the analysis.


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
47 months temperature data was given for every 3 hours.
The data includes
- date
- time
- temperature


## Methods of analysis
Time series analysis was conducted. 
To make a prediction, three different algorythms which were linear model, HoltWinter(triple exponential smoothing), Arima were used.












