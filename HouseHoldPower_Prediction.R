######################################################################################
# Time series analysis and visualization with Energy Consumption Data                #
#                                                                                    #
# Energy consumption data of 47months in a household with Submetering system         #
# in France was given.                                                               #
# Temperature of this area was added to the energy consumption Data.                 # 
# Time series analysis was used for the analysis of these data.                      #
# Three different models were used for the prediction of Energy consumption.         #                                                                                    #
#                                                                                    #
# Mizuki Kakei                                                                       #
#                                                                                    #
# Version 1.0                                                                        #
#                                                                                    #      
# Date : 26.06.2019                                                                  #     
#                                                                                    #
######################################################################################


## Calling a package ####
library(stats)                                                                       # R statistical function
library(readr)                                                                       # Reading file
library(plyr)                                                                        # Breaking big problems down into managable pecies and bringing back together
library(ggplot2)                                                                     # Function for Visualization             
library(data.table)                                                                  # Function for fast aggregation of large data          
library(corrplot)                                                                    # Correlation matrix format
library(colorspace)                                                                  # For using color palettes
library(dplyr)                                                                       # Function for working with dataframe such as filtering, mutating, summarizing 
library(lubridate)                                                                   # 
library(gridExtra)                                                                   #
library(plotly)                                                                      #
library(ggfortify)                                                                   #
library(forecast)                                                                    #
library(zoo)                                                                         #
library(reshape2)                                                                    #
library(Rserve)                                                                      #

## Connecting R and Tableau ####
Rserve()


## Calling data set of PowerConsumption and TempInParis ####
rawdataPoCo <- fread("household_power_consumption.txt")
PoCo <- rawdataPoCo # PoCo means PowerConsumption

rawdataTemp <- read.csv("TemperatureInParisToImport.csv")
TempInParis <- rawdataTemp


## Preprocessing with PowerConsumption Data ####
## Changing data type in PowerConsumption
PoCo$Date <- as.Date(PoCo$Date, format = "%d/%m/%Y")
PoCo$Global_active_power <- as.numeric(PoCo$Global_active_power)
PoCo$Global_reactive_power <- as.numeric(PoCo$Global_reactive_power)
PoCo$Voltage <- as.numeric(PoCo$Voltage)
PoCo$Global_intensity <- as.numeric(PoCo$Global_intensity)
PoCo$Sub_metering_1 <- as.numeric(PoCo$Sub_metering_1)
PoCo$Sub_metering_2 <- as.numeric(PoCo$Sub_metering_2)
PoCo$Sub_metering_3 <- as.numeric(PoCo$Sub_metering_3)

## combining date and time in PowerConsumption
PoCo$DateTime <- as.POSIXct(paste(PoCo$Date, PoCo$Time),
                            format="%Y-%m-%d %H:%M:%S")

## Adding the time zone in PowerConsumption 
attr(PoCo$DateTime, "tzone") <- "Europe/Paris"

## Creating attribute with lubridate in PowerConsumption 
PoCo$year <- year(PoCo$DateTime)
PoCo$month <- month(PoCo$DateTime)
PoCo$week <- week(PoCo$DateTime)
PoCo$weekday <- weekdays(PoCo$DateTime)
PoCo$day <- day(PoCo$DateTime)
PoCo$hour <- hour(PoCo$DateTime)
PoCo$minute <- minute(PoCo$DateTime)
PoCo$YearMonth <- as.yearmon(PoCo$Date, "%Y-%M")


## Preprocessing with Temperature Data ####
## Changing data type and Adding DateTime column in TempInParis
TempInParis$Date <- as.Date(TempInParis$Date, format = "%d-%m-%Y")
TempInParis$Time <- as.character(TempInParis$Time)

## Adding the time zone in PowerConsumption
TempInParis$DateTime <- as.POSIXct(paste(TempInParis$Date, TempInParis$Time),
                                   format="%Y-%m-%d %H:%M")

## Adding Time Zone in TempInParis 
attr(TempInParis$DateTime, "tzone") <- "Europe/Paris"

## Creating attribute with lubridate in TempInParis
TempInParis$year <- year(TempInParis$DateTime)
TempInParis$month <- month(TempInParis$DateTime)
TempInParis$week <- week(TempInParis$DateTime)
TempInParis$weekday <- weekdays(TempInParis$DateTime)
TempInParis$day <- day(TempInParis$DateTime)
TempInParis$hour <- hour(TempInParis$DateTime)
TempInParis$minute <- minute(TempInParis$DateTime)
TempInParis$YearMonth <- as.yearmon(TempInParis$Date, "%Y-%M")


## Grouping PowerConsumption Data YearAndMonth, Month, Year, Weekday, Hour ####
# PowerConsumption in YearandMonth with Sum Value
Y070809_YearMonth <- PoCo %>%
                     na.exclude() %>%
                     group_by(YearMonth) %>% 
                     mutate(SM_others = Global_active_power*1000/60 - 
                                        Sub_metering_1 - 
                                        Sub_metering_2 - 
                                        Sub_metering_3) %>% 
                     summarize(Sum_SM1 = sum(Sub_metering_1),
                               Sum_SM2 = sum(Sub_metering_2),
                               Sum_SM3 = sum(Sub_metering_3),
                               Sum_gap = sum(Global_active_power), 
                               Sum_SM_others = sum(SM_others))

# PowerConsumption in YearandMonth with mean Value
GroupedByYM <- PoCo %>%
               na.exclude() %>%
               group_by(YearMonth) %>%
               mutate(SM_others = mean(Global_active_power)*1000/60 - 
                                  mean(Sub_metering_1) - 
                                  mean(Sub_metering_2) - 
                                  mean(Sub_metering_3)) %>% 
               summarize(mean_SM1 = mean(Sub_metering_1),
                         mean_SM2 = mean(Sub_metering_2),
                         mean_SM3 = mean(Sub_metering_3),
                         mean_gap = mean(Global_active_power), 
                         mean_SM_others = mean(SM_others))

# PowerConsumption gourped by WeekDay with mean Value
GroupedByWD <- PoCo %>% 
               na.exclude() %>% 
               group_by(weekday) %>%
               mutate(SM_others = mean(Global_active_power)*1000/60 - 
                                  mean(Sub_metering_1) - 
                                  mean(Sub_metering_2) - 
                                  mean(Sub_metering_3)) %>% 
               summarize(mean_SM1 = mean(Sub_metering_1),
                         mean_SM2 = mean(Sub_metering_2),
                         mean_SM3 = mean(Sub_metering_3),
                         mean_gap = mean(Global_active_power), 
                         mean_SM_others = mean(SM_others))

# PowerConsumption Grouped by hour on weekdays with mean Value
GroupedByHonWD <- PoCo %>% 
                  na.exclude() %>% 
                  filter(weekday %in% c ("月曜日", "火曜日", "水曜日", "木曜日", "金曜日")) %>% 
                  group_by(hour) %>%
                  mutate(SM_others = mean(Global_active_power)*1000/60 - 
                                     mean(Sub_metering_1) - 
                                     mean(Sub_metering_2) - 
                                     mean(Sub_metering_3)) %>% 
                  summarize(mean_SM1 = mean(Sub_metering_1),
                            mean_SM2 = mean(Sub_metering_2),
                            mean_SM3 = mean(Sub_metering_3),
                            mean_gap = mean(Global_active_power), 
                            mean_SM_others = mean(SM_others))

# PowerConsumption Grouped by hour on weekdays with mean Value
GroupedByHonWE <- PoCo %>% 
                  na.exclude() %>% 
                  filter(weekday %in% c ("土曜日", "日曜日")) %>% 
                  group_by(hour) %>%
                  mutate(SM_others = mean(Global_active_power)*1000/60 - 
                                     mean(Sub_metering_1) - 
                                     mean(Sub_metering_2) - 
                                     mean(Sub_metering_3)) %>% 
                 summarize(mean_SM1 = mean(Sub_metering_1),
                           mean_SM2 = mean(Sub_metering_2),
                           mean_SM3 = mean(Sub_metering_3),
                           mean_gap = mean(Global_active_power), 
                           mean_SM_others = mean(SM_others))

# Global Active Power grouped by Year
GroupedByY <- PoCo$Global_active_power %>% 
              na.exclude() %>%
              group_by(year) %>% 
              summarize(sumPowerConsumption = sum(Global_active_power)/60) 


## Grouping Temperature Data YearAndMonth ####
# Temperature information grouped by month and Year
TempYM <- TempInParis %>% 
          group_by(YearMonth) %>% 
          summarize(meanTemp = mean(Temperature))


## Visualization with PowerConsumption ####
## Year-Monthly PowerConsumption Visualization ####
G_YM_PoCo <- ggplot(data = GroupedByYM, aes(x = YearMonth, y = GroupedByYM)) +
             geom_line(aes(x = GroupedByYM$YearMonth, y = GroupedByYM$mean_SM1, 
                       group = 1, color = "Submeter1")) +
             geom_line(aes(x = GroupedByYM$YearMonth, y = GroupedByYM$mean_SM2, 
                       group = 1, color = "Submeter2")) +
             geom_line(aes(x = GroupedByYM$YearMonth, y = GroupedByYM$mean_SM3, 
                       group = 1, color = "Submeter3")) +
             geom_line(aes(x = GroupedByYM$YearMonth, y = GroupedByYM$mean_gap*1000/60, 
                       group = 1, color = "GAP")) +
             geom_line(aes(x = GroupedByYM$YearMonth, y = GroupedByYM$mean_SM_others, 
                       group = 1, color = "others")) +
             ylab("Energy Consumption") +  xlab("YearMonth") + 
             ggtitle("Energy consumption") +
             theme(axis.text.x = element_text(face = "bold", size = 12, angle = 0))


## weekly PowerConsumption Visualization #### 
# Ordering weekday
GroupedByWD$weekday<-  ordered(GroupedByWD$weekday, 
                               levels = c("月曜日", "火曜日", "水曜日", "木曜日", 
                                          "金曜日", "土曜日", "日曜日"))

## Weekly PowerConsumption with mean
graphGroupedByWD <- ggplot(data = GroupedByWD, aes(x = weekday, y = GroupedByWD)) +
                    geom_line(aes(x = GroupedByWD$weekday, y = GroupedByWD$mean_SM1, 
                                  group = 1, color = "Submeter1")) +
                    geom_line(aes(x = GroupedByWD$weekday, y = GroupedByWD$mean_SM2, 
                                  group = 1, color = "Submeter2")) +
                    geom_line(aes(x = GroupedByWD$weekday, y = GroupedByWD$mean_SM3, 
                                  group = 1, color = "Submeter3")) +
                    geom_line(aes(x = GroupedByWD$weekday, y = GroupedByWD$mean_gap*1000/60, 
                                  group = 1, color = "GAP")) +
                    geom_line(aes(x = GroupedByWD$weekday, y = GroupedByWD$mean_SM_others, 
                                  group = 1, color = "others")) +
                    ylab("Energy Consumption") +  xlab("weekday") + ggtitle("Energy consumption") +
                    theme(axis.text.x = element_text(face = "bold", size = 12, angle = 0)) 


## Hourly PowerConsumption on weekdays Visualization ####
graphGroupedByHonWD <- ggplot(data = GroupedByHonWD, aes(x = hour, y = GroupedByHonWD)) +
                       geom_line(aes(x = GroupedByHonWD$hour, y = GroupedByHonWD$mean_SM1, 
                                 group = 1, color = "Submeter1")) +
                       geom_line(aes(x = GroupedByHonWD$hour, y = GroupedByHonWD$mean_SM2, 
                                 group = 1, color = "Submeter2")) +
                       geom_line(aes(x = GroupedByHonWD$hour, y = GroupedByHonWD$mean_SM3, 
                                 group = 1, color = "Submeter3")) +
                       geom_line(aes(x = GroupedByHonWD$hour, y = GroupedByHonWD$mean_gap*1000/60, 
                                 group = 1, color = "GAP")) +
                       geom_line(aes(x = GroupedByHonWD$hour, y = GroupedByHonWD$mean_SM_others, 
                                 group = 1, color = "others")) +
                       ylab("Energy Consumption") +  xlab("hour") + ggtitle("Energy consumption") +
                       theme(axis.text.x = element_text(face = "bold", size = 12, angle = 0)) 


## Hourly PowerConsumption on weekend Visualization ####
graphGroupedByHonWE <- ggplot(data = GroupedByHonWE, aes(x = hour, y = GroupedByHonWE)) +
                       geom_line(aes(x = GroupedByHonWE$hour, y = GroupedByHonWE$mean_SM1, 
                                 group = 1, color = "Submeter1")) +
                       geom_line(aes(x = GroupedByHonWE$hour, y = GroupedByHonWE$mean_SM2, 
                                 group = 1, color = "Submeter2")) +
                       geom_line(aes(x = GroupedByHonWE$hour, y = GroupedByHonWE$mean_SM3, 
                                 group = 1, color = "Submeter3")) +
                       geom_line(aes(x = GroupedByHonWE$hour, y = GroupedByHonWE$mean_gap*1000/60, 
                                 group = 1, color = "GAP")) +
                       geom_line(aes(x = GroupedByHonWE$hour, y = GroupedByHonWE$mean_SM_others, 
                                 group = 1, color = "others")) +
                       ylab("Energy Consumption") +  xlab("hour") + 
                       ggtitle("Energy consumption") +
                       theme(axis.text.x = element_text(face = "bold", size = 12, angle = 0)) 


## Visualization with Temprature ####
# Order the data 
dev.off()

# Plotting Year-Monthly Temperature
graphTemp <- ggplot(data = TempYM, aes(x = YearMonth, y = TempYM)) +
             geom_line(aes(x = TempYM$YearMonth, y = TempYM$meanTemp, 
                       group = 1, color = "Temperature")) +
             ggtitle("Energy consumption") +
             theme(axis.text.x = element_text(face = "bold", size = 12, angle = 0))


## Visualization for Temperature and PowerConsumption together ####
## Year-Monthly PowerConsumption and Temperature ####
G_YM_PoCoAndTemp <- ggplot(data = GroupedByYM, aes(x = YearMonth, y = GroupedByYM)) +
                    geom_line(aes(x = GroupedByYM$YearMonth, y = GroupedByYM$mean_SM1, 
                              group = 1, color = "Submeter1")) +
                    geom_line(aes(x = GroupedByYM$YearMonth, y = GroupedByYM$mean_SM2, 
                              group = 1, color = "Submeter2")) +
                    geom_line(aes(x = GroupedByYM$YearMonth, y = GroupedByYM$mean_SM3, 
                              group = 1, color = "Submeter3")) +
                    geom_line(aes(x = GroupedByYM$YearMonth, y = GroupedByYM$mean_gap*1000/60, 
                              group = 1, color = "GAP")) +
                    geom_line(aes(x = GroupedByYM$YearMonth, y = GroupedByYM$mean_SM_others, 
                              group = 1, color = "others")) +
                    geom_line(aes(x = GroupedByYM$YearMonth, y = GroupedByYM$mean_SM1, 
                              group = 1, color = "Temperature")) +
                    ylab("Energy Consumption") +  xlab("YearMonth") + 
                    ggtitle("Energy consumption") +
                    theme(axis.text.x = element_text(face = "bold", size = 12, angle = 0))


## Dataframe and Another way of Visualization for Year-Monthly PowerConsumption and Temperature ####
## Creating data frame 
Dataframe.Temp <- data.frame(TempYM$YearMonth, TempYM$meanTemp, GroupedByYM$mean_SM1, 
                             GroupedByYM$mean_SM2, GroupedByYM$mean_SM3, 
                             GroupedByYM$mean_gap*1000/60, GroupedByYM$mean_SM_others)
Dataframe.Temp.Renamed <- setNames(Dataframe.Temp, c("YearMonth","Temperature","SubMeter1",
                                                     "SubMeter2","SubMeter3","GlobalActivePower",
                                                     "OtherPowerConsumption"))
str(Dataframe.Temp)

## Plotting
Dataframe.Temp.Renamed.Melt <- melt(Dataframe.Temp.Renamed, id.vars = "YearMonth")
ggplot(Dataframe.Temp.Renamed.Melt, 
       aes(x = Dataframe.Temp.Renamed.Melt$YearMonth, 
           y = value, color = variable)) + 
geom_line()


## Linear model ####
## Creating Train and Test model for Year-Monthly PowerConsumption
TSTRAINY070809_YearMonth <- Y070809_YearMonth$Sum_gap %>% 
                            ts(frequency = 12, start = c(2007,1), end = c(2009,12))

autoplot(TSTRAINY070809_YearMonth, ts.colour = "#FC4E07", xlab = "Time", 
         ylab = "Watt Hours", main = "GAP")

TSTESTY070809_YearMonth <- Y070809_YearMonth$Sum_gap %>% 
                           ts(frequency = 12, start = c(2010,1), end = c(2010,11))

## Forecasting
fitGAP <- tslm(TSTRAINY070809_YearMonth ~ trend + season)
forecastfitGAP <- forecast(fitGAP, h = 30, level = c(80,90))

plot(forecastfitGAP, ylim = c(0,40), ylab = "Watt-Hours", xlab = "Time")

# Checking the accuracy of model
accuracy(forecastfitGAP, TSTESTY070809_YearMonth)


## Decomposing GAP ####              
components070809GAP <- decompose(TSTRAINY070809_YearMonth)
plot(components070809GAP)
summary(components070809GAP)

## create seasonal components
seasonalGAP <- components070809GAP$seasonal
plot(seasonalGAP)

seasonalGAP_10 <- PoCo %>% 
                  filter((minute == 0) | minute == 10 | minute == 20 |
                           minute == 30 | minute == 40 | minute == 50)

plotly_seasonalGAP_10 <- seasonalGAP_10 %>% 
                         na.exclude() %>%
                         plot_ly(x = ~ DateTime, y = ~ GAP, 
                                 name = "Total Consumption", 
                                 type = "scatter", mode = "line") %>% 
                         add_trace(y = ~ Sub_metering_2, name = "Laundry Room",
                                   mode = "lines") %>% 
                         add_trace(y = ~ Sub_metering_3, name = "Water Hear & AC",
                                   mode = "lines") %>% 
                         layout(title = "Power Consumption January 9th, 2008",
                         xaxis = list(title = "Time"),
                         yaxis = list(title = "Power(watt-hours)"))


## Holt Winter ####
## Creating Train and Test set and exlude seasonal component
TSTRAINY070809_YearMonthAdjusted <- TSTRAINY070809_YearMonth - components070809GAP$seasonal
autoplot(TSTRAINY070809_YearMonthAdjusted)
plot(decompose(TSTRAINY070809_YearMonthAdjusted))

TSTRAINY070809_YearMonth_HW <- HoltWinters(TSTRAINY070809_YearMonthAdjusted, beta = FALSE, gamma = FALSE)
plot(TSTRAINY070809_YearMonth_HW, ylim = c(0,25))

## Forecasting
TSTRAINY070809_YearMonth_HWfor <- forecast(TSTRAINY070809_YearMonth_HW, h = 25)
plot(TSTRAINY070809_YearMonth_HWfor, ylim = c(0,30), ylab = "Watt-Hours", xlab = "Time - Sub-meter1")
plot(TSTRAINY070809_YearMonth_HWfor, ylim = c(0,30), xlim = c(2010.212, 2010.673), ylab = "Watt-Hours", xlab = "Time - Sub-meter1")

# Checking the accuracy of model
accuracy(TSTRAINY070809_YearMonth_HWfor,TSTESTY070809_YearMonth)


## Arima ####
## Use same train and test set as HoltWinter

## Forecast
fitARIMA <- arima(TSTRAINY070809_YearMonth,order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 12),method="ML")
fitARIMAfor <- forecast(fitARIMA, h = 12)
plot(fitARIMAfor, ylim = c(0,100000), ylab = "Watt-Hours", xlab = "Time - Sub-meter1")

# Checking the accuracy of model
accuracy(fitARIMAfor, TSTESTY070809_YearMonth)


## Correlation Coefficient ####
Dataframe.Temp.Renamed$YearMonth <- NULL
Dataframe.Temp.correlation <- cor(Dataframe.Temp.Renamed)
Dataframe.Temp.correlation.plot <- corrplot(Dataframe.Temp.correlation, 
                                            method="number", 
                                            type = "upper",
                                            tl.cex = 0.6,   
                                            number.cex = 0.8)
