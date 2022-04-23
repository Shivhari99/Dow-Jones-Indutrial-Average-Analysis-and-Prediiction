library(lubridate) # dates manipulation
library(fBasics) # enhanced summary statistics
library(lmtest) # coefficients significance tests
library(urca) # unit rooit test
library(ggplot2) # visualization
library(quantmod) # getting financial data
library(PerformanceAnalytics) # calculating returns
library(rugarch) # GARCH modeling
library(FinTS) # ARCH test
library(forecast) # ARMA modeling
library(strucchange) # structural changes
library(TSA) # ARMA order identification
library(dplyr)
library(lsr)
djia_data <- read.csv(file.choose())

head(djia_data)
tail(djia_data)

is.na(djia_data)

djia_data_1 <- na.omit(djia_data$DJIA)
djia_data_1
is.na(djia_data$observation_date)
is.na(djia_data$DJIA)
na.exclude(djia_data$DJIA)

suppressMessages(getSymbols("DJIA", from = "2011-11-23", to = "2021-11-22"))

chartSeries(DJIA, type = "bars", theme="white")

dj_close <- DJIA[,"DJIA.Adjusted"]
dj_close

dj_ret <- CalculateReturns(dj_close, method = "log")
dj_ret <- na.omit(dj_ret)
dj_ret

plot(dj_ret)

ggplot(djia_data, aes(x= djia_data$observation_date, y = djia_data$DJIA, color = DJIA))+geom_point()
ggplot(djia_data, aes(sample = djia_data$DJIA)) + stat_qq(colour = "blue") + stat_qq_line()
ggplot(data = djia_data, aes(x = djia_data$DJIA)) + geom_density(fill = "lightblue")

shapiro.test(djia_data$DJIA)

dj_ret_df <- xts_to_dataframe(dj_ret)
head(dj_ret_df)

#Basic Statistics 

dj_ret_df <- xts_to_dataframe(dj_ret)
head(dj_ret_df)
mean(djia_data$DJIA, na.rm = TRUE)
median(djia_data$DJIA, na.rm = TRUE)
skewness(djia_data$DJIA,na.rm = TRUE)
Kurtosis_data <-kurtosis(djia_data$DJIA, na.rm = TRUE)
Kurtosis_data
boxplot(djia_data$DJIA)

#Dow Jones weekly log-returns.

dj_weekly_ret <- apply.weekly(dj_ret, sum)
plot(dj_weekly_ret)

#Confidence Interval 
ciMean(djia_data$DJIA, conf = 0.95)

#sharp increses and decreases of volatility

dj_weekly_ret_df <- xts_to_dataframe(dj_weekly_ret)
dim(dj_weekly_ret_df)

mean(dj_weekly_ret, na.rm = TRUE)
hist(djia_data$DJIA)
skewness(dj_ret,0)
kurtosis(dj_ret,0)
shapiro.test(djia_data$DJIA)