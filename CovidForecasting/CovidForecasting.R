#Jayesh Bhagat

#clear environment variables
rm(list = ls())

##################################################

#load relevant libraries
library(ggplot2)
library(forecast)
library(tseries)
library(fpp2)
library(fpp)
library(urca) 

##################################################
#import dataset into R
CVUSA = read.csv("COVID19_USA.csv")
CVNZL = read.csv("COVID19_NZL.csv")

##################################################
#Plot datasets for both USA and NZL

#plot USA
autoplot(window(ts(CVUSA$cases)))+
  xlab("Days")+ylab("cases")+
  ggtitle("USA Covid Cases")

#plot NZL
autoplot(window(ts(CVNZL$cases)))+
  xlab("Days")+ylab("cases")+
  ggtitle("NZL Covid cases")

##################################################
#obtain training and test datasets for USA and NZL

#obtain training dataset
trainusa = window(ts(CVUSA$cases, start= 1, end= 100))
trainnzl = window(ts(CVNZL$cases, start= 1, end= 100))

#obtain testing dataset
testusa = window(ts(CVUSA$cases, start= 101, end= 108))
testnzl = window(ts(CVNZL$cases, start= 101, end= 108))

##################################################
#plot training and testing datasets for USA and NZL
#plot training & testing dataset
plot(trainusa)
plot(trainnzl)
plot(testusa)
plot(testnzl)

##################################################

#daily covid 8 period forecast using mean, naive, drift forecast
fcastusa_mean <- meanf(trainusa, h=8)
fcastusa_naive <- rwf(trainusa, h=8)
fcastusa_drift <- rwf(trainusa, drift=TRUE, h=8)

fcastnzl_mean <- meanf(trainnzl, h=8)
fcastnzl_naive <- rwf(trainnzl, h=8)
fcastnzl_drift <- rwf(trainnzl, drift=TRUE, h=8)

##################################################
accuracy_mean_usa <-accuracy(fcastusa_mean,testusa)
accuracy_naive_usa <-accuracy(fcastusa_naive,testusa)
accuracy_drift_usa <-accuracy(fcastusa_drift,testusa)

accuracy_mean_nzl <-accuracy(fcastnzl_mean,testnzl)
accuracy_naive_nzl <-accuracy(fcastnzl_naive,testnzl)
accuracy_drift_nzl <-accuracy(fcastnzl_drift,testnzl)

##################################################
#Print out results for mean, naive and drift forecast
#print out 
accuracy_mean_usa
accuracy_naive_usa
accuracy_drift_usa

accuracy_mean_nzl
accuracy_naive_nzl
accuracy_drift_nzl
##################################################
#Plot forecast for mean, naive and drift forecast for both
#USA and NZL

#plot forecast 
autoplot(window(ts(CVUSA$cases, start= 1, end= 108))) +
  autolayer(fcastusa_mean, PI=FALSE, series="Mean") +
  autolayer(fcastusa_naive, PI=FALSE, series="Naïve") +
  autolayer(fcastusa_drift, PI=FALSE, series="Drift") +
  xlab("Days")+ylab("cases")+
  ggtitle("USA Covid Prediction")+
  guides(colour=guide_legend(title="USA Forecast"))

autoplot(window(ts(CVNZL$cases, start= 1, end= 108))) +
  autolayer(fcastnzl_mean, PI=FALSE, series="Mean") +
  autolayer(fcastnzl_naive, PI=FALSE, series="Naïve") +
  autolayer(fcastnzl_drift, PI=FALSE, series="Drift") +
  xlab("Days")+ylab("cases")+
  ggtitle("NZL Covid Prediction")+
  guides(colour=guide_legend(title="NZL Forecast"))

##################################################
#run a stationary and diff test on both USA and NZL 

#Stationary Test for usa
CVUSAKPSS=ur.kpss(CVUSA$cases)
summary(CVUSAKPSS)

#Difference Test for USA
ndiffs(CVUSA$cases)

#run stationary test with diff function for USA
CVUSAKPSS2=ur.kpss(diff(CVUSA$cases))
summary(CVUSAKPSS2)

#Stationary Test for NZL
CVNZLKPSS=ur.kpss(CVNZL$cases)
summary(CVNZLKPSS)

#Difference Test for NZL
ndiffs(CVNZL$cases)

#run stationary test with diff function for NZL
CVNZLKPSS2=ur.kpss(diff(CVNZL$cases))
summary(CVNZLKPSS2)

##################################################
#ARIMA USA ouput
fitUSA <-auto.arima(trainusa, seasonal = FALSE)

#ARIMA USA forecast
ARIMAUSA <- forecast(auto.arima(diff(trainusa, seasonal = FALSE)),h=8)

autoplot(ARIMAUSA)+
  xlab("Days")+ylab("cases")+
  autolayer(fcastusa_mean, PI=FALSE, series="Arima")+
  ggtitle("USA Covid Prediction ARIMA")

accuracy_ARIMA_usa <-accuracy(ARIMAUSA, testusa)

#ARIMA NZL ouput
fitNZL <-auto.arima(trainnzl, seasonal = FALSE)

#ARIMA NZL forecast
ARIMANZL <- forecast(auto.arima(trainnzl, seasonal = FALSE),h=8)

autoplot(ARIMANZL)+
  xlab("Days")+ylab("cases")+
  ggtitle("NZL Covid Prediction ARIMA")

accuracy_ARIMA_nzl<- accuracy(ARIMANZL, testusa)
##################################################
#STL Decomposition 

#STL USA ouput
tsUSA <- ts(trainusa, frequency =8)
STLUSA <-stl(tsUSA, s.window="periodic")
fcastUSA_STL <-forecast(STLUSA, h=8, method="naive")

plot(fcastUSA_STL, main ="STL USA", ylab ="cases")
accuracy_STL_usa<-accuracy(fcastUSA_STL)


#STL NZL ouput
tsNZL <- ts(trainnzl, frequency =8)
STLNZL <-stl(tsNZL, s.window="periodic")
fcastNZL_STL <-forecast(STLNZL, h=8, method="naive")

plot(fcastNZL_STL, main ="STL NZL", ylab ="cases")
accuracy_STL_nzl <- accuracy(testnzl,fcastNZL_STL)

##################################################
#Performance Comparison 

accuracy_naive_usa
accuracy_drift_usa
accuracy_ARIMA_usa

accuracy_naive_nzl
accuracy_drift_nzl
accuracy_ARIMA_nzl

##################################################