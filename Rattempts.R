library(jsonlite)

dataset.btc <- fromJSON("https://min-api.cryptocompare.com/data/histohour?fsym=BTC&tsym=USD&limit=2000000&aggregate=1&e=CCCAGG")
newdata <- fromJSON("C:/Users/ALLEN_JACK/.spyder-py3/output.json")
# head is the old data and tail the new data

tail(dataset.btc$Data)

head(dataset.btc$Data)    

require(pracma)

dataset.btc$Data$high_avg<-movavg(dataset.btc$Data$high,7,"w")     
library(dplyr)

library(lubridate)

convertUnix <- function(datast){
  
  # This function inputs the dataframe discussed above and adds a new column which is conversion of Unix date to human readable dates
  
  # Args:
  
  #   datast: cryptocurrency dataframe with aforementioned format
  
  #
  
  # Return:
  
  #   datast: the same dataset + newly added column
  
  data.frame(datast) %>% mutate(date=as.Date(as.POSIXct(TimeTo, origin="1970-01-01")))->datast
  
  return(datast)
  
}

convertPercent <- function(data){
  
  # This function inputs a vector of numbers and returns a vector that is the differences between ith and (i-1)th element
  
  # Args:
  
  #   data: the numeric input vector and thus not a dataframe
  
  # Return:
  
  #   data: the percentage of the changes
  
  data= diff(data)/data[-NROW(data)] * 100
  
  return(data)
}
library(AnalyzeTS)
library(readr)
FTS_Predict<- function(data,year,month,day,freq){
  
  # This function builds a time series and predicts the 5 steps ahead
  
  # Args:
  
  #   data: the values that should be predicted (trend or actual data)
  
  #   year: starting point of the data in terms of year
  
  #   month: starting point of the data in terms of month
  
  #   day: starting point of the data in terms of day
  
  #   freq: frequency to build the time series
  
  #
  
  # Return:
  
  #   crypto_predict: the prediction and all attached information (such as accuracy and etc)
  
  #Changing format to TS
  
  crypto<-ts(data,start = c(year,month,day),frequency = freq )
  
  # Finding the best C value by DOC function
  
  # Abbasov-Mamedova model
  
  str.C1<-DOC(crypto,n=50,w=7,D1=0,D2=0,CEF="MAPE",type="Abbasov-Mamedova")
  
  C1<-as.numeric(str.C1[1])
  
  crypto_predict<-fuzzy.ts2(crypto,n=50,w=12,D1=0,D2=0,C=C1,forecast=50, type="Abbasov-Mamedova",trace=TRUE,plot=TRUE)
  
  return(crypto_predict)
  
}
TimeFrom <- as.Date(as.POSIXct(dataset.btc$TimeTo, origin="1970-01-01"))
TimeFrom2 <- as.Date(as.POSIXct(newdata$time, origin="1970-01-01"))

prediction <- FTS_Predict(dataset.btc$Data$open, 2012, 12, 27, 182)

newdataDated <- convertUnix(newdata)
secondPrediction <- FTS_Predict(newdataDated$close, 2018, 6, 19, 525600)

thirdPrediction <- FTS_Predict(newdataDated$close, 2018, 6, 19, 60)
