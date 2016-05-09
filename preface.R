###### PREFACE 



# Load the data: only once
#data_all = read.csv("consumption-dk-areas_2016_hourly.csv",sep=";",skip=2,header=T)
data_raw = read.csv('Nordpool_Wind.csv',skip=2)

# Transform time to POSIXct
n = length(data_raw[,1])
col_da = function(x){ paste( as.character(data_raw[x,1]),as.character(substr(data_raw[x,2],1,2)), collapse=" ") }
date_text = sapply(1:n,col_da)
data_all = data.frame(Time = as.POSIXlt(date_text,format="%d-%m-%Y %H",tz="GMT"),
                  DK1 = data_raw$DK1,
                  DK2 = data_raw$DK2,
                  DK = data_raw$DK)

# Load homemade function
fourier_me = function(t,terms,period,names_col=c("S","C")){
  n <- length(t)
  X <- matrix(NA,nrow=n,ncol=2*terms)
  for(i in 1:terms){
    X[,2*i-1] <- sin(2*pi*i*t/period)
    X[,2*i] <- cos(2*pi*i*t/period)
  }
  colnames(X) <- paste(names_col,rep(1:terms,rep(2,terms)),sep="")
  return(X)
}