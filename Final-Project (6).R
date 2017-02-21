
install.packages("xts")
library(xts)

install.packages("lubridate")
library(lubridate)

install.packages("caTools")
library(caTools)

install.packages("geosphere")
library(geosphere)

install.packages("fpp")
library(fpp)

install.packages("plyr")
library(plyr)

install.packages("qdapTools")
library(qdapTools)

install.packages("mapproj")
library(mapproj)

install.packages("ggmap")
library(ggmap)

path="D:/Documents/Project/Data/"
#path=path
fileList <- list.files(path=path, pattern=".csv")
numfiles<-length(fileList)


sloc1<-list()

for(i in 1:length(fileList))
{
  name<-paste(path,fileList[i],sep="")
  sloc1[[i]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
}


lat1<-vector(length=length(sloc1))
long1<-vector(length=length(sloc1))
for(i in 1:length(sloc1))
{
  lat1[i]<-sloc1[[i]]$V2[3]
  long1[i]<-sloc1[[i]]$V2[2]
}

loc<-data.frame(cbind(lat1,long1))

#Creating 64 zones on map
lim<-c(min(loc[,2]),max(loc[,2]),min(loc[,1]),max(loc[,1]))
nx=8
ny=8



map("state")
x <- seq(lim[1], lim[2], len = nx)
y <- seq(lim[3], lim[4], len = ny)
df<-expand.grid(x = c(seq(lim[1], lim[2], len = 100),  NA), y = y)
p<-mapproject(expand.grid(x = c(seq(lim[1], lim[2], len = 100), NA), y = y))
p = map.wrap(p)
lines(p)
lines(mapproject(expand.grid(y = c(seq(lim[3], lim[4], len = 100), NA), x = x)))

num<-seq(1,64)
zone<-cbind(expand.grid(x = x, y = y),num)

#Categorizing locations into zones
index<-seq(1,length(sloc1))
chart<-data.frame(index,lat1,long1,fileList)
chart$fileList<-as.character(chart$fileList)
chart<-chart[order(lat1),]

for(i in 1:nrow(chart))
{
  for(j in 1:(nrow(zone)-1))
    {
    if((chart$lat1[i]>=zone$y[j]) && chart$lat1[i]<zone$y[j+1] )
    {
      for(k in 1:nrow(zone))
      {
        if(zone$y[k]==zone$y[j])
        {
          if(chart$long1[i]>=zone$x[k] && chart$long1[i]<zone$x[k+1] )
         {
          chart$zone[i]=zone$num[k]
          break
         }
        }
      }
    }
  }
}

if(chart$lat1[nrow(chart)]==zone$y[nrow(zone)])
    {
    for(k in (nrow(zone)-7):nrow(zone))
    {
    if(chart$long1[nrow(chart)]>=zone$x[k] && chart$long1[nrow(chart)]<zone$x[k+1] )
          {
            chart$zone[i]=zone$num[k]
            break
          }
        }
      }


#--------SITE 51 Begin----
#Reading files by zone
s51<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==51)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s51[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc51<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==51)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc51[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s51) <- sprintf('s51.%d', 1:numfiles)
for(i in 1:numfiles)
{s51[[i]]$Time <- with(s51[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s51))
{names(s51[[i]])[names(s51[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s51))
{names(s51[[i]])[names(s51[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s51.ts<-vector("list", length(s51))
names(s51.ts) <- sprintf('s51.%d', 1:numfiles)

for(i in 1:length(s51))
{
  s51.ts[[i]]<-xts(s51[[i]]$speed,s51[[i]]$Time)
  colnames(s51.ts[[i]])='speed'
  s51.ts[[i]]$power<-s51[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s51))
for(i in 1:length(s51))
{
  ep<-endpoints(s51.ts[[i]], on="hours", k=1)
  a<-(period.apply(s51.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s51.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s51 <- list()
maxindex<-which.max(CF1)
data.s51[[oldcount]]<-s51.ts[[which.max(CF1)]]
names(data.s51)[[oldcount]] <- names(s51.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc51 <- lapply( paste('sloc51.', 1:numfiles, sep=''), get)
names(sloc51) <- sprintf('sloc51.%d', 1:numfiles)

num<-seq(1,length(s51))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s51))
long1<-vector(length=length(s51))
for(i in 1:length(s51))
{
  lat1[i]<-sloc51[[i]]$V2[3]
  long1[i]<-sloc51[[i]]$V2[2]
}

#------Data collection for Site 1 over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s51))
{
  ep<-endpoints(data.s51[[i]], on="hours", k=1)
  data.s51[[i]]<-(period.apply(data.s51[[i]],ep,mean))
}

#sd(data.s51[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s51[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s51[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s51<-vector("list",(length(data.s51)*2))
count<-1
for(j in 1:length(data.s51))
{
  names(arima.s51)[[count]]<-paste((names(data.s51)[[j]]),".april",sep='')
  arima.s51[[count]]<-data.s51[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s51[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s51[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s51[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}



  names(arima.s51)[[count]]<-paste(names(data.s51)[[1]],".july",sep='')
  arima.s51[[count]]<-data.s51[[1]]$speed["2011-07-01/2011-07-31"]
  
  july<-data.s51[[1]]$speed["2011-07-01/2011-07-31"]
  
  for(i in 0:(length(data.s51[[1]]$speed["2011-07-01/2011-07-31"])-73))
  {
    a=i+1
    b=i+72
    example<-july$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s51[[count]][i+73]<-as.numeric(point.forecast)
  }


#backup<-arima.s51
#Making a power curve

speed=as.double(s51[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s51[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s51[[1]]$power<-lookup(round_any(as.numeric(arima.s51[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s51[[2]]$power<-lookup(round_any(as.numeric(arima.s51[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s51<-vector("list",length(arima.s51))
count1<-1
count2<-length(arima.s51)/2 + 1
for(i in 1:length(data.s51))
{
  names(res.s51)[[count1]]<-paste((names(data.s51)[[i]]),".april",sep='')
  res.s51[[count1]]<-data.s51[[i]]$power["2011-04-01/2011-04-30"]-arima.s51[[count1]]$power
  count1<-count1+1
  
  names(res.s51)[[count2]]<-paste(names(data.s51)[[i]],".july",sep='')
  res.s51[[count2]]<-data.s51[[i]]$power["2011-07-01/2011-07-31"]-arima.s51[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s51)/2 + 1
for(i in 1:length(data.s51))
{
  
  plot(data.s51[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s51", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s51[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s51[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s51[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s51", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s51[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s51[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s51<-vector("list",length(arima.s51))
count1<-1
count2<-length(arima.s51)/2 + 1
for(i in 1:length(data.s51))
{
  names(mape.s51)[[count1]]<-paste((names(data.s51)[[i]]),".april",sep='')
  div<-res.s51[[count1]]/data.s51[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s51[[count1]]<-100*sum(abs(div))/length(data.s51[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s51)[[count2]]<-paste(names(data.s51)[[i]],".july",sep='')
  div<-res.s51[[count2]]/data.s51[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s51[[count2]]<-100*sum(abs(div))/nrow(data.s51[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s51))
{
  write.zoo(arima.s51[[i]], file = paste(names(arima.s51)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s51))
{
  write.zoo(data.s51[[i]], file = paste(names(data.s51)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s51))
{
  write.zoo(res.s51[[i]], file = paste(names(res.s51)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s51))
{
  write.zoo(mape.s51[[i]], file = paste(names(mape.s51)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 51 over -----

save.image()

#---------SITE 52 begin -----
#Reading files by zone
s52<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==52)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s52[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc52<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==52)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc52[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s52) <- sprintf('s52.%d', 1:numfiles)
for(i in 1:numfiles)
{s52[[i]]$Time <- with(s52[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s52))
{names(s52[[i]])[names(s52[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s52))
{names(s52[[i]])[names(s52[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s52.ts<-vector("list", length(s52))
names(s52.ts) <- sprintf('s52.%d', 1:numfiles)

for(i in 1:length(s52))
{
  s52.ts[[i]]<-xts(s52[[i]]$speed,s52[[i]]$Time)
  colnames(s52.ts[[i]])='speed'
  s52.ts[[i]]$power<-s52[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s52))
for(i in 1:length(s52))
{
  ep<-endpoints(s52.ts[[i]], on="hours", k=1)
  a<-(period.apply(s52.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s52.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s52 <- list()
maxindex<-which.max(CF1)
data.s52[[oldcount]]<-s52.ts[[which.max(CF1)]]
names(data.s52)[[oldcount]] <- names(s52.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc52 <- lapply( paste('sloc52.', 1:numfiles, sep=''), get)
names(sloc52) <- sprintf('sloc52.%d', 1:numfiles)

num<-seq(1,length(s52))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s52))
long1<-vector(length=length(s52))
for(i in 1:length(s52))
{
  lat1[i]<-sloc52[[i]]$V2[3]
  long1[i]<-sloc52[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s52))
{
  ep<-endpoints(data.s52[[i]], on="hours", k=1)
  data.s52[[i]]<-(period.apply(data.s52[[i]],ep,mean))
}

#sd(data.s52[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s52[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s52[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s52<-vector("list",(length(data.s52)*2))
count<-1
for(j in 1:length(data.s52))
{
  names(arima.s52)[[count]]<-paste((names(data.s52)[[j]]),".april",sep='')
  arima.s52[[count]]<-data.s52[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s52[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s52[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s52[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s52)[[count]]<-paste(names(data.s52)[[1]],".july",sep='')
arima.s52[[count]]<-data.s52[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s52[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s52[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s52[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s52
#Making a power curve

speed=as.double(s52[[which.max(CF1)]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s52[[which.max(CF1)]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s52[[1]]$power<-lookup(round_any(as.numeric(arima.s52[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s52[[2]]$power<-lookup(round_any(as.numeric(arima.s52[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

for(i in 1:nrow(arima.s52[[1]]))
{
  
  
  if(arima.s52[[1]]$speed[i]>=13)
  {
    arima.s52[[1]]$power[i]=6
  }
  
  if(arima.s52[[1]]$speed[i]<0)
  {
    arima.s52[[1]]$speed[i]=0
  }
  
  if(arima.s52[[1]]$speed[i]==0)
  {
    arima.s52[[1]]$power[i]=0
  }
  
}

for(i in 1:nrow(arima.s52[[2]]))
{
  if(arima.s52[[2]]$speed[i]>=13)
  {
    arima.s52[[2]]$power[i]=6
  }
  
  if(arima.s52[[2]]$speed[i]<0)
  {
    arima.s52[[2]]$speed[i]=0
  }
  
  if(arima.s52[[2]]$speed[i]==0)
  {
    arima.s52[[2]]$power[i]=0
  }
  
}

#Residuals
res.s52<-vector("list",length(arima.s52))
count1<-1
count2<-length(arima.s52)/2 + 1
for(i in 1:length(data.s52))
{
  names(res.s52)[[count1]]<-paste((names(data.s52)[[i]]),".april",sep='')
  res.s52[[count1]]<-data.s52[[i]]$power["2011-04-01/2011-04-30"]-arima.s52[[count1]]$power
  count1<-count1+1
  
  names(res.s52)[[count2]]<-paste(names(data.s52)[[i]],".july",sep='')
  res.s52[[count2]]<-data.s52[[i]]$power["2011-07-01/2011-07-31"]-arima.s52[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s52)/2 + 1
for(i in 1:length(data.s52))
{
  
  plot(data.s52[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s52", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s52[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s52[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s52[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s52", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s52[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s52[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s52<-vector("list",length(arima.s52))
count1<-1
count2<-length(arima.s52)/2 + 1
for(i in 1:length(data.s52))
{
  names(mape.s52)[[count1]]<-paste((names(data.s52)[[i]]),".april",sep='')
  div<-res.s52[[count1]]/data.s52[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s52[[count1]]<-100*sum(abs(div))/length(data.s52[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s52)[[count2]]<-paste(names(data.s52)[[i]],".july",sep='')
  div<-res.s52[[count2]]/data.s52[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s52[[count2]]<-100*sum(abs(div))/nrow(data.s52[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s52))
{
  write.zoo(arima.s52[[i]], file = paste(names(arima.s52)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s52))
{
  write.zoo(data.s52[[i]], file = paste(names(data.s52)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s52))
{
  write.zoo(res.s52[[i]], file = paste(names(res.s52)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s52))
{
  write.zoo(mape.s52[[i]], file = paste(names(mape.s52)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 52 over -----

save.image()


#---------SITE 53 begin -----
#Reading files by zone
s53<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==53)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s53[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc53<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==53)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc53[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s53) <- sprintf('s53.%d', 1:numfiles)
for(i in 1:numfiles)
{s53[[i]]$Time <- with(s53[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s53))
{names(s53[[i]])[names(s53[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s53))
{names(s53[[i]])[names(s53[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s53.ts<-vector("list", length(s53))
names(s53.ts) <- sprintf('s53.%d', 1:numfiles)

for(i in 1:length(s53))
{
  s53.ts[[i]]<-xts(s53[[i]]$speed,s53[[i]]$Time)
  colnames(s53.ts[[i]])='speed'
  s53.ts[[i]]$power<-s53[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s53))
for(i in 1:length(s53))
{
  ep<-endpoints(s53.ts[[i]], on="hours", k=1)
  a<-(period.apply(s53.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s53.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s53 <- list()
maxindex<-which.max(CF1)
data.s53[[oldcount]]<-s53.ts[[which.max(CF1)]]
names(data.s53)[[oldcount]] <- names(s53.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc53 <- lapply( paste('sloc53.', 1:numfiles, sep=''), get)
names(sloc53) <- sprintf('sloc53.%d', 1:numfiles)

num<-seq(1,length(s53))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s53))
long1<-vector(length=length(s53))
for(i in 1:length(s53))
{
  lat1[i]<-sloc53[[i]]$V2[3]
  long1[i]<-sloc53[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s53))
{
  ep<-endpoints(data.s53[[i]], on="hours", k=1)
  data.s53[[i]]<-(period.apply(data.s53[[i]],ep,mean))
}

#sd(data.s53[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s53[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s53[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s53<-vector("list",(length(data.s53)*2))
count<-1
for(j in 1:length(data.s53))
{
  names(arima.s53)[[count]]<-paste((names(data.s53)[[j]]),".april",sep='')
  arima.s53[[count]]<-data.s53[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s53[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s53[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s53[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s53)[[count]]<-paste(names(data.s53)[[1]],".july",sep='')
arima.s53[[count]]<-data.s53[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s53[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s53[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s53[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s53
#Making a power curve

speed=as.double(s53[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s53[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s53[[1]]$power<-lookup(round_any(as.numeric(arima.s53[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s53[[2]]$power<-lookup(round_any(as.numeric(arima.s53[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s53<-vector("list",length(arima.s53))
count1<-1
count2<-length(arima.s53)/2 + 1
for(i in 1:length(data.s53))
{
  names(res.s53)[[count1]]<-paste((names(data.s53)[[i]]),".april",sep='')
  res.s53[[count1]]<-data.s53[[i]]$power["2011-04-01/2011-04-30"]-arima.s53[[count1]]$power
  count1<-count1+1
  
  names(res.s53)[[count2]]<-paste(names(data.s53)[[i]],".july",sep='')
  res.s53[[count2]]<-data.s53[[i]]$power["2011-07-01/2011-07-31"]-arima.s53[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s53)/2 + 1
for(i in 1:length(data.s53))
{
  
  plot(data.s53[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s53", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s53[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s53[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s53[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s53", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s53[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s53[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s53<-vector("list",length(arima.s53))
count1<-1
count2<-length(arima.s53)/2 + 1
for(i in 1:length(data.s53))
{
  names(mape.s53)[[count1]]<-paste((names(data.s53)[[i]]),".april",sep='')
  div<-res.s53[[count1]]/data.s53[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s53[[count1]]<-100*sum(abs(div))/length(data.s53[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s53)[[count2]]<-paste(names(data.s53)[[i]],".july",sep='')
  div<-res.s53[[count2]]/data.s53[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s53[[count2]]<-100*sum(abs(div))/nrow(data.s53[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s53))
{
  write.zoo(arima.s53[[i]], file = paste(names(arima.s53)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s53))
{
  write.zoo(data.s53[[i]], file = paste(names(data.s53)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s53))
{
  write.zoo(res.s53[[i]], file = paste(names(res.s53)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s53))
{
  write.zoo(mape.s53[[i]], file = paste(names(mape.s53)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 53 over -----
save.image()



#---------SITE 54 begin -----
#Reading files by zone
s54<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==54)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s54[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc54<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==54)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc54[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s54) <- sprintf('s54.%d', 1:numfiles)
for(i in 1:numfiles)
{s54[[i]]$Time <- with(s54[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s54))
{names(s54[[i]])[names(s54[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s54))
{names(s54[[i]])[names(s54[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s54.ts<-vector("list", length(s54))
names(s54.ts) <- sprintf('s54.%d', 1:numfiles)

for(i in 1:length(s54))
{
  s54.ts[[i]]<-xts(s54[[i]]$speed,s54[[i]]$Time)
  colnames(s54.ts[[i]])='speed'
  s54.ts[[i]]$power<-s54[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s54))
for(i in 1:length(s54))
{
  ep<-endpoints(s54.ts[[i]], on="hours", k=1)
  a<-(period.apply(s54.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s54.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s54 <- list()
maxindex<-which.max(CF1)
data.s54[[oldcount]]<-s54.ts[[which.max(CF1)]]
names(data.s54)[[oldcount]] <- names(s54.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc54 <- lapply( paste('sloc54.', 1:numfiles, sep=''), get)
names(sloc54) <- sprintf('sloc54.%d', 1:numfiles)

num<-seq(1,length(s54))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s54))
long1<-vector(length=length(s54))
for(i in 1:length(s54))
{
  lat1[i]<-sloc54[[i]]$V2[3]
  long1[i]<-sloc54[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s54))
{
  ep<-endpoints(data.s54[[i]], on="hours", k=1)
  data.s54[[i]]<-(period.apply(data.s54[[i]],ep,mean))
}

#sd(data.s54[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s54[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s54[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s54<-vector("list",(length(data.s54)*2))
count<-1
for(j in 1:length(data.s54))
{
  names(arima.s54)[[count]]<-paste((names(data.s54)[[j]]),".april",sep='')
  arima.s54[[count]]<-data.s54[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s54[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s54[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s54[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s54)[[count]]<-paste(names(data.s54)[[1]],".july",sep='')
arima.s54[[count]]<-data.s54[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s54[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s54[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s54[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s54
#Making a power curve

speed=as.double(s54[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s54[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s54[[1]]$power<-lookup(round_any(as.numeric(arima.s54[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s54[[2]]$power<-lookup(round_any(as.numeric(arima.s54[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

for(i in 1:nrow(arima.s54[[1]]))
{
  
  
  if(arima.s54[[1]]$speed[i]>=13)
  {
    arima.s54[[1]]$power[i]=16
  }
  
  if(arima.s54[[1]]$speed[i]<0)
  {
    arima.s54[[1]]$speed[i]=0
  }
  
  if(arima.s54[[1]]$speed[i]==0)
  {
    arima.s54[[1]]$power[i]=0
  }
  
}

for(i in 1:nrow(arima.s54[[2]]))
{
  if(arima.s54[[2]]$speed[i]>=13)
  {
    arima.s54[[2]]$power[i]=16
  }
  
  if(arima.s54[[2]]$speed[i]<0)
  {
    arima.s54[[2]]$speed[i]=0
  }
  
  if(arima.s54[[2]]$speed[i]==0)
  {
    arima.s54[[2]]$power[i]=0
  }
  
}


#Residuals
res.s54<-vector("list",length(arima.s54))
count1<-1
count2<-length(arima.s54)/2 + 1
for(i in 1:length(data.s54))
{
  names(res.s54)[[count1]]<-paste((names(data.s54)[[i]]),".april",sep='')
  res.s54[[count1]]<-data.s54[[i]]$power["2011-04-01/2011-04-30"]-arima.s54[[count1]]$power
  count1<-count1+1
  
  names(res.s54)[[count2]]<-paste(names(data.s54)[[i]],".july",sep='')
  res.s54[[count2]]<-data.s54[[i]]$power["2011-07-01/2011-07-31"]-arima.s54[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s54)/2 + 1
for(i in 1:length(data.s54))
{
  
  plot(data.s54[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s54", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s54[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s54[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s54[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s54", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s54[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s54[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s54<-vector("list",length(arima.s54))
count1<-1
count2<-length(arima.s54)/2 + 1
for(i in 1:length(data.s54))
{
  names(mape.s54)[[count1]]<-paste((names(data.s54)[[i]]),".april",sep='')
  div<-res.s54[[count1]]/data.s54[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s54[[count1]]<-100*sum(abs(div))/length(data.s54[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s54)[[count2]]<-paste(names(data.s54)[[i]],".july",sep='')
  div<-res.s54[[count2]]/data.s54[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s54[[count2]]<-100*sum(abs(div))/nrow(data.s54[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s54))
{
  write.zoo(arima.s54[[i]], file = paste(names(arima.s54)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s54))
{
  write.zoo(data.s54[[i]], file = paste(names(data.s54)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s54))
{
  write.zoo(res.s54[[i]], file = paste(names(res.s54)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s54))
{
  write.zoo(mape.s54[[i]], file = paste(names(mape.s54)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 54 over -----
save.image()


#---------SITE 55 begin -----
#Reading files by zone
s55<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==55)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s55[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc55<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==55)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc55[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s55) <- sprintf('s55.%d', 1:numfiles)
for(i in 1:numfiles)
{s55[[i]]$Time <- with(s55[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s55))
{names(s55[[i]])[names(s55[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s55))
{names(s55[[i]])[names(s55[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s55.ts<-vector("list", length(s55))
names(s55.ts) <- sprintf('s55.%d', 1:numfiles)

for(i in 1:length(s55))
{
  s55.ts[[i]]<-xts(s55[[i]]$speed,s55[[i]]$Time)
  colnames(s55.ts[[i]])='speed'
  s55.ts[[i]]$power<-s55[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s55))
for(i in 1:length(s55))
{
  ep<-endpoints(s55.ts[[i]], on="hours", k=1)
  a<-(period.apply(s55.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s55.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s55 <- list()
maxindex<-which.max(CF1)
data.s55[[oldcount]]<-s55.ts[[which.max(CF1)]]
names(data.s55)[[oldcount]] <- names(s55.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc55 <- lapply( paste('sloc55.', 1:numfiles, sep=''), get)
names(sloc55) <- sprintf('sloc55.%d', 1:numfiles)

num<-seq(1,length(s55))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s55))
long1<-vector(length=length(s55))
for(i in 1:length(s55))
{
  lat1[i]<-sloc55[[i]]$V2[3]
  long1[i]<-sloc55[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s55))
{
  ep<-endpoints(data.s55[[i]], on="hours", k=1)
  data.s55[[i]]<-(period.apply(data.s55[[i]],ep,mean))
}

#sd(data.s55[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s55[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s55[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s55<-vector("list",(length(data.s55)*2))
count<-1
for(j in 1:length(data.s55))
{
  names(arima.s55)[[count]]<-paste((names(data.s55)[[j]]),".april",sep='')
  arima.s55[[count]]<-data.s55[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s55[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s55[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s55[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s55)[[count]]<-paste(names(data.s55)[[1]],".july",sep='')
arima.s55[[count]]<-data.s55[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s55[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s55[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s55[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s55
#Making a power curve

speed=as.double(s55[[which.max(CF1)]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s55[[which.max(CF1)]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s55[[1]]$power<-lookup(round_any(as.numeric(arima.s55[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s55[[2]]$power<-lookup(round_any(as.numeric(arima.s55[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

for(i in 1:nrow(arima.s55[[1]]))
{
  
  
  if(arima.s55[[1]]$speed[i]>=13)
  {
    arima.s55[[1]]$power[i]=6
  }
  
  if(arima.s55[[1]]$speed[i]<0)
  {
    arima.s55[[1]]$speed[i]=0
  }
  
  if(arima.s55[[1]]$speed[i]==0)
  {
    arima.s55[[1]]$power[i]=0
  }
  
}

for(i in 1:nrow(arima.s55[[2]]))
{
  if(arima.s55[[2]]$speed[i]>=13)
  {
    arima.s55[[2]]$power[i]=6
  }
  
  if(arima.s55[[2]]$speed[i]<0)
  {
    arima.s55[[2]]$speed[i]=0
  }
  
  if(arima.s55[[2]]$speed[i]==0)
  {
    arima.s55[[2]]$power[i]=0
  }
  
}

#Residuals
res.s55<-vector("list",length(arima.s55))
count1<-1
count2<-length(arima.s55)/2 + 1
for(i in 1:length(data.s55))
{
  names(res.s55)[[count1]]<-paste((names(data.s55)[[i]]),".april",sep='')
  res.s55[[count1]]<-data.s55[[i]]$power["2011-04-01/2011-04-30"]-arima.s55[[count1]]$power
  count1<-count1+1
  
  names(res.s55)[[count2]]<-paste(names(data.s55)[[i]],".july",sep='')
  res.s55[[count2]]<-data.s55[[i]]$power["2011-07-01/2011-07-31"]-arima.s55[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s55)/2 + 1
for(i in 1:length(data.s55))
{
  
  plot(data.s55[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s55", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s55[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s55[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s55[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s55", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s55[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s55[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s55<-vector("list",length(arima.s55))
count1<-1
count2<-length(arima.s55)/2 + 1
for(i in 1:length(data.s55))
{
  names(mape.s55)[[count1]]<-paste((names(data.s55)[[i]]),".april",sep='')
  div<-res.s55[[count1]]/data.s55[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s55[[count1]]<-100*sum(abs(div))/length(data.s55[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s55)[[count2]]<-paste(names(data.s55)[[i]],".july",sep='')
  div<-res.s55[[count2]]/data.s55[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s55[[count2]]<-100*sum(abs(div))/nrow(data.s55[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s55))
{
  write.zoo(arima.s55[[i]], file = paste(names(arima.s55)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s55))
{
  write.zoo(data.s55[[i]], file = paste(names(data.s55)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s55))
{
  write.zoo(res.s55[[i]], file = paste(names(res.s55)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s55))
{
  write.zoo(mape.s55[[i]], file = paste(names(mape.s55)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 55 over -----
save.image()


#---------SITE 56 begin -----
#Reading files by zone
s56<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==56)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s56[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc56<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==56)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc56[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s56) <- sprintf('s56.%d', 1:numfiles)
for(i in 1:numfiles)
{s56[[i]]$Time <- with(s56[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s56))
{names(s56[[i]])[names(s56[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s56))
{names(s56[[i]])[names(s56[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s56.ts<-vector("list", length(s56))
names(s56.ts) <- sprintf('s56.%d', 1:numfiles)

for(i in 1:length(s56))
{
  s56.ts[[i]]<-xts(s56[[i]]$speed,s56[[i]]$Time)
  colnames(s56.ts[[i]])='speed'
  s56.ts[[i]]$power<-s56[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s56))
for(i in 1:length(s56))
{
  ep<-endpoints(s56.ts[[i]], on="hours", k=1)
  a<-(period.apply(s56.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s56.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s56 <- list()
maxindex<-which.max(CF1)
data.s56[[oldcount]]<-s56.ts[[which.max(CF1)]]
names(data.s56)[[oldcount]] <- names(s56.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc56 <- lapply( paste('sloc56.', 1:numfiles, sep=''), get)
names(sloc56) <- sprintf('sloc56.%d', 1:numfiles)

num<-seq(1,length(s56))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s56))
long1<-vector(length=length(s56))
for(i in 1:length(s56))
{
  lat1[i]<-sloc56[[i]]$V2[3]
  long1[i]<-sloc56[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s56))
{
  ep<-endpoints(data.s56[[i]], on="hours", k=1)
  data.s56[[i]]<-(period.apply(data.s56[[i]],ep,mean))
}

#sd(data.s56[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s56[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s56[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s56<-vector("list",(length(data.s56)*2))
count<-1
for(j in 1:length(data.s56))
{
  names(arima.s56)[[count]]<-paste((names(data.s56)[[j]]),".april",sep='')
  arima.s56[[count]]<-data.s56[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s56[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s56[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s56[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s56)[[count]]<-paste(names(data.s56)[[1]],".july",sep='')
arima.s56[[count]]<-data.s56[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s56[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s56[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s56[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s56
#Making a power curve

speed=as.double(s56[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s56[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s56[[1]]$power<-lookup(round_any(as.numeric(arima.s56[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s56[[2]]$power<-lookup(round_any(as.numeric(arima.s56[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s56<-vector("list",length(arima.s56))
count1<-1
count2<-length(arima.s56)/2 + 1
for(i in 1:length(data.s56))
{
  names(res.s56)[[count1]]<-paste((names(data.s56)[[i]]),".april",sep='')
  res.s56[[count1]]<-data.s56[[i]]$power["2011-04-01/2011-04-30"]-arima.s56[[count1]]$power
  count1<-count1+1
  
  names(res.s56)[[count2]]<-paste(names(data.s56)[[i]],".july",sep='')
  res.s56[[count2]]<-data.s56[[i]]$power["2011-07-01/2011-07-31"]-arima.s56[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s56)/2 + 1
for(i in 1:length(data.s56))
{
  
  plot(data.s56[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s56", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s56[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s56[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s56[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s56", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s56[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s56[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s56<-vector("list",length(arima.s56))
count1<-1
count2<-length(arima.s56)/2 + 1
for(i in 1:length(data.s56))
{
  names(mape.s56)[[count1]]<-paste((names(data.s56)[[i]]),".april",sep='')
  div<-res.s56[[count1]]/data.s56[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s56[[count1]]<-100*sum(abs(div))/length(data.s56[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s56)[[count2]]<-paste(names(data.s56)[[i]],".july",sep='')
  div<-res.s56[[count2]]/data.s56[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s56[[count2]]<-100*sum(abs(div))/nrow(data.s56[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s56))
{
  write.zoo(arima.s56[[i]], file = paste(names(arima.s56)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s56))
{
  write.zoo(data.s56[[i]], file = paste(names(data.s56)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s56))
{
  write.zoo(res.s56[[i]], file = paste(names(res.s56)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s56))
{
  write.zoo(mape.s56[[i]], file = paste(names(mape.s56)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 56 over -----
save.image()


#---------SITE 57 begin -----
#Reading files by zone
s57<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==57)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s57[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc57<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==57)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc57[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s57) <- sprintf('s57.%d', 1:numfiles)
for(i in 1:numfiles)
{s57[[i]]$Time <- with(s57[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s57))
{names(s57[[i]])[names(s57[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s57))
{names(s57[[i]])[names(s57[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s57.ts<-vector("list", length(s57))
names(s57.ts) <- sprintf('s57.%d', 1:numfiles)

for(i in 1:length(s57))
{
  s57.ts[[i]]<-xts(s57[[i]]$speed,s57[[i]]$Time)
  colnames(s57.ts[[i]])='speed'
  s57.ts[[i]]$power<-s57[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s57))
for(i in 1:length(s57))
{
  ep<-endpoints(s57.ts[[i]], on="hours", k=1)
  a<-(period.apply(s57.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s57.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s57 <- list()
maxindex<-which.max(CF1)
data.s57[[oldcount]]<-s57.ts[[which.max(CF1)]]
names(data.s57)[[oldcount]] <- names(s57.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc57 <- lapply( paste('sloc57.', 1:numfiles, sep=''), get)
names(sloc57) <- sprintf('sloc57.%d', 1:numfiles)

num<-seq(1,length(s57))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s57))
long1<-vector(length=length(s57))
for(i in 1:length(s57))
{
  lat1[i]<-sloc57[[i]]$V2[3]
  long1[i]<-sloc57[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s57))
{
  ep<-endpoints(data.s57[[i]], on="hours", k=1)
  data.s57[[i]]<-(period.apply(data.s57[[i]],ep,mean))
}

#sd(data.s57[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s57[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s57[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s57<-vector("list",(length(data.s57)*2))
count<-1
for(j in 1:length(data.s57))
{
  names(arima.s57)[[count]]<-paste((names(data.s57)[[j]]),".april",sep='')
  arima.s57[[count]]<-data.s57[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s57[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s57[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s57[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s57)[[count]]<-paste(names(data.s57)[[1]],".july",sep='')
arima.s57[[count]]<-data.s57[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s57[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s57[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s57[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s57
#Making a power curve

speed=as.double(s57[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s57[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s57[[1]]$power<-lookup(round_any(as.numeric(arima.s57[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s57[[2]]$power<-lookup(round_any(as.numeric(arima.s57[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s57<-vector("list",length(arima.s57))
count1<-1
count2<-length(arima.s57)/2 + 1
for(i in 1:length(data.s57))
{
  names(res.s57)[[count1]]<-paste((names(data.s57)[[i]]),".april",sep='')
  res.s57[[count1]]<-data.s57[[i]]$power["2011-04-01/2011-04-30"]-arima.s57[[count1]]$power
  count1<-count1+1
  
  names(res.s57)[[count2]]<-paste(names(data.s57)[[i]],".july",sep='')
  res.s57[[count2]]<-data.s57[[i]]$power["2011-07-01/2011-07-31"]-arima.s57[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s57)/2 + 1
for(i in 1:length(data.s57))
{
  
  plot(data.s57[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s57", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s57[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s57[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s57[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s57", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s57[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s57[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s57<-vector("list",length(arima.s57))
count1<-1
count2<-length(arima.s57)/2 + 1
for(i in 1:length(data.s57))
{
  names(mape.s57)[[count1]]<-paste((names(data.s57)[[i]]),".april",sep='')
  div<-res.s57[[count1]]/data.s57[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s57[[count1]]<-100*sum(abs(div))/length(data.s57[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s57)[[count2]]<-paste(names(data.s57)[[i]],".july",sep='')
  div<-res.s57[[count2]]/data.s57[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s57[[count2]]<-100*sum(abs(div))/nrow(data.s57[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s57))
{
  write.zoo(arima.s57[[i]], file = paste(names(arima.s57)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s57))
{
  write.zoo(data.s57[[i]], file = paste(names(data.s57)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s57))
{
  write.zoo(res.s57[[i]], file = paste(names(res.s57)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s57))
{
  write.zoo(mape.s57[[i]], file = paste(names(mape.s57)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 57 over -----
save.image()


#---------SITE 58 begin -----
#Reading files by zone
s58<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==58)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s58[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc58<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==58)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc58[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s58) <- sprintf('s58.%d', 1:numfiles)
for(i in 1:numfiles)
{s58[[i]]$Time <- with(s58[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s58))
{names(s58[[i]])[names(s58[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s58))
{names(s58[[i]])[names(s58[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s58.ts<-vector("list", length(s58))
names(s58.ts) <- sprintf('s58.%d', 1:numfiles)

for(i in 1:length(s58))
{
  s58.ts[[i]]<-xts(s58[[i]]$speed,s58[[i]]$Time)
  colnames(s58.ts[[i]])='speed'
  s58.ts[[i]]$power<-s58[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s58))
for(i in 1:length(s58))
{
  ep<-endpoints(s58.ts[[i]], on="hours", k=1)
  a<-(period.apply(s58.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s58.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s58 <- list()
maxindex<-which.max(CF1)
data.s58[[oldcount]]<-s58.ts[[which.max(CF1)]]
names(data.s58)[[oldcount]] <- names(s58.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc58 <- lapply( paste('sloc58.', 1:numfiles, sep=''), get)
names(sloc58) <- sprintf('sloc58.%d', 1:numfiles)

num<-seq(1,length(s58))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s58))
long1<-vector(length=length(s58))
for(i in 1:length(s58))
{
  lat1[i]<-sloc58[[i]]$V2[3]
  long1[i]<-sloc58[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s58))
{
  ep<-endpoints(data.s58[[i]], on="hours", k=1)
  data.s58[[i]]<-(period.apply(data.s58[[i]],ep,mean))
}

#sd(data.s58[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s58[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s58[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s58<-vector("list",(length(data.s58)*2))
count<-1
for(j in 1:length(data.s58))
{
  names(arima.s58)[[count]]<-paste((names(data.s58)[[j]]),".april",sep='')
  arima.s58[[count]]<-data.s58[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s58[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s58[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s58[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s58)[[count]]<-paste(names(data.s58)[[1]],".july",sep='')
arima.s58[[count]]<-data.s58[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s58[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s58[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s58[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s58
#Making a power curve

speed=as.double(s58[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s58[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s58[[1]]$power<-lookup(round_any(as.numeric(arima.s58[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s58[[2]]$power<-lookup(round_any(as.numeric(arima.s58[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s58<-vector("list",length(arima.s58))
count1<-1
count2<-length(arima.s58)/2 + 1
for(i in 1:length(data.s58))
{
  names(res.s58)[[count1]]<-paste((names(data.s58)[[i]]),".april",sep='')
  res.s58[[count1]]<-data.s58[[i]]$power["2011-04-01/2011-04-30"]-arima.s58[[count1]]$power
  count1<-count1+1
  
  names(res.s58)[[count2]]<-paste(names(data.s58)[[i]],".july",sep='')
  res.s58[[count2]]<-data.s58[[i]]$power["2011-07-01/2011-07-31"]-arima.s58[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s58)/2 + 1
for(i in 1:length(data.s58))
{
  
  plot(data.s58[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s58", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s58[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s58[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s58[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s58", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s58[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s58[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s58<-vector("list",length(arima.s58))
count1<-1
count2<-length(arima.s58)/2 + 1
for(i in 1:length(data.s58))
{
  names(mape.s58)[[count1]]<-paste((names(data.s58)[[i]]),".april",sep='')
  div<-res.s58[[count1]]/data.s58[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s58[[count1]]<-100*sum(abs(div))/length(data.s58[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s58)[[count2]]<-paste(names(data.s58)[[i]],".july",sep='')
  div<-res.s58[[count2]]/data.s58[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s58[[count2]]<-100*sum(abs(div))/nrow(data.s58[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s58))
{
  write.zoo(arima.s58[[i]], file = paste(names(arima.s58)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s58))
{
  write.zoo(data.s58[[i]], file = paste(names(data.s58)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s58))
{
  write.zoo(res.s58[[i]], file = paste(names(res.s58)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s58))
{
  write.zoo(mape.s58[[i]], file = paste(names(mape.s58)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 58 over -----
save.image()


#---------SITE 59 begin -----
#Reading files by zone
s59<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==59)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s59[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc59<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==59)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc59[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s59) <- sprintf('s59.%d', 1:numfiles)
for(i in 1:numfiles)
{s59[[i]]$Time <- with(s59[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s59))
{names(s59[[i]])[names(s59[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s59))
{names(s59[[i]])[names(s59[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s59.ts<-vector("list", length(s59))
names(s59.ts) <- sprintf('s59.%d', 1:numfiles)

for(i in 1:length(s59))
{
  s59.ts[[i]]<-xts(s59[[i]]$speed,s59[[i]]$Time)
  colnames(s59.ts[[i]])='speed'
  s59.ts[[i]]$power<-s59[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s59))
for(i in 1:length(s59))
{
  ep<-endpoints(s59.ts[[i]], on="hours", k=1)
  a<-(period.apply(s59.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s59.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s59 <- list()
maxindex<-which.max(CF1)
data.s59[[oldcount]]<-s59.ts[[which.max(CF1)]]
names(data.s59)[[oldcount]] <- names(s59.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc59 <- lapply( paste('sloc59.', 1:numfiles, sep=''), get)
names(sloc59) <- sprintf('sloc59.%d', 1:numfiles)

num<-seq(1,length(s59))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s59))
long1<-vector(length=length(s59))
for(i in 1:length(s59))
{
  lat1[i]<-sloc59[[i]]$V2[3]
  long1[i]<-sloc59[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s59))
{
  ep<-endpoints(data.s59[[i]], on="hours", k=1)
  data.s59[[i]]<-(period.apply(data.s59[[i]],ep,mean))
}

#sd(data.s59[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s59[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s59[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s59<-vector("list",(length(data.s59)*2))
count<-1
for(j in 1:length(data.s59))
{
  names(arima.s59)[[count]]<-paste((names(data.s59)[[j]]),".april",sep='')
  arima.s59[[count]]<-data.s59[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s59[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s59[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s59[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s59)[[count]]<-paste(names(data.s59)[[1]],".july",sep='')
arima.s59[[count]]<-data.s59[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s59[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s59[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s59[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s59
#Making a power curve

speed=as.double(s59[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s59[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s59[[1]]$power<-lookup(round_any(as.numeric(arima.s59[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s59[[2]]$power<-lookup(round_any(as.numeric(arima.s59[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s59<-vector("list",length(arima.s59))
count1<-1
count2<-length(arima.s59)/2 + 1
for(i in 1:length(data.s59))
{
  names(res.s59)[[count1]]<-paste((names(data.s59)[[i]]),".april",sep='')
  res.s59[[count1]]<-data.s59[[i]]$power["2011-04-01/2011-04-30"]-arima.s59[[count1]]$power
  count1<-count1+1
  
  names(res.s59)[[count2]]<-paste(names(data.s59)[[i]],".july",sep='')
  res.s59[[count2]]<-data.s59[[i]]$power["2011-07-01/2011-07-31"]-arima.s59[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s59)/2 + 1
for(i in 1:length(data.s59))
{
  
  plot(data.s59[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s59", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s59[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s59[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s59[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s59", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s59[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s59[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s59<-vector("list",length(arima.s59))
count1<-1
count2<-length(arima.s59)/2 + 1
for(i in 1:length(data.s59))
{
  names(mape.s59)[[count1]]<-paste((names(data.s59)[[i]]),".april",sep='')
  div<-res.s59[[count1]]/data.s59[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s59[[count1]]<-100*sum(abs(div))/length(data.s59[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s59)[[count2]]<-paste(names(data.s59)[[i]],".july",sep='')
  div<-res.s59[[count2]]/data.s59[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s59[[count2]]<-100*sum(abs(div))/nrow(data.s59[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s59))
{
  write.zoo(arima.s59[[i]], file = paste(names(arima.s59)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s59))
{
  write.zoo(data.s59[[i]], file = paste(names(data.s59)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s59))
{
  write.zoo(res.s59[[i]], file = paste(names(res.s59)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s59))
{
  write.zoo(mape.s59[[i]], file = paste(names(mape.s59)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 59 over -----
save.image()


#---------SITE 60 begin -----
#Reading files by zone
s60<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==60)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s60[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc60<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==60)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc60[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s60) <- sprintf('s60.%d', 1:numfiles)
for(i in 1:numfiles)
{s60[[i]]$Time <- with(s60[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s60))
{names(s60[[i]])[names(s60[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s60))
{names(s60[[i]])[names(s60[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s60.ts<-vector("list", length(s60))
names(s60.ts) <- sprintf('s60.%d', 1:numfiles)

for(i in 1:length(s60))
{
  s60.ts[[i]]<-xts(s60[[i]]$speed,s60[[i]]$Time)
  colnames(s60.ts[[i]])='speed'
  s60.ts[[i]]$power<-s60[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s60))
for(i in 1:length(s60))
{
  ep<-endpoints(s60.ts[[i]], on="hours", k=1)
  a<-(period.apply(s60.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s60.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s60 <- list()
maxindex<-which.max(CF1)
data.s60[[oldcount]]<-s60.ts[[which.max(CF1)]]
names(data.s60)[[oldcount]] <- names(s60.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc60 <- lapply( paste('sloc60.', 1:numfiles, sep=''), get)
names(sloc60) <- sprintf('sloc60.%d', 1:numfiles)

num<-seq(1,length(s60))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s60))
long1<-vector(length=length(s60))
for(i in 1:length(s60))
{
  lat1[i]<-sloc60[[i]]$V2[3]
  long1[i]<-sloc60[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s60))
{
  ep<-endpoints(data.s60[[i]], on="hours", k=1)
  data.s60[[i]]<-(period.apply(data.s60[[i]],ep,mean))
}

#sd(data.s60[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s60[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s60[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s60<-vector("list",(length(data.s60)*2))
count<-1
for(j in 1:length(data.s60))
{
  names(arima.s60)[[count]]<-paste((names(data.s60)[[j]]),".april",sep='')
  arima.s60[[count]]<-data.s60[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s60[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s60[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s60[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s60)[[count]]<-paste(names(data.s60)[[1]],".july",sep='')
arima.s60[[count]]<-data.s60[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s60[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s60[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s60[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s60
#Making a power curve

speed=as.double(s60[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s60[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s60[[1]]$power<-lookup(round_any(as.numeric(arima.s60[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s60[[2]]$power<-lookup(round_any(as.numeric(arima.s60[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s60<-vector("list",length(arima.s60))
count1<-1
count2<-length(arima.s60)/2 + 1
for(i in 1:length(data.s60))
{
  names(res.s60)[[count1]]<-paste((names(data.s60)[[i]]),".april",sep='')
  res.s60[[count1]]<-data.s60[[i]]$power["2011-04-01/2011-04-30"]-arima.s60[[count1]]$power
  count1<-count1+1
  
  names(res.s60)[[count2]]<-paste(names(data.s60)[[i]],".july",sep='')
  res.s60[[count2]]<-data.s60[[i]]$power["2011-07-01/2011-07-31"]-arima.s60[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s60)/2 + 1
for(i in 1:length(data.s60))
{
  
  plot(data.s60[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s60", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s60[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s60[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s60[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s60", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s60[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s60[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s60<-vector("list",length(arima.s60))
count1<-1
count2<-length(arima.s60)/2 + 1
for(i in 1:length(data.s60))
{
  names(mape.s60)[[count1]]<-paste((names(data.s60)[[i]]),".april",sep='')
  div<-res.s60[[count1]]/data.s60[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s60[[count1]]<-100*sum(abs(div))/length(data.s60[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s60)[[count2]]<-paste(names(data.s60)[[i]],".july",sep='')
  div<-res.s60[[count2]]/data.s60[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s60[[count2]]<-100*sum(abs(div))/nrow(data.s60[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s60))
{
  write.zoo(arima.s60[[i]], file = paste(names(arima.s60)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s60))
{
  write.zoo(data.s60[[i]], file = paste(names(data.s60)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s60))
{
  write.zoo(res.s60[[i]], file = paste(names(res.s60)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s60))
{
  write.zoo(mape.s60[[i]], file = paste(names(mape.s60)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 60 over -----
save.image()


#--------SITE 61 Begin----
#Reading files by zone
s61<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==61)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s61[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc61<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==61)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc61[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s61) <- sprintf('s61.%d', 1:numfiles)
for(i in 1:numfiles)
{s61[[i]]$Time <- with(s61[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s61))
{names(s61[[i]])[names(s61[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s61))
{names(s61[[i]])[names(s61[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s61.ts<-vector("list", length(s61))
names(s61.ts) <- sprintf('s61.%d', 1:numfiles)

for(i in 1:length(s61))
{
  s61.ts[[i]]<-xts(s61[[i]]$speed,s61[[i]]$Time)
  colnames(s61.ts[[i]])='speed'
  s61.ts[[i]]$power<-s61[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s61))
for(i in 1:length(s61))
{
  ep<-endpoints(s61.ts[[i]], on="hours", k=1)
  a<-(period.apply(s61.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s61.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s61 <- list()
maxindex<-which.max(CF1)
data.s61[[oldcount]]<-s61.ts[[which.max(CF1)]]
names(data.s61)[[oldcount]] <- names(s61.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc61 <- lapply( paste('sloc61.', 1:numfiles, sep=''), get)
names(sloc61) <- sprintf('sloc61.%d', 1:numfiles)

num<-seq(1,length(s61))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s61))
long1<-vector(length=length(s61))
for(i in 1:length(s61))
{
  lat1[i]<-sloc61[[i]]$V2[3]
  long1[i]<-sloc61[[i]]$V2[2]
}

#------Data collection for Site 1 over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s61))
{
  ep<-endpoints(data.s61[[i]], on="hours", k=1)
  data.s61[[i]]<-(period.apply(data.s61[[i]],ep,mean))
}

#sd(data.s61[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s61[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s61[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s61<-vector("list",(length(data.s61)*2))
count<-1
for(j in 1:length(data.s61))
{
  names(arima.s61)[[count]]<-paste((names(data.s61)[[j]]),".april",sep='')
  arima.s61[[count]]<-data.s61[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s61[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s61[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s61[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}



names(arima.s61)[[count]]<-paste(names(data.s61)[[1]],".july",sep='')
arima.s61[[count]]<-data.s61[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s61[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s61[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s61[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s61
#Making a power curve

speed=as.double(s61[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s61[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s61[[1]]$power<-lookup(round_any(as.numeric(arima.s61[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s61[[2]]$power<-lookup(round_any(as.numeric(arima.s61[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s61<-vector("list",length(arima.s61))
count1<-1
count2<-length(arima.s61)/2 + 1
for(i in 1:length(data.s61))
{
  names(res.s61)[[count1]]<-paste((names(data.s61)[[i]]),".april",sep='')
  res.s61[[count1]]<-data.s61[[i]]$power["2011-04-01/2011-04-30"]-arima.s61[[count1]]$power
  count1<-count1+1
  
  names(res.s61)[[count2]]<-paste(names(data.s61)[[i]],".july",sep='')
  res.s61[[count2]]<-data.s61[[i]]$power["2011-07-01/2011-07-31"]-arima.s61[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s61)/2 + 1
for(i in 1:length(data.s61))
{
  
  plot(data.s61[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s61", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s61[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s61[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s61[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s61", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s61[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s61[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s61<-vector("list",length(arima.s61))
count1<-1
count2<-length(arima.s61)/2 + 1
for(i in 1:length(data.s61))
{
  names(mape.s61)[[count1]]<-paste((names(data.s61)[[i]]),".april",sep='')
  div<-res.s61[[count1]]/data.s61[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s61[[count1]]<-100*sum(abs(div))/length(data.s61[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s61)[[count2]]<-paste(names(data.s61)[[i]],".july",sep='')
  div<-res.s61[[count2]]/data.s61[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s61[[count2]]<-100*sum(abs(div))/nrow(data.s61[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s61))
{
  write.zoo(arima.s61[[i]], file = paste(names(arima.s61)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s61))
{
  write.zoo(data.s61[[i]], file = paste(names(data.s61)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s61))
{
  write.zoo(res.s61[[i]], file = paste(names(res.s61)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s61))
{
  write.zoo(mape.s61[[i]], file = paste(names(mape.s61)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 61 over -----

save.image()


#---------SITE 62 begin -----
#Reading files by zone
s62<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==62)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s62[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc62<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==62)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc62[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s62) <- sprintf('s62.%d', 1:numfiles)
for(i in 1:numfiles)
{s62[[i]]$Time <- with(s62[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s62))
{names(s62[[i]])[names(s62[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s62))
{names(s62[[i]])[names(s62[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s62.ts<-vector("list", length(s62))
names(s62.ts) <- sprintf('s62.%d', 1:numfiles)

for(i in 1:length(s62))
{
  s62.ts[[i]]<-xts(s62[[i]]$speed,s62[[i]]$Time)
  colnames(s62.ts[[i]])='speed'
  s62.ts[[i]]$power<-s62[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s62))
for(i in 1:length(s62))
{
  ep<-endpoints(s62.ts[[i]], on="hours", k=1)
  a<-(period.apply(s62.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s62.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s62 <- list()
maxindex<-which.max(CF1)
data.s62[[oldcount]]<-s62.ts[[which.max(CF1)]]
names(data.s62)[[oldcount]] <- names(s62.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc62 <- lapply( paste('sloc62.', 1:numfiles, sep=''), get)
names(sloc62) <- sprintf('sloc62.%d', 1:numfiles)

num<-seq(1,length(s62))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s62))
long1<-vector(length=length(s62))
for(i in 1:length(s62))
{
  lat1[i]<-sloc62[[i]]$V2[3]
  long1[i]<-sloc62[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s62))
{
  ep<-endpoints(data.s62[[i]], on="hours", k=1)
  data.s62[[i]]<-(period.apply(data.s62[[i]],ep,mean))
}

#sd(data.s62[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s62[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s62[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s62<-vector("list",(length(data.s62)*2))
count<-1
for(j in 1:length(data.s62))
{
  names(arima.s62)[[count]]<-paste((names(data.s62)[[j]]),".april",sep='')
  arima.s62[[count]]<-data.s62[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s62[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s62[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s62[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s62)[[count]]<-paste(names(data.s62)[[1]],".july",sep='')
arima.s62[[count]]<-data.s62[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s62[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s62[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s62[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s62
#Making a power curve

speed=as.double(s62[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s62[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s62[[1]]$power<-lookup(round_any(as.numeric(arima.s62[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s62[[2]]$power<-lookup(round_any(as.numeric(arima.s62[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s62<-vector("list",length(arima.s62))
count1<-1
count2<-length(arima.s62)/2 + 1
for(i in 1:length(data.s62))
{
  names(res.s62)[[count1]]<-paste((names(data.s62)[[i]]),".april",sep='')
  res.s62[[count1]]<-data.s62[[i]]$power["2011-04-01/2011-04-30"]-arima.s62[[count1]]$power
  count1<-count1+1
  
  names(res.s62)[[count2]]<-paste(names(data.s62)[[i]],".july",sep='')
  res.s62[[count2]]<-data.s62[[i]]$power["2011-07-01/2011-07-31"]-arima.s62[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s62)/2 + 1
for(i in 1:length(data.s62))
{
  
  plot(data.s62[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s62", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s62[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s62[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s62[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s62", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s62[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s62[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s62<-vector("list",length(arima.s62))
count1<-1
count2<-length(arima.s62)/2 + 1
for(i in 1:length(data.s62))
{
  names(mape.s62)[[count1]]<-paste((names(data.s62)[[i]]),".april",sep='')
  div<-res.s62[[count1]]/data.s62[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s62[[count1]]<-100*sum(abs(div))/length(data.s62[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s62)[[count2]]<-paste(names(data.s62)[[i]],".july",sep='')
  div<-res.s62[[count2]]/data.s62[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s62[[count2]]<-100*sum(abs(div))/nrow(data.s62[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s62))
{
  write.zoo(arima.s62[[i]], file = paste(names(arima.s62)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s62))
{
  write.zoo(data.s62[[i]], file = paste(names(data.s62)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s62))
{
  write.zoo(res.s62[[i]], file = paste(names(res.s62)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s62))
{
  write.zoo(mape.s62[[i]], file = paste(names(mape.s62)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 62 over -----

save.image()


#---------SITE 63 begin -----
#Reading files by zone
s63<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==63)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s63[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc63<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==63)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc63[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s63) <- sprintf('s63.%d', 1:numfiles)
for(i in 1:numfiles)
{s63[[i]]$Time <- with(s63[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s63))
{names(s63[[i]])[names(s63[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s63))
{names(s63[[i]])[names(s63[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s63.ts<-vector("list", length(s63))
names(s63.ts) <- sprintf('s63.%d', 1:numfiles)

for(i in 1:length(s63))
{
  s63.ts[[i]]<-xts(s63[[i]]$speed,s63[[i]]$Time)
  colnames(s63.ts[[i]])='speed'
  s63.ts[[i]]$power<-s63[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s63))
for(i in 1:length(s63))
{
  ep<-endpoints(s63.ts[[i]], on="hours", k=1)
  a<-(period.apply(s63.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s63.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s63 <- list()
maxindex<-which.max(CF1)
data.s63[[oldcount]]<-s63.ts[[which.max(CF1)]]
names(data.s63)[[oldcount]] <- names(s63.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc63 <- lapply( paste('sloc63.', 1:numfiles, sep=''), get)
names(sloc63) <- sprintf('sloc63.%d', 1:numfiles)

num<-seq(1,length(s63))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s63))
long1<-vector(length=length(s63))
for(i in 1:length(s63))
{
  lat1[i]<-sloc63[[i]]$V2[3]
  long1[i]<-sloc63[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s63))
{
  ep<-endpoints(data.s63[[i]], on="hours", k=1)
  data.s63[[i]]<-(period.apply(data.s63[[i]],ep,mean))
}

#sd(data.s63[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s63[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s63[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s63<-vector("list",(length(data.s63)*2))
count<-1
for(j in 1:length(data.s63))
{
  names(arima.s63)[[count]]<-paste((names(data.s63)[[j]]),".april",sep='')
  arima.s63[[count]]<-data.s63[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s63[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s63[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s63[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s63)[[count]]<-paste(names(data.s63)[[1]],".july",sep='')
arima.s63[[count]]<-data.s63[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s63[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s63[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s63[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s63
#Making a power curve

speed=as.double(s63[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s63[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s63[[1]]$power<-lookup(round_any(as.numeric(arima.s63[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s63[[2]]$power<-lookup(round_any(as.numeric(arima.s63[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s63<-vector("list",length(arima.s63))
count1<-1
count2<-length(arima.s63)/2 + 1
for(i in 1:length(data.s63))
{
  names(res.s63)[[count1]]<-paste((names(data.s63)[[i]]),".april",sep='')
  res.s63[[count1]]<-data.s63[[i]]$power["2011-04-01/2011-04-30"]-arima.s63[[count1]]$power
  count1<-count1+1
  
  names(res.s63)[[count2]]<-paste(names(data.s63)[[i]],".july",sep='')
  res.s63[[count2]]<-data.s63[[i]]$power["2011-07-01/2011-07-31"]-arima.s63[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s63)/2 + 1
for(i in 1:length(data.s63))
{
  
  plot(data.s63[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s63", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s63[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s63[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s63[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s63", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s63[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s63[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s63<-vector("list",length(arima.s63))
count1<-1
count2<-length(arima.s63)/2 + 1
for(i in 1:length(data.s63))
{
  names(mape.s63)[[count1]]<-paste((names(data.s63)[[i]]),".april",sep='')
  div<-res.s63[[count1]]/data.s63[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s63[[count1]]<-100*sum(abs(div))/length(data.s63[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s63)[[count2]]<-paste(names(data.s63)[[i]],".july",sep='')
  div<-res.s63[[count2]]/data.s63[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s63[[count2]]<-100*sum(abs(div))/nrow(data.s63[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s63))
{
  write.zoo(arima.s63[[i]], file = paste(names(arima.s63)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s63))
{
  write.zoo(data.s63[[i]], file = paste(names(data.s63)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s63))
{
  write.zoo(res.s63[[i]], file = paste(names(res.s63)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s63))
{
  write.zoo(mape.s63[[i]], file = paste(names(mape.s63)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 63 over -----
save.image()



#---------SITE 64 begin -----
#Reading files by zone
s64<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==64)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s64[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc64<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==64)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc64[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s64) <- sprintf('s64.%d', 1:numfiles)
for(i in 1:numfiles)
{s64[[i]]$Time <- with(s64[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s64))
{names(s64[[i]])[names(s64[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s64))
{names(s64[[i]])[names(s64[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s64.ts<-vector("list", length(s64))
names(s64.ts) <- sprintf('s64.%d', 1:numfiles)

for(i in 1:length(s64))
{
  s64.ts[[i]]<-xts(s64[[i]]$speed,s64[[i]]$Time)
  colnames(s64.ts[[i]])='speed'
  s64.ts[[i]]$power<-s64[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s64))
for(i in 1:length(s64))
{
  ep<-endpoints(s64.ts[[i]], on="hours", k=1)
  a<-(period.apply(s64.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s64.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s64 <- list()
maxindex<-which.max(CF1)
data.s64[[oldcount]]<-s64.ts[[which.max(CF1)]]
names(data.s64)[[oldcount]] <- names(s64.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc64 <- lapply( paste('sloc64.', 1:numfiles, sep=''), get)
names(sloc64) <- sprintf('sloc64.%d', 1:numfiles)

num<-seq(1,length(s64))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s64))
long1<-vector(length=length(s64))
for(i in 1:length(s64))
{
  lat1[i]<-sloc64[[i]]$V2[3]
  long1[i]<-sloc64[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s64))
{
  ep<-endpoints(data.s64[[i]], on="hours", k=1)
  data.s64[[i]]<-(period.apply(data.s64[[i]],ep,mean))
}

#sd(data.s64[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s64[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s64[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s64<-vector("list",(length(data.s64)*2))
count<-1
for(j in 1:length(data.s64))
{
  names(arima.s64)[[count]]<-paste((names(data.s64)[[j]]),".april",sep='')
  arima.s64[[count]]<-data.s64[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s64[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s64[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s64[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s64)[[count]]<-paste(names(data.s64)[[1]],".july",sep='')
arima.s64[[count]]<-data.s64[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s64[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s64[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s64[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s64
#Making a power curve

speed=as.double(s64[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s64[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s64[[1]]$power<-lookup(round_any(as.numeric(arima.s64[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s64[[2]]$power<-lookup(round_any(as.numeric(arima.s64[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s64<-vector("list",length(arima.s64))
count1<-1
count2<-length(arima.s64)/2 + 1
for(i in 1:length(data.s64))
{
  names(res.s64)[[count1]]<-paste((names(data.s64)[[i]]),".april",sep='')
  res.s64[[count1]]<-data.s64[[i]]$power["2011-04-01/2011-04-30"]-arima.s64[[count1]]$power
  count1<-count1+1
  
  names(res.s64)[[count2]]<-paste(names(data.s64)[[i]],".july",sep='')
  res.s64[[count2]]<-data.s64[[i]]$power["2011-07-01/2011-07-31"]-arima.s64[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s64)/2 + 1
for(i in 1:length(data.s64))
{
  
  plot(data.s64[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s64", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s64[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s64[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s64[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s64", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s64[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s64[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s64<-vector("list",length(arima.s64))
count1<-1
count2<-length(arima.s64)/2 + 1
for(i in 1:length(data.s64))
{
  names(mape.s64)[[count1]]<-paste((names(data.s64)[[i]]),".april",sep='')
  div<-res.s64[[count1]]/data.s64[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s64[[count1]]<-100*sum(abs(div))/length(data.s64[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s64)[[count2]]<-paste(names(data.s64)[[i]],".july",sep='')
  div<-res.s64[[count2]]/data.s64[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s64[[count2]]<-100*sum(abs(div))/nrow(data.s64[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s64))
{
  write.zoo(arima.s64[[i]], file = paste(names(arima.s64)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s64))
{
  write.zoo(data.s64[[i]], file = paste(names(data.s64)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s64))
{
  write.zoo(res.s64[[i]], file = paste(names(res.s64)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s64))
{
  write.zoo(mape.s64[[i]], file = paste(names(mape.s64)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 64 over -----
save.image()

for (i in 1:length(data.s60))
{
  write.zoo(data.s60[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s60)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s60[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s60)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s59))
{
  write.zoo(data.s59[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s59)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s59[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s59)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s58))
{
  write.zoo(data.s58[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s58)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s58[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s58)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s57))
{
  write.zoo(data.s57[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s57)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s57[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s57)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s56))
{
  write.zoo(data.s56[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s56)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s56[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s56)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s55))
{
  write.zoo(data.s55[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s55)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s55[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s55)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s54))
{
  write.zoo(data.s54[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s54)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s54[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s54)[[i]],'-july power data.csv',sep=''),sep=",")
  
}


for (i in 1:length(data.s53))
{
  write.zoo(data.s53[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s53)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s53[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s53)[[i]],'-july power data.csv',sep=''),sep=",")
  
}


for (i in 1:length(data.s52))
{
  write.zoo(data.s52[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s52)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s52[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s52)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s51))
{
  write.zoo(data.s51[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s51)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s51[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s51)[[i]],'-july power data.csv',sep=''),sep=",")
  
}


for (i in 1:length(data.s64))
{
  write.zoo(data.s64[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s64)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s64[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s64)[[i]],'-july power data.csv',sep=''),sep=",")
  
}


for (i in 1:length(data.s63))
{
  write.zoo(data.s63[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s63)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s63[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s63)[[i]],'-july power data.csv',sep=''),sep=",")
  
}


for (i in 1:length(data.s62))
{
  write.zoo(data.s62[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s62)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s62[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s62)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s61))
{
  write.zoo(data.s61[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s61)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s61[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s61)[[i]],'-july power data.csv',sep=''),sep=",")
  
}