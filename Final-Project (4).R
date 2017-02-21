
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


#--------SITE 31 Begin----
#Reading files by zone
s31<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==31)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s31[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc31<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==31)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc31[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s31) <- sprintf('s31.%d', 1:numfiles)
for(i in 1:numfiles)
{s31[[i]]$Time <- with(s31[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s31))
{names(s31[[i]])[names(s31[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s31))
{names(s31[[i]])[names(s31[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s31.ts<-vector("list", length(s31))
names(s31.ts) <- sprintf('s31.%d', 1:numfiles)

for(i in 1:length(s31))
{
  s31.ts[[i]]<-xts(s31[[i]]$speed,s31[[i]]$Time)
  colnames(s31.ts[[i]])='speed'
  s31.ts[[i]]$power<-s31[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s31))
for(i in 1:length(s31))
{
  ep<-endpoints(s31.ts[[i]], on="hours", k=1)
  a<-(period.apply(s31.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s31.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s31 <- list()
maxindex<-which.max(CF1)
data.s31[[oldcount]]<-s31.ts[[which.max(CF1)]]
names(data.s31)[[oldcount]] <- names(s31.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc31 <- lapply( paste('sloc31.', 1:numfiles, sep=''), get)
names(sloc31) <- sprintf('sloc31.%d', 1:numfiles)

num<-seq(1,length(s31))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s31))
long1<-vector(length=length(s31))
for(i in 1:length(s31))
{
  lat1[i]<-sloc31[[i]]$V2[3]
  long1[i]<-sloc31[[i]]$V2[2]
}

#------Data collection for Site 1 over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s31))
{
  ep<-endpoints(data.s31[[i]], on="hours", k=1)
  data.s31[[i]]<-(period.apply(data.s31[[i]],ep,mean))
}

#sd(data.s31[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s31[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s31[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s31<-vector("list",(length(data.s31)*2))
count<-1
for(j in 1:length(data.s31))
{
  names(arima.s31)[[count]]<-paste((names(data.s31)[[j]]),".april",sep='')
  arima.s31[[count]]<-data.s31[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s31[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s31[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s31[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}



  names(arima.s31)[[count]]<-paste(names(data.s31)[[1]],".july",sep='')
  arima.s31[[count]]<-data.s31[[1]]$speed["2011-07-01/2011-07-31"]
  
  july<-data.s31[[1]]$speed["2011-07-01/2011-07-31"]
  
  for(i in 0:(length(data.s31[[1]]$speed["2011-07-01/2011-07-31"])-73))
  {
    a=i+1
    b=i+72
    example<-july$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s31[[count]][i+73]<-as.numeric(point.forecast)
  }


#backup<-arima.s31
#Making a power curve

speed=as.double(s31[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s31[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s31[[1]]$power<-lookup(round_any(as.numeric(arima.s31[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s31[[2]]$power<-lookup(round_any(as.numeric(arima.s31[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)


for(i in 1:nrow(arima.s31[[1]]))
{
  
  
  if(arima.s31[[1]]$speed[i]>=13.10)
  {
    arima.s31[[1]]$power[i]=16
  }
  
  if(arima.s31[[1]]$speed[i]<0)
  {
    arima.s31[[1]]$speed[i]=0
  }
  
  if(arima.s31[[1]]$speed[i]==0)
  {
    arima.s31[[1]]$power[i]=0
  }
  
}

for(i in 1:nrow(arima.s31[[2]]))
{
  if(arima.s31[[2]]$speed[i]>=13.10)
  {
    arima.s31[[2]]$power[i]=16
  }
  
  if(arima.s31[[2]]$speed[i]<0)
  {
    arima.s31[[2]]$speed[i]=0
  }
  
  if(arima.s31[[2]]$speed[i]==0)
  {
    arima.s31[[2]]$power[i]=0
  }
  
}



#Residuals
res.s31<-vector("list",length(arima.s31))
count1<-1
count2<-length(arima.s31)/2 + 1
for(i in 1:length(data.s31))
{
  names(res.s31)[[count1]]<-paste((names(data.s31)[[i]]),".april",sep='')
  res.s31[[count1]]<-data.s31[[i]]$power["2011-04-01/2011-04-30"]-arima.s31[[count1]]$power
  count1<-count1+1
  
  names(res.s31)[[count2]]<-paste(names(data.s31)[[i]],".july",sep='')
  res.s31[[count2]]<-data.s31[[i]]$power["2011-07-01/2011-07-31"]-arima.s31[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s31)/2 + 1
for(i in 1:length(data.s31))
{
  
  plot(data.s31[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s31", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s31[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s31[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s31[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s31", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s31[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s31[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s31<-vector("list",length(arima.s31))
count1<-1
count2<-length(arima.s31)/2 + 1
for(i in 1:length(data.s31))
{
  names(mape.s31)[[count1]]<-paste((names(data.s31)[[i]]),".april",sep='')
  div<-res.s31[[count1]]/data.s31[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s31[[count1]]<-100*sum(abs(div))/length(data.s31[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s31)[[count2]]<-paste(names(data.s31)[[i]],".july",sep='')
  div<-res.s31[[count2]]/data.s31[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s31[[count2]]<-100*sum(abs(div))/nrow(data.s31[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s31))
{
  write.zoo(arima.s31[[i]], file = paste(names(arima.s31)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s31))
{
  write.zoo(data.s31[[i]], file = paste(names(data.s31)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s31))
{
  write.zoo(res.s31[[i]], file = paste(names(res.s31)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s31))
{
  write.zoo(mape.s31[[i]], file = paste(names(mape.s31)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 31 over -----

save.image()


#---------SITE 32 begin (no turbine) -----
#Reading files by zone
s32<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==32)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s32[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc32<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==32)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc32[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s32) <- sprintf('s32.%d', 1:numfiles)
for(i in 1:numfiles)
{s32[[i]]$Time <- with(s32[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s32))
{names(s32[[i]])[names(s32[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s32))
{names(s32[[i]])[names(s32[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s32.ts<-vector("list", length(s32))
names(s32.ts) <- sprintf('s32.%d', 1:numfiles)

for(i in 1:length(s32))
{
  s32.ts[[i]]<-xts(s32[[i]]$speed,s32[[i]]$Time)
  colnames(s32.ts[[i]])='speed'
  s32.ts[[i]]$power<-s32[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s32))
for(i in 1:length(s32))
{
  ep<-endpoints(s32.ts[[i]], on="hours", k=1)
  a<-(period.apply(s32.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s32.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s32 <- list()
maxindex<-which.max(CF1)
data.s32[[oldcount]]<-s32.ts[[which.max(CF1)]]
names(data.s32)[[oldcount]] <- names(s32.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc32 <- lapply( paste('sloc32.', 1:numfiles, sep=''), get)
names(sloc32) <- sprintf('sloc32.%d', 1:numfiles)

num<-seq(1,length(s32))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s32))
long1<-vector(length=length(s32))
for(i in 1:length(s32))
{
  lat1[i]<-sloc32[[i]]$V2[3]
  long1[i]<-sloc32[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s32))
{
  ep<-endpoints(data.s32[[i]], on="hours", k=1)
  data.s32[[i]]<-(period.apply(data.s32[[i]],ep,mean))
}

#sd(data.s32[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s32[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s32[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s32<-vector("list",(length(data.s32)*2))
count<-1
for(j in 1:length(data.s32))
{
  names(arima.s32)[[count]]<-paste((names(data.s32)[[j]]),".april",sep='')
  arima.s32[[count]]<-data.s32[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s32[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s32[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s32[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s32)[[count]]<-paste(names(data.s32)[[1]],".july",sep='')
arima.s32[[count]]<-data.s32[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s32[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s32[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s32[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s32
#Making a power curve

speed=as.double(s32[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s32[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s32[[1]]$power<-lookup(round_any(as.numeric(arima.s32[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s32[[2]]$power<-lookup(round_any(as.numeric(arima.s32[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s32<-vector("list",length(arima.s32))
count1<-1
count2<-length(arima.s32)/2 + 1
for(i in 1:length(data.s32))
{
  names(res.s32)[[count1]]<-paste((names(data.s32)[[i]]),".april",sep='')
  res.s32[[count1]]<-data.s32[[i]]$power["2011-04-01/2011-04-30"]-arima.s32[[count1]]$power
  count1<-count1+1
  
  names(res.s32)[[count2]]<-paste(names(data.s32)[[i]],".july",sep='')
  res.s32[[count2]]<-data.s32[[i]]$power["2011-07-01/2011-07-31"]-arima.s32[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s32)/2 + 1
for(i in 1:length(data.s32))
{
  
  plot(data.s32[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s32", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s32[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s32[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s32[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s32", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s32[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s32[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s32<-vector("list",length(arima.s32))
count1<-1
count2<-length(arima.s32)/2 + 1
for(i in 1:length(data.s32))
{
  names(mape.s32)[[count1]]<-paste((names(data.s32)[[i]]),".april",sep='')
  div<-res.s32[[count1]]/data.s32[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s32[[count1]]<-100*sum(abs(div))/length(data.s32[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s32)[[count2]]<-paste(names(data.s32)[[i]],".july",sep='')
  div<-res.s32[[count2]]/data.s32[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s32[[count2]]<-100*sum(abs(div))/nrow(data.s32[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s32))
{
  write.zoo(arima.s32[[i]], file = paste(names(arima.s32)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s32))
{
  write.zoo(data.s32[[i]], file = paste(names(data.s32)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s32))
{
  write.zoo(res.s32[[i]], file = paste(names(res.s32)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s32))
{
  write.zoo(mape.s32[[i]], file = paste(names(mape.s32)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 32 over -----

save.image()


#---------SITE 33 begin -----
#Reading files by zone
s33<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==33)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s33[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc33<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==33)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc33[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s33) <- sprintf('s33.%d', 1:numfiles)
for(i in 1:numfiles)
{s33[[i]]$Time <- with(s33[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s33))
{names(s33[[i]])[names(s33[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s33))
{names(s33[[i]])[names(s33[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s33.ts<-vector("list", length(s33))
names(s33.ts) <- sprintf('s33.%d', 1:numfiles)

for(i in 1:length(s33))
{
  s33.ts[[i]]<-xts(s33[[i]]$speed,s33[[i]]$Time)
  colnames(s33.ts[[i]])='speed'
  s33.ts[[i]]$power<-s33[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s33))
for(i in 1:length(s33))
{
  ep<-endpoints(s33.ts[[i]], on="hours", k=1)
  a<-(period.apply(s33.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s33.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s33 <- list()
maxindex<-which.max(CF1)
data.s33[[oldcount]]<-s33.ts[[which.max(CF1)]]
names(data.s33)[[oldcount]] <- names(s33.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc33 <- lapply( paste('sloc33.', 1:numfiles, sep=''), get)
names(sloc33) <- sprintf('sloc33.%d', 1:numfiles)

num<-seq(1,length(s33))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s33))
long1<-vector(length=length(s33))
for(i in 1:length(s33))
{
  lat1[i]<-sloc33[[i]]$V2[3]
  long1[i]<-sloc33[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s33))
{
  ep<-endpoints(data.s33[[i]], on="hours", k=1)
  data.s33[[i]]<-(period.apply(data.s33[[i]],ep,mean))
}

#sd(data.s33[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s33[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s33[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s33<-vector("list",(length(data.s33)*2))
count<-1
for(j in 1:length(data.s33))
{
  names(arima.s33)[[count]]<-paste((names(data.s33)[[j]]),".april",sep='')
  arima.s33[[count]]<-data.s33[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s33[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s33[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s33[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s33)[[count]]<-paste(names(data.s33)[[1]],".july",sep='')
arima.s33[[count]]<-data.s33[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s33[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s33[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s33[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s33
#Making a power curve

speed=as.double(s33[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s33[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s33[[1]]$power<-lookup(round_any(as.numeric(arima.s33[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s33[[2]]$power<-lookup(round_any(as.numeric(arima.s33[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)


for(i in 1:nrow(arima.s31[[1]]))
{
  
  
  if(arima.s31[[1]]$speed[i]>=13.10)
  {
    arima.s31[[1]]$power[i]=16
  }
  
  if(arima.s31[[1]]$speed[i]<0)
  {
    arima.s31[[1]]$speed[i]=0
  }
  
  if(arima.s31[[1]]$speed[i]==0)
  {
    arima.s31[[1]]$power[i]=0
  }
  
}

for(i in 1:nrow(arima.s31[[2]]))
{
  if(arima.s31[[2]]$speed[i]>=13.10)
  {
    arima.s31[[2]]$power[i]=16
  }
  
  if(arima.s31[[2]]$speed[i]<0)
  {
    arima.s31[[2]]$speed[i]=0
  }
  
  if(arima.s31[[2]]$speed[i]==0)
  {
    arima.s31[[2]]$power[i]=0
  }
  
}

#Residuals
res.s33<-vector("list",length(arima.s33))
count1<-1
count2<-length(arima.s33)/2 + 1
for(i in 1:length(data.s33))
{
  names(res.s33)[[count1]]<-paste((names(data.s33)[[i]]),".april",sep='')
  res.s33[[count1]]<-data.s33[[i]]$power["2011-04-01/2011-04-30"]-arima.s33[[count1]]$power
  count1<-count1+1
  
  names(res.s33)[[count2]]<-paste(names(data.s33)[[i]],".july",sep='')
  res.s33[[count2]]<-data.s33[[i]]$power["2011-07-01/2011-07-31"]-arima.s33[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s33)/2 + 1
for(i in 1:length(data.s33))
{
  
  plot(data.s33[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s33", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s33[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s33[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s33[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s33", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s33[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s33[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s33<-vector("list",length(arima.s33))
count1<-1
count2<-length(arima.s33)/2 + 1
for(i in 1:length(data.s33))
{
  names(mape.s33)[[count1]]<-paste((names(data.s33)[[i]]),".april",sep='')
  div<-res.s33[[count1]]/data.s33[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s33[[count1]]<-100*sum(abs(div))/length(data.s33[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s33)[[count2]]<-paste(names(data.s33)[[i]],".july",sep='')
  div<-res.s33[[count2]]/data.s33[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s33[[count2]]<-100*sum(abs(div))/nrow(data.s33[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s33))
{
  write.zoo(arima.s33[[i]], file = paste(names(arima.s33)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s33))
{
  write.zoo(data.s33[[i]], file = paste(names(data.s33)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s33))
{
  write.zoo(res.s33[[i]], file = paste(names(res.s33)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s33))
{
  write.zoo(mape.s33[[i]], file = paste(names(mape.s33)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 33 over -----
save.image()



#---------SITE 34 begin -----
#Reading files by zone
s34<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==34)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s34[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc34<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==34)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc34[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s34) <- sprintf('s34.%d', 1:numfiles)
for(i in 1:numfiles)
{s34[[i]]$Time <- with(s34[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s34))
{names(s34[[i]])[names(s34[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s34))
{names(s34[[i]])[names(s34[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s34.ts<-vector("list", length(s34))
names(s34.ts) <- sprintf('s34.%d', 1:numfiles)

for(i in 1:length(s34))
{
  s34.ts[[i]]<-xts(s34[[i]]$speed,s34[[i]]$Time)
  colnames(s34.ts[[i]])='speed'
  s34.ts[[i]]$power<-s34[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s34))
for(i in 1:length(s34))
{
  ep<-endpoints(s34.ts[[i]], on="hours", k=1)
  a<-(period.apply(s34.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s34.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s34 <- list()
maxindex<-which.max(CF1)
data.s34[[oldcount]]<-s34.ts[[which.max(CF1)]]
names(data.s34)[[oldcount]] <- names(s34.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc34 <- lapply( paste('sloc34.', 1:numfiles, sep=''), get)
names(sloc34) <- sprintf('sloc34.%d', 1:numfiles)

num<-seq(1,length(s34))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s34))
long1<-vector(length=length(s34))
for(i in 1:length(s34))
{
  lat1[i]<-sloc34[[i]]$V2[3]
  long1[i]<-sloc34[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s34))
{
  ep<-endpoints(data.s34[[i]], on="hours", k=1)
  data.s34[[i]]<-(period.apply(data.s34[[i]],ep,mean))
}

#sd(data.s34[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s34[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s34[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s34<-vector("list",(length(data.s34)*2))
count<-1
for(j in 1:length(data.s34))
{
  names(arima.s34)[[count]]<-paste((names(data.s34)[[j]]),".april",sep='')
  arima.s34[[count]]<-data.s34[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s34[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s34[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s34[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s34)[[count]]<-paste(names(data.s34)[[1]],".july",sep='')
arima.s34[[count]]<-data.s34[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s34[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s34[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s34[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s34
#Making a power curve

speed=as.double(s34[[which.max(CF1)]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s34[[which.max(CF1)]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s34[[1]]$power<-lookup(round_any(as.numeric(arima.s34[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s34[[2]]$power<-lookup(round_any(as.numeric(arima.s34[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)


for(i in 1:nrow(arima.s34[[1]]))
{
  
  
  if(arima.s34[[1]]$speed[i]>=13)
  {
    arima.s34[[1]]$power[i]=16
  }
  
  if(arima.s34[[1]]$speed[i]<0)
  {
    arima.s34[[1]]$speed[i]=0
  }
  
  if(arima.s34[[1]]$speed[i]==0)
  {
    arima.s34[[1]]$power[i]=0
  }
  
}

for(i in 1:nrow(arima.s34[[2]]))
{
  if(arima.s34[[2]]$speed[i]>=13)
  {
    arima.s34[[2]]$power[i]=16
  }
  
  if(arima.s34[[2]]$speed[i]<0)
  {
    arima.s34[[2]]$speed[i]=0
  }
  
  if(arima.s34[[2]]$speed[i]==0)
  {
    arima.s34[[2]]$power[i]=0
  }
  
}

#Residuals
res.s34<-vector("list",length(arima.s34))
count1<-1
count2<-length(arima.s34)/2 + 1
for(i in 1:length(data.s34))
{
  names(res.s34)[[count1]]<-paste((names(data.s34)[[i]]),".april",sep='')
  res.s34[[count1]]<-data.s34[[i]]$power["2011-04-01/2011-04-30"]-arima.s34[[count1]]$power
  count1<-count1+1
  
  names(res.s34)[[count2]]<-paste(names(data.s34)[[i]],".july",sep='')
  res.s34[[count2]]<-data.s34[[i]]$power["2011-07-01/2011-07-31"]-arima.s34[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s34)/2 + 1
for(i in 1:length(data.s34))
{
  
  plot(data.s34[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s34", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s34[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s34[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s34[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s34", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s34[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s34[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s34<-vector("list",length(arima.s34))
count1<-1
count2<-length(arima.s34)/2 + 1
for(i in 1:length(data.s34))
{
  names(mape.s34)[[count1]]<-paste((names(data.s34)[[i]]),".april",sep='')
  div<-res.s34[[count1]]/data.s34[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s34[[count1]]<-100*sum(abs(div))/length(data.s34[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s34)[[count2]]<-paste(names(data.s34)[[i]],".july",sep='')
  div<-res.s34[[count2]]/data.s34[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s34[[count2]]<-100*sum(abs(div))/nrow(data.s34[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s34))
{
  write.zoo(arima.s34[[i]], file = paste(names(arima.s34)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s34))
{
  write.zoo(data.s34[[i]], file = paste(names(data.s34)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s34))
{
  write.zoo(res.s34[[i]], file = paste(names(res.s34)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s34))
{
  write.zoo(mape.s34[[i]], file = paste(names(mape.s34)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 34 over -----
save.image()


#---------SITE 35 begin -----
#Reading files by zone
s35<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==35)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s35[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc35<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==35)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc35[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s35) <- sprintf('s35.%d', 1:numfiles)
for(i in 1:numfiles)
{s35[[i]]$Time <- with(s35[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s35))
{names(s35[[i]])[names(s35[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s35))
{names(s35[[i]])[names(s35[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s35.ts<-vector("list", length(s35))
names(s35.ts) <- sprintf('s35.%d', 1:numfiles)

for(i in 1:length(s35))
{
  s35.ts[[i]]<-xts(s35[[i]]$speed,s35[[i]]$Time)
  colnames(s35.ts[[i]])='speed'
  s35.ts[[i]]$power<-s35[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s35))
for(i in 1:length(s35))
{
  ep<-endpoints(s35.ts[[i]], on="hours", k=1)
  a<-(period.apply(s35.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s35.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s35 <- list()
maxindex<-which.max(CF1)
data.s35[[oldcount]]<-s35.ts[[which.max(CF1)]]
names(data.s35)[[oldcount]] <- names(s35.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc35 <- lapply( paste('sloc35.', 1:numfiles, sep=''), get)
names(sloc35) <- sprintf('sloc35.%d', 1:numfiles)

num<-seq(1,length(s35))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s35))
long1<-vector(length=length(s35))
for(i in 1:length(s35))
{
  lat1[i]<-sloc35[[i]]$V2[3]
  long1[i]<-sloc35[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s35))
{
  ep<-endpoints(data.s35[[i]], on="hours", k=1)
  data.s35[[i]]<-(period.apply(data.s35[[i]],ep,mean))
}

#sd(data.s35[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s35[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s35[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s35<-vector("list",(length(data.s35)*2))
count<-1
for(j in 1:length(data.s35))
{
  names(arima.s35)[[count]]<-paste((names(data.s35)[[j]]),".april",sep='')
  arima.s35[[count]]<-data.s35[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s35[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s35[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s35[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s35)[[count]]<-paste(names(data.s35)[[1]],".july",sep='')
arima.s35[[count]]<-data.s35[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s35[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s35[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s35[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s35
#Making a power curve

speed=as.double(s35[[which.max(CF1)]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s35[[which.max(CF1)]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s35[[1]]$power<-lookup(round_any(as.numeric(arima.s35[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s35[[2]]$power<-lookup(round_any(as.numeric(arima.s35[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)


for(i in 1:nrow(arima.s35[[1]]))
{
  
  
  if(arima.s35[[1]]$speed[i]>=12.35)
  {
    arima.s35[[1]]$power[i]=4
  }
  
  if(arima.s35[[1]]$speed[i]<0)
  {
    arima.s35[[1]]$speed[i]=0
  }
  
  if(arima.s35[[1]]$speed[i]==0)
  {
    arima.s35[[1]]$power[i]=0
  }
  
}

for(i in 1:nrow(arima.s35[[2]]))
{
  if(arima.s35[[2]]$speed[i]>=12.35)
  {
    arima.s35[[2]]$power[i]=4
  }
  
  if(arima.s35[[2]]$speed[i]<0)
  {
    arima.s35[[2]]$speed[i]=0
  }
  
  if(arima.s35[[2]]$speed[i]==0)
  {
    arima.s35[[2]]$power[i]=0
  }
  
}


#Residuals
res.s35<-vector("list",length(arima.s35))
count1<-1
count2<-length(arima.s35)/2 + 1
for(i in 1:length(data.s35))
{
  names(res.s35)[[count1]]<-paste((names(data.s35)[[i]]),".april",sep='')
  res.s35[[count1]]<-data.s35[[i]]$power["2011-04-01/2011-04-30"]-arima.s35[[count1]]$power
  count1<-count1+1
  
  names(res.s35)[[count2]]<-paste(names(data.s35)[[i]],".july",sep='')
  res.s35[[count2]]<-data.s35[[i]]$power["2011-07-01/2011-07-31"]-arima.s35[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s35)/2 + 1
for(i in 1:length(data.s35))
{
  
  plot(data.s35[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s35", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s35[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s35[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s35[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s35", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s35[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s35[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s35<-vector("list",length(arima.s35))
count1<-1
count2<-length(arima.s35)/2 + 1
for(i in 1:length(data.s35))
{
  names(mape.s35)[[count1]]<-paste((names(data.s35)[[i]]),".april",sep='')
  div<-res.s35[[count1]]/data.s35[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s35[[count1]]<-100*sum(abs(div))/length(data.s35[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s35)[[count2]]<-paste(names(data.s35)[[i]],".july",sep='')
  div<-res.s35[[count2]]/data.s35[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s35[[count2]]<-100*sum(abs(div))/nrow(data.s35[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s35))
{
  write.zoo(arima.s35[[i]], file = paste(names(arima.s35)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s35))
{
  write.zoo(data.s35[[i]], file = paste(names(data.s35)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s35))
{
  write.zoo(res.s35[[i]], file = paste(names(res.s35)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s35))
{
  write.zoo(mape.s35[[i]], file = paste(names(mape.s35)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 35 over -----
save.image()


#---------SITE 36 begin -----
#Reading files by zone
s36<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==36)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s36[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc36<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==36)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc36[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s36) <- sprintf('s36.%d', 1:numfiles)
for(i in 1:numfiles)
{s36[[i]]$Time <- with(s36[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s36))
{names(s36[[i]])[names(s36[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s36))
{names(s36[[i]])[names(s36[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s36.ts<-vector("list", length(s36))
names(s36.ts) <- sprintf('s36.%d', 1:numfiles)

for(i in 1:length(s36))
{
  s36.ts[[i]]<-xts(s36[[i]]$speed,s36[[i]]$Time)
  colnames(s36.ts[[i]])='speed'
  s36.ts[[i]]$power<-s36[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s36))
for(i in 1:length(s36))
{
  ep<-endpoints(s36.ts[[i]], on="hours", k=1)
  a<-(period.apply(s36.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s36.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s36 <- list()
maxindex<-which.max(CF1)
data.s36[[oldcount]]<-s36.ts[[which.max(CF1)]]
names(data.s36)[[oldcount]] <- names(s36.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc36 <- lapply( paste('sloc36.', 1:numfiles, sep=''), get)
names(sloc36) <- sprintf('sloc36.%d', 1:numfiles)

num<-seq(1,length(s36))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s36))
long1<-vector(length=length(s36))
for(i in 1:length(s36))
{
  lat1[i]<-sloc36[[i]]$V2[3]
  long1[i]<-sloc36[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s36))
{
  ep<-endpoints(data.s36[[i]], on="hours", k=1)
  data.s36[[i]]<-(period.apply(data.s36[[i]],ep,mean))
}

#sd(data.s36[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s36[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s36[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s36<-vector("list",(length(data.s36)*2))
count<-1
for(j in 1:length(data.s36))
{
  names(arima.s36)[[count]]<-paste((names(data.s36)[[j]]),".april",sep='')
  arima.s36[[count]]<-data.s36[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s36[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s36[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s36[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s36)[[count]]<-paste(names(data.s36)[[1]],".july",sep='')
arima.s36[[count]]<-data.s36[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s36[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s36[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s36[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s36
#Making a power curve

speed=as.double(s36[[which.max(CF1)]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s36[[which.max(CF1)]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s36[[1]]$power<-lookup(round_any(as.numeric(arima.s36[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s36[[2]]$power<-lookup(round_any(as.numeric(arima.s36[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s36<-vector("list",length(arima.s36))
count1<-1
count2<-length(arima.s36)/2 + 1
for(i in 1:length(data.s36))
{
  names(res.s36)[[count1]]<-paste((names(data.s36)[[i]]),".april",sep='')
  res.s36[[count1]]<-data.s36[[i]]$power["2011-04-01/2011-04-30"]-arima.s36[[count1]]$power
  count1<-count1+1
  
  names(res.s36)[[count2]]<-paste(names(data.s36)[[i]],".july",sep='')
  res.s36[[count2]]<-data.s36[[i]]$power["2011-07-01/2011-07-31"]-arima.s36[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s36)/2 + 1
for(i in 1:length(data.s36))
{
  
  plot(data.s36[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s36", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s36[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s36[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s36[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s36", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s36[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s36[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s36<-vector("list",length(arima.s36))
count1<-1
count2<-length(arima.s36)/2 + 1
for(i in 1:length(data.s36))
{
  names(mape.s36)[[count1]]<-paste((names(data.s36)[[i]]),".april",sep='')
  div<-res.s36[[count1]]/data.s36[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s36[[count1]]<-100*sum(abs(div))/length(data.s36[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s36)[[count2]]<-paste(names(data.s36)[[i]],".july",sep='')
  div<-res.s36[[count2]]/data.s36[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s36[[count2]]<-100*sum(abs(div))/nrow(data.s36[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s36))
{
  write.zoo(arima.s36[[i]], file = paste(names(arima.s36)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s36))
{
  write.zoo(data.s36[[i]], file = paste(names(data.s36)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s36))
{
  write.zoo(res.s36[[i]], file = paste(names(res.s36)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s36))
{
  write.zoo(mape.s36[[i]], file = paste(names(mape.s36)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 36 over -----
save.image()


#---------SITE 37 begin -----
#Reading files by zone
s37<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==37)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s37[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc37<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==37)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc37[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s37) <- sprintf('s37.%d', 1:numfiles)
for(i in 1:numfiles)
{s37[[i]]$Time <- with(s37[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s37))
{names(s37[[i]])[names(s37[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s37))
{names(s37[[i]])[names(s37[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s37.ts<-vector("list", length(s37))
names(s37.ts) <- sprintf('s37.%d', 1:numfiles)

for(i in 1:length(s37))
{
  s37.ts[[i]]<-xts(s37[[i]]$speed,s37[[i]]$Time)
  colnames(s37.ts[[i]])='speed'
  s37.ts[[i]]$power<-s37[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s37))
for(i in 1:length(s37))
{
  ep<-endpoints(s37.ts[[i]], on="hours", k=1)
  a<-(period.apply(s37.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s37.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s37 <- list()
maxindex<-which.max(CF1)
data.s37[[oldcount]]<-s37.ts[[which.max(CF1)]]
names(data.s37)[[oldcount]] <- names(s37.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc37 <- lapply( paste('sloc37.', 1:numfiles, sep=''), get)
names(sloc37) <- sprintf('sloc37.%d', 1:numfiles)

num<-seq(1,length(s37))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s37))
long1<-vector(length=length(s37))
for(i in 1:length(s37))
{
  lat1[i]<-sloc37[[i]]$V2[3]
  long1[i]<-sloc37[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s37))
{
  ep<-endpoints(data.s37[[i]], on="hours", k=1)
  data.s37[[i]]<-(period.apply(data.s37[[i]],ep,mean))
}

#sd(data.s37[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s37[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s37[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s37<-vector("list",(length(data.s37)*2))
count<-1
for(j in 1:length(data.s37))
{
  names(arima.s37)[[count]]<-paste((names(data.s37)[[j]]),".april",sep='')
  arima.s37[[count]]<-data.s37[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s37[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s37[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s37[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s37)[[count]]<-paste(names(data.s37)[[1]],".july",sep='')
arima.s37[[count]]<-data.s37[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s37[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s37[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s37[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s37
#Making a power curve

speed=as.double(s37[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s37[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s37[[1]]$power<-lookup(round_any(as.numeric(arima.s37[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s37[[2]]$power<-lookup(round_any(as.numeric(arima.s37[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s37<-vector("list",length(arima.s37))
count1<-1
count2<-length(arima.s37)/2 + 1
for(i in 1:length(data.s37))
{
  names(res.s37)[[count1]]<-paste((names(data.s37)[[i]]),".april",sep='')
  res.s37[[count1]]<-data.s37[[i]]$power["2011-04-01/2011-04-30"]-arima.s37[[count1]]$power
  count1<-count1+1
  
  names(res.s37)[[count2]]<-paste(names(data.s37)[[i]],".july",sep='')
  res.s37[[count2]]<-data.s37[[i]]$power["2011-07-01/2011-07-31"]-arima.s37[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s37)/2 + 1
for(i in 1:length(data.s37))
{
  
  plot(data.s37[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s37", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s37[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s37[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s37[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s37", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s37[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s37[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s37<-vector("list",length(arima.s37))
count1<-1
count2<-length(arima.s37)/2 + 1
for(i in 1:length(data.s37))
{
  names(mape.s37)[[count1]]<-paste((names(data.s37)[[i]]),".april",sep='')
  div<-res.s37[[count1]]/data.s37[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s37[[count1]]<-100*sum(abs(div))/length(data.s37[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s37)[[count2]]<-paste(names(data.s37)[[i]],".july",sep='')
  div<-res.s37[[count2]]/data.s37[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s37[[count2]]<-100*sum(abs(div))/nrow(data.s37[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s37))
{
  write.zoo(arima.s37[[i]], file = paste(names(arima.s37)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s37))
{
  write.zoo(data.s37[[i]], file = paste(names(data.s37)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s37))
{
  write.zoo(res.s37[[i]], file = paste(names(res.s37)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s37))
{
  write.zoo(mape.s37[[i]], file = paste(names(mape.s37)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 37 over -----
save.image()


#---------SITE 38 begin -----
#Reading files by zone
s38<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==38)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s38[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc38<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==38)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc38[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s38) <- sprintf('s38.%d', 1:numfiles)
for(i in 1:numfiles)
{s38[[i]]$Time <- with(s38[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s38))
{names(s38[[i]])[names(s38[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s38))
{names(s38[[i]])[names(s38[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s38.ts<-vector("list", length(s38))
names(s38.ts) <- sprintf('s38.%d', 1:numfiles)

for(i in 1:length(s38))
{
  s38.ts[[i]]<-xts(s38[[i]]$speed,s38[[i]]$Time)
  colnames(s38.ts[[i]])='speed'
  s38.ts[[i]]$power<-s38[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s38))
for(i in 1:length(s38))
{
  ep<-endpoints(s38.ts[[i]], on="hours", k=1)
  a<-(period.apply(s38.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s38.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s38 <- list()
maxindex<-which.max(CF1)
data.s38[[oldcount]]<-s38.ts[[which.max(CF1)]]
names(data.s38)[[oldcount]] <- names(s38.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc38 <- lapply( paste('sloc38.', 1:numfiles, sep=''), get)
names(sloc38) <- sprintf('sloc38.%d', 1:numfiles)

num<-seq(1,length(s38))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s38))
long1<-vector(length=length(s38))
for(i in 1:length(s38))
{
  lat1[i]<-sloc38[[i]]$V2[3]
  long1[i]<-sloc38[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s38))
{
  ep<-endpoints(data.s38[[i]], on="hours", k=1)
  data.s38[[i]]<-(period.apply(data.s38[[i]],ep,mean))
}

#sd(data.s38[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s38[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s38[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s38<-vector("list",(length(data.s38)*2))
count<-1
for(j in 1:length(data.s38))
{
  names(arima.s38)[[count]]<-paste((names(data.s38)[[j]]),".april",sep='')
  arima.s38[[count]]<-data.s38[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s38[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s38[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s38[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s38)[[count]]<-paste(names(data.s38)[[1]],".july",sep='')
arima.s38[[count]]<-data.s38[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s38[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s38[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s38[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s38
#Making a power curve

speed=as.double(s38[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s38[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s38[[1]]$power<-lookup(round_any(as.numeric(arima.s38[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s38[[2]]$power<-lookup(round_any(as.numeric(arima.s38[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s38<-vector("list",length(arima.s38))
count1<-1
count2<-length(arima.s38)/2 + 1
for(i in 1:length(data.s38))
{
  names(res.s38)[[count1]]<-paste((names(data.s38)[[i]]),".april",sep='')
  res.s38[[count1]]<-data.s38[[i]]$power["2011-04-01/2011-04-30"]-arima.s38[[count1]]$power
  count1<-count1+1
  
  names(res.s38)[[count2]]<-paste(names(data.s38)[[i]],".july",sep='')
  res.s38[[count2]]<-data.s38[[i]]$power["2011-07-01/2011-07-31"]-arima.s38[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s38)/2 + 1
for(i in 1:length(data.s38))
{
  
  plot(data.s38[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s38", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s38[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s38[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s38[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s38", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s38[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s38[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s38<-vector("list",length(arima.s38))
count1<-1
count2<-length(arima.s38)/2 + 1
for(i in 1:length(data.s38))
{
  names(mape.s38)[[count1]]<-paste((names(data.s38)[[i]]),".april",sep='')
  div<-res.s38[[count1]]/data.s38[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s38[[count1]]<-100*sum(abs(div))/length(data.s38[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s38)[[count2]]<-paste(names(data.s38)[[i]],".july",sep='')
  div<-res.s38[[count2]]/data.s38[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s38[[count2]]<-100*sum(abs(div))/nrow(data.s38[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s38))
{
  write.zoo(arima.s38[[i]], file = paste(names(arima.s38)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s38))
{
  write.zoo(data.s38[[i]], file = paste(names(data.s38)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s38))
{
  write.zoo(res.s38[[i]], file = paste(names(res.s38)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s38))
{
  write.zoo(mape.s38[[i]], file = paste(names(mape.s38)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 38 over -----
save.image()


#---------SITE 39 begin -----
#Reading files by zone
s39<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==39)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s39[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc39<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==39)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc39[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s39) <- sprintf('s39.%d', 1:numfiles)
for(i in 1:numfiles)
{s39[[i]]$Time <- with(s39[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s39))
{names(s39[[i]])[names(s39[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s39))
{names(s39[[i]])[names(s39[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s39.ts<-vector("list", length(s39))
names(s39.ts) <- sprintf('s39.%d', 1:numfiles)

for(i in 1:length(s39))
{
  s39.ts[[i]]<-xts(s39[[i]]$speed,s39[[i]]$Time)
  colnames(s39.ts[[i]])='speed'
  s39.ts[[i]]$power<-s39[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s39))
for(i in 1:length(s39))
{
  ep<-endpoints(s39.ts[[i]], on="hours", k=1)
  a<-(period.apply(s39.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s39.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s39 <- list()
maxindex<-which.max(CF1)
data.s39[[oldcount]]<-s39.ts[[which.max(CF1)]]
names(data.s39)[[oldcount]] <- names(s39.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc39 <- lapply( paste('sloc39.', 1:numfiles, sep=''), get)
names(sloc39) <- sprintf('sloc39.%d', 1:numfiles)

num<-seq(1,length(s39))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s39))
long1<-vector(length=length(s39))
for(i in 1:length(s39))
{
  lat1[i]<-sloc39[[i]]$V2[3]
  long1[i]<-sloc39[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s39))
{
  ep<-endpoints(data.s39[[i]], on="hours", k=1)
  data.s39[[i]]<-(period.apply(data.s39[[i]],ep,mean))
}

#sd(data.s39[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s39[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s39[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s39<-vector("list",(length(data.s39)*2))
count<-1
for(j in 1:length(data.s39))
{
  names(arima.s39)[[count]]<-paste((names(data.s39)[[j]]),".april",sep='')
  arima.s39[[count]]<-data.s39[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s39[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s39[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s39[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s39)[[count]]<-paste(names(data.s39)[[1]],".july",sep='')
arima.s39[[count]]<-data.s39[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s39[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s39[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s39[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s39
#Making a power curve

speed=as.double(s39[[which.max(CF1)]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s39[[which.max(CF1)]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s39[[1]]$power<-lookup(round_any(as.numeric(arima.s39[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s39[[2]]$power<-lookup(round_any(as.numeric(arima.s39[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)


for(i in 1:nrow(arima.s39[[1]]))
{
  
  
  if(arima.s39[[1]]$speed[i]>=12.85)
  {
    arima.s39[[1]]$power[i]=8
  }
  
  if(arima.s39[[1]]$speed[i]<0)
  {
    arima.s39[[1]]$speed[i]=0
  }
  
  if(arima.s39[[1]]$speed[i]==0)
  {
    arima.s39[[1]]$power[i]=0
  }
  
}

for(i in 1:nrow(arima.s39[[2]]))
{
  if(arima.s39[[2]]$speed[i]>=12.85)
  {
    arima.s39[[2]]$power[i]=9
  }
  
  if(arima.s39[[2]]$speed[i]<0)
  {
    arima.s39[[2]]$speed[i]=0
  }
  
  if(arima.s39[[2]]$speed[i]==0)
  {
    arima.s39[[2]]$power[i]=0
  }
  
}


#Residuals
res.s39<-vector("list",length(arima.s39))
count1<-1
count2<-length(arima.s39)/2 + 1
for(i in 1:length(data.s39))
{
  names(res.s39)[[count1]]<-paste((names(data.s39)[[i]]),".april",sep='')
  res.s39[[count1]]<-data.s39[[i]]$power["2011-04-01/2011-04-30"]-arima.s39[[count1]]$power
  count1<-count1+1
  
  names(res.s39)[[count2]]<-paste(names(data.s39)[[i]],".july",sep='')
  res.s39[[count2]]<-data.s39[[i]]$power["2011-07-01/2011-07-31"]-arima.s39[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s39)/2 + 1
for(i in 1:length(data.s39))
{
  
  plot(data.s39[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s39", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s39[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s39[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s39[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s39", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s39[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s39[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s39<-vector("list",length(arima.s39))
count1<-1
count2<-length(arima.s39)/2 + 1
for(i in 1:length(data.s39))
{
  names(mape.s39)[[count1]]<-paste((names(data.s39)[[i]]),".april",sep='')
  div<-res.s39[[count1]]/data.s39[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s39[[count1]]<-100*sum(abs(div))/length(data.s39[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s39)[[count2]]<-paste(names(data.s39)[[i]],".july",sep='')
  div<-res.s39[[count2]]/data.s39[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s39[[count2]]<-100*sum(abs(div))/nrow(data.s39[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s39))
{
  write.zoo(arima.s39[[i]], file = paste(names(arima.s39)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s39))
{
  write.zoo(data.s39[[i]], file = paste(names(data.s39)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s39))
{
  write.zoo(res.s39[[i]], file = paste(names(res.s39)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s39))
{
  write.zoo(mape.s39[[i]], file = paste(names(mape.s39)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 39 over -----
save.image()


#---------SITE 40 begin -----
#Reading files by zone
s40<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==40)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s40[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc40<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==40)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc40[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s40) <- sprintf('s40.%d', 1:numfiles)
for(i in 1:numfiles)
{s40[[i]]$Time <- with(s40[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s40))
{names(s40[[i]])[names(s40[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s40))
{names(s40[[i]])[names(s40[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s40.ts<-vector("list", length(s40))
names(s40.ts) <- sprintf('s40.%d', 1:numfiles)

for(i in 1:length(s40))
{
  s40.ts[[i]]<-xts(s40[[i]]$speed,s40[[i]]$Time)
  colnames(s40.ts[[i]])='speed'
  s40.ts[[i]]$power<-s40[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s40))
for(i in 1:length(s40))
{
  ep<-endpoints(s40.ts[[i]], on="hours", k=1)
  a<-(period.apply(s40.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s40.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s40 <- list()
maxindex<-which.max(CF1)
data.s40[[oldcount]]<-s40.ts[[which.max(CF1)]]
names(data.s40)[[oldcount]] <- names(s40.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc40 <- lapply( paste('sloc40.', 1:numfiles, sep=''), get)
names(sloc40) <- sprintf('sloc40.%d', 1:numfiles)

num<-seq(1,length(s40))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s40))
long1<-vector(length=length(s40))
for(i in 1:length(s40))
{
  lat1[i]<-sloc40[[i]]$V2[3]
  long1[i]<-sloc40[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s40))
{
  ep<-endpoints(data.s40[[i]], on="hours", k=1)
  data.s40[[i]]<-(period.apply(data.s40[[i]],ep,mean))
}

#sd(data.s40[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s40[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s40[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s40<-vector("list",(length(data.s40)*2))
count<-1
for(j in 1:length(data.s40))
{
  names(arima.s40)[[count]]<-paste((names(data.s40)[[j]]),".april",sep='')
  arima.s40[[count]]<-data.s40[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s40[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s40[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s40[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s40)[[count]]<-paste(names(data.s40)[[1]],".july",sep='')
arima.s40[[count]]<-data.s40[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s40[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s40[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s40[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s40
#Making a power curve

speed=as.double(s40[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s40[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s40[[1]]$power<-lookup(round_any(as.numeric(arima.s40[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s40[[2]]$power<-lookup(round_any(as.numeric(arima.s40[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s40<-vector("list",length(arima.s40))
count1<-1
count2<-length(arima.s40)/2 + 1
for(i in 1:length(data.s40))
{
  names(res.s40)[[count1]]<-paste((names(data.s40)[[i]]),".april",sep='')
  res.s40[[count1]]<-data.s40[[i]]$power["2011-04-01/2011-04-30"]-arima.s40[[count1]]$power
  count1<-count1+1
  
  names(res.s40)[[count2]]<-paste(names(data.s40)[[i]],".july",sep='')
  res.s40[[count2]]<-data.s40[[i]]$power["2011-07-01/2011-07-31"]-arima.s40[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s40)/2 + 1
for(i in 1:length(data.s40))
{
  
  plot(data.s40[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s40", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s40[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s40[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s40[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s40", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s40[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s40[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s40<-vector("list",length(arima.s40))
count1<-1
count2<-length(arima.s40)/2 + 1
for(i in 1:length(data.s40))
{
  names(mape.s40)[[count1]]<-paste((names(data.s40)[[i]]),".april",sep='')
  div<-res.s40[[count1]]/data.s40[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s40[[count1]]<-100*sum(abs(div))/length(data.s40[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s40)[[count2]]<-paste(names(data.s40)[[i]],".july",sep='')
  div<-res.s40[[count2]]/data.s40[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s40[[count2]]<-100*sum(abs(div))/nrow(data.s40[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s40))
{
  write.zoo(arima.s40[[i]], file = paste(names(arima.s40)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s40))
{
  write.zoo(data.s40[[i]], file = paste(names(data.s40)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s40))
{
  write.zoo(res.s40[[i]], file = paste(names(res.s40)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s40))
{
  write.zoo(mape.s40[[i]], file = paste(names(mape.s40)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 40 over -----
save.image()

for (i in 1:length(data.s40))
{
  write.zoo(data.s40[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s40)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s40[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s40)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s39))
{
  write.zoo(data.s39[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s39)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s39[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s39)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s38))
{
  write.zoo(data.s38[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s38)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s38[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s38)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s37))
{
  write.zoo(data.s37[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s37)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s37[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s37)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s36))
{
  write.zoo(data.s36[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s36)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s36[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s36)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s35))
{
  write.zoo(data.s35[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s35)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s35[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s35)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s34))
{
  write.zoo(data.s34[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s34)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s34[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s34)[[i]],'-july power data.csv',sep=''),sep=",")
  
}


for (i in 1:length(data.s33))
{
  write.zoo(data.s33[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s33)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s33[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s33)[[i]],'-july power data.csv',sep=''),sep=",")
  
}


for (i in 1:length(data.s32))
{
  write.zoo(data.s32[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s32)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s32[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s32)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s31))
{
  write.zoo(data.s31[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s31)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s31[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s31)[[i]],'-july power data.csv',sep=''),sep=",")
  
}