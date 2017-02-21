
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


#--------SITE 11 Begin----
#Reading files by zone
s11<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==11)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s11[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc11<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==11)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc11[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s11) <- sprintf('s11.%d', 1:numfiles)
for(i in 1:numfiles)
{s11[[i]]$Time <- with(s11[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s11))
{names(s11[[i]])[names(s11[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s11))
{names(s11[[i]])[names(s11[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s11.ts<-vector("list", length(s11))
names(s11.ts) <- sprintf('s11.%d', 1:numfiles)

for(i in 1:length(s11))
{
  s11.ts[[i]]<-xts(s11[[i]]$speed,s11[[i]]$Time)
  colnames(s11.ts[[i]])='speed'
  s11.ts[[i]]$power<-s11[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s11))
for(i in 1:length(s11))
{
  ep<-endpoints(s11.ts[[i]], on="hours", k=1)
  a<-(period.apply(s11.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s11.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s11 <- list()
maxindex<-which.max(CF1)
data.s11[[oldcount]]<-s11.ts[[which.max(CF1)]]
names(data.s11)[[oldcount]] <- names(s11.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc11 <- lapply( paste('sloc11.', 1:numfiles, sep=''), get)
names(sloc11) <- sprintf('sloc11.%d', 1:numfiles)

num<-seq(1,length(s11))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s11))
long1<-vector(length=length(s11))
for(i in 1:length(s11))
{
  lat1[i]<-sloc11[[i]]$V2[3]
  long1[i]<-sloc11[[i]]$V2[2]
}

#------Data collection for Site 1 over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s11))
{
  ep<-endpoints(data.s11[[i]], on="hours", k=1)
  data.s11[[i]]<-(period.apply(data.s11[[i]],ep,mean))
}

#sd(data.s11[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s11[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s11[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s11<-vector("list",(length(data.s11)*2))
count<-1
for(j in 1:length(data.s11))
{
  names(arima.s11)[[count]]<-paste((names(data.s11)[[j]]),".april",sep='')
  arima.s11[[count]]<-data.s11[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s11[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s11[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s11[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}



  names(arima.s11)[[count]]<-paste(names(data.s11)[[1]],".july",sep='')
  arima.s11[[count]]<-data.s11[[1]]$speed["2011-07-01/2011-07-31"]
  
  july<-data.s11[[1]]$speed["2011-07-01/2011-07-31"]
  
  for(i in 0:(length(data.s11[[1]]$speed["2011-07-01/2011-07-31"])-73))
  {
    a=i+1
    b=i+72
    example<-july$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s11[[count]][i+73]<-as.numeric(point.forecast)
  }


#backup<-arima.s11
#Making a power curve

speed=as.double(s11[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s11[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s11[[1]]$power<-lookup(round_any(as.numeric(arima.s11[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s11[[2]]$power<-lookup(round_any(as.numeric(arima.s11[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s11<-vector("list",length(arima.s11))
count1<-1
count2<-length(arima.s11)/2 + 1
for(i in 1:length(data.s11))
{
  names(res.s11)[[count1]]<-paste((names(data.s11)[[i]]),".april",sep='')
  res.s11[[count1]]<-data.s11[[i]]$power["2011-04-01/2011-04-30"]-arima.s11[[count1]]$power
  count1<-count1+1
  
  names(res.s11)[[count2]]<-paste(names(data.s11)[[i]],".july",sep='')
  res.s11[[count2]]<-data.s11[[i]]$power["2011-07-01/2011-07-31"]-arima.s11[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s11)/2 + 1
for(i in 1:length(data.s11))
{
  
  plot(data.s11[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s11", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s11[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s11[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s11[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s11", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s11[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s11[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s11<-vector("list",length(arima.s11))
count1<-1
count2<-length(arima.s11)/2 + 1
for(i in 1:length(data.s11))
{
  names(mape.s11)[[count1]]<-paste((names(data.s11)[[i]]),".april",sep='')
  div<-res.s11[[count1]]/data.s11[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s11[[count1]]<-100*sum(abs(div))/length(data.s11[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s11)[[count2]]<-paste(names(data.s11)[[i]],".july",sep='')
  div<-res.s11[[count2]]/data.s11[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s11[[count2]]<-100*sum(abs(div))/nrow(data.s11[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s11))
{
  write.zoo(arima.s11[[i]], file = paste(names(arima.s11)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s11))
{
  write.zoo(data.s11[[i]], file = paste(names(data.s11)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s11))
{
  write.zoo(res.s11[[i]], file = paste(names(res.s11)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s11))
{
  write.zoo(mape.s11[[i]], file = paste(names(mape.s11)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 11 over -----

save.image()

#---------SITE 12 begin -----
#Reading files by zone
s12<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==12)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s12[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc12<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==12)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc12[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s12) <- sprintf('s12.%d', 1:numfiles)
for(i in 1:numfiles)
{s12[[i]]$Time <- with(s12[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s12))
{names(s12[[i]])[names(s12[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s12))
{names(s12[[i]])[names(s12[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s12.ts<-vector("list", length(s12))
names(s12.ts) <- sprintf('s12.%d', 1:numfiles)

for(i in 1:length(s12))
{
  s12.ts[[i]]<-xts(s12[[i]]$speed,s12[[i]]$Time)
  colnames(s12.ts[[i]])='speed'
  s12.ts[[i]]$power<-s12[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s12))
for(i in 1:length(s12))
{
  ep<-endpoints(s12.ts[[i]], on="hours", k=1)
  a<-(period.apply(s12.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s12.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s12 <- list()
maxindex<-which.max(CF1)
data.s12[[oldcount]]<-s12.ts[[which.max(CF1)]]
names(data.s12)[[oldcount]] <- names(s12.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc12 <- lapply( paste('sloc12.', 1:numfiles, sep=''), get)
names(sloc12) <- sprintf('sloc12.%d', 1:numfiles)

num<-seq(1,length(s12))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s12))
long1<-vector(length=length(s12))
for(i in 1:length(s12))
{
  lat1[i]<-sloc12[[i]]$V2[3]
  long1[i]<-sloc12[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s12))
{
  ep<-endpoints(data.s12[[i]], on="hours", k=1)
  data.s12[[i]]<-(period.apply(data.s12[[i]],ep,mean))
}

#sd(data.s12[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s12[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s12[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s12<-vector("list",(length(data.s12)*2))
count<-1
for(j in 1:length(data.s12))
{
  names(arima.s12)[[count]]<-paste((names(data.s12)[[j]]),".april",sep='')
  arima.s12[[count]]<-data.s12[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s12[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s12[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s12[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s12)[[count]]<-paste(names(data.s12)[[1]],".july",sep='')
arima.s12[[count]]<-data.s12[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s12[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s12[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s12[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s12
#Making a power curve

speed=as.double(s12[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s12[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s12[[1]]$power<-lookup(round_any(as.numeric(arima.s12[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s12[[2]]$power<-lookup(round_any(as.numeric(arima.s12[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s12<-vector("list",length(arima.s12))
count1<-1
count2<-length(arima.s12)/2 + 1
for(i in 1:length(data.s12))
{
  names(res.s12)[[count1]]<-paste((names(data.s12)[[i]]),".april",sep='')
  res.s12[[count1]]<-data.s12[[i]]$power["2011-04-01/2011-04-30"]-arima.s12[[count1]]$power
  count1<-count1+1
  
  names(res.s12)[[count2]]<-paste(names(data.s12)[[i]],".july",sep='')
  res.s12[[count2]]<-data.s12[[i]]$power["2011-07-01/2011-07-31"]-arima.s12[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s12)/2 + 1
for(i in 1:length(data.s12))
{
  
  plot(data.s12[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s12", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s12[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s12[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s12[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s12", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s12[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s12[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s12<-vector("list",length(arima.s12))
count1<-1
count2<-length(arima.s12)/2 + 1
for(i in 1:length(data.s12))
{
  names(mape.s12)[[count1]]<-paste((names(data.s12)[[i]]),".april",sep='')
  div<-res.s12[[count1]]/data.s12[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s12[[count1]]<-100*sum(abs(div))/length(data.s12[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s12)[[count2]]<-paste(names(data.s12)[[i]],".july",sep='')
  div<-res.s12[[count2]]/data.s12[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s12[[count2]]<-100*sum(abs(div))/nrow(data.s12[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s12))
{
  write.zoo(arima.s12[[i]], file = paste(names(arima.s12)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s12))
{
  write.zoo(data.s12[[i]], file = paste(names(data.s12)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s12))
{
  write.zoo(res.s12[[i]], file = paste(names(res.s12)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s12))
{
  write.zoo(mape.s12[[i]], file = paste(names(mape.s12)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 12 over -----

save.image()


#---------SITE 13 begin -----
#Reading files by zone
s13<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==13)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s13[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc13<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==13)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc13[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s13) <- sprintf('s13.%d', 1:numfiles)
for(i in 1:numfiles)
{s13[[i]]$Time <- with(s13[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s13))
{names(s13[[i]])[names(s13[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s13))
{names(s13[[i]])[names(s13[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s13.ts<-vector("list", length(s13))
names(s13.ts) <- sprintf('s13.%d', 1:numfiles)

for(i in 1:length(s13))
{
  s13.ts[[i]]<-xts(s13[[i]]$speed,s13[[i]]$Time)
  colnames(s13.ts[[i]])='speed'
  s13.ts[[i]]$power<-s13[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s13))
for(i in 1:length(s13))
{
  ep<-endpoints(s13.ts[[i]], on="hours", k=1)
  a<-(period.apply(s13.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s13.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s13 <- list()
maxindex<-which.max(CF1)
data.s13[[oldcount]]<-s13.ts[[which.max(CF1)]]
names(data.s13)[[oldcount]] <- names(s13.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc13 <- lapply( paste('sloc13.', 1:numfiles, sep=''), get)
names(sloc13) <- sprintf('sloc13.%d', 1:numfiles)

num<-seq(1,length(s13))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s13))
long1<-vector(length=length(s13))
for(i in 1:length(s13))
{
  lat1[i]<-sloc13[[i]]$V2[3]
  long1[i]<-sloc13[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s13))
{
  ep<-endpoints(data.s13[[i]], on="hours", k=1)
  data.s13[[i]]<-(period.apply(data.s13[[i]],ep,mean))
}

#sd(data.s13[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s13[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s13[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s13<-vector("list",(length(data.s13)*2))
count<-1
for(j in 1:length(data.s13))
{
  names(arima.s13)[[count]]<-paste((names(data.s13)[[j]]),".april",sep='')
  arima.s13[[count]]<-data.s13[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s13[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s13[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s13[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s13)[[count]]<-paste(names(data.s13)[[1]],".july",sep='')
arima.s13[[count]]<-data.s13[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s13[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s13[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s13[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s13
#Making a power curve

speed=as.double(s13[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s13[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s13[[1]]$power<-lookup(round_any(as.numeric(arima.s13[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s13[[2]]$power<-lookup(round_any(as.numeric(arima.s13[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s13<-vector("list",length(arima.s13))
count1<-1
count2<-length(arima.s13)/2 + 1
for(i in 1:length(data.s13))
{
  names(res.s13)[[count1]]<-paste((names(data.s13)[[i]]),".april",sep='')
  res.s13[[count1]]<-data.s13[[i]]$power["2011-04-01/2011-04-30"]-arima.s13[[count1]]$power
  count1<-count1+1
  
  names(res.s13)[[count2]]<-paste(names(data.s13)[[i]],".july",sep='')
  res.s13[[count2]]<-data.s13[[i]]$power["2011-07-01/2011-07-31"]-arima.s13[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s13)/2 + 1
for(i in 1:length(data.s13))
{
  
  plot(data.s13[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s13", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s13[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s13[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s13[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s13", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s13[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s13[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s13<-vector("list",length(arima.s13))
count1<-1
count2<-length(arima.s13)/2 + 1
for(i in 1:length(data.s13))
{
  names(mape.s13)[[count1]]<-paste((names(data.s13)[[i]]),".april",sep='')
  div<-res.s13[[count1]]/data.s13[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s13[[count1]]<-100*sum(abs(div))/length(data.s13[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s13)[[count2]]<-paste(names(data.s13)[[i]],".july",sep='')
  div<-res.s13[[count2]]/data.s13[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s13[[count2]]<-100*sum(abs(div))/nrow(data.s13[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s13))
{
  write.zoo(arima.s13[[i]], file = paste(names(arima.s13)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s13))
{
  write.zoo(data.s13[[i]], file = paste(names(data.s13)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s13))
{
  write.zoo(res.s13[[i]], file = paste(names(res.s13)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s13))
{
  write.zoo(mape.s13[[i]], file = paste(names(mape.s13)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 13 over -----
save.image()



#---------SITE 14 begin -----
#Reading files by zone
s14<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==14)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s14[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc14<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==14)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc14[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s14) <- sprintf('s14.%d', 1:numfiles)
for(i in 1:numfiles)
{s14[[i]]$Time <- with(s14[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s14))
{names(s14[[i]])[names(s14[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s14))
{names(s14[[i]])[names(s14[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s14.ts<-vector("list", length(s14))
names(s14.ts) <- sprintf('s14.%d', 1:numfiles)

for(i in 1:length(s14))
{
  s14.ts[[i]]<-xts(s14[[i]]$speed,s14[[i]]$Time)
  colnames(s14.ts[[i]])='speed'
  s14.ts[[i]]$power<-s14[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s14))
for(i in 1:length(s14))
{
  ep<-endpoints(s14.ts[[i]], on="hours", k=1)
  a<-(period.apply(s14.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s14.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s14 <- list()
maxindex<-which.max(CF1)
data.s14[[oldcount]]<-s14.ts[[which.max(CF1)]]
names(data.s14)[[oldcount]] <- names(s14.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc14 <- lapply( paste('sloc14.', 1:numfiles, sep=''), get)
names(sloc14) <- sprintf('sloc14.%d', 1:numfiles)

num<-seq(1,length(s14))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s14))
long1<-vector(length=length(s14))
for(i in 1:length(s14))
{
  lat1[i]<-sloc14[[i]]$V2[3]
  long1[i]<-sloc14[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s14))
{
  ep<-endpoints(data.s14[[i]], on="hours", k=1)
  data.s14[[i]]<-(period.apply(data.s14[[i]],ep,mean))
}

#sd(data.s14[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s14[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s14[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s14<-vector("list",(length(data.s14)*2))
count<-1
for(j in 1:length(data.s14))
{
  names(arima.s14)[[count]]<-paste((names(data.s14)[[j]]),".april",sep='')
  arima.s14[[count]]<-data.s14[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s14[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s14[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s14[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s14)[[count]]<-paste(names(data.s14)[[1]],".july",sep='')
arima.s14[[count]]<-data.s14[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s14[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s14[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s14[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s14
#Making a power curve

speed=as.double(s14[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s14[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s14[[1]]$power<-lookup(round_any(as.numeric(arima.s14[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s14[[2]]$power<-lookup(round_any(as.numeric(arima.s14[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s14<-vector("list",length(arima.s14))
count1<-1
count2<-length(arima.s14)/2 + 1
for(i in 1:length(data.s14))
{
  names(res.s14)[[count1]]<-paste((names(data.s14)[[i]]),".april",sep='')
  res.s14[[count1]]<-data.s14[[i]]$power["2011-04-01/2011-04-30"]-arima.s14[[count1]]$power
  count1<-count1+1
  
  names(res.s14)[[count2]]<-paste(names(data.s14)[[i]],".july",sep='')
  res.s14[[count2]]<-data.s14[[i]]$power["2011-07-01/2011-07-31"]-arima.s14[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s14)/2 + 1
for(i in 1:length(data.s14))
{
  
  plot(data.s14[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s14", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s14[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s14[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s14[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s14", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s14[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s14[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s14<-vector("list",length(arima.s14))
count1<-1
count2<-length(arima.s14)/2 + 1
for(i in 1:length(data.s14))
{
  names(mape.s14)[[count1]]<-paste((names(data.s14)[[i]]),".april",sep='')
  div<-res.s14[[count1]]/data.s14[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s14[[count1]]<-100*sum(abs(div))/length(data.s14[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s14)[[count2]]<-paste(names(data.s14)[[i]],".july",sep='')
  div<-res.s14[[count2]]/data.s14[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s14[[count2]]<-100*sum(abs(div))/nrow(data.s14[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s14))
{
  write.zoo(arima.s14[[i]], file = paste(names(arima.s14)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s14))
{
  write.zoo(data.s14[[i]], file = paste(names(data.s14)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s14))
{
  write.zoo(res.s14[[i]], file = paste(names(res.s14)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s14))
{
  write.zoo(mape.s14[[i]], file = paste(names(mape.s14)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 14 over -----
save.image()


#---------SITE 15 begin -----
#Reading files by zone
s15<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==15)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s15[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc15<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==15)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc15[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s15) <- sprintf('s15.%d', 1:numfiles)
for(i in 1:numfiles)
{s15[[i]]$Time <- with(s15[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s15))
{names(s15[[i]])[names(s15[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s15))
{names(s15[[i]])[names(s15[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s15.ts<-vector("list", length(s15))
names(s15.ts) <- sprintf('s15.%d', 1:numfiles)

for(i in 1:length(s15))
{
  s15.ts[[i]]<-xts(s15[[i]]$speed,s15[[i]]$Time)
  colnames(s15.ts[[i]])='speed'
  s15.ts[[i]]$power<-s15[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s15))
for(i in 1:length(s15))
{
  ep<-endpoints(s15.ts[[i]], on="hours", k=1)
  a<-(period.apply(s15.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s15.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s15 <- list()
maxindex<-which.max(CF1)
data.s15[[oldcount]]<-s15.ts[[which.max(CF1)]]
names(data.s15)[[oldcount]] <- names(s15.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc15 <- lapply( paste('sloc15.', 1:numfiles, sep=''), get)
names(sloc15) <- sprintf('sloc15.%d', 1:numfiles)

num<-seq(1,length(s15))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s15))
long1<-vector(length=length(s15))
for(i in 1:length(s15))
{
  lat1[i]<-sloc15[[i]]$V2[3]
  long1[i]<-sloc15[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s15))
{
  ep<-endpoints(data.s15[[i]], on="hours", k=1)
  data.s15[[i]]<-(period.apply(data.s15[[i]],ep,mean))
}

#sd(data.s15[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s15[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s15[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s15<-vector("list",(length(data.s15)*2))
count<-1
for(j in 1:length(data.s15))
{
  names(arima.s15)[[count]]<-paste((names(data.s15)[[j]]),".april",sep='')
  arima.s15[[count]]<-data.s15[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s15[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s15[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s15[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s15)[[count]]<-paste(names(data.s15)[[1]],".july",sep='')
arima.s15[[count]]<-data.s15[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s15[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s15[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s15[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s15
#Making a power curve

speed=as.double(s15[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s15[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s15[[1]]$power<-lookup(round_any(as.numeric(arima.s15[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s15[[2]]$power<-lookup(round_any(as.numeric(arima.s15[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s15<-vector("list",length(arima.s15))
count1<-1
count2<-length(arima.s15)/2 + 1
for(i in 1:length(data.s15))
{
  names(res.s15)[[count1]]<-paste((names(data.s15)[[i]]),".april",sep='')
  res.s15[[count1]]<-data.s15[[i]]$power["2011-04-01/2011-04-30"]-arima.s15[[count1]]$power
  count1<-count1+1
  
  names(res.s15)[[count2]]<-paste(names(data.s15)[[i]],".july",sep='')
  res.s15[[count2]]<-data.s15[[i]]$power["2011-07-01/2011-07-31"]-arima.s15[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s15)/2 + 1
for(i in 1:length(data.s15))
{
  
  plot(data.s15[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s15", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s15[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s15[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s15[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s15", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s15[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s15[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s15<-vector("list",length(arima.s15))
count1<-1
count2<-length(arima.s15)/2 + 1
for(i in 1:length(data.s15))
{
  names(mape.s15)[[count1]]<-paste((names(data.s15)[[i]]),".april",sep='')
  div<-res.s15[[count1]]/data.s15[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s15[[count1]]<-100*sum(abs(div))/length(data.s15[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s15)[[count2]]<-paste(names(data.s15)[[i]],".july",sep='')
  div<-res.s15[[count2]]/data.s15[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s15[[count2]]<-100*sum(abs(div))/nrow(data.s15[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s15))
{
  write.zoo(arima.s15[[i]], file = paste(names(arima.s15)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s15))
{
  write.zoo(data.s15[[i]], file = paste(names(data.s15)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s15))
{
  write.zoo(res.s15[[i]], file = paste(names(res.s15)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s15))
{
  write.zoo(mape.s15[[i]], file = paste(names(mape.s15)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 15 over -----
save.image()


#---------SITE 16 begin -----
#Reading files by zone
s16<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==16)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s16[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc16<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==16)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc16[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s16) <- sprintf('s16.%d', 1:numfiles)
for(i in 1:numfiles)
{s16[[i]]$Time <- with(s16[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s16))
{names(s16[[i]])[names(s16[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s16))
{names(s16[[i]])[names(s16[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s16.ts<-vector("list", length(s16))
names(s16.ts) <- sprintf('s16.%d', 1:numfiles)

for(i in 1:length(s16))
{
  s16.ts[[i]]<-xts(s16[[i]]$speed,s16[[i]]$Time)
  colnames(s16.ts[[i]])='speed'
  s16.ts[[i]]$power<-s16[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s16))
for(i in 1:length(s16))
{
  ep<-endpoints(s16.ts[[i]], on="hours", k=1)
  a<-(period.apply(s16.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s16.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s16 <- list()
maxindex<-which.max(CF1)
data.s16[[oldcount]]<-s16.ts[[which.max(CF1)]]
names(data.s16)[[oldcount]] <- names(s16.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc16 <- lapply( paste('sloc16.', 1:numfiles, sep=''), get)
names(sloc16) <- sprintf('sloc16.%d', 1:numfiles)

num<-seq(1,length(s16))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s16))
long1<-vector(length=length(s16))
for(i in 1:length(s16))
{
  lat1[i]<-sloc16[[i]]$V2[3]
  long1[i]<-sloc16[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s16))
{
  ep<-endpoints(data.s16[[i]], on="hours", k=1)
  data.s16[[i]]<-(period.apply(data.s16[[i]],ep,mean))
}

#sd(data.s16[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s16[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s16[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s16<-vector("list",(length(data.s16)*2))
count<-1
for(j in 1:length(data.s16))
{
  names(arima.s16)[[count]]<-paste((names(data.s16)[[j]]),".april",sep='')
  arima.s16[[count]]<-data.s16[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s16[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s16[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s16[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s16)[[count]]<-paste(names(data.s16)[[1]],".july",sep='')
arima.s16[[count]]<-data.s16[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s16[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s16[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s16[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s16
#Making a power curve

speed=as.double(s16[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s16[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s16[[1]]$power<-lookup(round_any(as.numeric(arima.s16[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s16[[2]]$power<-lookup(round_any(as.numeric(arima.s16[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s16<-vector("list",length(arima.s16))
count1<-1
count2<-length(arima.s16)/2 + 1
for(i in 1:length(data.s16))
{
  names(res.s16)[[count1]]<-paste((names(data.s16)[[i]]),".april",sep='')
  res.s16[[count1]]<-data.s16[[i]]$power["2011-04-01/2011-04-30"]-arima.s16[[count1]]$power
  count1<-count1+1
  
  names(res.s16)[[count2]]<-paste(names(data.s16)[[i]],".july",sep='')
  res.s16[[count2]]<-data.s16[[i]]$power["2011-07-01/2011-07-31"]-arima.s16[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s16)/2 + 1
for(i in 1:length(data.s16))
{
  
  plot(data.s16[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s16", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s16[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s16[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s16[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s16", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s16[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s16[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s16<-vector("list",length(arima.s16))
count1<-1
count2<-length(arima.s16)/2 + 1
for(i in 1:length(data.s16))
{
  names(mape.s16)[[count1]]<-paste((names(data.s16)[[i]]),".april",sep='')
  div<-res.s16[[count1]]/data.s16[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s16[[count1]]<-100*sum(abs(div))/length(data.s16[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s16)[[count2]]<-paste(names(data.s16)[[i]],".july",sep='')
  div<-res.s16[[count2]]/data.s16[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s16[[count2]]<-100*sum(abs(div))/nrow(data.s16[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s16))
{
  write.zoo(arima.s16[[i]], file = paste(names(arima.s16)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s16))
{
  write.zoo(data.s16[[i]], file = paste(names(data.s16)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s16))
{
  write.zoo(res.s16[[i]], file = paste(names(res.s16)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s16))
{
  write.zoo(mape.s16[[i]], file = paste(names(mape.s16)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 16 over -----
save.image()


#---------SITE 17 begin -----
#Reading files by zone
s17<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==17)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s17[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc17<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==17)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc17[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s17) <- sprintf('s17.%d', 1:numfiles)
for(i in 1:numfiles)
{s17[[i]]$Time <- with(s17[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s17))
{names(s17[[i]])[names(s17[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s17))
{names(s17[[i]])[names(s17[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s17.ts<-vector("list", length(s17))
names(s17.ts) <- sprintf('s17.%d', 1:numfiles)

for(i in 1:length(s17))
{
  s17.ts[[i]]<-xts(s17[[i]]$speed,s17[[i]]$Time)
  colnames(s17.ts[[i]])='speed'
  s17.ts[[i]]$power<-s17[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s17))
for(i in 1:length(s17))
{
  ep<-endpoints(s17.ts[[i]], on="hours", k=1)
  a<-(period.apply(s17.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s17.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s17 <- list()
maxindex<-which.max(CF1)
data.s17[[oldcount]]<-s17.ts[[which.max(CF1)]]
names(data.s17)[[oldcount]] <- names(s17.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc17 <- lapply( paste('sloc17.', 1:numfiles, sep=''), get)
names(sloc17) <- sprintf('sloc17.%d', 1:numfiles)

num<-seq(1,length(s17))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s17))
long1<-vector(length=length(s17))
for(i in 1:length(s17))
{
  lat1[i]<-sloc17[[i]]$V2[3]
  long1[i]<-sloc17[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s17))
{
  ep<-endpoints(data.s17[[i]], on="hours", k=1)
  data.s17[[i]]<-(period.apply(data.s17[[i]],ep,mean))
}

#sd(data.s17[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s17[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s17[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s17<-vector("list",(length(data.s17)*2))
count<-1
for(j in 1:length(data.s17))
{
  names(arima.s17)[[count]]<-paste((names(data.s17)[[j]]),".april",sep='')
  arima.s17[[count]]<-data.s17[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s17[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s17[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s17[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s17)[[count]]<-paste(names(data.s17)[[1]],".july",sep='')
arima.s17[[count]]<-data.s17[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s17[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s17[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s17[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s17
#Making a power curve

speed=as.double(s17[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s17[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s17[[1]]$power<-lookup(round_any(as.numeric(arima.s17[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s17[[2]]$power<-lookup(round_any(as.numeric(arima.s17[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s17<-vector("list",length(arima.s17))
count1<-1
count2<-length(arima.s17)/2 + 1
for(i in 1:length(data.s17))
{
  names(res.s17)[[count1]]<-paste((names(data.s17)[[i]]),".april",sep='')
  res.s17[[count1]]<-data.s17[[i]]$power["2011-04-01/2011-04-30"]-arima.s17[[count1]]$power
  count1<-count1+1
  
  names(res.s17)[[count2]]<-paste(names(data.s17)[[i]],".july",sep='')
  res.s17[[count2]]<-data.s17[[i]]$power["2011-07-01/2011-07-31"]-arima.s17[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s17)/2 + 1
for(i in 1:length(data.s17))
{
  
  plot(data.s17[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s17", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s17[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s17[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s17[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s17", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s17[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s17[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s17<-vector("list",length(arima.s17))
count1<-1
count2<-length(arima.s17)/2 + 1
for(i in 1:length(data.s17))
{
  names(mape.s17)[[count1]]<-paste((names(data.s17)[[i]]),".april",sep='')
  div<-res.s17[[count1]]/data.s17[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s17[[count1]]<-100*sum(abs(div))/length(data.s17[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s17)[[count2]]<-paste(names(data.s17)[[i]],".july",sep='')
  div<-res.s17[[count2]]/data.s17[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s17[[count2]]<-100*sum(abs(div))/nrow(data.s17[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s17))
{
  write.zoo(arima.s17[[i]], file = paste(names(arima.s17)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s17))
{
  write.zoo(data.s17[[i]], file = paste(names(data.s17)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s17))
{
  write.zoo(res.s17[[i]], file = paste(names(res.s17)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s17))
{
  write.zoo(mape.s17[[i]], file = paste(names(mape.s17)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 17 over -----
save.image()


#---------SITE 18 begin -----
#Reading files by zone
s18<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==18)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s18[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc18<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==18)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc18[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s18) <- sprintf('s18.%d', 1:numfiles)
for(i in 1:numfiles)
{s18[[i]]$Time <- with(s18[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s18))
{names(s18[[i]])[names(s18[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s18))
{names(s18[[i]])[names(s18[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s18.ts<-vector("list", length(s18))
names(s18.ts) <- sprintf('s18.%d', 1:numfiles)

for(i in 1:length(s18))
{
  s18.ts[[i]]<-xts(s18[[i]]$speed,s18[[i]]$Time)
  colnames(s18.ts[[i]])='speed'
  s18.ts[[i]]$power<-s18[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s18))
for(i in 1:length(s18))
{
  ep<-endpoints(s18.ts[[i]], on="hours", k=1)
  a<-(period.apply(s18.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s18.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s18 <- list()
maxindex<-which.max(CF1)
data.s18[[oldcount]]<-s18.ts[[which.max(CF1)]]
names(data.s18)[[oldcount]] <- names(s18.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc18 <- lapply( paste('sloc18.', 1:numfiles, sep=''), get)
names(sloc18) <- sprintf('sloc18.%d', 1:numfiles)

num<-seq(1,length(s18))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s18))
long1<-vector(length=length(s18))
for(i in 1:length(s18))
{
  lat1[i]<-sloc18[[i]]$V2[3]
  long1[i]<-sloc18[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s18))
{
  ep<-endpoints(data.s18[[i]], on="hours", k=1)
  data.s18[[i]]<-(period.apply(data.s18[[i]],ep,mean))
}

#sd(data.s18[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s18[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s18[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s18<-vector("list",(length(data.s18)*2))
count<-1
for(j in 1:length(data.s18))
{
  names(arima.s18)[[count]]<-paste((names(data.s18)[[j]]),".april",sep='')
  arima.s18[[count]]<-data.s18[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s18[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s18[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s18[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s18)[[count]]<-paste(names(data.s18)[[1]],".july",sep='')
arima.s18[[count]]<-data.s18[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s18[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s18[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s18[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s18
#Making a power curve

speed=as.double(s18[[which.max(CF1)]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s18[[which.max(CF1)]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s18[[1]]$power<-lookup(round_any(as.numeric(arima.s18[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s18[[2]]$power<-lookup(round_any(as.numeric(arima.s18[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)


for(i in 1:nrow(arima.s18[[1]]))
{
  
  
  if(arima.s18[[1]]$speed[i]>=12.35)
  {
    arima.s18[[1]]$power[i]=2
  }
  
  if(arima.s18[[1]]$speed[i]<0)
  {
    arima.s18[[1]]$speed[i]=0
  }
  
  if(arima.s18[[1]]$speed[i]==0)
  {
    arima.s18[[1]]$power[i]=0
  }
  
}

for(i in 1:nrow(arima.s18[[2]]))
{
  if(arima.s18[[2]]$speed[i]>=12.35)
  {
    arima.s18[[2]]$power[i]=2
  }
  
  if(arima.s18[[2]]$speed[i]<0)
  {
    arima.s18[[2]]$speed[i]=0
  }
  
  if(arima.s18[[2]]$speed[i]==0)
  {
    arima.s18[[2]]$power[i]=0
  }
  
}


#Residuals
res.s18<-vector("list",length(arima.s18))
count1<-1
count2<-length(arima.s18)/2 + 1
for(i in 1:length(data.s18))
{
  names(res.s18)[[count1]]<-paste((names(data.s18)[[i]]),".april",sep='')
  res.s18[[count1]]<-data.s18[[i]]$power["2011-04-01/2011-04-30"]-arima.s18[[count1]]$power
  count1<-count1+1
  
  names(res.s18)[[count2]]<-paste(names(data.s18)[[i]],".july",sep='')
  res.s18[[count2]]<-data.s18[[i]]$power["2011-07-01/2011-07-31"]-arima.s18[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s18)/2 + 1
for(i in 1:length(data.s18))
{
  
  plot(data.s18[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s18", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s18[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s18[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s18[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s18", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s18[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s18[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s18<-vector("list",length(arima.s18))
count1<-1
count2<-length(arima.s18)/2 + 1
for(i in 1:length(data.s18))
{
  names(mape.s18)[[count1]]<-paste((names(data.s18)[[i]]),".april",sep='')
  div<-res.s18[[count1]]/data.s18[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s18[[count1]]<-100*sum(abs(div))/length(data.s18[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s18)[[count2]]<-paste(names(data.s18)[[i]],".july",sep='')
  div<-res.s18[[count2]]/data.s18[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s18[[count2]]<-100*sum(abs(div))/nrow(data.s18[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s18))
{
  write.zoo(arima.s18[[i]], file = paste(names(arima.s18)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s18))
{
  write.zoo(data.s18[[i]], file = paste(names(data.s18)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s18))
{
  write.zoo(res.s18[[i]], file = paste(names(res.s18)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s18))
{
  write.zoo(mape.s18[[i]], file = paste(names(mape.s18)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 18 over -----
save.image()


#---------SITE 19 begin -----
#Reading files by zone
s19<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==19)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s19[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc19<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==19)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc19[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s19) <- sprintf('s19.%d', 1:numfiles)
for(i in 1:numfiles)
{s19[[i]]$Time <- with(s19[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s19))
{names(s19[[i]])[names(s19[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s19))
{names(s19[[i]])[names(s19[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s19.ts<-vector("list", length(s19))
names(s19.ts) <- sprintf('s19.%d', 1:numfiles)

for(i in 1:length(s19))
{
  s19.ts[[i]]<-xts(s19[[i]]$speed,s19[[i]]$Time)
  colnames(s19.ts[[i]])='speed'
  s19.ts[[i]]$power<-s19[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s19))
for(i in 1:length(s19))
{
  ep<-endpoints(s19.ts[[i]], on="hours", k=1)
  a<-(period.apply(s19.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s19.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s19 <- list()
maxindex<-which.max(CF1)
data.s19[[oldcount]]<-s19.ts[[which.max(CF1)]]
names(data.s19)[[oldcount]] <- names(s19.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc19 <- lapply( paste('sloc19.', 1:numfiles, sep=''), get)
names(sloc19) <- sprintf('sloc19.%d', 1:numfiles)

num<-seq(1,length(s19))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s19))
long1<-vector(length=length(s19))
for(i in 1:length(s19))
{
  lat1[i]<-sloc19[[i]]$V2[3]
  long1[i]<-sloc19[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s19))
{
  ep<-endpoints(data.s19[[i]], on="hours", k=1)
  data.s19[[i]]<-(period.apply(data.s19[[i]],ep,mean))
}

#sd(data.s19[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s19[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s19[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s19<-vector("list",(length(data.s19)*2))
count<-1
for(j in 1:length(data.s19))
{
  names(arima.s19)[[count]]<-paste((names(data.s19)[[j]]),".april",sep='')
  arima.s19[[count]]<-data.s19[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s19[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s19[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s19[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s19)[[count]]<-paste(names(data.s19)[[1]],".july",sep='')
arima.s19[[count]]<-data.s19[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s19[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s19[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s19[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s19
#Making a power curve

speed=as.double(s19[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s19[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s19[[1]]$power<-lookup(round_any(as.numeric(arima.s19[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s19[[2]]$power<-lookup(round_any(as.numeric(arima.s19[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s19<-vector("list",length(arima.s19))
count1<-1
count2<-length(arima.s19)/2 + 1
for(i in 1:length(data.s19))
{
  names(res.s19)[[count1]]<-paste((names(data.s19)[[i]]),".april",sep='')
  res.s19[[count1]]<-data.s19[[i]]$power["2011-04-01/2011-04-30"]-arima.s19[[count1]]$power
  count1<-count1+1
  
  names(res.s19)[[count2]]<-paste(names(data.s19)[[i]],".july",sep='')
  res.s19[[count2]]<-data.s19[[i]]$power["2011-07-01/2011-07-31"]-arima.s19[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s19)/2 + 1
for(i in 1:length(data.s19))
{
  
  plot(data.s19[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s19", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s19[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s19[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s19[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s19", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s19[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s19[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s19<-vector("list",length(arima.s19))
count1<-1
count2<-length(arima.s19)/2 + 1
for(i in 1:length(data.s19))
{
  names(mape.s19)[[count1]]<-paste((names(data.s19)[[i]]),".april",sep='')
  div<-res.s19[[count1]]/data.s19[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s19[[count1]]<-100*sum(abs(div))/length(data.s19[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s19)[[count2]]<-paste(names(data.s19)[[i]],".july",sep='')
  div<-res.s19[[count2]]/data.s19[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s19[[count2]]<-100*sum(abs(div))/nrow(data.s19[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s19))
{
  write.zoo(arima.s19[[i]], file = paste(names(arima.s19)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s19))
{
  write.zoo(data.s19[[i]], file = paste(names(data.s19)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s19))
{
  write.zoo(res.s19[[i]], file = paste(names(res.s19)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s19))
{
  write.zoo(mape.s19[[i]], file = paste(names(mape.s19)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 19 over -----
save.image()


#---------SITE 20 begin -----
#Reading files by zone
s20<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==20)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s20[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc20<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==20)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc20[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s20) <- sprintf('s20.%d', 1:numfiles)
for(i in 1:numfiles)
{s20[[i]]$Time <- with(s20[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s20))
{names(s20[[i]])[names(s20[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s20))
{names(s20[[i]])[names(s20[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s20.ts<-vector("list", length(s20))
names(s20.ts) <- sprintf('s20.%d', 1:numfiles)

for(i in 1:length(s20))
{
  s20.ts[[i]]<-xts(s20[[i]]$speed,s20[[i]]$Time)
  colnames(s20.ts[[i]])='speed'
  s20.ts[[i]]$power<-s20[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s20))
for(i in 1:length(s20))
{
  ep<-endpoints(s20.ts[[i]], on="hours", k=1)
  a<-(period.apply(s20.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s20.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s20 <- list()
maxindex<-which.max(CF1)
data.s20[[oldcount]]<-s20.ts[[which.max(CF1)]]
names(data.s20)[[oldcount]] <- names(s20.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc20 <- lapply( paste('sloc20.', 1:numfiles, sep=''), get)
names(sloc20) <- sprintf('sloc20.%d', 1:numfiles)

num<-seq(1,length(s20))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s20))
long1<-vector(length=length(s20))
for(i in 1:length(s20))
{
  lat1[i]<-sloc20[[i]]$V2[3]
  long1[i]<-sloc20[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s20))
{
  ep<-endpoints(data.s20[[i]], on="hours", k=1)
  data.s20[[i]]<-(period.apply(data.s20[[i]],ep,mean))
}

#sd(data.s20[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s20[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s20[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s20<-vector("list",(length(data.s20)*2))
count<-1
for(j in 1:length(data.s20))
{
  names(arima.s20)[[count]]<-paste((names(data.s20)[[j]]),".april",sep='')
  arima.s20[[count]]<-data.s20[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s20[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s20[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s20[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s20)[[count]]<-paste(names(data.s20)[[1]],".july",sep='')
arima.s20[[count]]<-data.s20[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s20[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s20[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s20[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s20
#Making a power curve

speed=as.double(s20[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s20[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s20[[1]]$power<-lookup(round_any(as.numeric(arima.s20[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s20[[2]]$power<-lookup(round_any(as.numeric(arima.s20[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s20<-vector("list",length(arima.s20))
count1<-1
count2<-length(arima.s20)/2 + 1
for(i in 1:length(data.s20))
{
  names(res.s20)[[count1]]<-paste((names(data.s20)[[i]]),".april",sep='')
  res.s20[[count1]]<-data.s20[[i]]$power["2011-04-01/2011-04-30"]-arima.s20[[count1]]$power
  count1<-count1+1
  
  names(res.s20)[[count2]]<-paste(names(data.s20)[[i]],".july",sep='')
  res.s20[[count2]]<-data.s20[[i]]$power["2011-07-01/2011-07-31"]-arima.s20[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s20)/2 + 1
for(i in 1:length(data.s20))
{
  
  plot(data.s20[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s20", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s20[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s20[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s20[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s20", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s20[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s20[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s20<-vector("list",length(arima.s20))
count1<-1
count2<-length(arima.s20)/2 + 1
for(i in 1:length(data.s20))
{
  names(mape.s20)[[count1]]<-paste((names(data.s20)[[i]]),".april",sep='')
  div<-res.s20[[count1]]/data.s20[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s20[[count1]]<-100*sum(abs(div))/length(data.s20[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s20)[[count2]]<-paste(names(data.s20)[[i]],".july",sep='')
  div<-res.s20[[count2]]/data.s20[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s20[[count2]]<-100*sum(abs(div))/nrow(data.s20[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s20))
{
  write.zoo(arima.s20[[i]], file = paste(names(arima.s20)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s20))
{
  write.zoo(data.s20[[i]], file = paste(names(data.s20)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s20))
{
  write.zoo(res.s20[[i]], file = paste(names(res.s20)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s20))
{
  write.zoo(mape.s20[[i]], file = paste(names(mape.s20)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 20 over -----
save.image()

for (i in 1:length(data.s20))
{
  write.zoo(data.s20[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s20)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s20[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s20)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s19))
{
  write.zoo(data.s19[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s19)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s19[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s19)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s18))
{
  write.zoo(data.s18[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s18)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s18[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s18)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s17))
{
  write.zoo(data.s17[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s17)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s17[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s17)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s16))
{
  write.zoo(data.s16[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s16)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s16[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s16)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s15))
{
  write.zoo(data.s15[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s15)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s15[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s15)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s14))
{
  write.zoo(data.s14[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s14)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s14[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s14)[[i]],'-july power data.csv',sep=''),sep=",")
  
}


for (i in 1:length(data.s13))
{
  write.zoo(data.s13[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s13)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s13[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s13)[[i]],'-july power data.csv',sep=''),sep=",")
  
}


for (i in 1:length(data.s12))
{
  write.zoo(data.s12[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s12)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s12[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s12)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s11))
{
  write.zoo(data.s11[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s11)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s11[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s11)[[i]],'-july power data.csv',sep=''),sep=",")
  
}