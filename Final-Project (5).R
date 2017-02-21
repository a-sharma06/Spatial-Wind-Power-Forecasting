
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


#--------SITE 41 Begin----
#Reading files by zone
s41<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==41)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s41[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc41<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==41)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc41[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s41) <- sprintf('s41.%d', 1:numfiles)
for(i in 1:numfiles)
{s41[[i]]$Time <- with(s41[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s41))
{names(s41[[i]])[names(s41[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s41))
{names(s41[[i]])[names(s41[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s41.ts<-vector("list", length(s41))
names(s41.ts) <- sprintf('s41.%d', 1:numfiles)

for(i in 1:length(s41))
{
  s41.ts[[i]]<-xts(s41[[i]]$speed,s41[[i]]$Time)
  colnames(s41.ts[[i]])='speed'
  s41.ts[[i]]$power<-s41[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s41))
for(i in 1:length(s41))
{
  ep<-endpoints(s41.ts[[i]], on="hours", k=1)
  a<-(period.apply(s41.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s41.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s41 <- list()
maxindex<-which.max(CF1)
data.s41[[oldcount]]<-s41.ts[[which.max(CF1)]]
names(data.s41)[[oldcount]] <- names(s41.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc41 <- lapply( paste('sloc41.', 1:numfiles, sep=''), get)
names(sloc41) <- sprintf('sloc41.%d', 1:numfiles)

num<-seq(1,length(s41))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s41))
long1<-vector(length=length(s41))
for(i in 1:length(s41))
{
  lat1[i]<-sloc41[[i]]$V2[3]
  long1[i]<-sloc41[[i]]$V2[2]
}

#------Data collection for Site 1 over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s41))
{
  ep<-endpoints(data.s41[[i]], on="hours", k=1)
  data.s41[[i]]<-(period.apply(data.s41[[i]],ep,mean))
}

#sd(data.s41[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s41[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s41[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s41<-vector("list",(length(data.s41)*2))
count<-1
for(j in 1:length(data.s41))
{
  names(arima.s41)[[count]]<-paste((names(data.s41)[[j]]),".april",sep='')
  arima.s41[[count]]<-data.s41[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s41[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s41[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s41[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}



  names(arima.s41)[[count]]<-paste(names(data.s41)[[1]],".july",sep='')
  arima.s41[[count]]<-data.s41[[1]]$speed["2011-07-01/2011-07-31"]
  
  july<-data.s41[[1]]$speed["2011-07-01/2011-07-31"]
  
  for(i in 0:(length(data.s41[[1]]$speed["2011-07-01/2011-07-31"])-73))
  {
    a=i+1
    b=i+72
    example<-july$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s41[[count]][i+73]<-as.numeric(point.forecast)
  }


#backup<-arima.s41
#Making a power curve

speed=as.double(s41[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s41[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s41[[1]]$power<-lookup(round_any(as.numeric(arima.s41[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s41[[2]]$power<-lookup(round_any(as.numeric(arima.s41[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)


for(i in 1:nrow(arima.s41[[1]]))
{
  
  
  if(arima.s41[[1]]$speed[i]>=13.05)
  {
    arima.s41[[1]]$power[i]=16
  }
  
  if(arima.s41[[1]]$speed[i]<0)
  {
    arima.s41[[1]]$speed[i]=0
  }
  
  if(arima.s41[[1]]$speed[i]==0)
  {
    arima.s41[[1]]$power[i]=0
  }
  
}

for(i in 1:nrow(arima.s41[[2]]))
{
  if(arima.s41[[2]]$speed[i]>=13.05)
  {
    arima.s41[[2]]$power[i]=16
  }
  
  if(arima.s41[[2]]$speed[i]<0)
  {
    arima.s41[[2]]$speed[i]=0
  }
  
  if(arima.s41[[2]]$speed[i]==0)
  {
    arima.s41[[2]]$power[i]=0
  }
  
}


#Residuals
res.s41<-vector("list",length(arima.s41))
count1<-1
count2<-length(arima.s41)/2 + 1
for(i in 1:length(data.s41))
{
  names(res.s41)[[count1]]<-paste((names(data.s41)[[i]]),".april",sep='')
  res.s41[[count1]]<-data.s41[[i]]$power["2011-04-01/2011-04-30"]-arima.s41[[count1]]$power
  count1<-count1+1
  
  names(res.s41)[[count2]]<-paste(names(data.s41)[[i]],".july",sep='')
  res.s41[[count2]]<-data.s41[[i]]$power["2011-07-01/2011-07-31"]-arima.s41[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s41)/2 + 1
for(i in 1:length(data.s41))
{
  
  plot(data.s41[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s41", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s41[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s41[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s41[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s41", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s41[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s41[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s41<-vector("list",length(arima.s41))
count1<-1
count2<-length(arima.s41)/2 + 1
for(i in 1:length(data.s41))
{
  names(mape.s41)[[count1]]<-paste((names(data.s41)[[i]]),".april",sep='')
  div<-res.s41[[count1]]/data.s41[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s41[[count1]]<-100*sum(abs(div))/length(data.s41[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s41)[[count2]]<-paste(names(data.s41)[[i]],".july",sep='')
  div<-res.s41[[count2]]/data.s41[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s41[[count2]]<-100*sum(abs(div))/nrow(data.s41[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s41))
{
  write.zoo(arima.s41[[i]], file = paste(names(arima.s41)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s41))
{
  write.zoo(data.s41[[i]], file = paste(names(data.s41)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s41))
{
  write.zoo(res.s41[[i]], file = paste(names(res.s41)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s41))
{
  write.zoo(mape.s41[[i]], file = paste(names(mape.s41)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 41 over -----

save.image()

#---------SITE 42 begin -----
#Reading files by zone
s42<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==42)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s42[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc42<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==42)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc42[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s42) <- sprintf('s42.%d', 1:numfiles)
for(i in 1:numfiles)
{s42[[i]]$Time <- with(s42[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s42))
{names(s42[[i]])[names(s42[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s42))
{names(s42[[i]])[names(s42[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s42.ts<-vector("list", length(s42))
names(s42.ts) <- sprintf('s42.%d', 1:numfiles)

for(i in 1:length(s42))
{
  s42.ts[[i]]<-xts(s42[[i]]$speed,s42[[i]]$Time)
  colnames(s42.ts[[i]])='speed'
  s42.ts[[i]]$power<-s42[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s42))
for(i in 1:length(s42))
{
  ep<-endpoints(s42.ts[[i]], on="hours", k=1)
  a<-(period.apply(s42.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s42.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s42 <- list()
maxindex<-which.max(CF1)
data.s42[[oldcount]]<-s42.ts[[which.max(CF1)]]
names(data.s42)[[oldcount]] <- names(s42.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc42 <- lapply( paste('sloc42.', 1:numfiles, sep=''), get)
names(sloc42) <- sprintf('sloc42.%d', 1:numfiles)

num<-seq(1,length(s42))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s42))
long1<-vector(length=length(s42))
for(i in 1:length(s42))
{
  lat1[i]<-sloc42[[i]]$V2[3]
  long1[i]<-sloc42[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s42))
{
  ep<-endpoints(data.s42[[i]], on="hours", k=1)
  data.s42[[i]]<-(period.apply(data.s42[[i]],ep,mean))
}

#sd(data.s42[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s42[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s42[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s42<-vector("list",(length(data.s42)*2))
count<-1
for(j in 1:length(data.s42))
{
  names(arima.s42)[[count]]<-paste((names(data.s42)[[j]]),".april",sep='')
  arima.s42[[count]]<-data.s42[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s42[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s42[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s42[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s42)[[count]]<-paste(names(data.s42)[[1]],".july",sep='')
arima.s42[[count]]<-data.s42[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s42[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s42[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s42[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s42
#Making a power curve

speed=as.double(s42[[which.max(CF1)]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s42[[which.max(CF1)]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s42[[1]]$power<-lookup(round_any(as.numeric(arima.s42[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s42[[2]]$power<-lookup(round_any(as.numeric(arima.s42[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)



#Residuals
res.s42<-vector("list",length(arima.s42))
count1<-1
count2<-length(arima.s42)/2 + 1
for(i in 1:length(data.s42))
{
  names(res.s42)[[count1]]<-paste((names(data.s42)[[i]]),".april",sep='')
  res.s42[[count1]]<-data.s42[[i]]$power["2011-04-01/2011-04-30"]-arima.s42[[count1]]$power
  count1<-count1+1
  
  names(res.s42)[[count2]]<-paste(names(data.s42)[[i]],".july",sep='')
  res.s42[[count2]]<-data.s42[[i]]$power["2011-07-01/2011-07-31"]-arima.s42[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s42)/2 + 1
for(i in 1:length(data.s42))
{
  
  plot(data.s42[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s42", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s42[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s42[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s42[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s42", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s42[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s42[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s42<-vector("list",length(arima.s42))
count1<-1
count2<-length(arima.s42)/2 + 1
for(i in 1:length(data.s42))
{
  names(mape.s42)[[count1]]<-paste((names(data.s42)[[i]]),".april",sep='')
  div<-res.s42[[count1]]/data.s42[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s42[[count1]]<-100*sum(abs(div))/length(data.s42[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s42)[[count2]]<-paste(names(data.s42)[[i]],".july",sep='')
  div<-res.s42[[count2]]/data.s42[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s42[[count2]]<-100*sum(abs(div))/nrow(data.s42[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s42))
{
  write.zoo(arima.s42[[i]], file = paste(names(arima.s42)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s42))
{
  write.zoo(data.s42[[i]], file = paste(names(data.s42)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s42))
{
  write.zoo(res.s42[[i]], file = paste(names(res.s42)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s42))
{
  write.zoo(mape.s42[[i]], file = paste(names(mape.s42)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 42 over -----

save.image()


#---------SITE 43 begin -----
#Reading files by zone
s43<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==43)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s43[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc43<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==43)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc43[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s43) <- sprintf('s43.%d', 1:numfiles)
for(i in 1:numfiles)
{s43[[i]]$Time <- with(s43[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s43))
{names(s43[[i]])[names(s43[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s43))
{names(s43[[i]])[names(s43[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s43.ts<-vector("list", length(s43))
names(s43.ts) <- sprintf('s43.%d', 1:numfiles)

for(i in 1:length(s43))
{
  s43.ts[[i]]<-xts(s43[[i]]$speed,s43[[i]]$Time)
  colnames(s43.ts[[i]])='speed'
  s43.ts[[i]]$power<-s43[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s43))
for(i in 1:length(s43))
{
  ep<-endpoints(s43.ts[[i]], on="hours", k=1)
  a<-(period.apply(s43.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s43.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s43 <- list()
maxindex<-which.max(CF1)
data.s43[[oldcount]]<-s43.ts[[which.max(CF1)]]
names(data.s43)[[oldcount]] <- names(s43.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc43 <- lapply( paste('sloc43.', 1:numfiles, sep=''), get)
names(sloc43) <- sprintf('sloc43.%d', 1:numfiles)

num<-seq(1,length(s43))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s43))
long1<-vector(length=length(s43))
for(i in 1:length(s43))
{
  lat1[i]<-sloc43[[i]]$V2[3]
  long1[i]<-sloc43[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s43))
{
  ep<-endpoints(data.s43[[i]], on="hours", k=1)
  data.s43[[i]]<-(period.apply(data.s43[[i]],ep,mean))
}

#sd(data.s43[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s43[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s43[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s43<-vector("list",(length(data.s43)*2))
count<-1
for(j in 1:length(data.s43))
{
  names(arima.s43)[[count]]<-paste((names(data.s43)[[j]]),".april",sep='')
  arima.s43[[count]]<-data.s43[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s43[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s43[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s43[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s43)[[count]]<-paste(names(data.s43)[[1]],".july",sep='')
arima.s43[[count]]<-data.s43[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s43[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s43[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s43[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s43
#Making a power curve

speed=as.double(s43[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s43[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s43[[1]]$power<-lookup(round_any(as.numeric(arima.s43[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s43[[2]]$power<-lookup(round_any(as.numeric(arima.s43[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s43<-vector("list",length(arima.s43))
count1<-1
count2<-length(arima.s43)/2 + 1
for(i in 1:length(data.s43))
{
  names(res.s43)[[count1]]<-paste((names(data.s43)[[i]]),".april",sep='')
  res.s43[[count1]]<-data.s43[[i]]$power["2011-04-01/2011-04-30"]-arima.s43[[count1]]$power
  count1<-count1+1
  
  names(res.s43)[[count2]]<-paste(names(data.s43)[[i]],".july",sep='')
  res.s43[[count2]]<-data.s43[[i]]$power["2011-07-01/2011-07-31"]-arima.s43[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s43)/2 + 1
for(i in 1:length(data.s43))
{
  
  plot(data.s43[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s43", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s43[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s43[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s43[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s43", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s43[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s43[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s43<-vector("list",length(arima.s43))
count1<-1
count2<-length(arima.s43)/2 + 1
for(i in 1:length(data.s43))
{
  names(mape.s43)[[count1]]<-paste((names(data.s43)[[i]]),".april",sep='')
  div<-res.s43[[count1]]/data.s43[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s43[[count1]]<-100*sum(abs(div))/length(data.s43[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s43)[[count2]]<-paste(names(data.s43)[[i]],".july",sep='')
  div<-res.s43[[count2]]/data.s43[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s43[[count2]]<-100*sum(abs(div))/nrow(data.s43[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s43))
{
  write.zoo(arima.s43[[i]], file = paste(names(arima.s43)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s43))
{
  write.zoo(data.s43[[i]], file = paste(names(data.s43)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s43))
{
  write.zoo(res.s43[[i]], file = paste(names(res.s43)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s43))
{
  write.zoo(mape.s43[[i]], file = paste(names(mape.s43)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 43 over -----
save.image()



#---------SITE 44 begin -----
#Reading files by zone
s44<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==44)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s44[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc44<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==44)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc44[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s44) <- sprintf('s44.%d', 1:numfiles)
for(i in 1:numfiles)
{s44[[i]]$Time <- with(s44[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s44))
{names(s44[[i]])[names(s44[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s44))
{names(s44[[i]])[names(s44[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s44.ts<-vector("list", length(s44))
names(s44.ts) <- sprintf('s44.%d', 1:numfiles)

for(i in 1:length(s44))
{
  s44.ts[[i]]<-xts(s44[[i]]$speed,s44[[i]]$Time)
  colnames(s44.ts[[i]])='speed'
  s44.ts[[i]]$power<-s44[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s44))
for(i in 1:length(s44))
{
  ep<-endpoints(s44.ts[[i]], on="hours", k=1)
  a<-(period.apply(s44.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s44.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s44 <- list()
maxindex<-which.max(CF1)
data.s44[[oldcount]]<-s44.ts[[which.max(CF1)]]
names(data.s44)[[oldcount]] <- names(s44.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc44 <- lapply( paste('sloc44.', 1:numfiles, sep=''), get)
names(sloc44) <- sprintf('sloc44.%d', 1:numfiles)

num<-seq(1,length(s44))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s44))
long1<-vector(length=length(s44))
for(i in 1:length(s44))
{
  lat1[i]<-sloc44[[i]]$V2[3]
  long1[i]<-sloc44[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s44))
{
  ep<-endpoints(data.s44[[i]], on="hours", k=1)
  data.s44[[i]]<-(period.apply(data.s44[[i]],ep,mean))
}

#sd(data.s44[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s44[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s44[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s44<-vector("list",(length(data.s44)*2))
count<-1
for(j in 1:length(data.s44))
{
  names(arima.s44)[[count]]<-paste((names(data.s44)[[j]]),".april",sep='')
  arima.s44[[count]]<-data.s44[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s44[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s44[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s44[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s44)[[count]]<-paste(names(data.s44)[[1]],".july",sep='')
arima.s44[[count]]<-data.s44[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s44[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s44[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s44[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s44
#Making a power curve

speed=as.double(s44[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s44[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s44[[1]]$power<-lookup(round_any(as.numeric(arima.s44[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s44[[2]]$power<-lookup(round_any(as.numeric(arima.s44[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s44<-vector("list",length(arima.s44))
count1<-1
count2<-length(arima.s44)/2 + 1
for(i in 1:length(data.s44))
{
  names(res.s44)[[count1]]<-paste((names(data.s44)[[i]]),".april",sep='')
  res.s44[[count1]]<-data.s44[[i]]$power["2011-04-01/2011-04-30"]-arima.s44[[count1]]$power
  count1<-count1+1
  
  names(res.s44)[[count2]]<-paste(names(data.s44)[[i]],".july",sep='')
  res.s44[[count2]]<-data.s44[[i]]$power["2011-07-01/2011-07-31"]-arima.s44[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s44)/2 + 1
for(i in 1:length(data.s44))
{
  
  plot(data.s44[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s44", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s44[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s44[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s44[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s44", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s44[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s44[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s44<-vector("list",length(arima.s44))
count1<-1
count2<-length(arima.s44)/2 + 1
for(i in 1:length(data.s44))
{
  names(mape.s44)[[count1]]<-paste((names(data.s44)[[i]]),".april",sep='')
  div<-res.s44[[count1]]/data.s44[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s44[[count1]]<-100*sum(abs(div))/length(data.s44[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s44)[[count2]]<-paste(names(data.s44)[[i]],".july",sep='')
  div<-res.s44[[count2]]/data.s44[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s44[[count2]]<-100*sum(abs(div))/nrow(data.s44[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s44))
{
  write.zoo(arima.s44[[i]], file = paste(names(arima.s44)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s44))
{
  write.zoo(data.s44[[i]], file = paste(names(data.s44)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s44))
{
  write.zoo(res.s44[[i]], file = paste(names(res.s44)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s44))
{
  write.zoo(mape.s44[[i]], file = paste(names(mape.s44)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 44 over -----
save.image()


#---------SITE 45 begin -----
#Reading files by zone
s45<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==45)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s45[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc45<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==45)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc45[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s45) <- sprintf('s45.%d', 1:numfiles)
for(i in 1:numfiles)
{s45[[i]]$Time <- with(s45[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s45))
{names(s45[[i]])[names(s45[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s45))
{names(s45[[i]])[names(s45[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s45.ts<-vector("list", length(s45))
names(s45.ts) <- sprintf('s45.%d', 1:numfiles)

for(i in 1:length(s45))
{
  s45.ts[[i]]<-xts(s45[[i]]$speed,s45[[i]]$Time)
  colnames(s45.ts[[i]])='speed'
  s45.ts[[i]]$power<-s45[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s45))
for(i in 1:length(s45))
{
  ep<-endpoints(s45.ts[[i]], on="hours", k=1)
  a<-(period.apply(s45.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s45.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s45 <- list()
maxindex<-which.max(CF1)
data.s45[[oldcount]]<-s45.ts[[which.max(CF1)]]
names(data.s45)[[oldcount]] <- names(s45.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc45 <- lapply( paste('sloc45.', 1:numfiles, sep=''), get)
names(sloc45) <- sprintf('sloc45.%d', 1:numfiles)

num<-seq(1,length(s45))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s45))
long1<-vector(length=length(s45))
for(i in 1:length(s45))
{
  lat1[i]<-sloc45[[i]]$V2[3]
  long1[i]<-sloc45[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s45))
{
  ep<-endpoints(data.s45[[i]], on="hours", k=1)
  data.s45[[i]]<-(period.apply(data.s45[[i]],ep,mean))
}

#sd(data.s45[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s45[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s45[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s45<-vector("list",(length(data.s45)*2))
count<-1
for(j in 1:length(data.s45))
{
  names(arima.s45)[[count]]<-paste((names(data.s45)[[j]]),".april",sep='')
  arima.s45[[count]]<-data.s45[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s45[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s45[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s45[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s45)[[count]]<-paste(names(data.s45)[[1]],".july",sep='')
arima.s45[[count]]<-data.s45[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s45[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s45[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s45[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s45
#Making a power curve

speed=as.double(s45[[which.max(CF1)]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s45[[which.max(CF1)]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s45[[1]]$power<-lookup(round_any(as.numeric(arima.s45[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s45[[2]]$power<-lookup(round_any(as.numeric(arima.s45[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)


for(i in 1:nrow(arima.s45[[1]]))
{
  
  
  if(arima.s45[[1]]$speed[i]>=12.35)
  {
    arima.s45[[1]]$power[i]=4
  }
  
  if(arima.s45[[1]]$speed[i]<0)
  {
    arima.s45[[1]]$speed[i]=0
  }
  
  if(arima.s45[[1]]$speed[i]==0)
  {
    arima.s45[[1]]$power[i]=0
  }
  
}

for(i in 1:nrow(arima.s45[[2]]))
{
  if(arima.s45[[2]]$speed[i]>=12.35)
  {
    arima.s45[[2]]$power[i]=4
  }
  
  if(arima.s45[[2]]$speed[i]<0)
  {
    arima.s45[[2]]$speed[i]=0
  }
  
  if(arima.s45[[2]]$speed[i]==0)
  {
    arima.s45[[2]]$power[i]=0
  }
  
}
#Residuals
res.s45<-vector("list",length(arima.s45))
count1<-1
count2<-length(arima.s45)/2 + 1
for(i in 1:length(data.s45))
{
  names(res.s45)[[count1]]<-paste((names(data.s45)[[i]]),".april",sep='')
  res.s45[[count1]]<-data.s45[[i]]$power["2011-04-01/2011-04-30"]-arima.s45[[count1]]$power
  count1<-count1+1
  
  names(res.s45)[[count2]]<-paste(names(data.s45)[[i]],".july",sep='')
  res.s45[[count2]]<-data.s45[[i]]$power["2011-07-01/2011-07-31"]-arima.s45[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s45)/2 + 1
for(i in 1:length(data.s45))
{
  
  plot(data.s45[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s45", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s45[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s45[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s45[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s45", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s45[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s45[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s45<-vector("list",length(arima.s45))
count1<-1
count2<-length(arima.s45)/2 + 1
for(i in 1:length(data.s45))
{
  names(mape.s45)[[count1]]<-paste((names(data.s45)[[i]]),".april",sep='')
  div<-res.s45[[count1]]/data.s45[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s45[[count1]]<-100*sum(abs(div))/length(data.s45[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s45)[[count2]]<-paste(names(data.s45)[[i]],".july",sep='')
  div<-res.s45[[count2]]/data.s45[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s45[[count2]]<-100*sum(abs(div))/nrow(data.s45[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s45))
{
  write.zoo(arima.s45[[i]], file = paste(names(arima.s45)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s45))
{
  write.zoo(data.s45[[i]], file = paste(names(data.s45)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s45))
{
  write.zoo(res.s45[[i]], file = paste(names(res.s45)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s45))
{
  write.zoo(mape.s45[[i]], file = paste(names(mape.s45)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 45 over -----
save.image()


#---------SITE 46 begin -----
#Reading files by zone
s46<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==46)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s46[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc46<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==46)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc46[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s46) <- sprintf('s46.%d', 1:numfiles)
for(i in 1:numfiles)
{s46[[i]]$Time <- with(s46[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s46))
{names(s46[[i]])[names(s46[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s46))
{names(s46[[i]])[names(s46[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s46.ts<-vector("list", length(s46))
names(s46.ts) <- sprintf('s46.%d', 1:numfiles)

for(i in 1:length(s46))
{
  s46.ts[[i]]<-xts(s46[[i]]$speed,s46[[i]]$Time)
  colnames(s46.ts[[i]])='speed'
  s46.ts[[i]]$power<-s46[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s46))
for(i in 1:length(s46))
{
  ep<-endpoints(s46.ts[[i]], on="hours", k=1)
  a<-(period.apply(s46.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s46.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s46 <- list()
maxindex<-which.max(CF1)
data.s46[[oldcount]]<-s46.ts[[which.max(CF1)]]
names(data.s46)[[oldcount]] <- names(s46.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc46 <- lapply( paste('sloc46.', 1:numfiles, sep=''), get)
names(sloc46) <- sprintf('sloc46.%d', 1:numfiles)

num<-seq(1,length(s46))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s46))
long1<-vector(length=length(s46))
for(i in 1:length(s46))
{
  lat1[i]<-sloc46[[i]]$V2[3]
  long1[i]<-sloc46[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s46))
{
  ep<-endpoints(data.s46[[i]], on="hours", k=1)
  data.s46[[i]]<-(period.apply(data.s46[[i]],ep,mean))
}

#sd(data.s46[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s46[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s46[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s46<-vector("list",(length(data.s46)*2))
count<-1
for(j in 1:length(data.s46))
{
  names(arima.s46)[[count]]<-paste((names(data.s46)[[j]]),".april",sep='')
  arima.s46[[count]]<-data.s46[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s46[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s46[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s46[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s46)[[count]]<-paste(names(data.s46)[[1]],".july",sep='')
arima.s46[[count]]<-data.s46[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s46[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s46[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s46[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s46
#Making a power curve

speed=as.double(s46[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s46[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s46[[1]]$power<-lookup(round_any(as.numeric(arima.s46[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s46[[2]]$power<-lookup(round_any(as.numeric(arima.s46[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s46<-vector("list",length(arima.s46))
count1<-1
count2<-length(arima.s46)/2 + 1
for(i in 1:length(data.s46))
{
  names(res.s46)[[count1]]<-paste((names(data.s46)[[i]]),".april",sep='')
  res.s46[[count1]]<-data.s46[[i]]$power["2011-04-01/2011-04-30"]-arima.s46[[count1]]$power
  count1<-count1+1
  
  names(res.s46)[[count2]]<-paste(names(data.s46)[[i]],".july",sep='')
  res.s46[[count2]]<-data.s46[[i]]$power["2011-07-01/2011-07-31"]-arima.s46[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s46)/2 + 1
for(i in 1:length(data.s46))
{
  
  plot(data.s46[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s46", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s46[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s46[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s46[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s46", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s46[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s46[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s46<-vector("list",length(arima.s46))
count1<-1
count2<-length(arima.s46)/2 + 1
for(i in 1:length(data.s46))
{
  names(mape.s46)[[count1]]<-paste((names(data.s46)[[i]]),".april",sep='')
  div<-res.s46[[count1]]/data.s46[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s46[[count1]]<-100*sum(abs(div))/length(data.s46[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s46)[[count2]]<-paste(names(data.s46)[[i]],".july",sep='')
  div<-res.s46[[count2]]/data.s46[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s46[[count2]]<-100*sum(abs(div))/nrow(data.s46[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s46))
{
  write.zoo(arima.s46[[i]], file = paste(names(arima.s46)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s46))
{
  write.zoo(data.s46[[i]], file = paste(names(data.s46)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s46))
{
  write.zoo(res.s46[[i]], file = paste(names(res.s46)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s46))
{
  write.zoo(mape.s46[[i]], file = paste(names(mape.s46)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 46 over -----
save.image()


#---------SITE 47 begin -----
#Reading files by zone
s47<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==47)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s47[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc47<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==47)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc47[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s47) <- sprintf('s47.%d', 1:numfiles)
for(i in 1:numfiles)
{s47[[i]]$Time <- with(s47[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s47))
{names(s47[[i]])[names(s47[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s47))
{names(s47[[i]])[names(s47[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s47.ts<-vector("list", length(s47))
names(s47.ts) <- sprintf('s47.%d', 1:numfiles)

for(i in 1:length(s47))
{
  s47.ts[[i]]<-xts(s47[[i]]$speed,s47[[i]]$Time)
  colnames(s47.ts[[i]])='speed'
  s47.ts[[i]]$power<-s47[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s47))
for(i in 1:length(s47))
{
  ep<-endpoints(s47.ts[[i]], on="hours", k=1)
  a<-(period.apply(s47.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s47.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s47 <- list()
maxindex<-which.max(CF1)
data.s47[[oldcount]]<-s47.ts[[which.max(CF1)]]
names(data.s47)[[oldcount]] <- names(s47.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc47 <- lapply( paste('sloc47.', 1:numfiles, sep=''), get)
names(sloc47) <- sprintf('sloc47.%d', 1:numfiles)

num<-seq(1,length(s47))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s47))
long1<-vector(length=length(s47))
for(i in 1:length(s47))
{
  lat1[i]<-sloc47[[i]]$V2[3]
  long1[i]<-sloc47[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s47))
{
  ep<-endpoints(data.s47[[i]], on="hours", k=1)
  data.s47[[i]]<-(period.apply(data.s47[[i]],ep,mean))
}

#sd(data.s47[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s47[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s47[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s47<-vector("list",(length(data.s47)*2))
count<-1
for(j in 1:length(data.s47))
{
  names(arima.s47)[[count]]<-paste((names(data.s47)[[j]]),".april",sep='')
  arima.s47[[count]]<-data.s47[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s47[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s47[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s47[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s47)[[count]]<-paste(names(data.s47)[[1]],".july",sep='')
arima.s47[[count]]<-data.s47[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s47[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s47[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s47[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s47
#Making a power curve

speed=as.double(s47[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s47[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s47[[1]]$power<-lookup(round_any(as.numeric(arima.s47[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s47[[2]]$power<-lookup(round_any(as.numeric(arima.s47[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)



for(i in 1:nrow(arima.s47[[1]]))
{
  
  
  if(arima.s47[[1]]$speed[i]>=13)
  {
    arima.s47[[1]]$power[i]=16
  }
  
  if(arima.s47[[1]]$speed[i]<0)
  {
    arima.s47[[1]]$speed[i]=0
  }
  
  if(arima.s47[[1]]$speed[i]==0)
  {
    arima.s47[[1]]$power[i]=0
  }
  
}

for(i in 1:nrow(arima.s47[[2]]))
{
  if(arima.s47[[2]]$speed[i]>=13)
  {
    arima.s47[[2]]$power[i]=16
  }
  
  if(arima.s47[[2]]$speed[i]<0)
  {
    arima.s47[[2]]$speed[i]=0
  }
  
  if(arima.s47[[2]]$speed[i]==0)
  {
    arima.s47[[2]]$power[i]=0
  }
  
}

#Residuals
res.s47<-vector("list",length(arima.s47))
count1<-1
count2<-length(arima.s47)/2 + 1
for(i in 1:length(data.s47))
{
  names(res.s47)[[count1]]<-paste((names(data.s47)[[i]]),".april",sep='')
  res.s47[[count1]]<-data.s47[[i]]$power["2011-04-01/2011-04-30"]-arima.s47[[count1]]$power
  count1<-count1+1
  
  names(res.s47)[[count2]]<-paste(names(data.s47)[[i]],".july",sep='')
  res.s47[[count2]]<-data.s47[[i]]$power["2011-07-01/2011-07-31"]-arima.s47[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s47)/2 + 1
for(i in 1:length(data.s47))
{
  
  plot(data.s47[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s47", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s47[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s47[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s47[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s47", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s47[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s47[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s47<-vector("list",length(arima.s47))
count1<-1
count2<-length(arima.s47)/2 + 1
for(i in 1:length(data.s47))
{
  names(mape.s47)[[count1]]<-paste((names(data.s47)[[i]]),".april",sep='')
  div<-res.s47[[count1]]/data.s47[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s47[[count1]]<-100*sum(abs(div))/length(data.s47[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s47)[[count2]]<-paste(names(data.s47)[[i]],".july",sep='')
  div<-res.s47[[count2]]/data.s47[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s47[[count2]]<-100*sum(abs(div))/nrow(data.s47[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s47))
{
  write.zoo(arima.s47[[i]], file = paste(names(arima.s47)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s47))
{
  write.zoo(data.s47[[i]], file = paste(names(data.s47)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s47))
{
  write.zoo(res.s47[[i]], file = paste(names(res.s47)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s47))
{
  write.zoo(mape.s47[[i]], file = paste(names(mape.s47)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 47 over -----
save.image()


#---------SITE 48 begin -----
#Reading files by zone
s48<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==48)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s48[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc48<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==48)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc48[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s48) <- sprintf('s48.%d', 1:numfiles)
for(i in 1:numfiles)
{s48[[i]]$Time <- with(s48[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s48))
{names(s48[[i]])[names(s48[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s48))
{names(s48[[i]])[names(s48[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s48.ts<-vector("list", length(s48))
names(s48.ts) <- sprintf('s48.%d', 1:numfiles)

for(i in 1:length(s48))
{
  s48.ts[[i]]<-xts(s48[[i]]$speed,s48[[i]]$Time)
  colnames(s48.ts[[i]])='speed'
  s48.ts[[i]]$power<-s48[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s48))
for(i in 1:length(s48))
{
  ep<-endpoints(s48.ts[[i]], on="hours", k=1)
  a<-(period.apply(s48.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s48.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s48 <- list()
maxindex<-which.max(CF1)
data.s48[[oldcount]]<-s48.ts[[which.max(CF1)]]
names(data.s48)[[oldcount]] <- names(s48.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc48 <- lapply( paste('sloc48.', 1:numfiles, sep=''), get)
names(sloc48) <- sprintf('sloc48.%d', 1:numfiles)

num<-seq(1,length(s48))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s48))
long1<-vector(length=length(s48))
for(i in 1:length(s48))
{
  lat1[i]<-sloc48[[i]]$V2[3]
  long1[i]<-sloc48[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s48))
{
  ep<-endpoints(data.s48[[i]], on="hours", k=1)
  data.s48[[i]]<-(period.apply(data.s48[[i]],ep,mean))
}

#sd(data.s48[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s48[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s48[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s48<-vector("list",(length(data.s48)*2))
count<-1
for(j in 1:length(data.s48))
{
  names(arima.s48)[[count]]<-paste((names(data.s48)[[j]]),".april",sep='')
  arima.s48[[count]]<-data.s48[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s48[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s48[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s48[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s48)[[count]]<-paste(names(data.s48)[[1]],".july",sep='')
arima.s48[[count]]<-data.s48[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s48[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s48[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s48[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s48
#Making a power curve

speed=as.double(s48[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s48[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s48[[1]]$power<-lookup(round_any(as.numeric(arima.s48[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s48[[2]]$power<-lookup(round_any(as.numeric(arima.s48[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s48<-vector("list",length(arima.s48))
count1<-1
count2<-length(arima.s48)/2 + 1
for(i in 1:length(data.s48))
{
  names(res.s48)[[count1]]<-paste((names(data.s48)[[i]]),".april",sep='')
  res.s48[[count1]]<-data.s48[[i]]$power["2011-04-01/2011-04-30"]-arima.s48[[count1]]$power
  count1<-count1+1
  
  names(res.s48)[[count2]]<-paste(names(data.s48)[[i]],".july",sep='')
  res.s48[[count2]]<-data.s48[[i]]$power["2011-07-01/2011-07-31"]-arima.s48[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s48)/2 + 1
for(i in 1:length(data.s48))
{
  
  plot(data.s48[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s48", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s48[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s48[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s48[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s48", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s48[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s48[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s48<-vector("list",length(arima.s48))
count1<-1
count2<-length(arima.s48)/2 + 1
for(i in 1:length(data.s48))
{
  names(mape.s48)[[count1]]<-paste((names(data.s48)[[i]]),".april",sep='')
  div<-res.s48[[count1]]/data.s48[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s48[[count1]]<-100*sum(abs(div))/length(data.s48[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s48)[[count2]]<-paste(names(data.s48)[[i]],".july",sep='')
  div<-res.s48[[count2]]/data.s48[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s48[[count2]]<-100*sum(abs(div))/nrow(data.s48[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s48))
{
  write.zoo(arima.s48[[i]], file = paste(names(arima.s48)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s48))
{
  write.zoo(data.s48[[i]], file = paste(names(data.s48)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s48))
{
  write.zoo(res.s48[[i]], file = paste(names(res.s48)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s48))
{
  write.zoo(mape.s48[[i]], file = paste(names(mape.s48)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 48 over -----
save.image()


#---------SITE 49 begin -----
#Reading files by zone
s49<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==49)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s49[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc49<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==49)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc49[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s49) <- sprintf('s49.%d', 1:numfiles)
for(i in 1:numfiles)
{s49[[i]]$Time <- with(s49[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s49))
{names(s49[[i]])[names(s49[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s49))
{names(s49[[i]])[names(s49[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s49.ts<-vector("list", length(s49))
names(s49.ts) <- sprintf('s49.%d', 1:numfiles)

for(i in 1:length(s49))
{
  s49.ts[[i]]<-xts(s49[[i]]$speed,s49[[i]]$Time)
  colnames(s49.ts[[i]])='speed'
  s49.ts[[i]]$power<-s49[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s49))
for(i in 1:length(s49))
{
  ep<-endpoints(s49.ts[[i]], on="hours", k=1)
  a<-(period.apply(s49.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s49.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s49 <- list()
maxindex<-which.max(CF1)
data.s49[[oldcount]]<-s49.ts[[which.max(CF1)]]
names(data.s49)[[oldcount]] <- names(s49.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc49 <- lapply( paste('sloc49.', 1:numfiles, sep=''), get)
names(sloc49) <- sprintf('sloc49.%d', 1:numfiles)

num<-seq(1,length(s49))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s49))
long1<-vector(length=length(s49))
for(i in 1:length(s49))
{
  lat1[i]<-sloc49[[i]]$V2[3]
  long1[i]<-sloc49[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s49))
{
  ep<-endpoints(data.s49[[i]], on="hours", k=1)
  data.s49[[i]]<-(period.apply(data.s49[[i]],ep,mean))
}

#sd(data.s49[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s49[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s49[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s49<-vector("list",(length(data.s49)*2))
count<-1
for(j in 1:length(data.s49))
{
  names(arima.s49)[[count]]<-paste((names(data.s49)[[j]]),".april",sep='')
  arima.s49[[count]]<-data.s49[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s49[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s49[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s49[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s49)[[count]]<-paste(names(data.s49)[[1]],".july",sep='')
arima.s49[[count]]<-data.s49[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s49[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s49[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s49[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s49
#Making a power curve

speed=as.double(s49[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s49[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s49[[1]]$power<-lookup(round_any(as.numeric(arima.s49[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s49[[2]]$power<-lookup(round_any(as.numeric(arima.s49[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s49<-vector("list",length(arima.s49))
count1<-1
count2<-length(arima.s49)/2 + 1
for(i in 1:length(data.s49))
{
  names(res.s49)[[count1]]<-paste((names(data.s49)[[i]]),".april",sep='')
  res.s49[[count1]]<-data.s49[[i]]$power["2011-04-01/2011-04-30"]-arima.s49[[count1]]$power
  count1<-count1+1
  
  names(res.s49)[[count2]]<-paste(names(data.s49)[[i]],".july",sep='')
  res.s49[[count2]]<-data.s49[[i]]$power["2011-07-01/2011-07-31"]-arima.s49[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s49)/2 + 1
for(i in 1:length(data.s49))
{
  
  plot(data.s49[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s49", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s49[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s49[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s49[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s49", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s49[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s49[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s49<-vector("list",length(arima.s49))
count1<-1
count2<-length(arima.s49)/2 + 1
for(i in 1:length(data.s49))
{
  names(mape.s49)[[count1]]<-paste((names(data.s49)[[i]]),".april",sep='')
  div<-res.s49[[count1]]/data.s49[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s49[[count1]]<-100*sum(abs(div))/length(data.s49[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s49)[[count2]]<-paste(names(data.s49)[[i]],".july",sep='')
  div<-res.s49[[count2]]/data.s49[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s49[[count2]]<-100*sum(abs(div))/nrow(data.s49[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s49))
{
  write.zoo(arima.s49[[i]], file = paste(names(arima.s49)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s49))
{
  write.zoo(data.s49[[i]], file = paste(names(data.s49)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s49))
{
  write.zoo(res.s49[[i]], file = paste(names(res.s49)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s49))
{
  write.zoo(mape.s49[[i]], file = paste(names(mape.s49)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 49 over -----
save.image()


#---------SITE 50 begin -----
#Reading files by zone
s50<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==50)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s50[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc50<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==50)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc50[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s50) <- sprintf('s50.%d', 1:numfiles)
for(i in 1:numfiles)
{s50[[i]]$Time <- with(s50[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s50))
{names(s50[[i]])[names(s50[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s50))
{names(s50[[i]])[names(s50[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s50.ts<-vector("list", length(s50))
names(s50.ts) <- sprintf('s50.%d', 1:numfiles)

for(i in 1:length(s50))
{
  s50.ts[[i]]<-xts(s50[[i]]$speed,s50[[i]]$Time)
  colnames(s50.ts[[i]])='speed'
  s50.ts[[i]]$power<-s50[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s50))
for(i in 1:length(s50))
{
  ep<-endpoints(s50.ts[[i]], on="hours", k=1)
  a<-(period.apply(s50.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s50.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s50 <- list()
maxindex<-which.max(CF1)
data.s50[[oldcount]]<-s50.ts[[which.max(CF1)]]
names(data.s50)[[oldcount]] <- names(s50.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc50 <- lapply( paste('sloc50.', 1:numfiles, sep=''), get)
names(sloc50) <- sprintf('sloc50.%d', 1:numfiles)

num<-seq(1,length(s50))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s50))
long1<-vector(length=length(s50))
for(i in 1:length(s50))
{
  lat1[i]<-sloc50[[i]]$V2[3]
  long1[i]<-sloc50[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s50))
{
  ep<-endpoints(data.s50[[i]], on="hours", k=1)
  data.s50[[i]]<-(period.apply(data.s50[[i]],ep,mean))
}

#sd(data.s50[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s50[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s50[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s50<-vector("list",(length(data.s50)*2))
count<-1
for(j in 1:length(data.s50))
{
  names(arima.s50)[[count]]<-paste((names(data.s50)[[j]]),".april",sep='')
  arima.s50[[count]]<-data.s50[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s50[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s50[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s50[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s50)[[count]]<-paste(names(data.s50)[[1]],".july",sep='')
arima.s50[[count]]<-data.s50[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s50[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s50[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s50[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s50
#Making a power curve

speed=as.double(s50[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s50[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s50[[1]]$power<-lookup(round_any(as.numeric(arima.s50[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s50[[2]]$power<-lookup(round_any(as.numeric(arima.s50[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s50<-vector("list",length(arima.s50))
count1<-1
count2<-length(arima.s50)/2 + 1
for(i in 1:length(data.s50))
{
  names(res.s50)[[count1]]<-paste((names(data.s50)[[i]]),".april",sep='')
  res.s50[[count1]]<-data.s50[[i]]$power["2011-04-01/2011-04-30"]-arima.s50[[count1]]$power
  count1<-count1+1
  
  names(res.s50)[[count2]]<-paste(names(data.s50)[[i]],".july",sep='')
  res.s50[[count2]]<-data.s50[[i]]$power["2011-07-01/2011-07-31"]-arima.s50[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s50)/2 + 1
for(i in 1:length(data.s50))
{
  
  plot(data.s50[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s50", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s50[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s50[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s50[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s50", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s50[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s50[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s50<-vector("list",length(arima.s50))
count1<-1
count2<-length(arima.s50)/2 + 1
for(i in 1:length(data.s50))
{
  names(mape.s50)[[count1]]<-paste((names(data.s50)[[i]]),".april",sep='')
  div<-res.s50[[count1]]/data.s50[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s50[[count1]]<-100*sum(abs(div))/length(data.s50[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s50)[[count2]]<-paste(names(data.s50)[[i]],".july",sep='')
  div<-res.s50[[count2]]/data.s50[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s50[[count2]]<-100*sum(abs(div))/nrow(data.s50[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s50))
{
  write.zoo(arima.s50[[i]], file = paste(names(arima.s50)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s50))
{
  write.zoo(data.s50[[i]], file = paste(names(data.s50)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s50))
{
  write.zoo(res.s50[[i]], file = paste(names(res.s50)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s50))
{
  write.zoo(mape.s50[[i]], file = paste(names(mape.s50)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 50 over -----
save.image()

for (i in 1:length(data.s50))
{
  write.zoo(data.s50[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s50)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s50[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s50)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s49))
{
  write.zoo(data.s49[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s49)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s49[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s49)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s48))
{
  write.zoo(data.s48[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s48)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s48[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s48)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s47))
{
  write.zoo(data.s47[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s47)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s47[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s47)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s46))
{
  write.zoo(data.s46[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s46)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s46[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s46)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s45))
{
  write.zoo(data.s45[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s45)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s45[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s45)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s44))
{
  write.zoo(data.s44[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s44)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s44[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s44)[[i]],'-july power data.csv',sep=''),sep=",")
  
}


for (i in 1:length(data.s43))
{
  write.zoo(data.s43[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s43)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s43[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s43)[[i]],'-july power data.csv',sep=''),sep=",")
  
}


for (i in 1:length(data.s42))
{
  write.zoo(data.s42[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s42)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s42[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s42)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s41))
{
  write.zoo(data.s41[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s41)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s41[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s41)[[i]],'-july power data.csv',sep=''),sep=",")
  
}