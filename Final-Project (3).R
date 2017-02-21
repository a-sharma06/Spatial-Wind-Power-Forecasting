
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


#--------SITE 21 Begin----
#Reading files by zone
s21<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==21)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s21[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc21<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==21)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc21[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s21) <- sprintf('s21.%d', 1:numfiles)
for(i in 1:numfiles)
{s21[[i]]$Time <- with(s21[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s21))
{names(s21[[i]])[names(s21[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s21))
{names(s21[[i]])[names(s21[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s21.ts<-vector("list", length(s21))
names(s21.ts) <- sprintf('s21.%d', 1:numfiles)

for(i in 1:length(s21))
{
  s21.ts[[i]]<-xts(s21[[i]]$speed,s21[[i]]$Time)
  colnames(s21.ts[[i]])='speed'
  s21.ts[[i]]$power<-s21[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s21))
for(i in 1:length(s21))
{
  ep<-endpoints(s21.ts[[i]], on="hours", k=1)
  a<-(period.apply(s21.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s21.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s21 <- list()
maxindex<-which.max(CF1)
data.s21[[oldcount]]<-s21.ts[[which.max(CF1)]]
names(data.s21)[[oldcount]] <- names(s21.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc21 <- lapply( paste('sloc21.', 1:numfiles, sep=''), get)
names(sloc21) <- sprintf('sloc21.%d', 1:numfiles)

num<-seq(1,length(s21))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s21))
long1<-vector(length=length(s21))
for(i in 1:length(s21))
{
  lat1[i]<-sloc21[[i]]$V2[3]
  long1[i]<-sloc21[[i]]$V2[2]
}

#------Data collection for Site 1 over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s21))
{
  ep<-endpoints(data.s21[[i]], on="hours", k=1)
  data.s21[[i]]<-(period.apply(data.s21[[i]],ep,mean))
}

#sd(data.s21[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s21[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s21[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s21<-vector("list",(length(data.s21)*2))
count<-1
for(j in 1:length(data.s21))
{
  names(arima.s21)[[count]]<-paste((names(data.s21)[[j]]),".april",sep='')
  arima.s21[[count]]<-data.s21[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s21[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s21[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s21[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}



  names(arima.s21)[[count]]<-paste(names(data.s21)[[1]],".july",sep='')
  arima.s21[[count]]<-data.s21[[1]]$speed["2011-07-01/2011-07-31"]
  
  july<-data.s21[[1]]$speed["2011-07-01/2011-07-31"]
  
  for(i in 0:(length(data.s21[[1]]$speed["2011-07-01/2011-07-31"])-73))
  {
    a=i+1
    b=i+72
    example<-july$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s21[[count]][i+73]<-as.numeric(point.forecast)
  }


#backup<-arima.s21
#Making a power curve

speed=as.double(s21[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s21[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s21[[1]]$power<-lookup(round_any(as.numeric(arima.s21[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s21[[2]]$power<-lookup(round_any(as.numeric(arima.s21[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s21<-vector("list",length(arima.s21))
count1<-1
count2<-length(arima.s21)/2 + 1
for(i in 1:length(data.s21))
{
  names(res.s21)[[count1]]<-paste((names(data.s21)[[i]]),".april",sep='')
  res.s21[[count1]]<-data.s21[[i]]$power["2011-04-01/2011-04-30"]-arima.s21[[count1]]$power
  count1<-count1+1
  
  names(res.s21)[[count2]]<-paste(names(data.s21)[[i]],".july",sep='')
  res.s21[[count2]]<-data.s21[[i]]$power["2011-07-01/2011-07-31"]-arima.s21[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s21)/2 + 1
for(i in 1:length(data.s21))
{
  
  plot(data.s21[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s21", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s21[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s21[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s21[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s21", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s21[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s21[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s21<-vector("list",length(arima.s21))
count1<-1
count2<-length(arima.s21)/2 + 1
for(i in 1:length(data.s21))
{
  names(mape.s21)[[count1]]<-paste((names(data.s21)[[i]]),".april",sep='')
  div<-res.s21[[count1]]/data.s21[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s21[[count1]]<-100*sum(abs(div))/length(data.s21[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s21)[[count2]]<-paste(names(data.s21)[[i]],".july",sep='')
  div<-res.s21[[count2]]/data.s21[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s21[[count2]]<-100*sum(abs(div))/nrow(data.s21[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s21))
{
  write.zoo(arima.s21[[i]], file = paste(names(arima.s21)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s21))
{
  write.zoo(data.s21[[i]], file = paste(names(data.s21)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s21))
{
  write.zoo(res.s21[[i]], file = paste(names(res.s21)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s21))
{
  write.zoo(mape.s21[[i]], file = paste(names(mape.s21)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 21 over -----

save.image()


#---------SITE 22 begin -----
#Reading files by zone
s22<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==22)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s22[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc22<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==22)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc22[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s22) <- sprintf('s22.%d', 1:numfiles)
for(i in 1:numfiles)
{s22[[i]]$Time <- with(s22[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s22))
{names(s22[[i]])[names(s22[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s22))
{names(s22[[i]])[names(s22[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s22.ts<-vector("list", length(s22))
names(s22.ts) <- sprintf('s22.%d', 1:numfiles)

for(i in 1:length(s22))
{
  s22.ts[[i]]<-xts(s22[[i]]$speed,s22[[i]]$Time)
  colnames(s22.ts[[i]])='speed'
  s22.ts[[i]]$power<-s22[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s22))
for(i in 1:length(s22))
{
  ep<-endpoints(s22.ts[[i]], on="hours", k=1)
  a<-(period.apply(s22.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s22.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s22 <- list()
maxindex<-which.max(CF1)
data.s22[[oldcount]]<-s22.ts[[which.max(CF1)]]
names(data.s22)[[oldcount]] <- names(s22.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc22 <- lapply( paste('sloc22.', 1:numfiles, sep=''), get)
names(sloc22) <- sprintf('sloc22.%d', 1:numfiles)

num<-seq(1,length(s22))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s22))
long1<-vector(length=length(s22))
for(i in 1:length(s22))
{
  lat1[i]<-sloc22[[i]]$V2[3]
  long1[i]<-sloc22[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s22))
{
  ep<-endpoints(data.s22[[i]], on="hours", k=1)
  data.s22[[i]]<-(period.apply(data.s22[[i]],ep,mean))
}

#sd(data.s22[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s22[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s22[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s22<-vector("list",(length(data.s22)*2))
count<-1
for(j in 1:length(data.s22))
{
  names(arima.s22)[[count]]<-paste((names(data.s22)[[j]]),".april",sep='')
  arima.s22[[count]]<-data.s22[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s22[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s22[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s22[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s22)[[count]]<-paste(names(data.s22)[[1]],".july",sep='')
arima.s22[[count]]<-data.s22[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s22[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s22[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s22[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s22
#Making a power curve

speed=as.double(s22[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s22[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s22[[1]]$power<-lookup(round_any(as.numeric(arima.s22[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s22[[2]]$power<-lookup(round_any(as.numeric(arima.s22[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)


for(i in 1:nrow(arima.s22[[1]]))
{
  
  
  if(arima.s22[[1]]$speed[i]>=13.0)
  {
    arima.s22[[1]]$power[i]=16
  }
  
  if(arima.s22[[1]]$speed[i]<0)
  {
    arima.s22[[1]]$speed[i]=0
  }
  
  if(arima.s22[[1]]$speed[i]==0)
  {
    arima.s22[[1]]$power[i]=0
  }
  
}

for(i in 1:nrow(arima.s22[[2]]))
{
  if(arima.s22[[2]]$speed[i]>=12.35)
  {
    arima.s22[[2]]$power[i]=2
  }
  
  if(arima.s22[[2]]$speed[i]<0)
  {
    arima.s22[[2]]$speed[i]=0
  }
  
  if(arima.s22[[2]]$speed[i]==0)
  {
    arima.s22[[2]]$power[i]=0
  }
  
}

#Residuals
res.s22<-vector("list",length(arima.s22))
count1<-1
count2<-length(arima.s22)/2 + 1
for(i in 1:length(data.s22))
{
  names(res.s22)[[count1]]<-paste((names(data.s22)[[i]]),".april",sep='')
  res.s22[[count1]]<-data.s22[[i]]$power["2011-04-01/2011-04-30"]-arima.s22[[count1]]$power
  count1<-count1+1
  
  names(res.s22)[[count2]]<-paste(names(data.s22)[[i]],".july",sep='')
  res.s22[[count2]]<-data.s22[[i]]$power["2011-07-01/2011-07-31"]-arima.s22[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s22)/2 + 1
for(i in 1:length(data.s22))
{
  
  plot(data.s22[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s22", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s22[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s22[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s22[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s22", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s22[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s22[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s22<-vector("list",length(arima.s22))
count1<-1
count2<-length(arima.s22)/2 + 1
for(i in 1:length(data.s22))
{
  names(mape.s22)[[count1]]<-paste((names(data.s22)[[i]]),".april",sep='')
  div<-res.s22[[count1]]/data.s22[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s22[[count1]]<-100*sum(abs(div))/length(data.s22[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s22)[[count2]]<-paste(names(data.s22)[[i]],".july",sep='')
  div<-res.s22[[count2]]/data.s22[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s22[[count2]]<-100*sum(abs(div))/nrow(data.s22[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s22))
{
  write.zoo(arima.s22[[i]], file = paste(names(arima.s22)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s22))
{
  write.zoo(data.s22[[i]], file = paste(names(data.s22)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s22))
{
  write.zoo(res.s22[[i]], file = paste(names(res.s22)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s22))
{
  write.zoo(mape.s22[[i]], file = paste(names(mape.s22)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 22 over -----

save.image()


#---------SITE 23 begin -----
#Reading files by zone
s23<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==23)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s23[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc13<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==23)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc13[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s23) <- sprintf('s23.%d', 1:numfiles)
for(i in 1:numfiles)
{s23[[i]]$Time <- with(s23[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s23))
{names(s23[[i]])[names(s23[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s23))
{names(s23[[i]])[names(s23[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s23.ts<-vector("list", length(s23))
names(s23.ts) <- sprintf('s23.%d', 1:numfiles)

for(i in 1:length(s23))
{
  s23.ts[[i]]<-xts(s23[[i]]$speed,s23[[i]]$Time)
  colnames(s23.ts[[i]])='speed'
  s23.ts[[i]]$power<-s23[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s23))
for(i in 1:length(s23))
{
  ep<-endpoints(s23.ts[[i]], on="hours", k=1)
  a<-(period.apply(s23.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s23.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s23 <- list()
maxindex<-which.max(CF1)
data.s23[[oldcount]]<-s23.ts[[which.max(CF1)]]
names(data.s23)[[oldcount]] <- names(s23.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc13 <- lapply( paste('sloc13.', 1:numfiles, sep=''), get)
names(sloc13) <- sprintf('sloc13.%d', 1:numfiles)

num<-seq(1,length(s23))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s23))
long1<-vector(length=length(s23))
for(i in 1:length(s23))
{
  lat1[i]<-sloc13[[i]]$V2[3]
  long1[i]<-sloc13[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s23))
{
  ep<-endpoints(data.s23[[i]], on="hours", k=1)
  data.s23[[i]]<-(period.apply(data.s23[[i]],ep,mean))
}

#sd(data.s23[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s23[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s23[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s23<-vector("list",(length(data.s23)*2))
count<-1
for(j in 1:length(data.s23))
{
  names(arima.s23)[[count]]<-paste((names(data.s23)[[j]]),".april",sep='')
  arima.s23[[count]]<-data.s23[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s23[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s23[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s23[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s23)[[count]]<-paste(names(data.s23)[[1]],".july",sep='')
arima.s23[[count]]<-data.s23[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s23[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s23[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s23[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s23
#Making a power curve

speed=as.double(s23[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s23[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s23[[1]]$power<-lookup(round_any(as.numeric(arima.s23[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s23[[2]]$power<-lookup(round_any(as.numeric(arima.s23[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s23<-vector("list",length(arima.s23))
count1<-1
count2<-length(arima.s23)/2 + 1
for(i in 1:length(data.s23))
{
  names(res.s23)[[count1]]<-paste((names(data.s23)[[i]]),".april",sep='')
  res.s23[[count1]]<-data.s23[[i]]$power["2011-04-01/2011-04-30"]-arima.s23[[count1]]$power
  count1<-count1+1
  
  names(res.s23)[[count2]]<-paste(names(data.s23)[[i]],".july",sep='')
  res.s23[[count2]]<-data.s23[[i]]$power["2011-07-01/2011-07-31"]-arima.s23[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s23)/2 + 1
for(i in 1:length(data.s23))
{
  
  plot(data.s23[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s23", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s23[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s23[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s23[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s23", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s23[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s23[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s23<-vector("list",length(arima.s23))
count1<-1
count2<-length(arima.s23)/2 + 1
for(i in 1:length(data.s23))
{
  names(mape.s23)[[count1]]<-paste((names(data.s23)[[i]]),".april",sep='')
  div<-res.s23[[count1]]/data.s23[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s23[[count1]]<-100*sum(abs(div))/length(data.s23[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s23)[[count2]]<-paste(names(data.s23)[[i]],".july",sep='')
  div<-res.s23[[count2]]/data.s23[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s23[[count2]]<-100*sum(abs(div))/nrow(data.s23[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s23))
{
  write.zoo(arima.s23[[i]], file = paste(names(arima.s23)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s23))
{
  write.zoo(data.s23[[i]], file = paste(names(data.s23)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s23))
{
  write.zoo(res.s23[[i]], file = paste(names(res.s23)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s23))
{
  write.zoo(mape.s23[[i]], file = paste(names(mape.s23)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 23 over -----
save.image()



#---------SITE 24 begin- (no turbine) -----
#Reading files by zone
s24<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==24)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s24[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc14<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==24)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc14[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s24) <- sprintf('s24.%d', 1:numfiles)
for(i in 1:numfiles)
{s24[[i]]$Time <- with(s24[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s24))
{names(s24[[i]])[names(s24[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s24))
{names(s24[[i]])[names(s24[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s24.ts<-vector("list", length(s24))
names(s24.ts) <- sprintf('s24.%d', 1:numfiles)

for(i in 1:length(s24))
{
  s24.ts[[i]]<-xts(s24[[i]]$speed,s24[[i]]$Time)
  colnames(s24.ts[[i]])='speed'
  s24.ts[[i]]$power<-s24[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s24))
for(i in 1:length(s24))
{
  ep<-endpoints(s24.ts[[i]], on="hours", k=1)
  a<-(period.apply(s24.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s24.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s24 <- list()
maxindex<-which.max(CF1)
data.s24[[oldcount]]<-s24.ts[[which.max(CF1)]]
names(data.s24)[[oldcount]] <- names(s24.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc14 <- lapply( paste('sloc14.', 1:numfiles, sep=''), get)
names(sloc14) <- sprintf('sloc14.%d', 1:numfiles)

num<-seq(1,length(s24))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s24))
long1<-vector(length=length(s24))
for(i in 1:length(s24))
{
  lat1[i]<-sloc14[[i]]$V2[3]
  long1[i]<-sloc14[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s24))
{
  ep<-endpoints(data.s24[[i]], on="hours", k=1)
  data.s24[[i]]<-(period.apply(data.s24[[i]],ep,mean))
}

#sd(data.s24[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s24[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s24[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s24<-vector("list",(length(data.s24)*2))
count<-1
for(j in 1:length(data.s24))
{
  names(arima.s24)[[count]]<-paste((names(data.s24)[[j]]),".april",sep='')
  arima.s24[[count]]<-data.s24[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s24[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s24[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s24[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s24)[[count]]<-paste(names(data.s24)[[1]],".july",sep='')
arima.s24[[count]]<-data.s24[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s24[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s24[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s24[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s24
#Making a power curve

speed=as.double(s24[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s24[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s24[[1]]$power<-lookup(round_any(as.numeric(arima.s24[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s24[[2]]$power<-lookup(round_any(as.numeric(arima.s24[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s24<-vector("list",length(arima.s24))
count1<-1
count2<-length(arima.s24)/2 + 1
for(i in 1:length(data.s24))
{
  names(res.s24)[[count1]]<-paste((names(data.s24)[[i]]),".april",sep='')
  res.s24[[count1]]<-data.s24[[i]]$power["2011-04-01/2011-04-30"]-arima.s24[[count1]]$power
  count1<-count1+1
  
  names(res.s24)[[count2]]<-paste(names(data.s24)[[i]],".july",sep='')
  res.s24[[count2]]<-data.s24[[i]]$power["2011-07-01/2011-07-31"]-arima.s24[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s24)/2 + 1
for(i in 1:length(data.s24))
{
  
  plot(data.s24[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s24", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s24[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s24[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s24[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s24", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s24[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s24[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s24<-vector("list",length(arima.s24))
count1<-1
count2<-length(arima.s24)/2 + 1
for(i in 1:length(data.s24))
{
  names(mape.s24)[[count1]]<-paste((names(data.s24)[[i]]),".april",sep='')
  div<-res.s24[[count1]]/data.s24[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s24[[count1]]<-100*sum(abs(div))/length(data.s24[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s24)[[count2]]<-paste(names(data.s24)[[i]],".july",sep='')
  div<-res.s24[[count2]]/data.s24[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s24[[count2]]<-100*sum(abs(div))/nrow(data.s24[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s24))
{
  write.zoo(arima.s24[[i]], file = paste(names(arima.s24)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s24))
{
  write.zoo(data.s24[[i]], file = paste(names(data.s24)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s24))
{
  write.zoo(res.s24[[i]], file = paste(names(res.s24)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s24))
{
  write.zoo(mape.s24[[i]], file = paste(names(mape.s24)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 24 over -----
save.image()


#---------SITE 25 begin -----
#Reading files by zone
s25<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==25)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s25[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc15<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==25)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc15[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s25) <- sprintf('s25.%d', 1:numfiles)
for(i in 1:numfiles)
{s25[[i]]$Time <- with(s25[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s25))
{names(s25[[i]])[names(s25[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s25))
{names(s25[[i]])[names(s25[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s25.ts<-vector("list", length(s25))
names(s25.ts) <- sprintf('s25.%d', 1:numfiles)

for(i in 1:length(s25))
{
  s25.ts[[i]]<-xts(s25[[i]]$speed,s25[[i]]$Time)
  colnames(s25.ts[[i]])='speed'
  s25.ts[[i]]$power<-s25[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s25))
for(i in 1:length(s25))
{
  ep<-endpoints(s25.ts[[i]], on="hours", k=1)
  a<-(period.apply(s25.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s25.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s25 <- list()
maxindex<-which.max(CF1)
data.s25[[oldcount]]<-s25.ts[[which.max(CF1)]]
names(data.s25)[[oldcount]] <- names(s25.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc15 <- lapply( paste('sloc15.', 1:numfiles, sep=''), get)
names(sloc15) <- sprintf('sloc15.%d', 1:numfiles)

num<-seq(1,length(s25))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s25))
long1<-vector(length=length(s25))
for(i in 1:length(s25))
{
  lat1[i]<-sloc15[[i]]$V2[3]
  long1[i]<-sloc15[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s25))
{
  ep<-endpoints(data.s25[[i]], on="hours", k=1)
  data.s25[[i]]<-(period.apply(data.s25[[i]],ep,mean))
}

#sd(data.s25[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s25[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s25[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s25<-vector("list",(length(data.s25)*2))
count<-1
for(j in 1:length(data.s25))
{
  names(arima.s25)[[count]]<-paste((names(data.s25)[[j]]),".april",sep='')
  arima.s25[[count]]<-data.s25[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s25[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s25[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s25[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s25)[[count]]<-paste(names(data.s25)[[1]],".july",sep='')
arima.s25[[count]]<-data.s25[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s25[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s25[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s25[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s25
#Making a power curve

speed=as.double(s25[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s25[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s25[[1]]$power<-lookup(round_any(as.numeric(arima.s25[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s25[[2]]$power<-lookup(round_any(as.numeric(arima.s25[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)


for(i in 1:nrow(arima.s25[[1]]))
{
  
  
  if(arima.s25[[1]]$speed[i]>=13.0)
  {
    arima.s25[[1]]$power[i]=16
  }
  
  if(arima.s25[[1]]$speed[i]<0)
  {
    arima.s25[[1]]$speed[i]=0
  }
  
  if(arima.s25[[1]]$speed[i]==0)
  {
    arima.s25[[1]]$power[i]=0
  }
  
}

for(i in 1:nrow(arima.s25[[2]]))
{
  if(arima.s25[[2]]$speed[i]>=12.35)
  {
    arima.s25[[2]]$power[i]=2
  }
  
  if(arima.s25[[2]]$speed[i]<0)
  {
    arima.s25[[2]]$speed[i]=0
  }
  
  if(arima.s25[[2]]$speed[i]==0)
  {
    arima.s25[[2]]$power[i]=0
  }
  
}

#Residuals
res.s25<-vector("list",length(arima.s25))
count1<-1
count2<-length(arima.s25)/2 + 1
for(i in 1:length(data.s25))
{
  names(res.s25)[[count1]]<-paste((names(data.s25)[[i]]),".april",sep='')
  res.s25[[count1]]<-data.s25[[i]]$power["2011-04-01/2011-04-30"]-arima.s25[[count1]]$power
  count1<-count1+1
  
  names(res.s25)[[count2]]<-paste(names(data.s25)[[i]],".july",sep='')
  res.s25[[count2]]<-data.s25[[i]]$power["2011-07-01/2011-07-31"]-arima.s25[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s25)/2 + 1
for(i in 1:length(data.s25))
{
  
  plot(data.s25[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s25", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s25[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s25[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s25[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s25", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s25[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s25[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s25<-vector("list",length(arima.s25))
count1<-1
count2<-length(arima.s25)/2 + 1
for(i in 1:length(data.s25))
{
  names(mape.s25)[[count1]]<-paste((names(data.s25)[[i]]),".april",sep='')
  div<-res.s25[[count1]]/data.s25[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s25[[count1]]<-100*sum(abs(div))/length(data.s25[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s25)[[count2]]<-paste(names(data.s25)[[i]],".july",sep='')
  div<-res.s25[[count2]]/data.s25[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s25[[count2]]<-100*sum(abs(div))/nrow(data.s25[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s25))
{
  write.zoo(arima.s25[[i]], file = paste(names(arima.s25)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s25))
{
  write.zoo(data.s25[[i]], file = paste(names(data.s25)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s25))
{
  write.zoo(res.s25[[i]], file = paste(names(res.s25)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s25))
{
  write.zoo(mape.s25[[i]], file = paste(names(mape.s25)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 25 over -----
save.image()


#---------SITE 26 begin -----
#Reading files by zone
s26<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==26)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s26[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc16<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==26)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc16[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s26) <- sprintf('s26.%d', 1:numfiles)
for(i in 1:numfiles)
{s26[[i]]$Time <- with(s26[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s26))
{names(s26[[i]])[names(s26[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s26))
{names(s26[[i]])[names(s26[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s26.ts<-vector("list", length(s26))
names(s26.ts) <- sprintf('s26.%d', 1:numfiles)

for(i in 1:length(s26))
{
  s26.ts[[i]]<-xts(s26[[i]]$speed,s26[[i]]$Time)
  colnames(s26.ts[[i]])='speed'
  s26.ts[[i]]$power<-s26[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s26))
for(i in 1:length(s26))
{
  ep<-endpoints(s26.ts[[i]], on="hours", k=1)
  a<-(period.apply(s26.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s26.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s26 <- list()
maxindex<-which.max(CF1)
data.s26[[oldcount]]<-s26.ts[[which.max(CF1)]]
names(data.s26)[[oldcount]] <- names(s26.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc16 <- lapply( paste('sloc16.', 1:numfiles, sep=''), get)
names(sloc16) <- sprintf('sloc16.%d', 1:numfiles)

num<-seq(1,length(s26))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s26))
long1<-vector(length=length(s26))
for(i in 1:length(s26))
{
  lat1[i]<-sloc16[[i]]$V2[3]
  long1[i]<-sloc16[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s26))
{
  ep<-endpoints(data.s26[[i]], on="hours", k=1)
  data.s26[[i]]<-(period.apply(data.s26[[i]],ep,mean))
}

#sd(data.s26[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s26[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s26[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s26<-vector("list",(length(data.s26)*2))
count<-1
for(j in 1:length(data.s26))
{
  names(arima.s26)[[count]]<-paste((names(data.s26)[[j]]),".april",sep='')
  arima.s26[[count]]<-data.s26[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s26[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s26[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s26[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s26)[[count]]<-paste(names(data.s26)[[1]],".july",sep='')
arima.s26[[count]]<-data.s26[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s26[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s26[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s26[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s26
#Making a power curve

speed=as.double(s26[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s26[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s26[[1]]$power<-lookup(round_any(as.numeric(arima.s26[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s26[[2]]$power<-lookup(round_any(as.numeric(arima.s26[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s26<-vector("list",length(arima.s26))
count1<-1
count2<-length(arima.s26)/2 + 1
for(i in 1:length(data.s26))
{
  names(res.s26)[[count1]]<-paste((names(data.s26)[[i]]),".april",sep='')
  res.s26[[count1]]<-data.s26[[i]]$power["2011-04-01/2011-04-30"]-arima.s26[[count1]]$power
  count1<-count1+1
  
  names(res.s26)[[count2]]<-paste(names(data.s26)[[i]],".july",sep='')
  res.s26[[count2]]<-data.s26[[i]]$power["2011-07-01/2011-07-31"]-arima.s26[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s26)/2 + 1
for(i in 1:length(data.s26))
{
  
  plot(data.s26[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s26", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s26[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s26[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s26[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s26", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s26[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s26[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s26<-vector("list",length(arima.s26))
count1<-1
count2<-length(arima.s26)/2 + 1
for(i in 1:length(data.s26))
{
  names(mape.s26)[[count1]]<-paste((names(data.s26)[[i]]),".april",sep='')
  div<-res.s26[[count1]]/data.s26[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s26[[count1]]<-100*sum(abs(div))/length(data.s26[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s26)[[count2]]<-paste(names(data.s26)[[i]],".july",sep='')
  div<-res.s26[[count2]]/data.s26[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s26[[count2]]<-100*sum(abs(div))/nrow(data.s26[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s26))
{
  write.zoo(arima.s26[[i]], file = paste(names(arima.s26)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s26))
{
  write.zoo(data.s26[[i]], file = paste(names(data.s26)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s26))
{
  write.zoo(res.s26[[i]], file = paste(names(res.s26)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s26))
{
  write.zoo(mape.s26[[i]], file = paste(names(mape.s26)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 26 over -----
save.image()


#---------SITE 27 begin -----
#Reading files by zone
s27<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==27)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s27[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc17<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==27)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc17[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s27) <- sprintf('s27.%d', 1:numfiles)
for(i in 1:numfiles)
{s27[[i]]$Time <- with(s27[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s27))
{names(s27[[i]])[names(s27[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s27))
{names(s27[[i]])[names(s27[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s27.ts<-vector("list", length(s27))
names(s27.ts) <- sprintf('s27.%d', 1:numfiles)

for(i in 1:length(s27))
{
  s27.ts[[i]]<-xts(s27[[i]]$speed,s27[[i]]$Time)
  colnames(s27.ts[[i]])='speed'
  s27.ts[[i]]$power<-s27[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s27))
for(i in 1:length(s27))
{
  ep<-endpoints(s27.ts[[i]], on="hours", k=1)
  a<-(period.apply(s27.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s27.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s27 <- list()
maxindex<-which.max(CF1)
data.s27[[oldcount]]<-s27.ts[[which.max(CF1)]]
names(data.s27)[[oldcount]] <- names(s27.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc17 <- lapply( paste('sloc17.', 1:numfiles, sep=''), get)
names(sloc17) <- sprintf('sloc17.%d', 1:numfiles)

num<-seq(1,length(s27))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s27))
long1<-vector(length=length(s27))
for(i in 1:length(s27))
{
  lat1[i]<-sloc17[[i]]$V2[3]
  long1[i]<-sloc17[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s27))
{
  ep<-endpoints(data.s27[[i]], on="hours", k=1)
  data.s27[[i]]<-(period.apply(data.s27[[i]],ep,mean))
}

#sd(data.s27[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s27[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s27[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s27<-vector("list",(length(data.s27)*2))
count<-1
for(j in 1:length(data.s27))
{
  names(arima.s27)[[count]]<-paste((names(data.s27)[[j]]),".april",sep='')
  arima.s27[[count]]<-data.s27[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s27[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s27[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s27[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s27)[[count]]<-paste(names(data.s27)[[1]],".july",sep='')
arima.s27[[count]]<-data.s27[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s27[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s27[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s27[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s27
#Making a power curve

speed=as.double(s27[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s27[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s27[[1]]$power<-lookup(round_any(as.numeric(arima.s27[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s27[[2]]$power<-lookup(round_any(as.numeric(arima.s27[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

for(i in 1:nrow(arima.s27[[1]]))
{
  
  
  if(arima.s27[[1]]$speed[i]>=13.0)
  {
    arima.s27[[1]]$power[i]=16
  }
  
  if(arima.s27[[1]]$speed[i]<0)
  {
    arima.s27[[1]]$speed[i]=0
  }
  
  if(arima.s27[[1]]$speed[i]==0)
  {
    arima.s27[[1]]$power[i]=0
  }
  
}

for(i in 1:nrow(arima.s27[[2]]))
{
  if(arima.s27[[2]]$speed[i]>=12.35)
  {
    arima.s27[[2]]$power[i]=2
  }
  
  if(arima.s27[[2]]$speed[i]<0)
  {
    arima.s27[[2]]$speed[i]=0
  }
  
  if(arima.s27[[2]]$speed[i]==0)
  {
    arima.s27[[2]]$power[i]=0
  }
  
}


#Residuals
res.s27<-vector("list",length(arima.s27))
count1<-1
count2<-length(arima.s27)/2 + 1
for(i in 1:length(data.s27))
{
  names(res.s27)[[count1]]<-paste((names(data.s27)[[i]]),".april",sep='')
  res.s27[[count1]]<-data.s27[[i]]$power["2011-04-01/2011-04-30"]-arima.s27[[count1]]$power
  count1<-count1+1
  
  names(res.s27)[[count2]]<-paste(names(data.s27)[[i]],".july",sep='')
  res.s27[[count2]]<-data.s27[[i]]$power["2011-07-01/2011-07-31"]-arima.s27[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s27)/2 + 1
for(i in 1:length(data.s27))
{
  
  plot(data.s27[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s27", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s27[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s27[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s27[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s27", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s27[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s27[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s27<-vector("list",length(arima.s27))
count1<-1
count2<-length(arima.s27)/2 + 1
for(i in 1:length(data.s27))
{
  names(mape.s27)[[count1]]<-paste((names(data.s27)[[i]]),".april",sep='')
  div<-res.s27[[count1]]/data.s27[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s27[[count1]]<-100*sum(abs(div))/length(data.s27[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s27)[[count2]]<-paste(names(data.s27)[[i]],".july",sep='')
  div<-res.s27[[count2]]/data.s27[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s27[[count2]]<-100*sum(abs(div))/nrow(data.s27[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s27))
{
  write.zoo(arima.s27[[i]], file = paste(names(arima.s27)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s27))
{
  write.zoo(data.s27[[i]], file = paste(names(data.s27)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s27))
{
  write.zoo(res.s27[[i]], file = paste(names(res.s27)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s27))
{
  write.zoo(mape.s27[[i]], file = paste(names(mape.s27)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 27 over -----
save.image()


#---------SITE 28 begin -----
#Reading files by zone
s28<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==28)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s28[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc18<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==28)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc18[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s28) <- sprintf('s28.%d', 1:numfiles)
for(i in 1:numfiles)
{s28[[i]]$Time <- with(s28[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s28))
{names(s28[[i]])[names(s28[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s28))
{names(s28[[i]])[names(s28[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s28.ts<-vector("list", length(s28))
names(s28.ts) <- sprintf('s28.%d', 1:numfiles)

for(i in 1:length(s28))
{
  s28.ts[[i]]<-xts(s28[[i]]$speed,s28[[i]]$Time)
  colnames(s28.ts[[i]])='speed'
  s28.ts[[i]]$power<-s28[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s28))
for(i in 1:length(s28))
{
  ep<-endpoints(s28.ts[[i]], on="hours", k=1)
  a<-(period.apply(s28.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s28.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s28 <- list()
maxindex<-which.max(CF1)
data.s28[[oldcount]]<-s28.ts[[which.max(CF1)]]
names(data.s28)[[oldcount]] <- names(s28.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc18 <- lapply( paste('sloc18.', 1:numfiles, sep=''), get)
names(sloc18) <- sprintf('sloc18.%d', 1:numfiles)

num<-seq(1,length(s28))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s28))
long1<-vector(length=length(s28))
for(i in 1:length(s28))
{
  lat1[i]<-sloc18[[i]]$V2[3]
  long1[i]<-sloc18[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s28))
{
  ep<-endpoints(data.s28[[i]], on="hours", k=1)
  data.s28[[i]]<-(period.apply(data.s28[[i]],ep,mean))
}

#sd(data.s28[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s28[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s28[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s28<-vector("list",(length(data.s28)*2))
count<-1
for(j in 1:length(data.s28))
{
  names(arima.s28)[[count]]<-paste((names(data.s28)[[j]]),".april",sep='')
  arima.s28[[count]]<-data.s28[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s28[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s28[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s28[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s28)[[count]]<-paste(names(data.s28)[[1]],".july",sep='')
arima.s28[[count]]<-data.s28[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s28[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s28[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s28[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s28
#Making a power curve

speed=as.double(s28[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s28[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s28[[1]]$power<-lookup(round_any(as.numeric(arima.s28[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s28[[2]]$power<-lookup(round_any(as.numeric(arima.s28[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s28<-vector("list",length(arima.s28))
count1<-1
count2<-length(arima.s28)/2 + 1
for(i in 1:length(data.s28))
{
  names(res.s28)[[count1]]<-paste((names(data.s28)[[i]]),".april",sep='')
  res.s28[[count1]]<-data.s28[[i]]$power["2011-04-01/2011-04-30"]-arima.s28[[count1]]$power
  count1<-count1+1
  
  names(res.s28)[[count2]]<-paste(names(data.s28)[[i]],".july",sep='')
  res.s28[[count2]]<-data.s28[[i]]$power["2011-07-01/2011-07-31"]-arima.s28[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s28)/2 + 1
for(i in 1:length(data.s28))
{
  
  plot(data.s28[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s28", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s28[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s28[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s28[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s28", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s28[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s28[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s28<-vector("list",length(arima.s28))
count1<-1
count2<-length(arima.s28)/2 + 1
for(i in 1:length(data.s28))
{
  names(mape.s28)[[count1]]<-paste((names(data.s28)[[i]]),".april",sep='')
  div<-res.s28[[count1]]/data.s28[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s28[[count1]]<-100*sum(abs(div))/length(data.s28[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s28)[[count2]]<-paste(names(data.s28)[[i]],".july",sep='')
  div<-res.s28[[count2]]/data.s28[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s28[[count2]]<-100*sum(abs(div))/nrow(data.s28[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s28))
{
  write.zoo(arima.s28[[i]], file = paste(names(arima.s28)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s28))
{
  write.zoo(data.s28[[i]], file = paste(names(data.s28)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s28))
{
  write.zoo(res.s28[[i]], file = paste(names(res.s28)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s28))
{
  write.zoo(mape.s28[[i]], file = paste(names(mape.s28)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 28 over -----
save.image()


#---------SITE 29 begin -----
#Reading files by zone
s29<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==29)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s29[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc19<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==29)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc19[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s29) <- sprintf('s29.%d', 1:numfiles)
for(i in 1:numfiles)
{s29[[i]]$Time <- with(s29[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s29))
{names(s29[[i]])[names(s29[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s29))
{names(s29[[i]])[names(s29[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s29.ts<-vector("list", length(s29))
names(s29.ts) <- sprintf('s29.%d', 1:numfiles)

for(i in 1:length(s29))
{
  s29.ts[[i]]<-xts(s29[[i]]$speed,s29[[i]]$Time)
  colnames(s29.ts[[i]])='speed'
  s29.ts[[i]]$power<-s29[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s29))
for(i in 1:length(s29))
{
  ep<-endpoints(s29.ts[[i]], on="hours", k=1)
  a<-(period.apply(s29.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s29.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s29 <- list()
maxindex<-which.max(CF1)
data.s29[[oldcount]]<-s29.ts[[which.max(CF1)]]
names(data.s29)[[oldcount]] <- names(s29.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc19 <- lapply( paste('sloc19.', 1:numfiles, sep=''), get)
names(sloc19) <- sprintf('sloc19.%d', 1:numfiles)

num<-seq(1,length(s29))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s29))
long1<-vector(length=length(s29))
for(i in 1:length(s29))
{
  lat1[i]<-sloc19[[i]]$V2[3]
  long1[i]<-sloc19[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s29))
{
  ep<-endpoints(data.s29[[i]], on="hours", k=1)
  data.s29[[i]]<-(period.apply(data.s29[[i]],ep,mean))
}

#sd(data.s29[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s29[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s29[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s29<-vector("list",(length(data.s29)*2))
count<-1
for(j in 1:length(data.s29))
{
  names(arima.s29)[[count]]<-paste((names(data.s29)[[j]]),".april",sep='')
  arima.s29[[count]]<-data.s29[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s29[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s29[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s29[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s29)[[count]]<-paste(names(data.s29)[[1]],".july",sep='')
arima.s29[[count]]<-data.s29[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s29[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s29[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s29[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s29
#Making a power curve

speed=as.double(s29[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s29[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s29[[1]]$power<-lookup(round_any(as.numeric(arima.s29[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s29[[2]]$power<-lookup(round_any(as.numeric(arima.s29[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s29<-vector("list",length(arima.s29))
count1<-1
count2<-length(arima.s29)/2 + 1
for(i in 1:length(data.s29))
{
  names(res.s29)[[count1]]<-paste((names(data.s29)[[i]]),".april",sep='')
  res.s29[[count1]]<-data.s29[[i]]$power["2011-04-01/2011-04-30"]-arima.s29[[count1]]$power
  count1<-count1+1
  
  names(res.s29)[[count2]]<-paste(names(data.s29)[[i]],".july",sep='')
  res.s29[[count2]]<-data.s29[[i]]$power["2011-07-01/2011-07-31"]-arima.s29[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s29)/2 + 1
for(i in 1:length(data.s29))
{
  
  plot(data.s29[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s29", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s29[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s29[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s29[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s29", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s29[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s29[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s29<-vector("list",length(arima.s29))
count1<-1
count2<-length(arima.s29)/2 + 1
for(i in 1:length(data.s29))
{
  names(mape.s29)[[count1]]<-paste((names(data.s29)[[i]]),".april",sep='')
  div<-res.s29[[count1]]/data.s29[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s29[[count1]]<-100*sum(abs(div))/length(data.s29[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s29)[[count2]]<-paste(names(data.s29)[[i]],".july",sep='')
  div<-res.s29[[count2]]/data.s29[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s29[[count2]]<-100*sum(abs(div))/nrow(data.s29[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s29))
{
  write.zoo(arima.s29[[i]], file = paste(names(arima.s29)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s29))
{
  write.zoo(data.s29[[i]], file = paste(names(data.s29)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s29))
{
  write.zoo(res.s29[[i]], file = paste(names(res.s29)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s29))
{
  write.zoo(mape.s29[[i]], file = paste(names(mape.s29)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 29 over -----
save.image()


#---------SITE 30 begin -----
#Reading files by zone
s30<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==30)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s30[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc30<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==30)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc30[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s30) <- sprintf('s30.%d', 1:numfiles)
for(i in 1:numfiles)
{s30[[i]]$Time <- with(s30[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s30))
{names(s30[[i]])[names(s30[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s30))
{names(s30[[i]])[names(s30[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s30.ts<-vector("list", length(s30))
names(s30.ts) <- sprintf('s30.%d', 1:numfiles)

for(i in 1:length(s30))
{
  s30.ts[[i]]<-xts(s30[[i]]$speed,s30[[i]]$Time)
  colnames(s30.ts[[i]])='speed'
  s30.ts[[i]]$power<-s30[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s30))
for(i in 1:length(s30))
{
  ep<-endpoints(s30.ts[[i]], on="hours", k=1)
  a<-(period.apply(s30.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s30.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s30 <- list()
maxindex<-which.max(CF1)
data.s30[[oldcount]]<-s30.ts[[which.max(CF1)]]
names(data.s30)[[oldcount]] <- names(s30.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc30 <- lapply( paste('sloc30.', 1:numfiles, sep=''), get)
names(sloc30) <- sprintf('sloc30.%d', 1:numfiles)

num<-seq(1,length(s30))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s30))
long1<-vector(length=length(s30))
for(i in 1:length(s30))
{
  lat1[i]<-sloc30[[i]]$V2[3]
  long1[i]<-sloc30[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s30))
{
  ep<-endpoints(data.s30[[i]], on="hours", k=1)
  data.s30[[i]]<-(period.apply(data.s30[[i]],ep,mean))
}

#sd(data.s30[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s30[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s30[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s30<-vector("list",(length(data.s30)*2))
count<-1
for(j in 1:length(data.s30))
{
  names(arima.s30)[[count]]<-paste((names(data.s30)[[j]]),".april",sep='')
  arima.s30[[count]]<-data.s30[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s30[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s30[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s30[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s30)[[count]]<-paste(names(data.s30)[[1]],".july",sep='')
arima.s30[[count]]<-data.s30[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s30[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s30[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s30[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s30
#Making a power curve

speed=as.double(s30[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s30[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s30[[1]]$power<-lookup(round_any(as.numeric(arima.s30[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s30[[2]]$power<-lookup(round_any(as.numeric(arima.s30[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)


for(i in 1:nrow(arima.s30[[1]]))
{
  
  
  if(arima.s30[[1]]$speed[i]>=13.0)
  {
    arima.s30[[1]]$power[i]=16
  }
  
  if(arima.s30[[1]]$speed[i]<0)
  {
    arima.s30[[1]]$speed[i]=0
  }
  
  if(arima.s30[[1]]$speed[i]==0)
  {
    arima.s30[[1]]$power[i]=0
  }
  
}

for(i in 1:nrow(arima.s30[[2]]))
{
  if(arima.s30[[2]]$speed[i]>=12.35)
  {
    arima.s30[[2]]$power[i]=2
  }
  
  if(arima.s30[[2]]$speed[i]<0)
  {
    arima.s30[[2]]$speed[i]=0
  }
  
  if(arima.s30[[2]]$speed[i]==0)
  {
    arima.s30[[2]]$power[i]=0
  }
  
}


#Residuals
res.s30<-vector("list",length(arima.s30))
count1<-1
count2<-length(arima.s30)/2 + 1
for(i in 1:length(data.s30))
{
  names(res.s30)[[count1]]<-paste((names(data.s30)[[i]]),".april",sep='')
  res.s30[[count1]]<-data.s30[[i]]$power["2011-04-01/2011-04-30"]-arima.s30[[count1]]$power
  count1<-count1+1
  
  names(res.s30)[[count2]]<-paste(names(data.s30)[[i]],".july",sep='')
  res.s30[[count2]]<-data.s30[[i]]$power["2011-07-01/2011-07-31"]-arima.s30[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s30)/2 + 1
for(i in 1:length(data.s30))
{
  
  plot(data.s30[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s30", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s30[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s30[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s30[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s30", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s30[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s30[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s30<-vector("list",length(arima.s30))
count1<-1
count2<-length(arima.s30)/2 + 1
for(i in 1:length(data.s30))
{
  names(mape.s30)[[count1]]<-paste((names(data.s30)[[i]]),".april",sep='')
  div<-res.s30[[count1]]/data.s30[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s30[[count1]]<-100*sum(abs(div))/length(data.s30[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s30)[[count2]]<-paste(names(data.s30)[[i]],".july",sep='')
  div<-res.s30[[count2]]/data.s30[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s30[[count2]]<-100*sum(abs(div))/nrow(data.s30[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s30))
{
  write.zoo(arima.s30[[i]], file = paste(names(arima.s30)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s30))
{
  write.zoo(data.s30[[i]], file = paste(names(data.s30)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s30))
{
  write.zoo(res.s30[[i]], file = paste(names(res.s30)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s30))
{
  write.zoo(mape.s30[[i]], file = paste(names(mape.s30)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 30 over -----
save.image()

for (i in 1:length(data.s30))
{
  write.zoo(data.s30[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s30)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s30[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s30)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s29))
{
  write.zoo(data.s29[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s29)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s29[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s29)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s28))
{
  write.zoo(data.s28[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s28)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s28[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s28)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s27))
{
  write.zoo(data.s27[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s27)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s27[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s27)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s26))
{
  write.zoo(data.s26[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s26)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s26[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s26)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s25))
{
  write.zoo(data.s25[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s25)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s25[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s25)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s24))
{
  write.zoo(data.s24[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s24)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s24[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s24)[[i]],'-july power data.csv',sep=''),sep=",")
  
}


for (i in 1:length(data.s23))
{
  write.zoo(data.s23[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s23)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s23[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s23)[[i]],'-july power data.csv',sep=''),sep=",")
  
}


for (i in 1:length(data.s22))
{
  write.zoo(data.s22[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s22)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s22[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s22)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s1))
{
  write.zoo(data.s21[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s21)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s21[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s21)[[i]],'-july power data.csv',sep=''),sep=",")
  
}