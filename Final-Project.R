
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

#path="D:/Documents/Project/Data/"
path="C:/Users/Akshay/Google Drive/Spring 2016/Energy Infrastructure/Project/Data/"
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

#------ Site 1 begin-----

#Reading files by zone
s1<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==1)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s1[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc1<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==1)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc1[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s1) <- sprintf('s1.%d', 1:numfiles)
for(i in 1:numfiles)
{s1[[i]]$Time <- with(s1[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s1))
{names(s1[[i]])[names(s1[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s1))
{names(s1[[i]])[names(s1[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s1.ts<-vector("list", length(s1))
names(s1.ts) <- sprintf('s1.%d', 1:numfiles)

for(i in 1:length(s1))
{
  s1.ts[[i]]<-xts(s1[[i]]$speed,s1[[i]]$Time)
  colnames(s1.ts[[i]])='speed'
  s1.ts[[i]]$power<-s1[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s1))
for(i in 1:length(s1))
{
  ep<-endpoints(s1.ts[[i]], on="hours", k=1)
  a<-(period.apply(s1.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s1.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s1 <- list()
maxindex<-which.max(CF1)
data.s1[[oldcount]]<-s1.ts[[which.max(CF1)]]
names(data.s1)[[oldcount]] <- names(s1.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc1 <- lapply( paste('sloc1.', 1:numfiles, sep=''), get)
names(sloc1) <- sprintf('sloc1.%d', 1:numfiles)

num<-seq(1,length(s1))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s1))
long1<-vector(length=length(s1))
for(i in 1:length(s1))
{
  lat1[i]<-sloc1[[i]]$V2[3]
  long1[i]<-sloc1[[i]]$V2[2]
}

#------Data collection for Site 1 over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s1))
{
  ep<-endpoints(data.s1[[i]], on="hours", k=1)
  data.s1[[i]]<-(period.apply(data.s1[[i]],ep,mean))
}

#sd(data.s1[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s1[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s1[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s1<-vector("list",(length(data.s1)*2))
count<-1
for(j in 1:length(data.s1))
{
  names(arima.s1)[[count]]<-paste((names(data.s1)[[j]]),".april",sep='')
  arima.s1[[count]]<-data.s1[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s1[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s1[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s1[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}



  names(arima.s1)[[count]]<-paste(names(data.s1)[[1]],".july",sep='')
  arima.s1[[count]]<-data.s1[[1]]$speed["2011-07-01/2011-07-31"]
  
  july<-data.s1[[1]]$speed["2011-07-01/2011-07-31"]
  
  for(i in 0:(length(data.s1[[1]]$speed["2011-07-01/2011-07-31"])-73))
  {
    a=i+1
    b=i+72
    example<-july$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s1[[count]][i+73]<-as.numeric(point.forecast)
  }


#backup<-arima.s1
#Making a power curve

speed=as.double(s1[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s1[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s1[[1]]$power<-lookup(round_any(as.numeric(arima.s1[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s1[[2]]$power<-lookup(round_any(as.numeric(arima.s1[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s1<-vector("list",length(arima.s1))
count1<-1
count2<-length(arima.s1)/2 + 1
for(i in 1:length(data.s1))
{
  names(res.s1)[[count1]]<-paste((names(data.s1)[[i]]),".april",sep='')
  res.s1[[count1]]<-data.s1[[i]]$power["2011-04-01/2011-04-30"]-arima.s1[[count1]]$power
  count1<-count1+1
  
  names(res.s1)[[count2]]<-paste(names(data.s1)[[i]],".july",sep='')
  res.s1[[count2]]<-data.s1[[i]]$power["2011-07-01/2011-07-31"]-arima.s1[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s1)/2 + 1
for(i in 1:length(data.s1))
{
  
  plot(data.s1[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for S1", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s1[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s1[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s1[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s1", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s1[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s1[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s1<-vector("list",length(arima.s1))
count1<-1
count2<-length(arima.s1)/2 + 1
for(i in 1:length(data.s1))
{
  names(mape.s1)[[count1]]<-paste((names(data.s1)[[i]]),".april",sep='')
  div<-res.s1[[count1]]/data.s1[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s1[[count1]]<-100*sum(abs(div))/length(data.s1[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s1)[[count2]]<-paste(names(data.s1)[[i]],".july",sep='')
  div<-res.s1[[count2]]/data.s1[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s1[[count2]]<-100*sum(abs(div))/nrow(data.s1[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s1))
{
  write.zoo(arima.s1[[i]], file = paste(names(arima.s1)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s1))
{
  write.zoo(data.s1[[i]], file = paste(names(data.s1)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s1))
{
  write.zoo(res.s1[[i]], file = paste(names(res.s1)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s1))
{
  write.zoo(mape.s1[[i]], file = paste(names(mape.s1)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 1 over -----
save.image()

#---------SITE 2 begin -----
#Reading files by zone
s2<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==2)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s2[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc2<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==2)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc2[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s2) <- sprintf('s2.%d', 1:numfiles)
for(i in 1:numfiles)
{s2[[i]]$Time <- with(s2[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s2))
{names(s2[[i]])[names(s2[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s2))
{names(s2[[i]])[names(s2[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s2.ts<-vector("list", length(s2))
names(s2.ts) <- sprintf('s2.%d', 1:numfiles)

for(i in 1:length(s2))
{
  s2.ts[[i]]<-xts(s2[[i]]$speed,s2[[i]]$Time)
  colnames(s2.ts[[i]])='speed'
  s2.ts[[i]]$power<-s2[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s2))
for(i in 1:length(s2))
{
  ep<-endpoints(s2.ts[[i]], on="hours", k=1)
  a<-(period.apply(s2.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s2.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s2 <- list()
maxindex<-which.max(CF1)
data.s2[[oldcount]]<-s2.ts[[which.max(CF1)]]
names(data.s2)[[oldcount]] <- names(s2.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc2 <- lapply( paste('sloc2.', 1:numfiles, sep=''), get)
names(sloc2) <- sprintf('sloc2.%d', 1:numfiles)

num<-seq(1,length(s2))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s2))
long1<-vector(length=length(s2))
for(i in 1:length(s2))
{
  lat1[i]<-sloc2[[i]]$V2[3]
  long1[i]<-sloc2[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s2))
{
  ep<-endpoints(data.s2[[i]], on="hours", k=1)
  data.s2[[i]]<-(period.apply(data.s2[[i]],ep,mean))
}

#sd(data.s2[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s2[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s2[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s2<-vector("list",(length(data.s2)*2))
count<-1
for(j in 1:length(data.s2))
{
  names(arima.s2)[[count]]<-paste((names(data.s2)[[j]]),".april",sep='')
  arima.s2[[count]]<-data.s2[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s2[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s2[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s2[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s2)[[count]]<-paste(names(data.s2)[[1]],".july",sep='')
arima.s2[[count]]<-data.s2[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s2[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s2[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s2[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s2
#Making a power curve

speed=as.double(s2[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s2[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s2[[1]]$power<-lookup(round_any(as.numeric(arima.s2[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s2[[2]]$power<-lookup(round_any(as.numeric(arima.s2[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s2<-vector("list",length(arima.s2))
count1<-1
count2<-length(arima.s2)/2 + 1
for(i in 1:length(data.s2))
{
  names(res.s2)[[count1]]<-paste((names(data.s2)[[i]]),".april",sep='')
  res.s2[[count1]]<-data.s2[[i]]$power["2011-04-01/2011-04-30"]-arima.s2[[count1]]$power
  count1<-count1+1
  
  names(res.s2)[[count2]]<-paste(names(data.s2)[[i]],".july",sep='')
  res.s2[[count2]]<-data.s2[[i]]$power["2011-07-01/2011-07-31"]-arima.s2[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s2)/2 + 1
for(i in 1:length(data.s2))
{
  
  plot(data.s2[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s2", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s2[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s2[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s2[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s2", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s2[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s2[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s2<-vector("list",length(arima.s2))
count1<-1
count2<-length(arima.s2)/2 + 1
for(i in 1:length(data.s2))
{
  names(mape.s2)[[count1]]<-paste((names(data.s2)[[i]]),".april",sep='')
  div<-res.s2[[count1]]/data.s2[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s2[[count1]]<-100*sum(abs(div))/length(data.s2[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s2)[[count2]]<-paste(names(data.s2)[[i]],".july",sep='')
  div<-res.s2[[count2]]/data.s2[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s2[[count2]]<-100*sum(abs(div))/nrow(data.s2[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s2))
{
  write.zoo(arima.s2[[i]], file = paste(names(arima.s2)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s2))
{
  write.zoo(data.s2[[i]], file = paste(names(data.s2)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s2))
{
  write.zoo(res.s2[[i]], file = paste(names(res.s2)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s2))
{
  write.zoo(mape.s2[[i]], file = paste(names(mape.s2)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 2 over -----
save.image()


#---------SITE 3 begin -----
#Reading files by zone
s3<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==3)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s3[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc3<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==3)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc3[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s3) <- sprintf('s3.%d', 1:numfiles)
for(i in 1:numfiles)
{s3[[i]]$Time <- with(s3[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s3))
{names(s3[[i]])[names(s3[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s3))
{names(s3[[i]])[names(s3[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s3.ts<-vector("list", length(s3))
names(s3.ts) <- sprintf('s3.%d', 1:numfiles)

for(i in 1:length(s3))
{
  s3.ts[[i]]<-xts(s3[[i]]$speed,s3[[i]]$Time)
  colnames(s3.ts[[i]])='speed'
  s3.ts[[i]]$power<-s3[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s3))
for(i in 1:length(s3))
{
  ep<-endpoints(s3.ts[[i]], on="hours", k=1)
  a<-(period.apply(s3.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s3.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s3 <- list()
maxindex<-which.max(CF1)
data.s3[[oldcount]]<-s3.ts[[which.max(CF1)]]
names(data.s3)[[oldcount]] <- names(s3.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc3 <- lapply( paste('sloc3.', 1:numfiles, sep=''), get)
names(sloc3) <- sprintf('sloc3.%d', 1:numfiles)

num<-seq(1,length(s3))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s3))
long1<-vector(length=length(s3))
for(i in 1:length(s3))
{
  lat1[i]<-sloc3[[i]]$V2[3]
  long1[i]<-sloc3[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s3))
{
  ep<-endpoints(data.s3[[i]], on="hours", k=1)
  data.s3[[i]]<-(period.apply(data.s3[[i]],ep,mean))
}

#sd(data.s3[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s3[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s3[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s3<-vector("list",(length(data.s3)*2))
count<-1
for(j in 1:length(data.s3))
{
  names(arima.s3)[[count]]<-paste((names(data.s3)[[j]]),".april",sep='')
  arima.s3[[count]]<-data.s3[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s3[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s3[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s3[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s3)[[count]]<-paste(names(data.s3)[[1]],".july",sep='')
arima.s3[[count]]<-data.s3[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s3[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s3[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s3[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s3
#Making a power curve

speed=as.double(s3[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s3[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s3[[1]]$power<-lookup(round_any(as.numeric(arima.s3[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s3[[2]]$power<-lookup(round_any(as.numeric(arima.s3[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s3<-vector("list",length(arima.s3))
count1<-1
count2<-length(arima.s3)/2 + 1
for(i in 1:length(data.s3))
{
  names(res.s3)[[count1]]<-paste((names(data.s3)[[i]]),".april",sep='')
  res.s3[[count1]]<-data.s3[[i]]$power["2011-04-01/2011-04-30"]-arima.s3[[count1]]$power
  count1<-count1+1
  
  names(res.s3)[[count2]]<-paste(names(data.s3)[[i]],".july",sep='')
  res.s3[[count2]]<-data.s3[[i]]$power["2011-07-01/2011-07-31"]-arima.s3[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s3)/2 + 1
for(i in 1:length(data.s3))
{
  
  plot(data.s3[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s3", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s3[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s3[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s3[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s3", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s3[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s3[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s3<-vector("list",length(arima.s3))
count1<-1
count2<-length(arima.s3)/2 + 1
for(i in 1:length(data.s3))
{
  names(mape.s3)[[count1]]<-paste((names(data.s3)[[i]]),".april",sep='')
  div<-res.s3[[count1]]/data.s3[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s3[[count1]]<-100*sum(abs(div))/length(data.s3[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s3)[[count2]]<-paste(names(data.s3)[[i]],".july",sep='')
  div<-res.s3[[count2]]/data.s3[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s3[[count2]]<-100*sum(abs(div))/nrow(data.s3[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s3))
{
  write.zoo(arima.s3[[i]], file = paste(names(arima.s3)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s3))
{
  write.zoo(data.s3[[i]], file = paste(names(data.s3)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s3))
{
  write.zoo(res.s3[[i]], file = paste(names(res.s3)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s3))
{
  write.zoo(mape.s3[[i]], file = paste(names(mape.s3)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 3 over -----
save.image()


#---------SITE 4 begin -----
#Reading files by zone
s4<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==4)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s4[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc4<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==4)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc4[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s4) <- sprintf('s4.%d', 1:numfiles)
for(i in 1:numfiles)
{s4[[i]]$Time <- with(s4[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s4))
{names(s4[[i]])[names(s4[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s4))
{names(s4[[i]])[names(s4[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s4.ts<-vector("list", length(s4))
names(s4.ts) <- sprintf('s4.%d', 1:numfiles)

for(i in 1:length(s4))
{
  s4.ts[[i]]<-xts(s4[[i]]$speed,s4[[i]]$Time)
  colnames(s4.ts[[i]])='speed'
  s4.ts[[i]]$power<-s4[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s4))
for(i in 1:length(s4))
{
  ep<-endpoints(s4.ts[[i]], on="hours", k=1)
  a<-(period.apply(s4.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s4.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s4 <- list()
maxindex<-which.max(CF1)
data.s4[[oldcount]]<-s4.ts[[which.max(CF1)]]
names(data.s4)[[oldcount]] <- names(s4.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc4 <- lapply( paste('sloc4.', 1:numfiles, sep=''), get)
names(sloc4) <- sprintf('sloc4.%d', 1:numfiles)

num<-seq(1,length(s4))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s4))
long1<-vector(length=length(s4))
for(i in 1:length(s4))
{
  lat1[i]<-sloc4[[i]]$V2[3]
  long1[i]<-sloc4[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s4))
{
  ep<-endpoints(data.s4[[i]], on="hours", k=1)
  data.s4[[i]]<-(period.apply(data.s4[[i]],ep,mean))
}

#sd(data.s4[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s4[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s4[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s4<-vector("list",(length(data.s4)*2))
count<-1
for(j in 1:length(data.s4))
{
  names(arima.s4)[[count]]<-paste((names(data.s4)[[j]]),".april",sep='')
  arima.s4[[count]]<-data.s4[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s4[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s4[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s4[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s4)[[count]]<-paste(names(data.s4)[[1]],".july",sep='')
arima.s4[[count]]<-data.s4[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s4[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s4[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s4[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s4
#Making a power curve

speed=as.double(s4[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s4[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s4[[1]]$power<-lookup(round_any(as.numeric(arima.s4[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s4[[2]]$power<-lookup(round_any(as.numeric(arima.s4[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s4<-vector("list",length(arima.s4))
count1<-1
count2<-length(arima.s4)/2 + 1
for(i in 1:length(data.s4))
{
  names(res.s4)[[count1]]<-paste((names(data.s4)[[i]]),".april",sep='')
  res.s4[[count1]]<-data.s4[[i]]$power["2011-04-01/2011-04-30"]-arima.s4[[count1]]$power
  count1<-count1+1
  
  names(res.s4)[[count2]]<-paste(names(data.s4)[[i]],".july",sep='')
  res.s4[[count2]]<-data.s4[[i]]$power["2011-07-01/2011-07-31"]-arima.s4[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s4)/2 + 1
for(i in 1:length(data.s4))
{
  
  plot(data.s4[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s4", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s4[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s4[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s4[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s4", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s4[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s4[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s4<-vector("list",length(arima.s4))
count1<-1
count2<-length(arima.s4)/2 + 1
for(i in 1:length(data.s4))
{
  names(mape.s4)[[count1]]<-paste((names(data.s4)[[i]]),".april",sep='')
  div<-res.s4[[count1]]/data.s4[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s4[[count1]]<-100*sum(abs(div))/length(data.s4[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s4)[[count2]]<-paste(names(data.s4)[[i]],".july",sep='')
  div<-res.s4[[count2]]/data.s4[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s4[[count2]]<-100*sum(abs(div))/nrow(data.s4[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s4))
{
  write.zoo(arima.s4[[i]], file = paste(names(arima.s4)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s4))
{
  write.zoo(data.s4[[i]], file = paste(names(data.s4)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s4))
{
  write.zoo(res.s4[[i]], file = paste(names(res.s4)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s4))
{
  write.zoo(mape.s4[[i]], file = paste(names(mape.s4)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 4 over -----
save.image()


#---------SITE 5 begin -----
#Reading files by zone
s5<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==5)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s5[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc5<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==5)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc5[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s5) <- sprintf('s5.%d', 1:numfiles)
for(i in 1:numfiles)
{s5[[i]]$Time <- with(s5[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s5))
{names(s5[[i]])[names(s5[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s5))
{names(s5[[i]])[names(s5[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s5.ts<-vector("list", length(s5))
names(s5.ts) <- sprintf('s5.%d', 1:numfiles)

for(i in 1:length(s5))
{
  s5.ts[[i]]<-xts(s5[[i]]$speed,s5[[i]]$Time)
  colnames(s5.ts[[i]])='speed'
  s5.ts[[i]]$power<-s5[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s5))
for(i in 1:length(s5))
{
  ep<-endpoints(s5.ts[[i]], on="hours", k=1)
  a<-(period.apply(s5.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s5.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s5 <- list()
maxindex<-which.max(CF1)
data.s5[[oldcount]]<-s5.ts[[which.max(CF1)]]
names(data.s5)[[oldcount]] <- names(s5.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc5 <- lapply( paste('sloc5.', 1:numfiles, sep=''), get)
names(sloc5) <- sprintf('sloc5.%d', 1:numfiles)

num<-seq(1,length(s5))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s5))
long1<-vector(length=length(s5))
for(i in 1:length(s5))
{
  lat1[i]<-sloc5[[i]]$V2[3]
  long1[i]<-sloc5[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s5))
{
  ep<-endpoints(data.s5[[i]], on="hours", k=1)
  data.s5[[i]]<-(period.apply(data.s5[[i]],ep,mean))
}

#sd(data.s5[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s5[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s5[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s5<-vector("list",(length(data.s5)*2))
count<-1
for(j in 1:length(data.s5))
{
  names(arima.s5)[[count]]<-paste((names(data.s5)[[j]]),".april",sep='')
  arima.s5[[count]]<-data.s5[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s5[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s5[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s5[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s5)[[count]]<-paste(names(data.s5)[[1]],".july",sep='')
arima.s5[[count]]<-data.s5[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s5[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s5[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s5[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s5
#Making a power curve

speed=as.double(s5[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s5[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s5[[1]]$power<-lookup(round_any(as.numeric(arima.s5[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s5[[2]]$power<-lookup(round_any(as.numeric(arima.s5[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s5<-vector("list",length(arima.s5))
count1<-1
count2<-length(arima.s5)/2 + 1
for(i in 1:length(data.s5))
{
  names(res.s5)[[count1]]<-paste((names(data.s5)[[i]]),".april",sep='')
  res.s5[[count1]]<-data.s5[[i]]$power["2011-04-01/2011-04-30"]-arima.s5[[count1]]$power
  count1<-count1+1
  
  names(res.s5)[[count2]]<-paste(names(data.s5)[[i]],".july",sep='')
  res.s5[[count2]]<-data.s5[[i]]$power["2011-07-01/2011-07-31"]-arima.s5[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s5)/2 + 1
for(i in 1:length(data.s5))
{
  
  plot(data.s5[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s5", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s5[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s5[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s5[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s5", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s5[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s5[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s5<-vector("list",length(arima.s5))
count1<-1
count2<-length(arima.s5)/2 + 1
for(i in 1:length(data.s5))
{
  names(mape.s5)[[count1]]<-paste((names(data.s5)[[i]]),".april",sep='')
  div<-res.s5[[count1]]/data.s5[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s5[[count1]]<-100*sum(abs(div))/length(data.s5[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s5)[[count2]]<-paste(names(data.s5)[[i]],".july",sep='')
  div<-res.s5[[count2]]/data.s5[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s5[[count2]]<-100*sum(abs(div))/nrow(data.s5[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s5))
{
  write.zoo(arima.s5[[i]], file = paste(names(arima.s5)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s5))
{
  write.zoo(data.s5[[i]], file = paste(names(data.s5)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s5))
{
  write.zoo(res.s5[[i]], file = paste(names(res.s5)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s5))
{
  write.zoo(mape.s5[[i]], file = paste(names(mape.s5)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 5 over -----
save.image()


#---------SITE 6 begin -----
#Reading files by zone
s6<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==6)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s6[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc6<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==6)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc6[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s6) <- sprintf('s6.%d', 1:numfiles)
for(i in 1:numfiles)
{s6[[i]]$Time <- with(s6[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s6))
{names(s6[[i]])[names(s6[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s6))
{names(s6[[i]])[names(s6[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s6.ts<-vector("list", length(s6))
names(s6.ts) <- sprintf('s6.%d', 1:numfiles)

for(i in 1:length(s6))
{
  s6.ts[[i]]<-xts(s6[[i]]$speed,s6[[i]]$Time)
  colnames(s6.ts[[i]])='speed'
  s6.ts[[i]]$power<-s6[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s6))
for(i in 1:length(s6))
{
  ep<-endpoints(s6.ts[[i]], on="hours", k=1)
  a<-(period.apply(s6.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s6.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s6 <- list()
maxindex<-which.max(CF1)
data.s6[[oldcount]]<-s6.ts[[which.max(CF1)]]
names(data.s6)[[oldcount]] <- names(s6.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc6 <- lapply( paste('sloc6.', 1:numfiles, sep=''), get)
names(sloc6) <- sprintf('sloc6.%d', 1:numfiles)

num<-seq(1,length(s6))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s6))
long1<-vector(length=length(s6))
for(i in 1:length(s6))
{
  lat1[i]<-sloc6[[i]]$V2[3]
  long1[i]<-sloc6[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s6))
{
  ep<-endpoints(data.s6[[i]], on="hours", k=1)
  data.s6[[i]]<-(period.apply(data.s6[[i]],ep,mean))
}

#sd(data.s6[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s6[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s6[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s6<-vector("list",(length(data.s6)*2))
count<-1
for(j in 1:length(data.s6))
{
  names(arima.s6)[[count]]<-paste((names(data.s6)[[j]]),".april",sep='')
  arima.s6[[count]]<-data.s6[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s6[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s6[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s6[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s6)[[count]]<-paste(names(data.s6)[[1]],".july",sep='')
arima.s6[[count]]<-data.s6[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s6[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s6[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s6[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s6
#Making a power curve

speed=as.double(s6[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s6[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s6[[1]]$power<-lookup(round_any(as.numeric(arima.s6[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s6[[2]]$power<-lookup(round_any(as.numeric(arima.s6[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s6<-vector("list",length(arima.s6))
count1<-1
count2<-length(arima.s6)/2 + 1
for(i in 1:length(data.s6))
{
  names(res.s6)[[count1]]<-paste((names(data.s6)[[i]]),".april",sep='')
  res.s6[[count1]]<-data.s6[[i]]$power["2011-04-01/2011-04-30"]-arima.s6[[count1]]$power
  count1<-count1+1
  
  names(res.s6)[[count2]]<-paste(names(data.s6)[[i]],".july",sep='')
  res.s6[[count2]]<-data.s6[[i]]$power["2011-07-01/2011-07-31"]-arima.s6[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s6)/2 + 1
for(i in 1:length(data.s6))
{
  
  plot(data.s6[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s6", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s6[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s6[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s6[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s6", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s6[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s6[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s6<-vector("list",length(arima.s6))
count1<-1
count2<-length(arima.s6)/2 + 1
for(i in 1:length(data.s6))
{
  names(mape.s6)[[count1]]<-paste((names(data.s6)[[i]]),".april",sep='')
  div<-res.s6[[count1]]/data.s6[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s6[[count1]]<-100*sum(abs(div))/length(data.s6[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s6)[[count2]]<-paste(names(data.s6)[[i]],".july",sep='')
  div<-res.s6[[count2]]/data.s6[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s6[[count2]]<-100*sum(abs(div))/nrow(data.s6[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s6))
{
  write.zoo(arima.s6[[i]], file = paste(names(arima.s6)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s6))
{
  write.zoo(data.s6[[i]], file = paste(names(data.s6)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s6))
{
  write.zoo(res.s6[[i]], file = paste(names(res.s6)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s6))
{
  write.zoo(mape.s6[[i]], file = paste(names(mape.s6)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 6 over -----
save.image()


#---------SITE 7 begin -----
#Reading files by zone
s7<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==7)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s7[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc7<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==7)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc7[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s7) <- sprintf('s7.%d', 1:numfiles)
for(i in 1:numfiles)
{s7[[i]]$Time <- with(s7[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s7))
{names(s7[[i]])[names(s7[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s7))
{names(s7[[i]])[names(s7[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s7.ts<-vector("list", length(s7))
names(s7.ts) <- sprintf('s7.%d', 1:numfiles)

for(i in 1:length(s7))
{
  s7.ts[[i]]<-xts(s7[[i]]$speed,s7[[i]]$Time)
  colnames(s7.ts[[i]])='speed'
  s7.ts[[i]]$power<-s7[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s7))
for(i in 1:length(s7))
{
  ep<-endpoints(s7.ts[[i]], on="hours", k=1)
  a<-(period.apply(s7.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s7.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s7 <- list()
maxindex<-which.max(CF1)
data.s7[[oldcount]]<-s7.ts[[which.max(CF1)]]
names(data.s7)[[oldcount]] <- names(s7.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc7 <- lapply( paste('sloc7.', 1:numfiles, sep=''), get)
names(sloc7) <- sprintf('sloc7.%d', 1:numfiles)

num<-seq(1,length(s7))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s7))
long1<-vector(length=length(s7))
for(i in 1:length(s7))
{
  lat1[i]<-sloc7[[i]]$V2[3]
  long1[i]<-sloc7[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s7))
{
  ep<-endpoints(data.s7[[i]], on="hours", k=1)
  data.s7[[i]]<-(period.apply(data.s7[[i]],ep,mean))
}

#sd(data.s7[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s7[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s7[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s7<-vector("list",(length(data.s7)*2))
count<-1
for(j in 1:length(data.s7))
{
  names(arima.s7)[[count]]<-paste((names(data.s7)[[j]]),".april",sep='')
  arima.s7[[count]]<-data.s7[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s7[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s7[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s7[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s7)[[count]]<-paste(names(data.s7)[[1]],".july",sep='')
arima.s7[[count]]<-data.s7[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s7[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s7[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s7[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s7
#Making a power curve

speed=as.double(s7[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s7[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s7[[1]]$power<-lookup(round_any(as.numeric(arima.s7[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s7[[2]]$power<-lookup(round_any(as.numeric(arima.s7[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s7<-vector("list",length(arima.s7))
count1<-1
count2<-length(arima.s7)/2 + 1
for(i in 1:length(data.s7))
{
  names(res.s7)[[count1]]<-paste((names(data.s7)[[i]]),".april",sep='')
  res.s7[[count1]]<-data.s7[[i]]$power["2011-04-01/2011-04-30"]-arima.s7[[count1]]$power
  count1<-count1+1
  
  names(res.s7)[[count2]]<-paste(names(data.s7)[[i]],".july",sep='')
  res.s7[[count2]]<-data.s7[[i]]$power["2011-07-01/2011-07-31"]-arima.s7[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s7)/2 + 1
for(i in 1:length(data.s7))
{
  
  plot(data.s7[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s7", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s7[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s7[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s7[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s7", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s7[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s7[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s7<-vector("list",length(arima.s7))
count1<-1
count2<-length(arima.s7)/2 + 1
for(i in 1:length(data.s7))
{
  names(mape.s7)[[count1]]<-paste((names(data.s7)[[i]]),".april",sep='')
  div<-res.s7[[count1]]/data.s7[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s7[[count1]]<-100*sum(abs(div))/length(data.s7[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s7)[[count2]]<-paste(names(data.s7)[[i]],".july",sep='')
  div<-res.s7[[count2]]/data.s7[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s7[[count2]]<-100*sum(abs(div))/nrow(data.s7[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s7))
{
  write.zoo(arima.s7[[i]], file = paste(names(arima.s7)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s7))
{
  write.zoo(data.s7[[i]], file = paste(names(data.s7)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s7))
{
  write.zoo(res.s7[[i]], file = paste(names(res.s7)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s7))
{
  write.zoo(mape.s7[[i]], file = paste(names(mape.s7)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 7 over -----
save.image()


#---------SITE 8 begin -----
#Reading files by zone
s8<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==8)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s8[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc8<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==8)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc8[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s8) <- sprintf('s8.%d', 1:numfiles)
for(i in 1:numfiles)
{s8[[i]]$Time <- with(s8[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s8))
{names(s8[[i]])[names(s8[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s8))
{names(s8[[i]])[names(s8[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s8.ts<-vector("list", length(s8))
names(s8.ts) <- sprintf('s8.%d', 1:numfiles)

for(i in 1:length(s8))
{
  s8.ts[[i]]<-xts(s8[[i]]$speed,s8[[i]]$Time)
  colnames(s8.ts[[i]])='speed'
  s8.ts[[i]]$power<-s8[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s8))
for(i in 1:length(s8))
{
  ep<-endpoints(s8.ts[[i]], on="hours", k=1)
  a<-(period.apply(s8.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s8.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s8 <- list()
maxindex<-which.max(CF1)
data.s8[[oldcount]]<-s8.ts[[which.max(CF1)]]
names(data.s8)[[oldcount]] <- names(s8.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc8 <- lapply( paste('sloc8.', 1:numfiles, sep=''), get)
names(sloc8) <- sprintf('sloc8.%d', 1:numfiles)

num<-seq(1,length(s8))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s8))
long1<-vector(length=length(s8))
for(i in 1:length(s8))
{
  lat1[i]<-sloc8[[i]]$V2[3]
  long1[i]<-sloc8[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s8))
{
  ep<-endpoints(data.s8[[i]], on="hours", k=1)
  data.s8[[i]]<-(period.apply(data.s8[[i]],ep,mean))
}

#sd(data.s8[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s8[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s8[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s8<-vector("list",(length(data.s8)*2))
count<-1
for(j in 1:length(data.s8))
{
  names(arima.s8)[[count]]<-paste((names(data.s8)[[j]]),".april",sep='')
  arima.s8[[count]]<-data.s8[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s8[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s8[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s8[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s8)[[count]]<-paste(names(data.s8)[[1]],".july",sep='')
arima.s8[[count]]<-data.s8[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s8[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s8[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s8[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s8
#Making a power curve

speed=as.double(s8[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s8[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s8[[1]]$power<-lookup(round_any(as.numeric(arima.s8[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s8[[2]]$power<-lookup(round_any(as.numeric(arima.s8[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s8<-vector("list",length(arima.s8))
count1<-1
count2<-length(arima.s8)/2 + 1
for(i in 1:length(data.s8))
{
  names(res.s8)[[count1]]<-paste((names(data.s8)[[i]]),".april",sep='')
  res.s8[[count1]]<-data.s8[[i]]$power["2011-04-01/2011-04-30"]-arima.s8[[count1]]$power
  count1<-count1+1
  
  names(res.s8)[[count2]]<-paste(names(data.s8)[[i]],".july",sep='')
  res.s8[[count2]]<-data.s8[[i]]$power["2011-07-01/2011-07-31"]-arima.s8[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s8)/2 + 1
for(i in 1:length(data.s8))
{
  
  plot(data.s8[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s8", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s8[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s8[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s8[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s8", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s8[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s8[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s8<-vector("list",length(arima.s8))
count1<-1
count2<-length(arima.s8)/2 + 1
for(i in 1:length(data.s8))
{
  names(mape.s8)[[count1]]<-paste((names(data.s8)[[i]]),".april",sep='')
  div<-res.s8[[count1]]/data.s8[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s8[[count1]]<-100*sum(abs(div))/length(data.s8[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s8)[[count2]]<-paste(names(data.s8)[[i]],".july",sep='')
  div<-res.s8[[count2]]/data.s8[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s8[[count2]]<-100*sum(abs(div))/nrow(data.s8[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s8))
{
  write.zoo(arima.s8[[i]], file = paste(names(arima.s8)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s8))
{
  write.zoo(data.s8[[i]], file = paste(names(data.s8)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s8))
{
  write.zoo(res.s8[[i]], file = paste(names(res.s8)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s8))
{
  write.zoo(mape.s8[[i]], file = paste(names(mape.s8)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 8 over -----
save.image()


#---------SITE 9 begin -----
#Reading files by zone
s9<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==9)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s9[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc9<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==9)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc9[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s9) <- sprintf('s9.%d', 1:numfiles)
for(i in 1:numfiles)
{s9[[i]]$Time <- with(s9[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s9))
{names(s9[[i]])[names(s9[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s9))
{names(s9[[i]])[names(s9[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s9.ts<-vector("list", length(s9))
names(s9.ts) <- sprintf('s9.%d', 1:numfiles)

for(i in 1:length(s9))
{
  s9.ts[[i]]<-xts(s9[[i]]$speed,s9[[i]]$Time)
  colnames(s9.ts[[i]])='speed'
  s9.ts[[i]]$power<-s9[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s9))
for(i in 1:length(s9))
{
  ep<-endpoints(s9.ts[[i]], on="hours", k=1)
  a<-(period.apply(s9.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s9.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s9 <- list()
maxindex<-which.max(CF1)
data.s9[[oldcount]]<-s9.ts[[which.max(CF1)]]
names(data.s9)[[oldcount]] <- names(s9.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc9 <- lapply( paste('sloc9.', 1:numfiles, sep=''), get)
names(sloc9) <- sprintf('sloc9.%d', 1:numfiles)

num<-seq(1,length(s9))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s9))
long1<-vector(length=length(s9))
for(i in 1:length(s9))
{
  lat1[i]<-sloc9[[i]]$V2[3]
  long1[i]<-sloc9[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s9))
{
  ep<-endpoints(data.s9[[i]], on="hours", k=1)
  data.s9[[i]]<-(period.apply(data.s9[[i]],ep,mean))
}

#sd(data.s9[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s9[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s9[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s9<-vector("list",(length(data.s9)*2))
count<-1
for(j in 1:length(data.s9))
{
  names(arima.s9)[[count]]<-paste((names(data.s9)[[j]]),".april",sep='')
  arima.s9[[count]]<-data.s9[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s9[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s9[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s9[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s9)[[count]]<-paste(names(data.s9)[[1]],".july",sep='')
arima.s9[[count]]<-data.s9[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s9[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s9[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s9[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s9
#Making a power curve

speed=as.double(s9[[which.max(CF1)]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s9[[which.max(CF1)]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s9[[1]]$power<-lookup(round_any(as.numeric(arima.s9[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s9[[2]]$power<-lookup(round_any(as.numeric(arima.s9[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

for(i in 1:nrow(arima.s9[[1]]))
{
  
  
  if(arima.s9[[1]]$speed[i]>=12.35)
  {
    arima.s9[[1]]$power[i]=2
  }
  
  if(arima.s9[[1]]$speed[i]<0)
  {
    arima.s9[[1]]$speed[i]=0
  }
  
  if(arima.s9[[1]]$speed[i]==0)
  {
    arima.s9[[1]]$power[i]=0
  }
  
}

for(i in 1:nrow(arima.s9[[2]]))
{
  if(arima.s9[[2]]$speed[i]>=12.35)
  {
    arima.s9[[2]]$power[i]=2
  }
  
  if(arima.s9[[2]]$speed[i]<0)
  {
    arima.s9[[2]]$speed[i]=0
  }
  
  if(arima.s9[[2]]$speed[i]==0)
  {
    arima.s9[[2]]$power[i]=0
  }
  
}


#Residuals
res.s9<-vector("list",length(arima.s9))
count1<-1
count2<-length(arima.s9)/2 + 1
for(i in 1:length(data.s9))
{
  names(res.s9)[[count1]]<-paste((names(data.s9)[[i]]),".april",sep='')
  res.s9[[count1]]<-data.s9[[i]]$power["2011-04-01/2011-04-30"]-arima.s9[[count1]]$power
  count1<-count1+1
  
  names(res.s9)[[count2]]<-paste(names(data.s9)[[i]],".july",sep='')
  res.s9[[count2]]<-data.s9[[i]]$power["2011-07-01/2011-07-31"]-arima.s9[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s9)/2 + 1
for(i in 1:length(data.s9))
{
  
  plot(data.s9[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s9", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s9[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s9[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s9[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s9", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s9[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s9[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s9<-vector("list",length(arima.s9))
count1<-1
count2<-length(arima.s9)/2 + 1
for(i in 1:length(data.s9))
{
  names(mape.s9)[[count1]]<-paste((names(data.s9)[[i]]),".april",sep='')
  div<-res.s9[[count1]]/data.s9[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s9[[count1]]<-100*sum(abs(div))/length(data.s9[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s9)[[count2]]<-paste(names(data.s9)[[i]],".july",sep='')
  div<-res.s9[[count2]]/data.s9[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s9[[count2]]<-100*sum(abs(div))/nrow(data.s9[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s9))
{
  write.zoo(arima.s9[[i]], file = paste(names(arima.s9)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s9))
{
  write.zoo(data.s9[[i]], file = paste(names(data.s9)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s9))
{
  write.zoo(res.s9[[i]], file = paste(names(res.s9)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s9))
{
  write.zoo(mape.s9[[i]], file = paste(names(mape.s9)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 9 over -----
save.image()

#---------SITE 10 begin -----
#Reading files by zone
s10<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==10)
  {
    name<-paste(path,chart$fileList[i],sep="")
    s10[[numfiles]]<-read.csv(name,skip=3,stringsAsFactors = FALSE)
    numfiles<-numfiles+1
  }
}

sloc10<-list()
numfiles<-1
for(i in 1:nrow(chart))
{
  if(chart$zone[i]==10)
  {
    name<-paste(path,chart$fileList[i],sep="")
    sloc10[[numfiles]]<-read.csv(name,header =FALSE,nrows=3,row.names=1)
    numfiles<-numfiles+1
  }
}
numfiles<-numfiles-1

#Creating a list from the data frames and generating time stamps
names(s10) <- sprintf('s10.%d', 1:numfiles)
for(i in 1:numfiles)
{s10[[i]]$Time <- with(s10[[i]], ISOdate(Year, Month, Day, hour=Hour, min=Minute,sec=0),tz="GMT")
}


#Changing column names of speed and power
for(i in 1:length(s10))
{names(s10[[i]])[names(s10[[i]]) == 'wind.speed.at.100m..m.s.'] <- 'speed'
}

for(i in 1:length(s10))
{names(s10[[i]])[names(s10[[i]]) == 'power..MW.'] <- 'power'
}

#Making a time series of wind and power
s10.ts<-vector("list", length(s10))
names(s10.ts) <- sprintf('s10.%d', 1:numfiles)

for(i in 1:length(s10))
{
  s10.ts[[i]]<-xts(s10[[i]]$speed,s10[[i]]$Time)
  colnames(s10.ts[[i]])='speed'
  s10.ts[[i]]$power<-s10[[i]]$power
}

#Finding the capacity factor by first calculating average power produced on an hourly basis
CF1<-vector(length=length(s10))
for(i in 1:length(s10))
{
  ep<-endpoints(s10.ts[[i]], on="hours", k=1)
  a<-(period.apply(s10.ts[[i]],ep,mean))
  CF1[i]<-sum(a$power)/(max(s10.ts[[i]]$power)*8760)
}

#The site with the largest CF is selected and put into a list of final data values
oldcount<-1
data.s10 <- list()
maxindex<-which.max(CF1)
data.s10[[oldcount]]<-s10.ts[[which.max(CF1)]]
names(data.s10)[[oldcount]] <- names(s10.ts)[[which.max(CF1)]]
oldcount<-oldcount+1


#Creating a list of site longitude and latitude
#Not rrequired aince reading is automated sloc10 <- lapply( paste('sloc10.', 1:numfiles, sep=''), get)
names(sloc10) <- sprintf('sloc10.%d', 1:numfiles)

num<-seq(1,length(s10))
mat<-combs(num,2) #Combinations of all locations

lat1<-vector(length=length(s10))
long1<-vector(length=length(s10))
for(i in 1:length(s10))
{
  lat1[i]<-sloc10[[i]]$V2[3]
  long1[i]<-sloc10[[i]]$V2[2]
}

#------Data collection over------

#------Arima Modeling for the turbines in site 1------

#Converting 15 minute data to hourly data

for(i in 1:length(data.s10))
{
  ep<-endpoints(data.s10[[i]], on="hours", k=1)
  data.s10[[i]]<-(period.apply(data.s10[[i]],ep,mean))
}

#sd(data.s10[[1]]$speed["2011-04-01/2011-04-30"])
#mean(data.s10[[1]]$speed["2011-04-01/2011-04-30"])
#plot(data.s10[[1]]$speed["2011-04-01/2011-04-30"])

# Our analyisis will cover April which shows a high standard deviation in wind speeds
#The anaysis also covers July where the wind fluctuates a lot


arima.s10<-vector("list",(length(data.s10)*2))
count<-1
for(j in 1:length(data.s10))
{
  names(arima.s10)[[count]]<-paste((names(data.s10)[[j]]),".april",sep='')
  arima.s10[[count]]<-data.s10[[j]]$speed["2011-04-01/2011-04-30"]
  april<-data.s10[[j]]["2011-04-01/2011-04-30"]
  
  for(i in 0:(length(data.s10[[j]]$speed["2011-04-01/2011-04-30"])-73))
  {
    a=i+1
    b=i+72
    example<-april$speed[a:b]
    
    arima.auto.fit <- auto.arima(example, stepwise=FALSE)
    point.forecast<-forecast(arima.auto.fit, h=1)$mean
    arima.s10[[count]][i+73]<-as.numeric(point.forecast)
  }
  count<-count+1
}


names(arima.s10)[[count]]<-paste(names(data.s10)[[1]],".july",sep='')
arima.s10[[count]]<-data.s10[[1]]$speed["2011-07-01/2011-07-31"]

july<-data.s10[[1]]$speed["2011-07-01/2011-07-31"]

for(i in 0:(length(data.s10[[1]]$speed["2011-07-01/2011-07-31"])-73))
{
  a=i+1
  b=i+72
  example<-july$speed[a:b]
  
  arima.auto.fit <- auto.arima(example, stepwise=FALSE)
  
  point.forecast<-forecast(arima.auto.fit, h=1)$mean
  arima.s10[[count]][i+73]<-as.numeric(point.forecast)
}


#backup<-arima.s10
#Making a power curve

speed=as.double(s10[[1]]$speed)
breaks=seq(0,25,by=0.05)
speed.cut=cut(speed,breaks, right=FALSE)
x<-tapply(s10[[1]]$power, speed.cut, mean)
pow.curve<-data.frame(bins=names(x),power=x)
pow.curve$speed<-seq(0,24.95,by=0.05)

#Looking up power
x<-list(nrow=500)
arima.s10[[1]]$power<-lookup(round_any(as.numeric(arima.s10[[1]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)
arima.s10[[2]]$power<-lookup(round_any(as.numeric(arima.s10[[2]][,1]),0.05,floor),pow.curve[,3],pow.curve[,2],missing=NULL)

#Residuals
res.s10<-vector("list",length(arima.s10))
count1<-1
count2<-length(arima.s10)/2 + 1
for(i in 1:length(data.s10))
{
  names(res.s10)[[count1]]<-paste((names(data.s10)[[i]]),".april",sep='')
  res.s10[[count1]]<-data.s10[[i]]$power["2011-04-01/2011-04-30"]-arima.s10[[count1]]$power
  count1<-count1+1
  
  names(res.s10)[[count2]]<-paste(names(data.s10)[[i]],".july",sep='')
  res.s10[[count2]]<-data.s10[[i]]$power["2011-07-01/2011-07-31"]-arima.s10[[count2]]$power
  count2<-count2+1
}

#Plots
count1<-1
count2<-length(arima.s10)/2 + 1
for(i in 1:length(data.s10))
{
  
  plot(data.s10[[i]]$power["2011-04-01/2011-04-30"], main="Wind Power in Apr for s10", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s10[[count1]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s10[[count1]]$power,col="red")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red"))
  count1<-count1+1
  
  
  plot(data.s10[[i]]$power["2011-07-01/2011-07-31"], main="Wind Power in Jul for s10", ylim=range(c(0,18)),xlab="Day and Time", ylab="Wind Speed in m/s")
  par(new=T)
  plot(arima.s10[[count2]]$power, ylim=range(c(0,18)),main=NA, xlab=NA, ylab=NA)
  lines(arima.s10[[count2]]$power,col="blue")
  legend("topleft",legend=c("Actual","Arima Model"), lty=c(1,1), lwd=c(2.5,2.5), col=c("black","blue"))
  count2<-count2+1
}

#MAPE
mape.s10<-vector("list",length(arima.s10))
count1<-1
count2<-length(arima.s10)/2 + 1
for(i in 1:length(data.s10))
{
  names(mape.s10)[[count1]]<-paste((names(data.s10)[[i]]),".april",sep='')
  div<-res.s10[[count1]]/data.s10[[i]]$power["2011-04-01/2011-04-30"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s10[[count1]]<-100*sum(abs(div))/length(data.s10[[i]]$power["2011-04-01/2011-04-30"])
  count1<-count1+1
  
  names(mape.s10)[[count2]]<-paste(names(data.s10)[[i]],".july",sep='')
  div<-res.s10[[count2]]/data.s10[[i]]$power["2011-07-01/2011-07-31"]
  if(!is.finite(sum(div)))
  {
    div[which(!is.finite(div))]<-0
  }
  mape.s10[[count2]]<-100*sum(abs(div))/nrow(data.s10[[i]]$power["2011-07-01/2011-07-31"])
  count2<-count2+1
}


for (i in 1:length(arima.s10))
{
  write.zoo(arima.s10[[i]], file = paste(names(arima.s10)[[i]],'-arima.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s10))
{
  write.zoo(data.s10[[i]], file = paste(names(data.s10)[[i]],'-data.csv',sep=''),sep=",")
  
}

for (i in 1:length(res.s10))
{
  write.zoo(res.s10[[i]], file = paste(names(res.s10)[[i]],'-res.csv',sep=''),sep=",")
  
}

for (i in 1:length(mape.s10))
{
  write.zoo(mape.s10[[i]], file = paste(names(mape.s10)[[i]],'-mape.csv',sep=''),sep=",")
  
}

#---------SITE 10 over -----
save.image()

for (i in 1:length(data.s10))
{
  write.zoo(data.s10[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s10)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s10[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s10)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s9))
{
  write.zoo(data.s9[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s9)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s9[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s9)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s8))
{
  write.zoo(data.s8[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s8)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s8[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s8)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s7))
{
  write.zoo(data.s7[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s7)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s7[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s7)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s6))
{
  write.zoo(data.s6[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s6)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s6[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s6)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s5))
{
  write.zoo(data.s5[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s5)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s5[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s5)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s4))
{
  write.zoo(data.s4[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s4)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s4[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s4)[[i]],'-july power data.csv',sep=''),sep=",")
  
}


for (i in 1:length(data.s3))
{
  write.zoo(data.s3[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s3)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s3[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s3)[[i]],'-july power data.csv',sep=''),sep=",")
  
}


for (i in 1:length(data.s2))
{
  write.zoo(data.s2[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s2)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s2[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s2)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

for (i in 1:length(data.s1))
{
  write.zoo(data.s1[[i]]$power["2011-04-01/2011-04-30"], file = paste(names(data.s1)[[i]],'-april power data.csv',sep=''),sep=",")
  write.zoo(data.s1[[i]]$power["2011-07-01/2011-07-31"], file = paste(names(data.s1)[[i]],'-july power data.csv',sep=''),sep=",")
  
}

#Plots
plot(data.s1[[1]], main="Scatter plot of actual wind speeds and power")
plot(data.s1[[1]][,1],xlab="hour",ylab="Wind speed in m/s", main="Wind speeeds at site 1 for year 1", type="l")

install.packages("MASS")
library(MASS)

fitdistr(data.s1[[1]]["2011-04-01/2011-04-30"]$speed, "weibull")

std.dev=sd(data.s1[[1]]["2011-04-01/2011-04-30"]$speed)
avg=mean(data.s1[[1]]["2011-04-01/2011-04-30"]$speed)
k=(std.dev/avg)^(-1.086) #Estimating parameters
c=avg/(gamma(1+ (1/k)))
k;c

plot(seq(0,30,by=0.05), dweibull(seq(0,30,by=0.05),k,c), xlab="Wind Speeds in m/s", ylab="Probability Density", main="Weibull Distribution of Wind Speed in April")

std.dev=sd(data.s1[[1]]["2011-07-01/2011-07-31"]$speed)
avg=mean(data.s1[[1]]["2011-07-01/2011-07-31"]$speed)
k=(std.dev/avg)^(-1.086) #Estimating parameters
c=avg/(gamma(1+ (1/k)))
k;c

plot(seq(0,30,by=0.05), dweibull(seq(0,30,by=0.05),k,c), xlab="Wind Speeds in m/s", ylab="Probability Density", main="Weibull Distribution of Wind Speed in July")


plot(data.s1[[1]]["2011-04-01/2011-04-30"]$speed,xlab="hour",ylab="Wind speed in m/s", main="Wind speeeds at site 1 for April 2011", type="l")
plot(pow.curve$speed,pow.curve$power, type="l", xlab="Speed in m/s", ylab="Power in MW", main="Power Curve")
