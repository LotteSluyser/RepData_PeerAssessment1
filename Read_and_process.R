##Read and process the data - Cours 5, Project 1
Monitor<- read.csv("activity.csv")
## Sum steps per day
a<- aggregate(steps ~ date, Monitor, sum)
a$date <- as.Date(a$date)
## make a histogram of total number of steps by day
hist(a$steps,
        main=expression('total number of steps taken each day'),
        xlab='number of steps', ylab="frequency",col="red", breaks=20)
## mean and median number of steps taken each day
summary(a$steps)
## median is 10760 and mean = 10770
##average number of steps: show median and main
abline(v=c(median(a$steps),mean(a$steps)), col=c("green", "blue"), lty=c(1,2), lwd=c(1, 3))
legend("right", bty = "n", lty=2,lwd=3,col=c("green","blue"),legend=c("median", "mean"), cex=0.7, y.intersp=0.1)

## time series plot of the average number of steps taken
b<- aggregate(steps ~ interval, Monitor, mean)
plot(b$interval, b$steps, type="l", col="orange",main="Average steps per 5 min. time interval", xlab="time interval", ylab="average number of steps taken") 


## which 5 minute interval contains the max number of steps on average?
library(dplyr)
filter(b, steps==max(b$steps))
##interval    steps
##   835      206.1698
## The number of rows with missing values in the dataset
nrow(Monitor)-sum(complete.cases(Monitor))
## 2304
## A strategy for imputing missing values is to impute with
## the mean value of the same interval
library(plyr) ## probably not necessary
library(Hmisc) ## probably not necessary
Monitor2<- read.csv("activity.csv")
Monitor2$steps[is.na(Monitor2$steps)] <- ave(Monitor2$steps, Monitor2$interval, 
        FUN = function(x) 
        mean(x, na.rm = TRUE))[c(which(is.na(Monitor2$steps)))]
## Monitor2 is now the new dataset with missing data filled in

## Sum steps per day
c<- aggregate(steps ~ date, Monitor2, sum)
c$date <- as.Date(c$date)
## make a histogram of total number of steps by day
hist(c$steps,
     main=expression('total number of steps taken each day'),
     xlab='number of steps', ylab="frequency",col="purple", breaks=20)
## mean and median number of steps taken each day
summary(c$steps)
## median is 10770 (in stead of 10760 as before) and mean = 10770 (as before)
##average number of steps: show median and main
abline(v=c(median(c$steps),mean(c$steps)), col=c("green", "blue"), lty=c(1,2), lwd=c(1, 3))
legend("right", bty = "n", lty=2,lwd=3,col=c("green","blue"),legend=c("median", "mean"), cex=0.7, y.intersp=0.1)

## time series plot of the average number of steps taken
d<- aggregate(steps ~ interval, Monitor2, mean)
plot(d$interval, d$steps, type="l", col="orange",main="Average steps per 5 min. time interval", xlab="time interval", ylab="average number of steps taken") 
## which 5 minute interval contains the max number of steps on average?
filter(d, steps==max(d$steps))
##interval    steps
##   835      206.1698 (exactly the same as before)

## total number of steps taken with imputed data vs
# without imputed data
par(mfrow=c(1,2), mar=c(4,4,2,1))
hist(a$steps,
      main=expression('total number of steps taken each day'),
      xlab='number of steps', ylab="frequency",col="red", breaks=20)
hist(c$steps,
      main=expression('total number of steps taken each day'),
      xlab='number of steps', ylab="frequency",col="purple", breaks=20)
## The histograms show that imputing missing values results in
## a higher count for number of steps that has the highest occurance

## Create panel plot that compares average no. steps taken
## per 5 minute interval across weekdays and weekends.
## Add column with weekdays to the dataset with imputed values
Monitor2ex<- mutate(Monitor2,day=weekdays(c$date))
Monitor2exweekend<- filter(Monitor2ex,day==c("zaterdag","zondag"))
Monitor2exweekdays<- Monitor2ex[!(Monitor2ex$day %in% Monitor2exweekend$day),]
d_weekend<- aggregate(steps ~ interval, Monitor2exweekend, mean)
d_weekdays<- aggregate(steps ~ interval, Monitor2exweekdays, mean)
## plot panel with 2 pictures
par(mfrow=c(1,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
plot(d_weekend$interval, d_weekend$steps, type="l", col="orange",main="Weekend", xlab="Interval", ylab="Number of steps",cex=1, ylim=range(c(d_weekend$steps, d_weekdays$steps))) 
plot(d_weekdays$interval, d_weekdays$steps, type="l", col="orange",main="Weekdays", xlab="Interval", ylab="Number of steps", cex=1, ylim=range(c(d_weekend$steps, d_weekdays$steps)))
## Conclusions:
##The activity level in the weekend is more irregular.
##weekdays are more regular in activity. 
##In the weekend is much more activity around 800 minutes,
##so at around 1 pm. 