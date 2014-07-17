library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
act = read.csv(file = unzip("activity.zip"),header = TRUE,stringsAsFactors=FALSE)

## Pad "interval" into mil-time format. 
act$interval=str_pad(string = act$interval,width = 4,side = "left",pad = 0)

## Create combined date-time column and parse with lubridate.
act = mutate(.data = act,datetime=ymd_hm(paste(act$date,act$interval)))
group_date = group_by(act,date)
sum_date = summarize(group_date,tot_steps=sum(steps,na.rm = FALSE))
ggplot(sum_date, aes(x=tot_steps))+geom_histogram(colour="white")+
  ylab("Frequency Count")+xlab("Total Steps Per Day")+
  ggtitle("Frequency of Occurence by Count of Steps")

## What is mean total number of steps taken per day?
summary(sum_date$tot_steps)

group_time = group_by(act,interval)
sum_time = summarize(group_time,avg_steps=mean(steps,na.rm = TRUE))


## What is the average daily activity pattern?
## Plot time series.
ggplot(data = sum_time,aes(x=as.numeric(interval),y=avg_steps,group=1))+
  geom_line()+ylab("Average Steps Taken")+xlab("Interval")+
  scale_x_continuous(breaks=seq(from = 0,to = 2400,by = 300)) + 
  ggtitle("Mean steps per 5 minutes")

## Highest Average steps @ interval:
arrange(sum_time,desc(avg_steps))[1,]


## Imputing missing values
## 1 Count NA's.
sum(as.numeric(is.na(act[,"steps"])))


## Are there differences in activity patterns between weekdays and weekends?
act=mutate(act,wdy = wday(act$datetime))
act=mutate(act,wkendBool = as.numeric(grepl(pattern = "1|7",x = act$wdy)))
group_weekend = group_by(act,wkendBool)
