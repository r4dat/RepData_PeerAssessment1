library(lubridate)
library(dplyr)
library(stringr)
# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
act = read.csv(file = unzip("activity.zip"),header = TRUE,stringsAsFactors=FALSE)

## Pad "interval" into mil-time format. 
act$interval=str_pad(string = act$interval,width = 4,side = "left",pad = 0)

## Create combined date-time column and parse with lubridate.
act = mutate(.data = act,datetime=ymd_hm(paste(act$date,act$interval)))
group_date = group_by(act,date)
summ = summarize(by_day,tot_steps=sum(steps,na.rm = FALSE))
ggplot(summ, aes(x=tot_steps))+geom_histogram(colour="white")+ylab("Frequency Count")+xlab("Total Steps Per Day")

## What is mean total number of steps taken per day?
summary(summ$tot_steps)


## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
