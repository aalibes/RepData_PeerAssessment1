---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
---

<!-- Reproducible Research: Peer Assessment 1 -->
<!-- ======================================== -->


```r
knitr::opts_chunk$set(echo = TRUE)
```


## Loading and preprocessing the data

```r
library(data.table)

activity <- fread(unzip("activity.zip"))
activity$date <- as.Date(activity$date)
summary(activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```



## What is mean total number of steps taken per day?

```r
library(magrittr)
activityDay <- activity[,.(stepsDay=sum(steps, na.rm=T)),by=date] 
meanDay <- as.integer(activityDay[,mean(stepsDay,na.rm = T)])
medianDay <- as.integer(activityDay[,median(stepsDay,na.rm = T)])
hist(activityDay$stepsDay,xlab = "Steps per Day", 
     main = "Histogram of the number of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

The mean number of steps per day is 9354 and the median 10395


## What is the average daily activity pattern?


```r
activityInterval <- activity[,.(stepsInterval=mean(steps,na.rm=T)),by=interval]
plot(activityInterval$interval,activityInterval$stepsInterval,type="l", 
     xlab="Interval",ylab="Mean steps per interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
maxInterval <- activityInterval[which.max(stepsInterval), interval]
```

The interval with a highest average number steps is 835.

## Imputing missing values


```r
narows <- nrow(activity) - sum(complete.cases(activity))
activity.fix <- copy(activity)
activity.fix$steps <- as.double(activity.fix$steps)
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
activity.fix[,steps:=impute.mean(steps),by=interval]
#activityInterval.fix <- activity.fix[,.(stepsInterval=mean(steps,na.rm=T)),by=interval]

activityDay.fix <- activity.fix[,.(stepsDay=sum(steps, na.rm=T)),by=date] 

meanDay.fix <- as.integer(activityDay.fix[,mean(stepsDay,na.rm = T)])
medianDay.fix <- as.integer(activityDay.fix[,median(stepsDay,na.rm = T)])
hist(activityDay.fix$stepsDay,xlab = "Steps per Day", 
     main = "Histogram of the number of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

The mean number of steps per day (with imputed NAs) is 10766 and the median 10766


## Are there differences in activity patterns between weekdays and weekends?
