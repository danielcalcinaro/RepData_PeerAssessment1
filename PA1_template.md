---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---





## What is mean total number of steps taken per day?

### For this part of the assignment, we will ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day





```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
activity<-read.csv("activity.csv")
activityNA<-is.na(activity$steps)
activityOk<-activity[!activityNA,]

activityOkbyDay<-group_by(activityOk,date)

activityOkbyDaySum<-summarize(activityOkbyDay,steps=sum(steps))

activityOkbyDaySum
```

```
## # A tibble: 53 x 2
##          date steps
##        <fctr> <int>
##  1 2012-10-02   126
##  2 2012-10-03 11352
##  3 2012-10-04 12116
##  4 2012-10-05 13294
##  5 2012-10-06 15420
##  6 2012-10-07 11015
##  7 2012-10-09 12811
##  8 2012-10-10  9900
##  9 2012-10-11 10304
## 10 2012-10-12 17382
## # ... with 43 more rows
```

2. Make a histogram of the total number of steps taken each day



```r
hist(activityOkbyDaySum$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
meanperday<-mean(activityOkbyDaySum$steps)

medianperday<-median(activityOkbyDaySum$steps)
```


The mean of the total number of steps taken per day is 1.0766189\times 10^{4}

The median of the total number of steps taken per day is 10765

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)



```r
activityOkbyinterval<-group_by(activityOk,interval)

activityOkbyintervalAvg<-summarize(activityOkbyinterval,steps=mean(steps))

plot(activityOkbyintervalAvg$interval,activityOkbyintervalAvg$steps,type="l",main="Average number of steps taken, averaged across all days",xlab="Interval",ylab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? 


```r
Intervalwithmaxsteps<-activityOkbyintervalAvg[activityOkbyintervalAvg$steps==max(activityOkbyintervalAvg$steps),1]
```

The 5-minutes interval with the maximum number of steps is the 835

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
numberofNAs<-sum(activityNA)
```

The number of missing values in the dataset is 2304

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I will use the mean for that 5-minute interval.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity_filled<-activity

for (i in 1:17568){

	if(is.na(activity_filled[i,1])){

	activity_filled[i,1] <- activityOkbyintervalAvg[activityOkbyintervalAvg$interval==activity_filled[i,3],2]

					}
 	else{

		activity_filled[i,1] <- activity[i,1]

	}

}
```


4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?




```r
activityfilledbyDay<-group_by(activity_filled,date)

activityfilledbyDaySum<-summarize(activityfilledbyDay,steps=sum(steps))

hist(activityfilledbyDaySum$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
fmeanperday<-mean(activityfilledbyDaySum$steps)

fmedianperday<-median(activityfilledbyDaySum$steps)
```
The mean of the total number of steps taken per day is 1.0766189\times 10^{4}

The median of the total number of steps taken per day is 1.0766189\times 10^{4}

Both the mean and the median ar quite similar to those calculated in the first part of the assignment.

The impact of imputing missing data is a reducement of the spread in the histogram.


## Are there differences in activity patterns between weekdays and weekends?



1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.



```r
activity_filled$date <- as.character(activity_filled$date)

activity_filled$date <- as.Date(activity_filled$date)

for (i in 1:17568){

	if((julian(activity_filled$date[i]) %% 7) == 1 || (julian(activity_filled$date[i]) %% 7) == 2){
	
	activity_filled$wdaywend[i] <- "weekend"

	}

	else{

	activity_filled$wdaywend[i] <- "weekday"
	
	}

 }

activity_filled$wdaywend<-as.factor(activity_filled$wdaywend)
```


2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)



```r
library(lattice)

activityfbyintervalandw<-group_by(activity_filled,interval,wdaywend)

activityfbyintervalandwAvg<-summarize(activityfbyintervalandw,steps=mean(steps))

xyplot(activityfbyintervalandwAvg$steps~activityfbyintervalandwAvg$interval|activityfbyintervalandwAvg$wdaywend,layout = c(1, 2),type="l",xlab="5 min interval",ylab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

