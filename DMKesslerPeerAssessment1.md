---
title: 'Reproducible Research: Peer Assessment 1'
author: "David Kessler"
date: "11/9/2020"
output:
  html_document:
    keep_md: yes
---

## Intro

This document is created for the Coursera Reproducible Research course peer assessment 1 assignment.

## Loading and preprocessing the data

Show any code that is needed to

    1. Load the data (i.e. read.csv())
    2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
data <- read.csv(unz(temp, "activity.csv"), na.strings = "NA")
unlink(temp)
data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

    1. Calculate the total number of steps taken per day
    2. If you do not understand the difference between a histogram and a barplot, research the difference between
    them. Make a histogram of the total number of steps taken each day
    3. Calculate and report the mean and median of the total number of steps taken per day


```r
#create dataframe total steps taken each day, including NA values
totalsteps <- tapply(data$steps, data$date, sum)

#create a histogram of the total number of steps taken each day
hist(totalsteps)
```

![](DMKesslerPeerAssessment1_files/figure-html/mean total number of steps taken per day-1.png)<!-- -->

```r
#calculate the mean total number of steps taken per day (ignoring NAs)
datamean <- mean(totalsteps, na.rm = TRUE)
datamean
```

```
## [1] 10766.19
```

```r
#calculate the median total number of steps taken per day (ignoring NAs)
datamed <- median(totalsteps, na.rm = TRUE)
datamed
```

```
## [1] 10765
```
The mean is 1.0766189\times 10^{4} the median is 10765

## What is the average daily activity pattern?
    1. Make a time series plot (i.e. type = "l" of the 5-minute interval (x-axis) and the average number of steps 
    taken, averaged across all days (y-axis).
    2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
    

```r
#create dataframe number of steps in each interval averaged (mean) across days
avgsteps <-  data.frame(tapply(data$steps, data$interval, mean, na.rm = TRUE))
avgsteps$interval <- rownames(avgsteps)
colnames(avgsteps) <- c("mean_steps", "interval")

#Make time series plot of interval vs steps, averaged (mean) across days
plot(avgsteps$interval, avgsteps$mean_steps, type = "l",
     xlab = "Interval", ylab = "Number of steps",
     main = "Average Number of Steps Taken each Interval")
```

![](DMKesslerPeerAssessment1_files/figure-html/What is the average daily activity pattern-1.png)<!-- -->

```r
#Which interval averaged across all dataset days, contains the max steps?
maxsteps <- which.max(avgsteps$mean_steps)
avgsteps$interval[maxsteps]
```

```
## [1] "835"
```
The interval containing the max average steps is 835

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing 
days may introduce bias into some calculations or summaries of the data.

    1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA)
    2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be 
    sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, 
    etc.
    3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
    4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median 
    total number of steps taken per day. Do these values differ from the estimates from the first part of the 
    assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
#Calculate and report the total number of missing values ("NA"s) in the dataset
summary(data)
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

```r
#Looking at the data, it appears that only "steps" has NA values
#There are "insert R code here" missing values
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
#My imputation stragegy is the average number of steps for that interval
#The below create the new data set with imputed values
dataimp <-data
dataimp <- merge(dataimp, avgsteps) 
dataimp$impsteps <- ifelse(is.na(dataimp$steps),
                                   dataimp$mean_steps,
                                   dataimp$steps)

#create dataframe total steps taken each day with imputed values
totalimpsteps <- tapply(dataimp$impsteps, dataimp$date, sum)

#create histogram of total steps taken each day with imputed values
hist(totalimpsteps)
```

![](DMKesslerPeerAssessment1_files/figure-html/Imputing missing values-1.png)<!-- -->

```r
#calculate mean number steps taken per day with imputed values
mean(totalimpsteps)
```

```
## [1] 10766.19
```

```r
#calculate median number steps taken per day with imputed values
impmed <- median(totalimpsteps)
impmed
```

```
## [1] 10766.19
```
The mean does not change from the imputation.
The median increased from 10765 to 1.0766189\times 10^{4}

## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for
this part.

    1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a 
    given date is a weekday or weekend day.
    2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the 
    average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in
    the GitHub repository to see an example of what this plot should look like using simulated data.

```r
#add a day variable
dataimp$day <- weekdays(dataimp$date)
wend <- subset(dataimp, day %in% c("Saturday", "Sunday"))
wday <- subset(dataimp, day %in% 
                 c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))

#create dataframes of steps per interval averaged (mean) across weekdays/ends
wendavg <-  data.frame(tapply(wend$impsteps, wend$interval, mean))
wendavg$interval <- rownames(wendavg)
colnames(wendavg) <- c("mean_steps", "interval")
wdayavg <-  data.frame(tapply(wday$impsteps, wday$interval, mean))
wdayavg$interval <- rownames(wdayavg)
colnames(wdayavg) <- c("mean_steps", "interval")

#Make time series plots of interval vs steps, averaged (mean) across days
par(mfrow = c(2,1))
plot(wendavg$interval, wendavg$mean_steps, type = "l",
     xlab = "Interval", ylab = "Number of steps",
     main = "weekend")
plot(wdayavg$interval, wdayavg$mean_steps, type = "l",
     xlab = "Interval", ylab = "Number of steps",
     main = "weekday")
```

![](DMKesslerPeerAssessment1_files/figure-html/Are there differences in activity patterns between weekdays and weekends-1.png)<!-- -->
