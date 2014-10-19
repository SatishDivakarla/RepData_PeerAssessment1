---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
activity_data = read.csv("activity.csv", header = TRUE)
count_steps_by_day <- aggregate(steps ~ date, FUN = sum, data=activity_data)
```


## What is mean total number of steps taken per day?

```r
hist(count_steps_by_day$steps)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
mean <- mean(count_steps_by_day$steps,na.rm=TRUE)
median <- median(count_steps_by_day$steps,na.rm=TRUE)
```
Mean is : 1.0766 &times; 10<sup>4</sup> and Median is 10765

## What is the average daily activity pattern?

```r
average_steps_by_interval <- aggregate(steps ~ interval, FUN = "mean", data=activity_data)
plot(average_steps_by_interval$steps, type="l",xlab="Interval",ylab="Average steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
max_interval <- max( average_steps_by_interval[ , "steps" ] )
max_interval
```

```
## [1] 206.2
```

```r
row <- subset(average_steps_by_interval, steps== max( average_steps_by_interval[ , "steps" ] ))
interval_with_max_numbers <- row$interval
```
Interval with max steps : 835

## Imputing missing values

```r
activity_data_na_df <- activity_data[rowSums(is.na(activity_data)) > 0,]
nrows <- nrow(activity_data_na_df)

# Removing NAs and replacing with interval mean
dataframe_with_mean_steps_interval <- merge(x=activity_data_na_df, y= average_steps_by_interval, by="interval", all.x=TRUE)
dataframe_with_mean_steps_interval$steps.x <- NULL
names(dataframe_with_mean_steps_interval)[names(dataframe_with_mean_steps_interval)=="steps.y"] <- "steps"

new_activity_data <- activity_data[complete.cases(activity_data),]
new_activity_data <- rbind(new_activity_data,dataframe_with_mean_steps_interval)
new_count_steps_by_day <- aggregate(steps ~ date, FUN = sum, data=new_activity_data)

hist(new_count_steps_by_day$steps)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
mean <- mean(new_count_steps_by_day$steps,na.rm=TRUE)
median <- median(new_count_steps_by_day$steps,na.rm=TRUE)
```

Number of rows with NA is : 2304

Mean of new activity data : 1.0766 &times; 10<sup>4</sup> and median : 1.0766 &times; 10<sup>4</sup>

## Are there differences in activity patterns between weekdays and weekends?


```r
library(lattice) 
new_activity_data["day"] <- ifelse(weekdays(as.Date(new_activity_data$date,"%Y-%m-%d")) %in% c("Saturday","Sunday"), "WEEKEND", "WEEKDAY")

average_steps_by_interval_weekdays <- aggregate(steps ~ interval, FUN = "mean", data=new_activity_data[new_activity_data$day == "WEEKDAY", ])

average_steps_by_interval_weekends <- aggregate(steps ~ interval, FUN = "mean", data=new_activity_data[new_activity_data$day == "WEEKEND", ])

attach(new_activity_data)
days.f <- new_activity_data$day
#par(mfrow = c(2,1))
plot(average_steps_by_interval_weekdays$steps, type="l",xlab="Interval",ylab="Average steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-51.png) 

```r
plot(average_steps_by_interval_weekends$steps, type="l",xlab="Interval",ylab="Average steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-52.png) 

