# Reproducible Research: Peer Assessment 1
================================================================
Submitted by Lorena Zuniga




## Loading and preprocessing the data
Data from activity monitoring device, ignoring NA values.

```r
activityData_all <- read.csv("activity.csv")

activityData <- na.omit(activityData_all)

```



## What is mean total number of steps taken per day?

Histogram of the total number of steps taken each day


```r

activityData$date <- as.Date(activityData$date)

totalByDay <- aggregate(activityData$steps, list(activityData$date), FUN = sum)
hist(totalByDay$x, main = "Total steps taken each day", xlab = "Total steps")
```

![plot of chunk histogram](figure/histogram.png) 


### The mean and median total number of steps taken per day:

```r

theMean <- mean(totalByDay$x)
theMedian <- median(totalByDay$x)

```

The mean is 1.0766 &times; 10<sup>4</sup>  and the median is 10765


## What is the average daily activity pattern?

```r
activityPattern <- aggregate(activityData$steps, list(activityData$interval), 
    FUN = mean)

colnames(activityPattern) <- c("Interval", "Average")

plot(activityPattern$Interval, activityPattern$Average, type = "l", xlab = "Interval", 
    ylab = "Average Steps", main = "Average steps taken by 5-minute interval")
```

![plot of chunk averageSteps](figure/averageSteps.png) 


## Imputing missing values
### Calculating the total number of missing values in the dataset

```r
totalMissing = sum(is.na(activityData_all))
```

Number of missing values is 2304

### Substitution of NA values
NA values are substituted by the mean of the correspondent 5minute interval


```r

for (i in 1:nrow(activityData_all)) {
    if (is.na(activityData_all[i, ]$steps)) {
        curInterval <- activityData_all[i, ]$interval
        meanSteps <- activityPattern[activityPattern$Interval == curInterval, 
            ]$Average
        activityData_all[i, ]$steps <- meanSteps
    }
}
```


### New Total number of steps taken each day (with replaced NA values)
Histogram of the total number of steps taken each day

```r
activityData_all$date <- as.Date(activityData_all$date)

newTotalByDay <- aggregate(activityData_all$steps, list(activityData_all$date), 
    FUN = sum)
hist(newTotalByDay$x, main = "Total steps taken each day", xlab = "Total steps")
```

![plot of chunk histogram2](figure/histogram2.png) 


### The mean and median total number of steps taken per day:

```r

theNewMean <- mean(newTotalByDay$x)
theNewMedian <- median(newTotalByDay$x)
```

The mean is 1.0766 &times; 10<sup>4</sup>  and the median is 1.0766 &times; 10<sup>4</sup>

In this case the mean and median obtained are practically the same as before the NA values substitution.

## Are there differences in activity patterns between weekdays and weekends?

Adding a new column (typeOfDay) to the dataframe in order to indicate if the day is a weekday or a weekend day

```r
activityData_all$typeOfDay <- ifelse(weekdays(activityData_all$date) == "Saturday" | 
    weekdays(activityData_all$date) == "Sunday", "weekend", "weekday")
```


### Plotting the average number of steps taken (y axis) on weekdays or weekend days (x-axis)


```r
library(lattice)
attach(activityData_all)
avgData <- aggregate(activityData_all$steps, by = list(interval, typeOfDay), 
    FUN = mean)
detach(activityData_all)
colnames(avgData) <- c("interval", "typeOfDay", "average")
xyplot(avgData$average ~ avgData$interval | avgData$typeOfDay, data = avgData, 
    layout = c(1, 2), type = "l", ylab = "Number of steps", xlab = "Interval")
```

![plot of chunk weeks](figure/weeks.png) 

