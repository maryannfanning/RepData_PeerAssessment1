# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```


## What is mean total number of steps taken per day?
We are now interested in visualizing the number of steps taken every day - fist calculating number of steps per day

```r
library(plyr)
stepsPerDay <- ddply(activity, c("date"), summarize, totsteps=sum(steps, na.rm=TRUE))
```

Histogram

```r
hist(stepsPerDay$totsteps, main="total number of steps per day", xlab="number of steps", ylab="number of days")
```

![](./PA1_template_files/figure-html/hist-1.png) 
Calculating Mean:

```r
mean(stepsPerDay$totsteps)
```

```
## [1] 9354.23
```
Calculating Median:

```r
median(stepsPerDay$totsteps)
```

```
## [1] 10395
```
This dataset is skewed towards lower measurements as reflected by the mean (9354.23) being lower than the median (10395). 


## What is the average daily activity pattern?
We will now make a plot for the time series plot to track activity throughout the day - first step is to calculate the average steps for each interval


```r
avgStepsPerInterval <- ddply(activity, c("interval"), summarize, totsteps=sum(steps, na.rm=TRUE))
intervalCounts <- count(activity$interval)

avgStepsPerInterval$intervalFreq <- intervalCounts$freq
avgStepsPerInterval$avgsteps <- with(avgStepsPerInterval, totsteps/intervalFreq)
```

Plotting the average number of steps per interval in a line graph:

```r
plot(avgStepsPerInterval$interval, avgStepsPerInterval$avgsteps, type="l", main="average number of steps per interval", xlab="interval", ylab="steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

Finding time with highest average number of steps:

```r
avgStepsPerInterval[avgStepsPerInterval$avgsteps == max(avgStepsPerInterval$avgsteps),]
```

```
##     interval totsteps intervalFreq avgsteps
## 104      835    10927           61 179.1311
```
Interval 835 has the highest number of average steps

The general pattern is that activity is low until around 5am, and then it begins to increase, with the largest jump around 9am. It then dramatically decreases around 9.30, with more moderate peaks around noon and four pm. There is then higher activity in the evening until what looks like 8.30pm, when it begins to taper off.


## Imputing missing values
Now we will count the NA's and replace them with appropriate data

```r
activityNoNA <- activity
summary(activityNoNA)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```
Number of NAs is 2304

function to identify NAs, and replace them with the approximate number of steps:

```r
newSteps <- c()

replaceNA <- function(steps, interval){
    if(is.na(steps)==TRUE){
        newRow <- avgStepsPerInterval[avgStepsPerInterval$interval==interval,]
        newSteps <- c(newSteps, newRow$avgsteps)
    }
    else{
        newSteps <- c(newSteps, steps)
    }
}

newSteps <- apply(activity[,c("steps", "interval")], 1, function(x) replaceNA(x[1], x[2]))

activityNoNA <- cbind(activityNoNA, newSteps)
adjustedstepsPerDay <- ddply(activityNoNA, c("date"), summarize, totsteps=sum(newSteps, na.rm=TRUE))
```
Generate histogram for adjusted steps per day:

```r
hist(adjustedstepsPerDay$totsteps, main="total number of steps per day, NAs removed", xlab="number of steps", ylab="number of days")
```

![](./PA1_template_files/figure-html/unnamed-chunk-10-1.png) 
Calculate mean and median:

```r
mean(adjustedstepsPerDay$totsteps)
```

```
## [1] 10581.01
```

```r
median(adjustedstepsPerDay$totsteps)
```

```
## [1] 10395
```

The biggest impact on this dataset is that the mean seems to move further to the right as a larger number of measurements are collected, but the median stays the same. This could reflect that the high frequency days were impacted more by replacing NA values, which would in turn indicate that the NAs skew low. 

There is a clear increase in the second bucket on the histogram, and the mean (10581.01) is now greater than the media(10395) indicating that the lower measurements may have been more related to missing data than to low activity.

## Are there differences in activity patterns between weekdays and weekends?

Comparing weekdays to weekends - we will compare the number of steps taken per day on the weekdays and the weekend

Function to add column for weekday, and then various calculations to break the dataset into weekday and weekend

```r
nameOfDay <- c()
daysOfWeek <- function(date){
    date <- as.Date(date)
    weekDay <- weekdays(date)
    nameOfDay <- c(nameOfDay, weekDay)
}

days <- apply(activityNoNA["date"], 1, function(x) daysOfWeek(x))


activityWithWeekDays <- cbind(activityNoNA, days)


mondays <- activityWithWeekDays[activityWithWeekDays$days==c("Monday"),]
tuesdays <- activityWithWeekDays[activityWithWeekDays$days==c("Tuesday"),]
wednesdays <- activityWithWeekDays[activityWithWeekDays$days==c("Wednesday"),]
thursdays <- activityWithWeekDays[activityWithWeekDays$days==c("Thursday"),]
fridays <- activityWithWeekDays[activityWithWeekDays$days==c("Friday"),]
workDays <- rbind(mondays, tuesdays, wednesdays, thursdays, fridays)


weekDayAvg <- ddply(workDays, c("interval"), summarize, totsteps=sum(newSteps))

intervalCounts <- count(workDays$interval)

weekDayAvg$intervalFreq <- intervalCounts$freq
weekDayAvg$avgsteps <- with(weekDayAvg, totsteps/intervalFreq)



saturdays <- activityWithWeekDays[activityWithWeekDays$days==c("Saturday"),]
sundays <- activityWithWeekDays[activityWithWeekDays$days==c("Sunday"),]
weekEnd <- rbind(saturdays, sundays)

weekEndAvg <- ddply(weekEnd, c("interval"), summarize, totsteps=sum(newSteps))

intervalCounts <- count(weekEnd$interval)

weekEndAvg$intervalFreq <- intervalCounts$freq
weekEndAvg$avgsteps <- with(weekEndAvg, totsteps/intervalFreq)
```

plotting weekday and weekend data for comparison:

```r
par(mfrow=c(2, 1))
plot(weekDayAvg$interval, weekDayAvg$avgsteps, type="l", main="Weekday interval averages", xlab="interval", ylab="steps")
plot(weekEndAvg$interval, weekEndAvg$avgsteps, type="l", main="weekend interval averages", xlab="interval", ylab="steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

Weekdays have a fairly well defined pattern reflecting the workday. Weekends in contrast have a less clear pattern and reflect a less rigid schedule. The weekday schedule is much closer to the overall schedule, indicating that although there is some variance between weekdays and weekends, it is not significant enough to impact the overall pattern.
