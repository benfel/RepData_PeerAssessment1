# Reproducible Research: Peer Assessment 1

###Assumptions:
1. Data ("activity.csv") should be saved on its working directory.
2. Package "lubridate" should be installed.

##Loading and preprocessing the data


```r
#Setting of the working directory
setwd("/Users/ruben.felomino/Documents/Unicorn/RWorkingFiles/RepData_PeerAssessment1/")

#Loading the data ("activity.csv")
activity <- read.csv("activity.csv")

#Create subset of data ("activity") to exclude all NA values
activitynoNA <- subset(activity,!is.na(steps))

#Convert date from "Factor" to "Date"
activitynoNA$date <- as.Date(activitynoNA$date)
```

##What is mean total number of steps taken per day?

```r
#Calculate the total number of steps taken per day
steps <- aggregate(steps~date,activitynoNA,FUN="sum")

#Make a histogram of the total number of steps taken each day
hist(steps$steps,breaks=20,col="red",main="",xlab="Total number of steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

####Calculate and report the mean and median of the total number of steps taken per day

```r
#Calculating the Mean
mean(steps$steps)
```

#####The Mean value is 

```
## [1] 10766.19
```


```r
#Calculating the Median
median(steps$steps)
```

#####The Median value is 

```
## [1] 10765
```

##What is the average daily activity pattern?

```r
#Computing the average number of steps across all day er 5 minute interval
interval <- aggregate(steps~interval,activitynoNA,FUN="mean")

#Plotting the data
plot(interval$interval,interval$steps,type="l",xlab="5 Minute Inteval",ylab="Average Steps",col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

####Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxsteps <- subset(interval,steps==max(steps))
```

####The maximum number of steps is at the 835 minute interval


##Imputing missing values
####Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
rowwithNA <- sum(is.na(activity))
```

####There are 2304 rows with NA values

####Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc


```r
#Using the mean for the 5 minute interval to fill all missing values.
activityNAwithvalue <- activity
activityNAwithvalue$steps[is.na(activityNAwithvalue$steps)] <- tapply(activityNAwithvalue$steps,activityNAwithvalue$interval,mean,na.rm=TRUE)

#Plotting the new data
stepsNAwithValue <- aggregate(steps~date,activityNAwithvalue,sum)
hist(stepsNAwithValue$steps,breaks=20,col="red",main="",xlab="Total number of steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

####Calculate and report the mean and median of the total number of steps taken per day


```r
#Computing the new Mean
mean(stepsNAwithValue$steps)
```

#####The new Mean value is 

```
## [1] 10766.19
```

```(r,echo=T,results="hide")
#Computing the new Median
median(stepsNAwithValue$steps)
```

#####The new Median value is 

```
## [1] 10766.19
```

##Are there differences in activity patterns between weekdays and weekends?



```r
#Adding factor "day" from dataset "activityNAwithvalue"
library("lubridate")
activityNAwithvalue$day <- ifelse(wday(activityNAwithvalue$date)=="1"  | wday(activityNAwithvalue$date)=="7","Weekend","Weekday")

#Plotting the steps taken across all weekday days and weekend days
library("lattice")
weekendVSweekday <- aggregate(steps~interval + day,activityNAwithvalue,FUN="mean")
xyplot(steps~interval|day,data=weekendVSweekday,layout=c(2,1),type="l",col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png) 
