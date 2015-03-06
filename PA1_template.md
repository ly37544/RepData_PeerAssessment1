#Data Analysis of Steps Pattern


##Steps Caculated in terms of days


Preprocessing code are as follows:


```r
setwd("~/reprod_assign")
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
data <- unique(read.csv("activity.csv"))
```

Then I am going to compute the average steps taken per day with the R code below:


```r
data1 <- group_by(data,date)
sum1 <- summarise(data1,steps = sum(steps))
```

###Here is the plot of total number of steps taken per day.


```r
hist(sum1$steps,xlab = "steps",col = "pink",main = "Histogram of Steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

###Below is the summary of tidy data including mean and median.

```r
summary(sum1$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10760   10770   13290   21190       8
```


##The analysis of relation between 5-minute-interval and average steps.


The preprocessing R code as follows:


```r
data2 <- group_by(data,interval)
sum2 <- summarise(data2,mean = mean(steps,na.rm = T))
max_interval <- subset(sum2,mean == max(sum2$mean),select = interval)[[1]]
showmsg <- paste("From the plot above,we can know that the interval",max_interval,"contains the maximum number of steps",sep = " ")
print(showmsg)
```

```
## [1] "From the plot above,we can know that the interval 835 contains the maximum number of steps"
```

###The time series plot of 5-minute-interval and average steps


```r
plot(sum2$interval,sum2$mean,type = "l",xlab = "interval",ylab = "steps",col = "red")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 


##Imputing Missing Values


Next I am going to deal with the missing values in the dataset.The stratigy is to substitute the NA values with corresponding mean of that interval in which the NA value falls. After that, I get the filled dataset named "final".


```r
count_na <- summary(data$steps)[[7]]
data3 <- merge(data,sum2,by = "interval")
data3 <- arrange(data3,date,interval)
nas <- is.na(data3$steps)
data3$steps[nas] <- data3$mean[nas]
final <- cbind(data3[,c(2,3,1)])
head(final,10)
```

```
##        steps       date interval
## 1  1.7169811 2012-10-01        0
## 2  0.3396226 2012-10-01        5
## 3  0.1320755 2012-10-01       10
## 4  0.1509434 2012-10-01       15
## 5  0.0754717 2012-10-01       20
## 6  2.0943396 2012-10-01       25
## 7  0.5283019 2012-10-01       30
## 8  0.8679245 2012-10-01       35
## 9  0.0000000 2012-10-01       40
## 10 1.4716981 2012-10-01       45
```

###The number of missing values is 

```r
count_na
```

```
## [1] 2304
```

###Based on the final dataset with subsituted NA, I will plot a histogram.

```r
data4 <- group_by(final,date)
sum3 <- summarise(data4,sum_date = sum(steps))
```

###The histogram of the new dataset with filled missing values:


```r
hist(sum3$sum_date,xlab = "steps",col = "yellow",main = "Histogram of new dataset")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

###The report of mean and median of new dataset:


```r
summary(sum3$sum_date)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```

From the summary we can conclude that the median value increases and mean value stays the same,compared with ones I caculate with incomplete dataset. Finally the median is equal with the mean.


##Adding new factor variable


I am going to add a new factor varible to the "final" dataset to specify the weekdays or weekends in which those steps are taken.

###In the code below I create a new factor variable "week"


```r
library(ggplot2)
weekday <- weekdays(as.Date(final$date))
week <- ordered(weekday)
week <- ifelse(week > "星期二" & week < "星期三","weekend","weekday")
final$week <- week
data5 <- group_by(final,week,interval)
sum4 <- summarise(data5,mean = mean(steps))
head(final,10)
```

```
##        steps       date interval    week
## 1  1.7169811 2012-10-01        0 weekday
## 2  0.3396226 2012-10-01        5 weekday
## 3  0.1320755 2012-10-01       10 weekday
## 4  0.1509434 2012-10-01       15 weekday
## 5  0.0754717 2012-10-01       20 weekday
## 6  2.0943396 2012-10-01       25 weekday
## 7  0.5283019 2012-10-01       30 weekday
## 8  0.8679245 2012-10-01       35 weekday
## 9  0.0000000 2012-10-01       40 weekday
## 10 1.4716981 2012-10-01       45 weekday
```

###The time series plot of 5-minute-interval and average steps bewteen weekdays and weekends


```r
qplot(interval,mean,data = sum4,facets = week ~., geom = "line")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 
