Reproductive Research - Project 1 
---------------------------------

### Loading and preprocessing the data

#### 1. Load the data

``` r
library(dplyr)
library(ggplot2)

dados <- read.csv("./activity.csv", header = T, stringsAsFactors = F)
```

#### 2. Process/transform the data (if necessary) into a format suitable for your analysis

``` r
dados$date <- as.Date(dados$date)

head(dados)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

``` r
str(dados)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

``` r
summary(dados)
```

    ##      steps             date               interval     
    ##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
    ##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
    ##  Median :  0.00   Median :2012-10-31   Median :1177.5  
    ##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
    ##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
    ##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
    ##  NA's   :2304

### What is mean total number of steps taken per day?

#### 1. Calculate the total number of steps taken per day

``` r
passosdia <- aggregate(steps ~ date, dados, sum, na.rm = T)
```

#### 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

``` r
passosdia %>%
    ggplot() +
    geom_histogram(aes(x=steps), bins = 5) +
    labs(title="Histogram", 
         subtitle="Steps per day (bins = 5)") +
    theme_minimal()
```

![](./figure/plot1.png)

#### 3. Calculate and report the mean and median of the total number of steps taken per day

``` r
mean(passosdia$steps)
```

    ## [1] 10766.19

``` r
median(passosdia$steps)
```

    ## [1] 10765

### What is the average daily activity pattern?

#### 1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
passosintervalo <- aggregate(steps ~ interval, dados, mean, na.rm = T)
```

``` r
passosintervalo %>%
    ggplot() +
    geom_line(aes(x=interval, y=steps)) +
    labs(title="Time Series Plot", 
         subtitle="Steps per interval") +
    theme_minimal() 
```

![](./figure/plot2.png)

#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
passosintervalo[which.max(passosintervalo$steps),]$interval
```

    ## [1] 835

### Imputing missing values

#### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

``` r
sum(is.na(dados$steps))
```

    ## [1] 2304

#### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

We will use the mean value per interval, making a merge with the
data.frame *passosintervalo*:

``` r
dados_com_na <- subset(dados, is.na(steps))
dados_sem_na <- subset(dados, !is.na(steps))

dados_com_na <- merge(dados_com_na, passosintervalo, by="interval")
dados_com_na <- select(dados_com_na, "steps"=steps.y, date, interval)
```

#### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
dados_sem_na <- rbind(dados_sem_na, dados_com_na)
```

#### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
passosdia_sem_na <- aggregate(steps ~ date, dados_sem_na, sum)
passosdia_sem_na %>%
    ggplot() +
    geom_histogram(aes(x=steps), bins = 5) +
    labs(title="Histogram", 
         subtitle="Steps per day (bins = 5, without NA values)") +
    theme_minimal()
```

![](./figure/plot3.png)

``` r
mean(passosdia_sem_na$steps)
```

    ## [1] 10766.19

``` r
median(passosdia_sem_na$steps)
```

    ## [1] 10766.19

The impact was not significant. It is noted that the mean was unchanged,
and the median had little change. Still, the histogram was very similar
in both cases.

``` r
mean(passosdia_sem_na$steps) - mean(passosdia$steps)
```

    ## [1] 0

``` r
median(passosdia_sem_na$steps) - median(passosdia$steps)
```

    ## [1] 1.188679

### Are there differences in activity patterns between weekdays and weekends?

#### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

``` r
dados_sem_na$day <- ifelse(weekdays(dados_sem_na$date, T) %in% c("sáb", "dom"), "weekend", "weekday")
dados_sem_na$day <- as.factor(dados_sem_na$day)
```

#### 2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

``` r
passosdia <- aggregate(dados_sem_na$steps ~ dados_sem_na$interval + dados_sem_na$day, dados_sem_na, mean)
```

``` r
passosdia %>%
    select("interval"=`dados_sem_na$interval`,
           "day"=`dados_sem_na$day`,
           "steps"=`dados_sem_na$steps`) %>%
    ggplot() +
    geom_line(aes(x=interval, y=steps)) +
    labs(title="Time Series Plot", 
         subtitle="Steps per interval") +
    theme_minimal() +
    facet_wrap(~day, nrow=2)
```

![](./figure/plot4.png)
