---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


```r
knitr::opts_chunk$set(cache = TRUE)

library(readr)
library(magrittr)
library(ggplot2)
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
setwd("/Users/ashipunova/work/data_science_coursera/intro_r/RepData_PeerAssessment1/")
```

## Loading and preprocessing the data

```r
# unzip("sales.zip", list = TRUE)
activity_data <- readr::read_csv(unzip("activity.zip", "activity.csv"))
```

```
## 
## ── Column specification ────────────────────────────────────────────────────────
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```

```r
str(activity_data)
```

```
## tibble [17,568 × 3] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ steps   : num [1:17568] NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date[1:17568], format: "2012-10-01" "2012-10-01" ...
##  $ interval: num [1:17568] 0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   steps = col_double(),
##   ..   date = col_date(format = ""),
##   ..   interval = col_double()
##   .. )
```

```r
# transform(activity_data, date = ymd(date))
```


## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
step_sums <- aggregate(steps ~ date, activity_data, sum, na.rm = T)
step_sums %>% head()
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
sum_plot <- qplot(step_sums$steps, geom = "histogram", binwidth = 25000/5) 
print(sum_plot)
```

![](PA1_template_files/figure-html/sum_hist-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
activity_data %>%
  group_by(date) %>%
  summarise(
    #sum = sum(steps, na.rm = T), 
            mean = mean(steps, na.rm = T), 
            median = median(steps, na.rm = T)) -> mean_med
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
mean_med
```

```
## # A tibble: 61 x 3
##    date          mean median
##    <date>       <dbl>  <dbl>
##  1 2012-10-01 NaN         NA
##  2 2012-10-02   0.438      0
##  3 2012-10-03  39.4        0
##  4 2012-10-04  42.1        0
##  5 2012-10-05  46.2        0
##  6 2012-10-06  53.5        0
##  7 2012-10-07  38.2        0
##  8 2012-10-08 NaN         NA
##  9 2012-10-09  44.5        0
## 10 2012-10-10  34.4        0
## # … with 51 more rows
```

```r
# qplot(date, mean, data = mean_med)
#step_means_medians <- 
# qplot(date, steps, data = step_means, geom = "smooth")
```

## What is the average daily activity pattern?  

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
step_interv <- aggregate(steps ~ interval + date, activity_data, mean, na.rm = T)

plot(step_interv$interval, step_interv$steps, type = "l")
```

![](PA1_template_files/figure-html/interval.steps-1.png)<!-- -->

```r
p_avg_activ = ggplot(data = step_interv, 
                     aes(x = interval,
                         y = steps, group = date)) +
              geom_line()
p_avg_activ
```

![](PA1_template_files/figure-html/interval.steps-2.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
step_interv[step_interv$steps == max(step_interv$steps), ]
```

```
##       interval       date steps
## 14476      615 2012-11-27   806
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)  


```r
sapply(activity_data, function(x) sum(is.na(x)))
```

```
##    steps     date interval 
##     2304        0        0
```

```r
# NAs by day:
table(activity_data$date, is.na(activity_data$steps))
```

```
##             
##              FALSE TRUE
##   2012-10-01     0  288
##   2012-10-02   288    0
##   2012-10-03   288    0
##   2012-10-04   288    0
##   2012-10-05   288    0
##   2012-10-06   288    0
##   2012-10-07   288    0
##   2012-10-08     0  288
##   2012-10-09   288    0
##   2012-10-10   288    0
##   2012-10-11   288    0
##   2012-10-12   288    0
##   2012-10-13   288    0
##   2012-10-14   288    0
##   2012-10-15   288    0
##   2012-10-16   288    0
##   2012-10-17   288    0
##   2012-10-18   288    0
##   2012-10-19   288    0
##   2012-10-20   288    0
##   2012-10-21   288    0
##   2012-10-22   288    0
##   2012-10-23   288    0
##   2012-10-24   288    0
##   2012-10-25   288    0
##   2012-10-26   288    0
##   2012-10-27   288    0
##   2012-10-28   288    0
##   2012-10-29   288    0
##   2012-10-30   288    0
##   2012-10-31   288    0
##   2012-11-01     0  288
##   2012-11-02   288    0
##   2012-11-03   288    0
##   2012-11-04     0  288
##   2012-11-05   288    0
##   2012-11-06   288    0
##   2012-11-07   288    0
##   2012-11-08   288    0
##   2012-11-09     0  288
##   2012-11-10     0  288
##   2012-11-11   288    0
##   2012-11-12   288    0
##   2012-11-13   288    0
##   2012-11-14     0  288
##   2012-11-15   288    0
##   2012-11-16   288    0
##   2012-11-17   288    0
##   2012-11-18   288    0
##   2012-11-19   288    0
##   2012-11-20   288    0
##   2012-11-21   288    0
##   2012-11-22   288    0
##   2012-11-23   288    0
##   2012-11-24   288    0
##   2012-11-25   288    0
##   2012-11-26   288    0
##   2012-11-27   288    0
##   2012-11-28   288    0
##   2012-11-29   288    0
##   2012-11-30     0  288
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  


```r
activity_data$step_mean <- ave(activity_data$steps, activity_data$interval, 
                          FUN = function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
# activity_data$step_mean %>% head
sapply(activity_data, function(x) sum(is.na(x)))
```

```
##     steps      date  interval step_mean 
##      2304         0         0         0
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.  


```r
activity_data %>% select(-c("steps")) %>%
  {.} -> imputed_dataset
  
imputed_dataset %>% head
```

```
## # A tibble: 6 x 3
##   date       interval step_mean
##   <date>        <dbl>     <dbl>
## 1 2012-10-01        0    1.72  
## 2 2012-10-01        5    0.340 
## 3 2012-10-01       10    0.132 
## 4 2012-10-01       15    0.151 
## 5 2012-10-01       20    0.0755
## 6 2012-10-01       25    2.09
```

4. Make a histogram of the total number of steps taken each day and 

#```{r new_tot, ref.label=c('step_sums', 'sum_hist')}
#<<step_sums>>
#```


```r
step_sums_imp <- aggregate(step_mean ~ date, imputed_dataset, sum, na.rm = T)
step_sums_imp %>% head(3)
```

```
##         date step_mean
## 1 2012-10-01  10766.19
## 2 2012-10-02    126.00
## 3 2012-10-03  11352.00
```

```r
sum_imp_plot <- qplot(step_sums$steps, geom = "histogram", binwidth = 25000/5) 
print(sum_plot)
```

![](PA1_template_files/figure-html/new_tot-1.png)<!-- -->

Calculate and report the mean and median total number of steps taken per day.


```r
imputed_dataset %>%
  group_by(date) %>%
  summarise(
    sum = sum(step_mean, na.rm = T), 
    mean = mean(step_mean, na.rm = T), 
    median = median(step_mean, na.rm = T)) %T>% head(3) %>%
    {.} -> mean_med_imp
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
#mean_med_imp
```

### Do these values differ from the estimates from the first part of the assignment? 
Yes

### What is the impact of imputing missing data on the estimates of the total daily number of steps?

## Are there differences in activity patterns between weekdays and weekends?

For this part the \color{red}{\verb|weekdays()|}weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.  
2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

