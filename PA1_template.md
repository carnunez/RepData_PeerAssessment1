---
# "Reproducible Research: Peer Assessment 1"
---

## Global setting
```{r setoptions}
echo = TRUE  # Always make code visible
```

## Loading and preprocessing the data

Download, extract and store the data in the mydata data frame. Also transform the date field to Date format and omit NAs.

```{r warning = FALSE}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="DataActivity.zip", method="curl")
unzip("DataActivity.zip")
mydata <- na.omit(read.csv("./activity.csv", colClasses = c("integer", "Date", "integer")))
```

## What is mean total number of steps taken per day?

A histogram of the total number of steps taken each day is calculated and shown below:
```{r warning = FALSE}
library("ggplot2")

ggplot(mydata, aes(date, steps)) + 
  geom_bar(stat="identity", fill="steelblue", colour="lightblue") + 
  labs(x="Date") + 
  labs(y="Total number of steps") + 
  labs(title="Histogram of Total number of steps taken each day")
```

Calculation of the mean and median total number of steps taken per day:

```{r message = FALSE}
library(dplyr)
summary <- summarize(group_by(mydata,date),mean=mean(steps), median=median(steps))
```

## What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
stats<-summarize(group_by(mydata,interval),mean=mean(steps))
ggplot(stats, aes(interval, mean)) + 
  geom_line(color="red") + 
  labs(x="5-minute intervals") + 
  labs(y="Average Number of Steps Taken") + 
  labs(title="Time Series Plot of the 5-minute Interval")
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r warning = FALSE}
stats[stats$mean == max(stats$mean), ]
```

## Imputing missing values

The total number of rows with NAs:

```{r}
sum(is.na(read.csv("./activity.csv", colClasses = c("integer", "Date", "integer"))))
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
* My strategy is to use the mean for that 5-minute interval to fill each NA value in the steps column.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
mydata_w_na <- read.csv("./activity.csv", colClasses = c("integer", "Date", "integer"))
myfixed <- merge(mydata_w_na, stats, by="interval", type="inner")
myfixed$new_steps <- ifelse(is.na(myfixed$steps), myfixed$mean, myfixed$steps)
myfixed <- select(myfixed, date, interval, new_steps)
sum(is.na(myfixed)) #Check if NAs got eliminated
ggplot(myfixed, aes(date, new_steps)) + 
  geom_bar(stat="identity", fill="steelblue", colour="lightblue") + 
  labs(x="Date") + 
  labs(y="Total number of steps") + 
  labs(title="Histogram of Total number of steps taken each day (no NAs)")
```

Calculate and report the mean and media values. We can see that the values differ from the initial calculation, and have higher value since the NAs have been replaced by the 5-minute interval mean.

```{r}
summary_new <- summarize(group_by(myfixed,date),mean=mean(new_steps), median=median(new_steps))
```

## Are there differences in activity patterns between weekdays and weekends?

Append "Weekend" or "Weekday" to a new field, conditionally.

```{r}
myfixed$day<-as.factor(ifelse(weekdays(myfixed$date) %in% c("Saturday","Sunday"),"Weekend","Weekday"))
```

Plot the 5-minute average of steps, by weekday/weekend.

```{r}
summary_day <- summarize(group_by(myfixed,day,interval), mean=mean(new_steps))
library(lattice)
xyplot(mean~interval | day, data=summary_day, layout=c(1, 2), type="l", xlab="Interval", ylab="Number of steps", group=day)
```
