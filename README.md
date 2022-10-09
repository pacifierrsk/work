# work
activity.tsteps<- with(activity, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))

names(activity.tsteps)<- c("dates", "steps")

hist(activity.tsteps$steps, main = "Total number of steps taken per day", xlab = "Total steps taken per day", col = "darkblue", ylim = c(0,20), breaks = seq(0,25000, by=2500))
Mean number of steps taken per day

mean(activity.tsteps$steps)
## [1] 9354.23
Median number of steps taken per day

median(activity.tsteps$steps)
## [1] 10395
2. What is the average daily activity pattern?
Time series plot (type=“l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

average.daily.activity<- aggregate(activity$steps, by= list(activity$interval), FUN = mean , na.rm = TRUE)

names(average.daily.activity)<-c("interval", "mean")

plot(average.daily.activity$interval, average.daily.activity$mean, type = "l", xlab = "Interval", ylab = "Average number of steps", main = "Average number of steps per interval")
average.daily.activity[which.max(average.daily.activity$mean),]$interval
## [1] 835
here are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

sum(is.na(activity$steps))
## [1] 2304
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

clean.steps<- average.daily.activity$mean[match(activity$interval,average.daily.activity$interval)]
Create a new dataset that is equal to the original dataset but with the missing data filled in.

activity.clean <- transform(activity, steps = ifelse(is.na(activity$steps), yes = clean.steps, no = activity$steps))

total.clean.steps<- aggregate(steps ~ date, activity.clean, sum)

names(total.clean.steps)<- c("date", "daily.steps")
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

hist(total.clean.steps$daily.steps, col = "darkblue", xlab = "Total steps per day", ylim = c(0,30), main = "Total number of steps taken each day", breaks = seq(0,25000,by=2500))
Mean of the total number of steps taken per day

mean(total.clean.steps$daily.steps)
## [1] 10766.19
Median of the total number of steps taken per day

median(total.clean.steps$daily.steps)
## [1] 10766.19
activity$datetype <- sapply(activity$date, function(x) {
        if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday") 
                {y <- "Weekend"} else 
                {y <- "Weekday"}
                y
        })
A panel plot containing a time series plot (type=“l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

activity.datetype<- aggregate(steps~interval+datetype, activity,mean, na.rm =TRUE)
ggplot(activity.datetype, aes(x = interval, y = steps, color = datetype))+ geom_line() + labs(title = "Average daily steps by date type", x = "Interval", y = "Average number of steps") + facet_wrap(~datetype, ncol = 1, nrow = 2) 
