Loading and preprocessing the data
----------------------------------

First, let’s read the CSV file into memory, assuming the activity.csv
file is in your current working directory

    actdata <- read.csv("activity.csv", header=T, sep=",")

and take a look at it, including performing some basic checks: number of
rows, variables and complete rows:

    head(actdata)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

    str(actdata)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

    nrows <- nrow(actdata)
    ncols <- ncol(actdata)

The number of rows in the activity data set is 17568 over 3 variables.

We convert the date from level to character.

    actdata$date <- as.character(actdata$date)

Usually, the check and management of missing values would happen at the
start of the file. However, since it is an explicit task in the
assignment description at a later stage, this preparatory step is
postponed (see below).

What is mean total number of steps taken per day?
-------------------------------------------------

First, aggregate the steps taken by day and calculate its mean and
median

    actdata.stepsDay <- aggregate(actdata$steps, by=list(actdata$date), FUN=sum, na.rm=F)
    actdata.stepsDayMean <- mean(actdata.stepsDay$x, na.rm=T)
    actdata.stepsDayMedian <- median(actdata.stepsDay$x, na.rm=T)

The mean of number of steps taken per day is 10766. The median of the
total number of steps taken per day is 10765.

A histogram of the total number of steps taken each day is shown below:

    hist(actdata.stepsDay$x, 
         breaks=seq(from=0, to=25000, by=2500),
         main="Total number of steps taken each day", 
         col="blue", 
         xlab="Total Steps taken")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

What is the average daily activity pattern?
-------------------------------------------

1.  Make a time series plot (i.e. type = “l”) of the 5-minute interval
    (x-axis) and the average number of steps taken, averaged across all
    days (y-axis). In other words: what is the average number of steps
    taken for each intervall across all days (note that intervall IDs
    repeat every 24h).

<!-- -->

    actdata.stepsInterval <- aggregate(actdata$steps, by=list(actdata$interval), FUN=mean, na.rm=T)
    plot( x=actdata.stepsInterval[,1], 
          y=actdata.stepsInterval[,2], 
          type="l",
          col="red",
          main="Time-series of the average number of steps per intervals",
          ylab="Average number of steps", 
          xlab="Interval (in minutes)")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

1.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

<!-- -->

    maxinterval <- actdata.stepsInterval[which.max(actdata.stepsInterval[,2]),1]

The 5 -minute interval with the maxium number of steps across all days
is 835.

Imputing missing values
-----------------------

1.  Calculate and report the total number of missing values in the
    dataset (i.e. the total number of rows with NA’s)

<!-- -->

    nas   <- sum(is.na(actdata))
    compl <- sum(complete.cases(actdata))

The number of NA’s is 2304.

1.  Devise a strategy for filling in all of the missing values in the
    dataset. The strategy does not need to be sophisticated. For
    example, you could use the mean/median for that day, or the mean for
    that 5-minute interval, etc

Strategy: For filling in the missing values, we match Intervall IDs and
use the mean steps across all days for a given interval to populate the
NAs in the newly created clean dataframe:

    actdata.clean <- cbind(actdata, actdata.stepsInterval[,2])
    names(actdata.clean)[4] <- c("mean")
    actdata.clean$steps <- ifelse( is.na(actdata.clean$steps), actdata.clean$mean, actdata.clean$steps)

On the new dataset, calculate again mean and median of total number of
steps taken each day:

    actdata.clean.stepsDay <- aggregate(actdata.clean$steps, by=list(actdata.clean$date), FUN=sum)
    actdata.clean.stepsDayMean <- mean(actdata.clean.stepsDay$x)
    actdata.clean.stepsDayMedian <- median(actdata.clean.stepsDay$x,)

The mean of steps taken per day for the cleaned dataset is
1.076618910^{4}. The median of steps taken per day is 1.076618910^{4}.

Now we can also draw the total step count for each day as historgram:

    hist(actdata.clean.stepsDay$x, 
          breaks=seq(from=0, to=25000, by=2500),
         main="Total number of steps taken per day", 
         col="red", 
         xlab="Total number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)

There are no big differences between the mean and median of the two
datasets. The impact of imputing the missing values is to have more
data, hence to obtain a bigger mean and median value.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

    actdata.clean$date <- strptime(actdata.clean$date, "%Y-%m-%d")
    actdata.clean$weekend <- (weekdays(actdata.clean$date) %in% c("Sunday", "Saturday"))
    actdata.weekend <- actdata.clean[actdata.clean$weekend == TRUE,]
    actdata.weekday <- actdata.clean[actdata.clean$weekend == FALSE,]

    actdata.weekend.steps <- aggregate(actdata.weekend$steps, by=list(actdata.weekend$interval), FUN=mean)
    actdata.weekday.steps <- aggregate(actdata.weekday$steps, by=list(actdata.weekday$interval), FUN=mean)
    par(mfrow=c(2,1))
    plot( x=actdata.weekend.steps[,1], 
          y=actdata.weekend.steps[,2], 
          type="l",
          col=124,
          main="Weekend average steps per interval",
          ylab="Average steps", 
          xlab="Interval ID")
    plot( x=actdata.weekday.steps[,1], 
          y=actdata.weekday.steps[,2], 
          type="l",
          col=554,
          main="Weekday average steps per interval",
          ylab="Average steps", 
          xlab="Interval ID")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-13-1.png)
