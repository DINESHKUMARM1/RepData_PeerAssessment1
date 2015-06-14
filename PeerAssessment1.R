
##Code for Reading the Dataset
connection <- unz("./repdata-data-activity.zip",filename = "activity.csv")
activity <- read.csv(file = connection,header = T)

summary(activity)

##Formating the Data and adding new Column
activity$dt <- as.POSIXct(with(activity,paste(date,paste(interval %/% 100, interval %% 100, sep=":"))),format="%Y-%m-%d %H:%M",tz="")

##Necessary Libraries for plot
library(knitr)
library(dplyr)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)

### Main Section - What is mean total number of steps taken per day?

##Question 1-  Calculate the total number of steps taken per day

stepsPerDay <- setNames(
    aggregate(
        steps~as.Date(date),
        activity,
        sum,
        na.rm = TRUE),
    c("date","steps")
)


##Question 2- Make a histogram of the total number of steps taken each day

ggplot(stepsPerDay,aes(x=date,y=steps)) + 
    geom_bar(stat="identity") + 
    ggtitle("Total number of steps per day")



##Question 3- Calculate and report the mean and median of the total number of steps taken per day


meanN <- c(mean = mean(stepsPerDay$steps),median = median(stepsPerDay$steps))

meanN

###Main Section- What is the average daily activity pattern?

##Question 1- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


avgPattern <- aggregate(steps~interval,activity,mean,na.rm = TRUE)

avgPattern$time <- as.POSIXct(with(avgPattern,paste(interval %/% 100, interval %% 100, sep=":")),format="%H:%M")

#Plotting 
ggplot(avgPattern,aes(x=time,y=steps)) + 
    geom_line() + 
    scale_x_datetime(breaks = date_breaks("2 hour"),labels = date_format("%H:%M"))

## Question 2- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

with(avgPattern,avgPattern[steps == max(steps),])


###Main Section - Imputing missing values

## Question 1- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


sum(is.na(activity))

## Question 2- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

data_imputed <- activity
for (i in 1:nrow(data_imputed)) {
    if (is.na(data_imputed$steps[i])) {
        interval_value <- data_imputed$interval[i]
        steps_value <- avgPattern[
            avgPattern$interval == interval_value,]
        data_imputed$steps[i] <- steps_value$steps
    }
}
##View(activity)
##View(data_imputed)

##Question 3- Create a new dataset that is equal to the original dataset but with the missing data filled in.

table_date_steps_by_day <- aggregate(steps ~ date, data_imputed, sum)
head(table_date_steps_by_day)


## Question 3- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

hist(table_date_steps_by_day$steps, col=1, main="(Imputed) Histogram of total number of steps per day", xlab="Total number of steps in a day")

# get mean and median of total number of steps per day

meanN2 <- c(mean = mean(table_date_steps_by_day$steps),median = median(table_date_steps_by_day$steps))

comparison <- rbind(source = meanN, fixed = meanN2, delta = meanN2-meanN)
comparison

###Main Section - Are there differences in activity patterns between weekdays and weekends?

##Question 1- Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day

data_imputed['type_of_day'] <- weekdays(as.Date(data_imputed$date))
data_imputed$type_of_day[data_imputed$type_of_day  %in% c('Saturday','Sunday') ] <- "weekend"
data_imputed$type_of_day[data_imputed$type_of_day != "weekend"] <- "weekday"


# convert type_of_day from character to factor
data_imputed$type_of_day <- as.factor(data_imputed$type_of_day)

# calculate average steps by interval across all days
data_imputed_steps_by_interval <- aggregate(steps ~ interval + type_of_day, data_imputed, mean)

# creat a plot
qplot(interval, 
      steps, 
      data = data_imputed_steps_by_interval, 
      type = 'l', 
      geom=c("line"),
      xlab = "Interval", 
      ylab = "Number of steps", 
      main = "") +
    facet_wrap(~ type_of_day, ncol = 1)


x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
n <- length(y)

beta1 <- cor(y, x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
yhat <- beta0 + beta1 * x
e <- y - yhat        # residuals
sigma <- sqrt(sum(e^2) / (n - 2))
ssx <- sum((x - mean(x))^2)
seBeta0 <- (1 / n + mean(x) ^ 2 / ssx) ^ .5 * sigma
seBeta1 <- sigma / sqrt(ssx)
tBeta0 <- beta0 / seBeta0; tBeta1 <- beta1 / seBeta1
pBeta0 <- 2 * pt(abs(tBeta0), df = n - 2, lower.tail = FALSE)

pBeta1 <- 2 * pt(abs(tBeta1), df = n - 2, lower.tail = FALSE)

sigma



rm(list=ls()) # clean environment

data(mtcars)
fit <- lm(mtcars$mpg ~ mtcars$wt)
sumCoef <- summary(fit)$coefficients
sumCoef[1,1] + c(-1,1) * qt(.975, df = fit$df) * sumCoef[1, 2]
sumCoef[2,1] + c(-1,1) * qt(.975, df = fit$df) * sumCoef[2, 2]


?mtcars
