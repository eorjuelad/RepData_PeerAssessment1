library(dplyr)
library(ggplot2)
library(lubridate)
library(gridExtra)

if (!file.exists('activity.csv')) {
        unzip(zipfile = "activity.zip")
}

dataSet <- read.csv("activity.csv", header = TRUE, na.strings = "NA")
dim(dataSet)
head(dataSet)

compCases <- complete.cases(dataSet)
dataSetCompleteCases <- dataSet[compCases,]
View(dataSetCompleteCases)

## 2. Histogram of the total number of steps taken each day
totalStepsByDay <- aggregate(steps ~ date, FUN = sum, data = dataSetCompleteCases)
hist(totalStepsByDay$steps, main = "Total number of stepsby day", xlab = "Steps")

## 3. Mean and median number of steps taken each day
meanSteps <- mean(totalStepsByDay$steps)
print(meanSteps)
medianSteps <- median(totalStepsByDay$steps)
print(medianSteps)

## 4. Time series plot of the average number of steps taken
timeSeries <- aggregate(steps ~ interval, FUN = mean, data = dataSetCompleteCases)
g <- ggplot(timeSeries, aes(x = interval, y = steps))
g <- g + geom_line() 
g <- g + labs(y = "Steps")
g <- g + labs(x = "Interval")
g <- g + labs(title = "Average number of steps")
g

## 5. The 5-minute interval that, on average, contains the maximum number of steps

filter(timeSeries, steps == max(steps))

## 6. Code to describe and show a strategy for imputing missing data
sum(is.na(as.character(dataSet$steps)))
sum(is.na(as.character(dataSet$date)))
sum(is.na(as.character(dataSet$interval)))

incompCases <- complete.cases(dataSet)
dataSetIncompleteCases <- dataSet[!incompCases,]
totalStepsByDayImpute <- aggregate(steps ~ interval, FUN = mean, data = dataSetCompleteCases)

for(i in 1:nrow(dataSet)){
        if(is.na(dataSet$steps[i])){
                valueAux <- filter(totalStepsByDayImpute, totalStepsByDayImpute$interval == dataSet$interval[i])
                dataSet$steps[i] <- valueAux$steps
        }
}


## 7. Histogram of the total number of steps taken each day after missing values are imputed
imputedDataset <- dataSet[!incompCases,]
totalStepsByDayImputed <- aggregate(steps ~ date, FUN = sum, data = imputedDataset)
hist(totalStepsByDay$steps, main = "Total number of steps by day Imputed", xlab = "Steps")
totalStepsByDayImputed

## 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
timeSeriesWeek <- mutate(dataSetCompleteCases, day = ifelse(wday(date) %in% c(2,3,4,5,6) ,"Weekday","Weekend"))

timeSeriesWD <- aggregate(steps ~ interval+day, FUN = mean, data = timeSeriesWeek)
timeSeriesWE <- aggregate(steps ~ interval, FUN = mean, data = timeSeriesWeekend)

g <- ggplot(timeSeriesWD, aes(x = interval, y = steps))
g <- g + geom_line() 
g <- g + labs(y = "Steps")
g <- g + labs(x = "Interval")
g <- g + facet_grid(day ~ .)
g


