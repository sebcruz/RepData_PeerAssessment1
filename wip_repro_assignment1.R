# Load Libraries
library(ggplot2)

# Set your Present Working Directory (pwd)
# Edit as necessary
setwd("~/GitHub/RepData_PeerAssessment1")

# Download and Unzip Data
setInternet2(use = TRUE) #Makes it work in Knitr
download.file( url="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile="activity.zip", method = "curl")
unzip("activity.zip")

# Import Data
act_raw = read.csv("activity.csv", header = TRUE)

#Clean Data (No NA)
act_clean = act_raw[complete.cases(act_raw), ]

# 
hpc$DateTime = as.POSIXct(paste(hpc$Date, hpc$Time, sep=" "), format="%d/%m/%Y %H:%M:%S")

# Convert Date field to Date datatype
hpc$Date = as.Date(hpc$Date, format="%d/%m/%Y")

# Aggregate by Date
You have a bunch of reading for a number of days
get the total for each day, this gives you a set of numbers that is the total for each day
make a histogram of this set, this is the histrogram of the total steps for each day
calculate the mean of this set
calculate the median of this set

act_clean_sumByDate = aggregate(steps ~ date, data=act_clean, FUN=sum)

# Histogram of Step Sum by Date
qplot(act_clean_sumByDate$steps,
      geom = "histogram",
      fill = I("grey"),
      col = I("white"),
      main = "Histogram of Total Daily Steps",
      xlab = "Daily Steps",
      ylab = "Frequency"
      )

# Calculate and report the mean and median of the total number of steps taken per day
mean(act_clean$steps)
median(act_clean$steps)

#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of 
#steps taken, averaged across all days (y-axis)
# Aggregate by Interval
act_clean_avgByInterval = aggregate(steps ~ interval, data=act_clean, FUN= mean)
qplot(interval, steps, data=act_clean_avgByInterval, 
      geom="line",
      main="Average Steps by Interval")



#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
act_clean_avgByInterval$interval[which.max(act_clean_avgByInterval$steps)]

## Input Missing Values
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(act_raw))

newData <- data 
for (i in 1:nrow(newData)) {
  if (is.na(newData$steps[i])) {
    newData$steps[i] <- avgSteps[which(newData$interval[i] == avgSteps$interval), ]$meanOfSteps
  }
}

head(newData)
sum(is.na(newData))

act_full = act_raw
for (i in 1:nrow(act_raw)) {
  if (is.na(act_full$steps[i])) {
    act_full$steps[i] = act_clean_avgByInterval[which(act_full$interval[i] == act_clean_avgByInterval$interval), ]$steps
  }
}


testdata = merge(act_raw, act_clean_avgByInterval, by="interval", all.x=TRUE)
testdata$modSteps = ifelse(is.na(testdata$steps.x), testdata$steps.y, testdata$steps.x)

table$size1<-ifelse(table$population<500,1,
                    ifelse(table$population>=500 & table$population<1000,2,
                           ifelse(table$population>=1000 & table$population<2000,3,
                                  ifelse(table$population>=2000 & table$population<3000,4,5
                                  ))))

testdata[, c(1, 3, 5)]


#Left Join act_clean_avgByInterval (Avg Steps by Interval, No NAs) to act_raw (has NAs)
act_full = merge(act_raw, act_clean_avgByInterval, by="interval", all.x=TRUE)
# Create new column modSteps that uses avg steps for those intervals that are NA
act_full$modSteps = ifelse(is.na(act_full$steps.x), act_full$steps.y, act_full$steps.x)

# Means Compared
mean(act_clean$steps) - mean(act_full$modSteps)

# Medians Compared
median(act_clean$steps) - median(act_full$modSteps)


#Weekend/Weekday
testdata$DayType = ifelse(weekdays(as.Date(testdata$date)) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

aggregate(steps ~ interval, data = activity, subset = activity$daytype == type, FUN = mean)
act_full_avgByIntervalByDayType = aggregate(modSteps ~ interval + DayType, data=act_full, FUN=mean)

ggplot(data=act_full_avgByIntervalByDayType, aes(x=interval, y=modSteps)) + 
  geom_line() + 
  facet_grid(DayType~.)


#png saves
png("hist.png")
qplot(act_clean_sumByDate$steps,
      geom = "histogram",
      fill = I("grey"),
      col = I("white"),
      main = "Histogram of Total Daily Steps",
      xlab = "Daily Steps",
      ylab = "Frequency"
)
dev.off()

png("avgStepsByInterval.png")
qplot(interval, steps, data=act_clean_avgByInterval, 
      geom="line",
      main="Average Steps by Interval")
dev.off()

png("modHist.png")
qplot(act_full_sumByDate$modSteps,
      geom = "histogram",
      fill = I("grey"),
      col = I("white"),
      main = "Histogram of Total Daily modSteps",
      xlab = "Daily modSteps (NAs replaced with Interval Means)",
      ylab = "Frequency"
)
dev.off()

png("panel.png")
ggplot(data=act_full_avgByIntervalByDayType, aes(x=interval, y=modSteps)) + 
  geom_line() + 
  facet_grid(DayType~.) +
  ggtitle("Average modSteps by Interval by DayType") +
  ylab("modSteps (NAs replaced with Interval Means)")
dev.off()




