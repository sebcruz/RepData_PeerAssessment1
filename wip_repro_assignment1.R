# Load Libraries
library(ggplot2)

# Set your Present Working Directory (pwd)
# Edit as necessary
setwd("~/GitHub/RepData_PeerAssessment1")

# Download and Unzip Data
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
act_clean_sumByDate = aggregate(steps ~ date, data=act_clean, FUN=sum)
# Histogram of Step Sum by Date
qplot(act_clean_sumByDate$steps,
      geom = "histogram",
      fill = I("grey"),
      col = I("white"),
      main = "Histogram of Daily Steps",
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
