## Load the data
df <- read.csv("activity.csv", header = TRUE)

## Inspect the raw data
summary(df)

## Transform the column data is of a usable format, i.e steps column is numeric,
## date colum is date format and the interval colum is numeric.
df$steps <- as.numeric(df$steps)
df$date <- as.character(df$date)
df$interval <- as.numeric(df$interval)

## Convert the date colum to the appropriately formatted date
df$date <- as.Date(df$date, format = "%Y-%m-%d")

## Summarize the number of steps by day
sumByDate <- tapply(df$steps, df$date, sum)

## Get the Mean of the number of steps per day
meanByDate <- mean(sumByDate, na.rm = TRUE)

## Get the Median of the numbner of steps per day
medianByDate <- median(sumByDate, na.rm = TRUE)

## Plot the histogram showing the number steps taken per day.
hist(sumByDate, xlab = "No. steps taken per day", col = "red",
     main = "Histogram depicting the number of steps taken per day")

## Summarize the number of steps per interval and the mean
## sumByInterval <- tapply(df$steps, df$interval, sum, na.rm = TRUE)
meanByInterval <- tapply(df$steps, df$interval, mean, na.rm = TRUE)

## Plot the pattern of daily activity
plot(unique(df$interval), meanByInterval, type = "l", col = "red", 
     xlab = "5-minute intervals", ylab = "Average no. steps taken for all days")

## Find the interval containing the maximum number of steps on average
interval <- df[which.max(meanByInterval), 3]

## Confirm the total number of missing values (NA) in the data set
totalMissing <- sum(is.na(df))
