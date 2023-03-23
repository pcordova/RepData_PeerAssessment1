library(dplyr)
library(ggplot2)

unzip("activity.zip")
activ <- read.csv("activity.csv")
str(activ)

pasosDia <- tapply(activ$steps, activ$date, sum, na.rm = TRUE) |>
                cbind(unique(activ$date)) |>
                as.data.frame()

colnames(pasosDia) <- c("Steps", "Dates")

pasosDia <- mutate(pasosDia, Steps = as.numeric(Steps), Dates = as.Date(Dates))

ggplot(pasosDia) +
    aes(Dates, Steps) +
    geom_bar(stat = "identity", colour = '#63ADCA', fill = '#337995') +
    labs(title = "Total number of steps taken each day",
         caption = "October-November 2012") +
    theme(plot.title = element_text(size = 22, colour = '#092733'),
          plot.subtitle = element_text(size = 18),
          axis.text=element_text(size=10),
          axis.title=element_text(size=14))

cat(" The mean of steps taken per day is:",
    mean(tapply(activ$steps, activ$date, sum), na.rm = TRUE),
    "\n\n And the median of steps taken per day is:",
    median(tapply(activ$steps, activ$date, sum), na.rm = TRUE))

## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
## and the average number of steps taken, averaged across all days (y-axis)
intvs <- tapply(activ$steps, activ$interval, mean, na.rm = TRUE) |>
                cbind(unique(activ$interval)) |>
                as.data.frame()

colnames(intvs) <- c("Steps", "Intervals")

ggplot(intvs) +
    aes(Intervals, Steps) +
    geom_line(linewidth = 1.2, colour = '#4079bf') +
    labs(title = "Average steps per each 5-minute interval",
         caption = "October-November 2012") +
    theme(plot.title = element_text(size = 22, colour = '#19304d'),
          plot.subtitle = element_text(size = 18),
          axis.text=element_text(size=10),
          axis.title=element_text(size=14))

cat(" The maximum number of recorded steps is:",
    max(intvs$Steps, na.rm = TRUE),
    "\n\n And the interval containing that maximum value is:",
    intvs$Intervals[intvs$Steps == max(intvs$Steps, na.rm = TRUE)]
)

## Imputing missing values
NAs <- is.na(activ$steps)
qNAs <- length(activ$steps[NAs])

cat("The total number of missing values in the dataset is:",
    qNAs, paste0("(",scales::percent(qNAs/nrow(activ)),")")
)

activ2 <- activ

for (i in c(1:nrow(activ2))) {
    if(is.na(activ2$steps[i])) {
        activ2$steps[i] <- intvs$Steps[intvs$Intervals==activ2$interval[i]]
    }
}
rm(i)

pasos2 <- tapply(activ2$steps, activ2$date, sum) |>
                cbind(unique(activ2$date)) |>
                as.data.frame()

colnames(pasos2) <- c("Steps", "Dates")

pasos2 <- mutate(pasos2, Steps = as.numeric(Steps), Dates = as.Date(Dates))

ggplot(pasos2) +
    aes(Dates, Steps) +
    geom_bar(stat = "identity", colour = '#63ADCA', fill = '#337995') +
    labs(title = "Total number of steps taken each day (imputed values)",
         caption = "October-November 2012") +
    theme(plot.title = element_text(size = 22, colour = '#092733'),
          plot.subtitle = element_text(size = 18),
          axis.text=element_text(size=10),
          axis.title=element_text(size=14))


cat("The mean of steps taken per day (imputed values) is:",
    mean(pasos2$Steps),
    "\n\n And the median of steps taken per day (imputed values) is:",
    median(pasos2$Steps)
)


# activ2 <- tapply(activ2$steps, activ2$interval, mean) |>
#                cbind(unique(activ2$interval), as.Date(unique(activ2$date))) |>
#               as.data.frame()


activ2 <- mutate(activ2, date = as.Date(date))

for (i in c(1:nrow(activ2))) {
    if(weekdays(activ2$date[i]) %in% c("Saturday", "Sunday")) {
        activ2$Days[i] <- "Weekend"
    } else(activ2$Days[i] <- "Weekday")
}

activ3 <- aggregate(activ2$steps, by=list(activ2$interval, activ2$Days), FUN = mean)
colnames(activ3) <- c("Intervals", "Days","Steps")

ggplot(activ3) +
    aes(Intervals, Steps) +
    geom_line(linewidth = 1.2, colour = '#4079bf') +
    labs(title = "Average steps per each 5-minute interval",
         caption = "October-November 2012") +
    theme(plot.title = element_text(size = 22, colour = '#19304d'),
          plot.subtitle = element_text(size = 18),
          axis.text=element_text(size=10),
          axis.title=element_text(size=14)) +
    facet_grid(rows = vars(Days))


          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)


str(activ2)
days <- weekdays(as.Date(activ$date))
days61 <- weekdays(as.Date(unique(activ$date)))
dates61 <- as.Date(unique(activ$date))


# Paleta de colores:
 #DDEDF3
 #63ADCA
 #337995
 #193B48
 #092733

