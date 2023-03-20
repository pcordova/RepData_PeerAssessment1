
unzip("activity.zip")
activ <- read.csv("activity.csv")
str(activ)

pasos <- tapply(activ$steps, activ$date, sum, na.rm = TRUE) |>
                cbind(unique(activ$date)) |>
                as.data.frame()

colnames(pasos) <- c("Steps", "Dates")

pasos <- mutate(pasos, Steps = as.integer(Steps)) |>
                mutate(Dates = as.Date(Dates))

ggplot(pasos) +
    aes(Dates, Steps) +
    geom_bar(stat = "identity", colour = '#4f84c4', fill = '#4079bf') +
    labs(title = "Total number of steps taken each day",
         caption = "October-November 2012") +
    theme(plot.title = element_text(size = 22, colour = '#19304d'),
          plot.subtitle = element_text(size = 18),
          axis.text=element_text(size=10),
          axis.title=element_text(size=14))



cat("- The **mean** of steps taken per day is:",
    mean(pasos$Steps, na.rm = TRUE),
    "\n\n- And the **median** of steps taken per day is:",
    median(pasos$Steps, na.rm = TRUE))



#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
#and the average number of steps taken, averaged across all days (y-axis)
intvs <- tapply(activ$steps, activ$interval, mean, na.rm = TRUE) |>
                cbind(unique(activ$interval)) |>
                as.data.frame()

colnames(intvs) <- c("Steps", "Intervals")

intvs <- mutate(intvs, Steps = as.integer(Steps))# |>
            # mutate(Dates = as.Date(Dates))

ggplot(intvs) +
    aes(Intervals, Steps) +
    geom_line(size = 1.2, colour = '#4079bf') +
    labs(title = "Average steps per each 5-minute interval",
         caption = "October-November 2012") +
    theme(plot.title = element_text(size = 22, colour = '#19304d'),
          plot.subtitle = element_text(size = 18),
          axis.text=element_text(size=10),
          axis.title=element_text(size=14))

cat("- The maximum number of recorded steps is:",
    max(intvs$Steps, na.rm = TRUE),
    "\n\n- And the interval containing that maximum value is:",
    intvs$Intervals[intvs$Steps == max(intvs$Steps, na.rm = TRUE)])





mean(activ$steps, na.rm = TRUE)
activ <- mutate(activ, date = as.Date(date, format = "%Y-%m-%d"))
unique(activ$date)
days <- weekdays(as.Date(unique(activ$date)))
days <- weekdays(as.Date(activ$date))
steps <- tapply(activ$steps, activ$date, sum, na.rm = TRUE)
dates <- as.Date(unique(activ$date), format = "%Y-%m-%d")





