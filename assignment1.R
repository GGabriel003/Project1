library(ggplot2)
activityData <- read.csv(file="activity.csv", header=TRUE)
#activityData
# calcular todos pasos por cada dia
totalSteps <- aggregate(steps ~ date, activityData, FUN=sum)
png(filename="plot1.png",width=480, height=480)
hist(totalSteps$steps,main = "Total Steps per Day",xlab = "Number of Steps")
dev.off()
# todos los pasos y la media del paso por dias
meanSteps <- mean(totalSteps$steps, na.rm = TRUE)
medSteps <- median(totalSteps$steps, na.rm = TRUE)
meanSteps
#10766.19
medSteps
#10765

png(filename="plot2.png",width=480, height=480)
meanStepsByInt <- aggregate(steps ~ interval, activityData, mean)
ggplot(data = meanStepsByInt, aes(x = interval, y = steps)) +
  geom_line() +
  ggtitle("Average Daily Activity Pattern") +
  xlab("5-minute Interval") +
  ylab("Average Number of Steps") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

# 5 minutos en todos los días contiene el número máximo de pasos
maxInt <- meanStepsByInt[which.max(meanStepsByInt$steps),]
missingVals <- is.na(activityData$steps)
#todos los valores que faltan
#nuevo conjunto de datos que sea igual al conjunto de datos original
imp_activityData <- transform(activityData,
                              steps = ifelse(is.na(activityData$steps),
                                             meanStepsByInt$steps[match(activityData$interval, 
                                                                        meanStepsByInt$interval)],
                                             activityData$steps))

##un histograma del número total de pasos dados cada día e informe la media y la mediana.
png(filename="plot3.png",width=480, height=480)
impStepsByInt <- aggregate(steps ~ date, imp_activityData, FUN=sum)
hist(impStepsByInt$steps,
     main = "Imputed Number of Steps Per Day",
     xlab = "Number of Steps")
impMeanSteps <- mean(impStepsByInt$steps, na.rm = TRUE)
impMedSteps <- median(impStepsByInt$steps, na.rm = TRUE)
diffMean = impMeanSteps - meanSteps
diffMed = impMedSteps - medSteps
diffTotal = sum(impStepsByInt$steps) - sum(totalSteps$steps)
dev.off()
diffMean
#0
diffMed
#1.188679
diffTotal
#86129.51

# "weekend" and "weekday"
png(filename="plot4.png",width=480, height=480)
DayType <- function(date) {
  day <- weekdays(date)
  if (day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
    return ("weekeday")
  else if (day %in% c('Saturday', 'Sunday'))
    return ("weekend")
  else
    stop ("Invalid Date Format.")
}
imp_activityData$date <- as.Date(imp_activityData$date)
imp_activityData$day <- sapply(imp_activityData$date, FUN = DayType)

# un gráfico contiene un serie de tiempo del intervalo de 5 minutos y el num promedio 
meanStepsByDay <- aggregate(steps ~ interval + day, imp_activityData, mean)
ggplot(data = meanStepsByDay, aes(x = interval, y = steps)) + 
  geom_line() +
  facet_grid(day ~ .) +
  ggtitle("Average Daily Activity Pattern") +
  xlab("5-minute Interval") +
  ylab("Average Number of Steps") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
