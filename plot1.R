library(dplyr)
library(ggplot2)

idat <- read.csv("activity.csv")
tab <- summarise(group_by(idat,date), Steps = sum(steps))
tab <- tab[!is.na(tab$Steps),]
##FIRST PLOT
hist(tab$Steps, ylim = c(0,30), xlab = "TOTAL NUMBER OF STEPS", main = "HISTOGRAM OF TOTAL NUMBER OF STEPS TAKEN EACH DAY", col = "purple")
mean.steps <- mean(tab$Steps)
median.steps <- median(tab$Steps)

##SECOND PLOT
tabint <- summarise(group_by(idat,interval), Mean = mean(steps,na.rm = TRUE))
plot(tabint$interval, tabint$Mean, type = "l", ylab= "AVERAGE NUMBER OF STEPS ", xlab = "5-MINUTE INTERVALS", main = "AVERAGE DAILY ACTIVITY PATTERN", col = "red")

tabint[tabint$Mean == max(tabint$Mean),]

##Missing Values
Tot.na <- sum(is.na(idat$steps))


##IMPUTING
newdat <- idat[is.na(idat$steps),]
newdat$steps[] <- tabint$Mean[]
nonnadat <- idat[!is.na(idat$steps),]##new data set
fdat <- rbind(newdat,nonnadat)
sumdat <- summarise(group_by(fdat,date),Steps = sum(steps))

##HISTOGRAM AFTER IMPUTING
hist(sumdat$Steps, ylim = c(0,30), xlab = "TOTAL NUMBER OF STEPS", main = "HISTOGRAM OF TOTAL NUMBER OF STEPS TAKEN EACH DAY\n REPLACING NA VALUES BY INTERVAL MEAN", col = "purple")

mean(sumdat$Steps)
median(sumdat$Steps)

##WEEKDAY
fdat$date <- as.Date(fdat$date)

fdat$day <- weekdays(fdat$date)

fdat$type <- ifelse(fdat$day == "Saturday" | fdat$day == "Sunday", "Weekend","Weekday")
grouped <- summarise(group_by(fdat,interval,type),Mean = mean(steps))
g <- ggplot(grouped, aes(interval,Mean))
g+ geom_line() + facet_grid(.~type) + ggtitle("AVERAGE DAILY STEPS BY TYPE OF DAY")






