library("r2d3", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("scales", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library(ggridges)
library(viridis)

Sleep2018 <- read.csv("~/Desktop/fitbitsleep/FitbitSleep/Sleep2018.csv", na.strings="N/A")
TBSSleep2018 <- read.csv("~/Desktop/fitbitsleep/FitbitSleep/TBSSleep2018.csv", na.strings="N/A")
Sleep2018 <- TBSSleep2018

Sleep2018$Date <- as.POSIXct(Sleep2018$DayofWeek, format="%m/%d/%y")
Sleep2018$SleepStart <- strptime(Sleep2018$StartTime, format="%Y-%m-%d %I:%M%p")
Sleep2018$SleepEnd <- strptime(Sleep2018$EndTime, format="%Y-%m-%d %I:%M%p")
Sleep2018$Wake <- substr(as.POSIXct(Sleep2018$SleepEnd, format="%H:%M"), 11, 16)
attach(Sleep2018)
Sleep2018[is.na(Sleep2018)] <- 0

plot(SleepStart, MinutesAsleep/60, type ="l", xlab = "Date", ylab = "Hours of Sleep")
            title(main ="Sleep Duration, 2018")

#bedtimeFreq <- table(format(Sleep2018$SleepStart, "%H"))
#bedtimeFreq <- c(bedtimeFreq[length(bedtimeFreq)], bedtimeFreq)
#bedtimeFreq <- bedtimeFreq[-length(bedtimeFreq)]
#timeLabels <- factor(names(bedtimeFreq), levels = names(bedtimeFreq))
#bedtime <- data.frame(Count = bedtimeFreq, Time = timeLabels)
#ggplot(bedtime, aes(Time, Count)) +
#  geom_point(col = "#1f77b4", size = 4) +
#  xlab("Bedtime Hour (24-hour clock)") +
#  ggtitle("Frequency of Bedtime Hours") +
#  theme_bw()

bedtimeFreq <- Frequency2
g <- ggplot(bedtimeFreq, aes(Hour, Frequency)) +
  geom_point(col = "#1f77b4", size = 4) +
  xlab("Bedtime Hour (24-hour clock)") +
  ggtitle("Frequency of Bedtime Hours") +
  theme_bw()
g + scale_x_discrete(name ="Bedtime Hour (24-Hour Clock)", 
                     limits=c("","", "15", "", "",
                              "18", "", "", "21", "","", "24", "",
                              "", "3", "", "", "6", "","","9","", "", ""))


## frequency of sleep duration
ggplot(Sleep2018, aes(MinutesAsleep/60)) + 
  geom_histogram(fill = "#1f77b4") +
  ylab("Frequency (Nights)") +
  xlab("Duration of Sleep (Hours)") +
  ggtitle("Histogram of Sleep Duration") +
  theme_bw()

#plot sleep duration over time
p <- ggplot(Sleep2018, aes(Date, MinutesAsleep/60)) + theme_bw()
p <- p + geom_point(aes(colour=MinutesAsleep), size=3.5) + scale_colour_gradient(limits=c(0, 120), low="red", high="green", space="Lab") 
p <- p + ggtitle("Sleep Duration Over Time") + xlab("Date") + ylab("Duration in Hours")
p <- p + labs(colour = "Minutes Asleep")
p + geom_smooth(method=loess)

#plot sleep duration over time
p <- ggplot(Sleep2018, aes(Date, MinutesAsleep/60)) + theme_bw()
p <- p + geom_point(aes(colour=MinutesAsleep), size=3.5) + scale_colour_gradient(limits=c(0, 90), low="red", high="green", 
                                                                                 fill = "Minutes", space="Lab") 
p <- p + ggtitle("Sleep Duration Over Time") + xlab("Date") + ylab("Duration in Hours")
p + geom_smooth(method=loess)


#Number of Minutes Asleep Each Day of the Week
Sleep2018$day <- weekdays(as.Date(Sleep2018$Date))
Sleep2018$day <- factor(Sleep2018$day, levels= c("Saturday", "Friday", "Thursday",
                                                 "Wednesday", "Tuesday", "Monday", "Sunday"))
Sleep2018 <- Sleep2018[order(Sleep2018$day), ]
ggplot(Sleep2018, aes(x = MinutesAsleep/60, y = day, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "Hours", option = "C") +
  labs(title = 'Hours Asleep By Day of the Week',
       subtitle = "Source: FitBit sleep data (2018)", 
       x = "Duration of Sleep (Hours)") +
  theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())

Sleep2018$Week <- as.Date(cut(Sleep2018$SleepStart,
                              breaks = "week",
                              start.on.monday = FALSE))

# graph by month:
ggplot(data = Sleep2018,
       aes(Week, MinutesAsleep/60/7)) +
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar") + # or "line"
  ylab("Average Hours of Sleep") +
  labs (title = "Average Sleep Per Night") +
  #  labs (title = "Average Sleep A Night, TBS") +
  xlab("Week") # custom x-axis labels

##################Activities Data#######################
Activities2018$DayofWeek <- Activities2018$Date
total <- merge(Sleep2018, Activities2018, by = c("DayofWeek"), sort = TRUE)
#attach(total)

