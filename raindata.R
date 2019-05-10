library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(plyr)
library(lubridate)

# data from - https://w2.weather.gov/climate/local_data.php?wfo=pqr

climedata <- data.frame(read.csv(file = 'Portland_dailyclimatedata.csv', header = T))
head(climedata)

# delete non-precipitation rows

raindata <- climedata[climedata$Type == 'PR',]

# limit to semi recent data

newraindata <- raindata[raindata$Year > '1990',]
#drop PR column
newraindata[,c("Type","Total","Average")] <- list(NULL)

head(newraindata)

colnames(newraindata[,-1:-2]) <- c(paste(seq(1:31)))
# unpivot data - row for every day
colnames(newraindata)

dailyraindata <- gather(newraindata, Day, Rain, X1:X31)

#remove X from day name
dailyraindata$Day <- substring(dailyraindata$Day,2)

#change day name and rain to numbers
dailyraindata$Day <- as.numeric(dailyraindata$Day)
dailyraindata$Rain <- as.numeric(dailyraindata$Rain)

#order correctly

dailyraindata <- dailyraindata[order(dailyraindata$Year, dailyraindata$Month, dailyraindata$Day), ]
dailyraindata <- dailyraindata %>%
  mutate(date = as.Date(paste(Year,"-",Month,"-",Day,sep="")))


#### grid plot month week

drd <- dailyraindata



# since 2000

#drd <- drd[drd$Year >= '2000', ]
drd$week <- strftime(drd$date, format = '%W')
drd$weekday <- weekdays(drd$date)
head(drd)
drd$yearmonth <- as.yearmon(drd$date)
drd$yearmonthf <- factor(drd$yearmonth)
drd$week <- as.numeric(drd$week)
drd <- ddply(drd,.(yearmonthf), transform, monthweek = 1+week-min(week))
head(drd,50)
drd$Rain[is.na(drd$Rain)] <- 0
#drd$RainDay <- 
drd$Rainday[drd$Rain > 0 & drd$Rain < 0.25] <- 0.25
drd$Rainday[drd$Rain >= 0.25 & drd$Rain < 0.5] <- 0.5
drd$Rainday[drd$Rain >= 0.5 & drd$Rain < 0.75] <- 0.75
drd$Rainday[drd$Rain >= 0.75 & drd$Rain < 1] <- 1
drd$Rainday[drd$Rain >= 1 & drd$Rain < 1.25] <- 1.25
drd$Rainday[drd$Rain >= 1.25 & drd$Rain < 1.5] <- 1.5
drd$Rainday[drd$Rain >= 1.5 & drd$Rain < 1.75] <- 1.75
drd$Rainday[drd$Rain >= 1.75 & drd$Rain < 2] <- 2
drd$Rainday[drd$Rain >= 2] <- 2.25
drd<-drd[complete.cases(drd),]

drd$wday <- wday(drd$date)





write.csv(drd, file = 'raindata.csv')



ggplot(drd, aes(monthweek, weekday,fill =  Rainday)) + 
  geom_tile(colour = "grey") + 
  facet_grid(Year~Month) + 
  scale_fill_continuous(low="light blue", high="blue") +
  labs(title = "PDX Daily Rainfall",
       subtitle = "Data up to April 2018",
       x = "Week of month",
       y = "Weekday",
       fill = "Rain (in)")
head(drd)



drd$newDay <- strftime(drd$date, format = '%j')

x <- ggplot(drd) +
  geom_bar(data = drd, aes(x = newDay ,y = Rain), stat = "identity") +
  facet_wrap(~Year)
x
