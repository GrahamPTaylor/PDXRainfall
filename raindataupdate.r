library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(plyr)
library(lubridate)

# data from - https://w2.weather.gov/climate/local_data.php?wfo=pqr

setwd('C:/Users/Graham/Desktop/Grad School')
climedata <- data.frame(read.csv(file = 'Portland_dailyclimatedata.csv', header = T))
head(climedata)

# delete non-precipitation rows

raindata <- climedata[climedata$Type == 'PR',]

# limit to semi recent data

newraindata <- raindata[raindata$Year > '1941',]
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
drd <- drd[drd$Year != '2018', ]
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


tail(drd,14)


write.csv(drd, file = 'raindata.csv')



p <- ggplot(drd, aes(monthweek, wday,fill =  Rainday)) + 
  geom_tile(colour = "grey") + 
  facet_grid(Year~Month) + 
  scale_fill_continuous(low="light blue", high="blue") +
  labs(title = "PDX Daily Rainfall 1991 - 2017",
       #subtitle = "Data up to April 2018",
       x = "Week of month",
       y = "Weekday",
       fill = "Rain (in)")

p
head(drd)

drd$weekyear <- strftime(drd$date, format = "%V")
drd$newDay <- strftime(drd$date, format = '%j')



q <- ggplot(drd, aes(newDay, Year, fill =  Rain)) + 
  geom_tile() + 
  #facet_grid(Year~Month) + 
  scale_fill_continuous(low="light blue", high="blue") +
  labs(title = "PDX Daily Rainfall 1991 - 2017",
       #subtitle = "Data up to April 2018",
       x = "Day of Year",
       y = "Year",
       fill = "Rain (in)") +
  theme(axis.text.x = element_blank()) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank())

q



# blank version



b <- ggplot(drd, aes(newDay, Year, fill =  Rain)) + 
  geom_tile() + 
  #facet_grid(Year~Month) + 
  scale_fill_continuous(low="#CCE5FF", high="#000066") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "none") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank()) 
#  coord_equal()

b


x <- ggplot(drd) +
  geom_bar(data = drd, aes(x = newDay ,y = Rain), stat = "identity") +
  facet_wrap(~Year)
#x

