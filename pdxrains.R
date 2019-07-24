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

newraindata <- raindata[raindata$Year > '1980',]
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
drd$Rainday[drd$Rain > 0 & drd$Rain < 0.1] <- 0.1
drd$Rainday[drd$Rain >= 0.1 & drd$Rain < 0.2] <- 0.2
drd$Rainday[drd$Rain >= 0.2 & drd$Rain < 0.3] <- 0.3
drd$Rainday[drd$Rain >= 0.3 & drd$Rain < 0.4] <- 0.4
drd$Rainday[drd$Rain >= 0.4 & drd$Rain < 0.5] <- 0.5
drd$Rainday[drd$Rain >= 0.5 & drd$Rain < 0.75] <- 0.75
drd$Rainday[drd$Rain >= 0.75 & drd$Rain < 1] <- 1
drd$Rainday[drd$Rain >= 1 & drd$Rain < 1.5] <- 1.5
drd$Rainday[drd$Rain >= 1.5] <- 1.75
drd<-drd[complete.cases(drd),]

drd$wday <- wday(drd$date)

ggplot(drd) +
  geom_histogram(aes(x = drd$Rain),binwidth = .05)
head(drd)

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
head(drd)

#daily rain plot

drd$newDay <- as.numeric(drd$newDay)
drd$Year <- as.numeric(drd$Year)


#define month day segment, for vertical line dividers

mds <- c(31, #jan
         31+28, #feb
         31+28+31, #mar
         31+28+31+30, #apr
         31+28+31+30+31, #may
         31+28+31+30+31+30, #june
         31+28+31+30+31+30+31, #jul
         31+28+31+30+31+30+31+31, #aug
         31+28+31+30+31+30+31+31+30, #sep
         31+28+31+30+31+30+31+31+30+31, #oct
         31+28+31+30+31+30+31+31+30+31+30, #nov
         31+28+31+30+31+30+31+31+30+31+30+31) #dec

q <- ggplot(drd, aes(newDay, Year, fill =  Rain)) + 
  geom_tile() + 
  #facet_grid(Year~Month) + 
  scale_fill_continuous(low="slategray2", high="purple4", guide = "colorbar") +
  labs(title = "PDX Daily Rainfall 1982 - 2017",
       #subtitle = "Data up to April 2018",
       x = "Day of Year",
       y = "Year",
       fill = "Rain (in)") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_blank(),
        axis.text=element_text(size=10))+
  theme(plot.title = element_text(hjust = 0.5, size = 12))+
  scale_x_continuous(limits = c(0,366), breaks = seq(0,365,30)) +
  scale_y_continuous(limits = c(1981,2018), breaks = seq(1982,2017,1)) +
  
  # lines indicating month breaks
  
  geom_segment(aes(x = mds[1], y = 1981.5, xend = mds[1], yend = 2017.5), color = "grey")+
  geom_segment(aes(x = mds[2], y = 1981.5, xend = mds[2], yend = 2017.5), color = "grey")+
  geom_segment(aes(x = mds[3], y = 1981.5, xend = mds[3], yend = 2017.5), color = "grey")+
  geom_segment(aes(x = mds[4], y = 1981.5, xend = mds[4], yend = 2017.5), color = "grey")+
  geom_segment(aes(x = mds[5], y = 1981.5, xend = mds[5], yend = 2017.5), color = "grey")+
  geom_segment(aes(x = mds[6], y = 1981.5, xend = mds[6], yend = 2017.5), color = "grey")+
  geom_segment(aes(x = mds[7], y = 1981.5, xend = mds[7], yend = 2017.5), color = "grey")+
  geom_segment(aes(x = mds[8], y = 1981.5, xend = mds[8], yend = 2017.5), color = "grey")+
  geom_segment(aes(x = mds[9], y = 1981.5, xend = mds[9], yend = 2017.5), color = "grey")+
  geom_segment(aes(x = mds[10], y = 1981.5, xend = mds[10], yend = 2017.5), color = "grey")+
  geom_segment(aes(x = mds[11], y = 1981.5, xend = mds[11], yend = 2017.5), color = "grey")+
  
  # lines indicating seasonal breaks
  # spring
#  geom_segment(aes(x = mds[2] + 20, y = 1981.5, xend = mds[2] + 20, yend = 2017.5),
#               size = 0.8, color = "dark green", alpha = 0.7) +
  #summer
#  geom_segment(aes(x = mds[5] + 20, y = 1981.5, xend = mds[5] + 20, yend = 2017.5),
#               size = 0.8, color = "dark green", alpha = 0.7) +
  #fall
#  geom_segment(aes(x = mds[8] + 20, y = 1981.5, xend = mds[8] + 20, yend = 2017.5),
#               size = 0.8, color = "dark green", alpha = 0.7) +
  #winter
#  geom_segment(aes(x = mds[11] + 20, y = 1981.5, xend = mds[11] + 20, yend = 2017.5),
#               size = 0.8, color = "dark green", alpha = 0.7) +
  
  # month labels
  
  annotate("text", x = mds[1]/2, y = 2018, label = "Jan", size = 4)+
  annotate("text", x = mds[2]-(mds[2]-mds[1])/2, y = 2018, label = "Feb", size = 4)+
  annotate("text", x = mds[3]-(mds[3]-mds[2])/2, y = 2018, label = "Mar", size = 4)+
  annotate("text", x = mds[4]-(mds[4]-mds[3])/2, y = 2018, label = "Apr", size = 4)+
  annotate("text", x = mds[5]-(mds[5]-mds[4])/2, y = 2018, label = "May", size = 4)+
  annotate("text", x = mds[6]-(mds[6]-mds[5])/2, y = 2018, label = "Jun", size = 4)+
  annotate("text", x = mds[7]-(mds[7]-mds[6])/2, y = 2018, label = "Jul", size = 4)+
  annotate("text", x = mds[8]-(mds[8]-mds[7])/2, y = 2018, label = "Aug", size = 4)+
  annotate("text", x = mds[9]-(mds[9]-mds[8])/2, y = 2018, label = "Sep", size = 4)+
  annotate("text", x = mds[10]-(mds[10]-mds[9])/2, y = 2018, label = "Oct", size = 4)+
  annotate("text", x = mds[11]-(mds[11]-mds[10])/2, y = 2018, label = "Nov", size = 4)+
  annotate("text", x = mds[12]-(mds[12]-mds[11])/2, y = 2018, label = "Dec", size = 4) 

q

tail(drd[drd$Year == 2005,],10)



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

