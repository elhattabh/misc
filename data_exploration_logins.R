library(csv)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plyr)
library(ggfortify)
library(anytime)
library(dygraphs)
library(plotly)


#coming to R to create dayofweek 
#bringing data in with the 15 minute intervals
logins=read.table("/Users/konafa/Documents/uber_15.csv",sep="\t", header=TRUE)
names(logins)

#creating day of week so that i can look at pattern by day
logins$dayofweek <- wday(logins$date, label=TRUE)

#creating a weekend column
logins$weekend <- ifelse(logins$dayofweek=="Sat" | logins$dayofweek=="Sun","wkend","wkday")
#checking that weekend column worked
head(dplyr::filter(logins, logins$date == "2010-01-08"))

#creating another column that approximates up to the hour without the date so that we aggregate on date
logins$time_hr <- format(round(anytime(logins$login_times), units="hours"), format="%H:%M:%S")
logins$date_hr <- format(round(anytime(logins$login_times), units="hours"), format="%Y-%m-%d %H:%M:%S")
subset(logins, select=c("login_times", "time_hr"))
#creating a data frame with the n per hour per day
ts_dateroundhour <- as.data.frame(dplyr::count(logins, date_hr))
ts_dateroundhour$hr <- substr(ts_dateroundhour$date_hr,12,19)
ts_dateroundhour$hr <- as.factor(ts_dateroundhour$hr)

#now to get average n per hour overall
day_cycle<- as.data.frame(aggregate(n~hr,data=ts_dateroundhour,FUN=mean))

ggplot(data=day_cycle, aes(x=substr(hr,1,2), y=n, group=1,color="red")) +
  geom_line() + geom_point() + 
  labs(x = "Hr in Day", y="Average Number of Logins By Hour", title="                               Daily Cylce of Uber Logins") +
  theme(panel.background = element_rect(fill = 'white', colour = 'black') ,
        panel.grid.minor.y = element_line(size=1),
        panel.grid.major = element_line(colour = "grey")) +
  theme(legend.position="none")

#looking for nulls
apply(logins,2,function(x){sum(is.na(x))})
#no nulls

#agg on the 15 minute mark
ts_15 <- dplyr::count(logins, date_15)
plot.ts(ts_15, xlab="Time")

#agg on date
ts_date <- dplyr::count(logins, date)
plot.ts(ts_date, xlab="Date")


#let's look at monthly cycles
ts_month <- dplyr::count(logins, month(date))
autoplot(as.ts(ts_month))

#looking at monthly data we can see that more logins during summer than february and march


#exporting data back into sql for more organic transformation and aggregated metrics
write.table(logins, "/Users/konafa/Downloads/uber_15_ref.csv", sep=",")

plot.ts(ts_15)

count_dayofweek<-as.data.frame(count_dayofweek<-dplyr::count(logins, dayofweek))

ddply(logins, .(dayofweek), function(x) mean(x$n) )

group_by(logins, dayofweek) %>% summarize(m = mean(speed))

ddply(count_dayofweek, .(dayofweek), summarize,  Rate1=mean(Rate1))

ggplot(count_dayofweek)

dplyr::count(logins, dayofweek)
dplyr::count(logins, weekend)

mean(logins$dayofweek)
count_dayofweek %>% group_by(dayofweek) %>% summarise(mean)

iris %>% group_by(Species) %>% summarise(…)
