#installing packages 
install.packages("RSQLite")
install.packages(c('devtools','curl'))
install.packages("dplyr")
install.packages("ggplot2")
install.packages("glmnet")

#importing libraries
library(dplyr)
library(RSQLite)
library(vctrs)
library(ggplot2)
library(glmnet)


#data setup

#bringing in the data from sqlite database
con <- dbConnect(drv=RSQLite::SQLite(), dbname="/Users/helhattab/Downloads/Take-Home_Materials/flights.sqlite")
tables <- dbListTables(con)


## create a data.frame for each table

for (table in tables) {
  print(paste("select * from",table))
  sendquery_character <- paste0(table, "<- dbSendQuery(con, 'select * from ", table, "')")
  eval(parse(text = sendquery_character)) 
  fetch_character <- paste0(table, "<- dbFetch(", table, ")")
  eval(parse(text = fetch_character))
  dbClearResult(dbListResults(con)[[1]])
  eval(parse(text = paste0("print(head(", table, "))")))
}

#checking that the above worked
x <- dbSendQuery(con, "select count(*) from ny_flights", n=-1 )
y<- dbFetch(x)

dim(ny_flights)

#bringing in the city, airport, and airline into the ny_flights data frame
#for origin and destination cities
data<-dplyr::inner_join(ny_flights, cities, by = c("origin_city_market_id"="id"))
data$origin_city<-data$city
data<-dplyr::inner_join(data, cities, by = c("dest_city_market_id"="id"))
data$dest_city<-data$city.y

#for origin and destination airports
data<-dplyr::inner_join(data, airports, by = c("origin_airport_id"="id"))
data$origin_airport<-data$airport_name
data<-dplyr::inner_join(data, airports, by = c("dest_airport_id"="id"))
data$dest_airport<-data$airport_name.y

#for airline
data<-dplyr::inner_join(data, airlines, by = c("op_carrier_airline_id"="id"))

#selecting the relevant columns  using this as get to know what's in the data time
#relevant columns are flight time, airline, origin/destination city/airport, scheduled/actual departure
#scheduled/actual arrival (in case there are patterns like flights that are supposed to arrive really late rarely get delayed)
#and delay in minutes for both, scheduled/actual elapsed time, taxi in/out

names(data)
data2 <-dplyr::select(data, 
                    fl_date,
                    op_carrier_fl_num,
                    airline_name,
                    origin_airport,
                    origin_city,
                    dest_airport,
                    dest_city,
                    crs_dep_time,
                    dep_time,
                    dep_delay,
                    arr_delay, 
                    cancelled,
                    diverted,
                    crs_arr_time,
                    arr_time,
                    arr_delay,
                    crs_elapsed_time,
                    actual_elapsed_time,
                    taxi_in,
                    taxi_out
                    )

########################################################################################
#                                       Part1
########################################################################################

# ### Part 1: Exploring a route
#
# Choose a route between two airports that you're interested in
# #, for example JFK to Los Angeles Int'l, or LaGuardia to O'Hare.
# # Do a brief exploratory analysis of this route using graphs to vizualize the data when you can.
# # You don't need to examine every possible variable or relationship; a handful of interesting ones is fine.
# #
# Some example questions you might investigate:
# #
#   - How busy is this route? Are there more flights on some days than others?
#   - Which airlines provide the most flights on this route?
#   - What do delays on this route look like?
#   - Are there strange-looking days on this route with fewer-than-normal flights or
#     longer-than-normal delays?
# 
# I'm interested in the NY-Miami flights because constantly wishing i'm in Florida on the beach where it's warm.
# Looking first at how popular the route is

data2%>%
  filter(origin_city=='New York City, NY (Metropolitan Area)')%>%
  select(dest_city)%>%
  group_by(dest_city)%>%
  tally()%>%
  arrange(desc(n))

# Miami is the second most popular route, out of NY! (This is looking by city, not airport)
# Okay so what airports are in  Miami (I've never  actually been), so I can pick a route, and which

data2%>%
  filter((origin_city=='New York City, NY (Metropolitan Area)' |
                  dest_city=='New York City, NY (Metropolitan Area)'),
         (origin_city=='Miami, FL (Metropolitan Area)' |
            dest_city=='Miami, FL (Metropolitan Area)'))%>%
  select(origin_airport,dest_airport)%>%
  group_by(origin_airport,dest_airport)%>%
  tally()%>%
  arrange(desc(n))

#Looks like the LaGuardia-Miami International are the most popular.


#limiting the data to flights from and to jfk/Orlando International
nyfl<- dplyr::filter(data2,dest_airport=='LaGuardia'| origin_airport=='LaGuardia',
                     dest_airport=='Miami International'| origin_airport=='Miami International')

#   - How busy is this route? Are there more flights on some days than others?
# Can see from the visualization that Saturday has less flights (1678 flights) than all the other days of the week
# , the rest of which are very similar averaging 1862 flights per day.

#creating a day of the week column
nyfl$day<-weekdays(as.Date(nyfl$fl_date,'%Y-%m-%d'))
nyfl$day<- factor(nyfl$day, levels=c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))

ggplot(data=(dplyr::count(nyfl,day)), aes(x = day, y=n, fill=day)) +
  geom_bar(stat = "identity")+ggtitle(label = "Flights by Day of the Week")+
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold"))+
  xlab("Day of the Week")+
  ylab("Number of Flights")

#What's the average number of flights? With and  without Saturday
y<-dplyr::count(nyfl,day)
mean(y$n)
y2<- dplyr::filter(y, day!='Saturday')
mean(y2$n)


#   - Which airlines provide the most flights on this route?
# American Airlines Inc. provides the most flights between LaGuardia and Miami International, with 64% of flights

nyfl%>%
  group_by(airline_name)%>%
  tally()%>%
  arrange(desc(n))%>%
  mutate(freq = n / sum(n))


#   - What do delays on this route look like?
# when looking at delays, i see there  are 188 cancelations on this route.
# I'm assuming anything above 0 is a delay, and below 0 is early, and not doing any
# transformations to account for a range where a flight is still considered ontime.
# Most flights on this route depart early surprisingly! 56% of flights on this route depart early
# and 37% depart late. Unsurprisingly,the same goes for Arrivals, where 60% arrive early, and 36% arrive late.
# Restating my assumption that there is probably a range where a flight could be consdered on time.

# Departure delays
hist(nyfl$dep_delay)
hist(nyfl$arr_delay)

nyfl$delay_cat[nyfl$dep_delay==0] <- "ontime"
nyfl$delay_cat[nyfl$dep_delay>0] <- "delay"
nyfl$delay_cat[nyfl$dep_delay<0] <- "early"

nyfl%>%
  group_by(delay_cat)%>%
  tally()%>%
  arrange(desc(n))%>%
  mutate(freq = n / sum(n))

# Arrival delays
nyfl$arr_cat[nyfl$arr_delay==0] <- "ontime"
nyfl$arr_cat[nyfl$arr_delay>0] <- "delay"
nyfl$arr_cat[nyfl$arr_delay<0] <- "early"

nyfl%>%
  group_by(arr_cat)%>%
  tally()%>%
  arrange(desc(n))%>%
  mutate(freq = n / sum(n))


# - Are there strange-looking days on this route with fewer-than-normal flights or longer-than-normal delays?
# Thir route has the highest  frequency of flights per day during the holidays (December 22nd to the end of the year),
# which is not surprising. The median number of flights is 34 flights per day, and from the histogram below, it is clear
# that days with less than 30 have a lower number of flights.
# March has the most days with less than 30 flights, March 21st-March 31st, to be exact have a particularly low number of flights.
# This could be a result of Spring Break seasonality. June has the highest number of flights in this route, where every day has >=34 flights.
# As for delays, I hypothesized that winter days would have the highest delays, specifically
# out of LaGuardia, but turns out summer months (June, July, August), have by far the highest number of delays!
# This could be a result of the high volume of daily flights on this route.


#to look at flight frequency by day 
nflights <- dplyr::count(nyfl,fl_date)

# Plot the chart with special scale lables
nflights<- nyfl%>%
  group_by(fl_date)%>%
  tally()%>%
  arrange(desc(n))

qplot(nflights$n,
      geom="histogram",
      binwidth =1,  
      main = "             Histogram of Number of Flights by Day", 
      xlab = "N Flights Per Day",  
      ylab = "Frequency",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(20,40),
      ylim=c(0,120)
      )

#to look at days with a low number of flights
lown<-dplyr::filter(nflights, n<30)
#creating a month column
lown$month<-months(as.POSIXct(lown$fl_date, format="%Y-%m-%d"))
lown$month<- factor(lown$month, levels=c("March","April", "May", "June", "July", "August","September","October","November"))

# by month
nflights$month<-months(as.POSIXct(nflights$fl_date, format="%Y-%m-%d"))
nflights$month<- factor(nflights$month, levels=c("January","February","March","April", "May", "June", "July", "August","September","October","November","December"))

nflights%>%
  group_by(month)%>%
  summarise(min=min(n))

#plotting months to vizualize
ggplot(data=(dplyr::count(lown,month)), aes(x = month, y=n, fill=month)) +
  geom_bar(stat = "identity")+
  ggtitle(label = "N Days With Less Than 30 Flights by Month")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold"))+
  xlab("Month")+
  ylab("Number of Days with less than 30 flights")

dplyr::filter(lown, month=='March')


#plotting months to vizualize
nyfl$month<-months(as.POSIXct(nyfl$fl_date, format="%Y-%m-%d"))
nyfl$month<- factor(nyfl$month, levels=c("January","February","March","April", "May", "June", "July", "August","September","October","November","December"))

nflights<- nyfl%>%
  group_by(month)%>%
  tally()%>%
  arrange(desc(n))

ggplot(data=nflights, aes(x = month, y=n, fill=month)) +
  geom_bar(stat = "identity")+
  ggtitle(label = "Flights by Month")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold"))+
  xlab("Month")+
  ylab("N Flights")

#any standout delays on specific days? checking to see if days are different when NY is the origin 
# Hypothesized that winter has highest delays, doesn't seem like that's true.
sumdelaysbymonth<- 
  nyfl %>%
  group_by(month) %>%
  filter(is.na(dep_delay)==FALSE)%>%
  summarise(sum = sum(dep_delay))%>%
  arrange(desc(sum))

ggplot(data=sumdelaysbymonth, aes(x = month, y=sum, fill=month)) +
  geom_bar(stat = "identity")+
  ggtitle(label = "Net Delay in Minutes by Month")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold"))+
  xlab("Month")+
  ylab("Net Delay in Minutes")



########################################################################################
#                                       Part2
########################################################################################

# ##Part 2: Comparing airports
# 
# Create some summary data and visualizations comparing the three NYC-area airports.
# Of particular interest are departure delays from the airports. Are there differences in
# the probability of a late (say, more than 30 min.) departure? How does time spent on the
# runway compare amongst the airports?
# 
# Assuming that the three NYC-area airports are LaGuardia, JFK, Newark, and limiting the data
# to flights that originate from those airports.
# 
# First thing to notice is how few flights are out of Newark, which doesn't seem right? 2877 flights out of
# Newark for the whole year which is only ~8 flights a day sounds wrong. Assuming this data is correct,
# La Guardia has the highest share of Flights out of the three airports.
# Regards to delays, ironically, Newark has the highest percent of delays at 23% of flights out of
# Newark having delays. Laguardia has the second highest percent of delays at 16%, followed by JFK at 13%.
# So Neward has the highest probability of having a delay. Cancelations also are twice as likely at Newark (6.5%)
# as they are at Laguardia, and less so at JFK.
# As for time spent on the runway (using taxi_out for this metric), the three airports have
# similar taxi time, with Newark's slightly lower (24.8 minutes on average). JFK and LaGuardia
# have an average of 27.6 and 27 per flight minutes respectively.


nyairports<-dplyr::filter(data2,origin_city=='New York City, NY (Metropolitan Area)')
dplyr::count(nyairports, origin_airport)

#getting the correct airports 
nyairports<-dplyr::filter(data2,origin_airport=='John F. Kennedy International'|origin_airport=='LaGuardia'|origin_airport=='Newark Liberty International')
nyairports$delay_cat<- ifelse(nyairports$dep_delay>=30,1,0)
nyairports$delay_cat[nyairports$cancelled==0 & is.na(nyairports$dep_delay)==TRUE] <- 0

dplyr::count(nyairports, origin_airport, delay_cat)

nairports<- as.data.frame(nyairports%>%
  group_by(origin_airport)%>%
  tally()%>%
  arrange(desc(n))%>%
  mutate(freq = n / sum(n)))

ggplot(nairports, aes(x=origin_airport,y = n, label = n, fill=origin_airport)) +
  geom_col() +
  geom_text(nudge_y = 100) + 
  ggtitle(label = "Number of Flights by NY Airport")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold"))+
  theme(legend.position = "none")+
  xlab("Airport")+
  ylab("N Flights")

# Are there differences in the probability of a late (say, more than 30 min.) departure? looking at the data, seeing that there are some
# NA dep_delay, but with crs_dep_time==dep_time. Will assume no delay there.
# Newark has the highest % of delays, and cancelations.
# taxi time: similar, Newark has the lowest.


nyairports%>%
  filter(cancelled==0)%>%
  count(origin_airport,delay_cat)


delays<-
  dplyr::inner_join(nyairports%>%
  filter(cancelled==0)%>%
  count(origin_airport,delay_cat),
  (nyairports%>%
     filter(cancelled==0)%>%
     count(origin_airport)), by='origin_airport')

delays<-as.data.frame(dplyr::rename(delays, n=n.x, total=n.y))

#delay % of flights
delays %>% 
  group_by(origin_airport) %>%
  filter(delay_cat==1)%>%
  summarise(n/total)

delays$percent_delay<- round((delays$n*100)/delays$total)
  
delays$delay_cat<-ifelse(delays$delay_cat==1,"delayed","on time")

#creating the correct position for the %s
library(plyr)
delayplot <- ddply(delays, .(origin_airport),
                     transform, pos = cumsum(percent_delay) - (0.5 * percent_delay))
detach(package:plyr)

ggplot() + geom_bar(aes(y = percent_delay, x = origin_airport, fill = delay_cat), data = delayplot,
                          stat="identity") +

  geom_text(data=delayplot, aes(x = origin_airport, y = pos,
                                           label = paste0(percent_delay,"%")), size=4)



#cancelations
cancelations<-
  dplyr::inner_join(nyairports%>%
                      count(origin_airport,cancelled),
                    (nyairports%>%
                       count(origin_airport)), by='origin_airport')

cancelations<-as.data.frame(dplyr::rename(cancelations, n=n.x, total=n.y))


cancelations %>% 
  group_by(origin_airport) %>%
  filter(cancelled==1)%>%
  summarise(n/total)

#time on the runway (taxi_out)

time_taxi<-  as.data.frame(
  dplyr::inner_join(nyairports %>% 
  group_by(origin_airport) %>%
  filter(is.na(taxi_out)==FALSE)%>%
  summarise(total_taxitime=sum(taxi_out)),
  nyairports %>% 
    group_by(origin_airport) %>%
    filter(is.na(taxi_out)==FALSE)%>%
    tally, by='origin_airport'))

  
time_taxi %>% 
  group_by(origin_airport) %>%
  summarise(total_taxitime/n)



########################################################################################
#                                       Part3
########################################################################################

### Part 3: Modeling delays

# 1. Build a simple model that estimates the probability that a given flight will have a late
# (greater than 30 min.) departure or arrival.
# 
# - You can interpret the term "model" loosely: Any method that, given some inputs known prior to a flight,
# will produce an estimated probability that the flight will be
# delayed or arrive late. You should also be able to use the model to show the uncertainty
# around the estimate.


#setting up the basefile, no cancelled flights,
#dependent variable is delay: 1 or 0

#looking at nulls, looks good mostly
bf<-dplyr::filter(data2, cancelled==0)

apply(bf,2,function(x){sum(is.na(x))})


#creating a day of the week column, month
bf$day<-weekdays(as.Date(bf$fl_date,'%Y-%m-%d'))
bf$month<-months(as.POSIXct(bf$fl_date, format="%Y-%m-%d"))

#creating time of day column
bf$timeofday[bf$crs_dep_time<1200] <-'a-Morning'
bf$timeofday[bf$crs_dep_time>=1200 & bf$crs_dep_time<1800] <-'b-Afternoon'
bf$timeofday[bf$crs_dep_time>=1800] <-'c-Night'

#checking flights length distribution
hist(bf$crs_elapsed_time)
# ok, most flights are under 200 minutes. setting long>200
bf$long_flight<- ifelse(bf$crs_elapsed_time>200,1,0)

#creating the dependent variable
bf$delay_cat<- ifelse(bf$dep_delay>=30,1,0)
bf$delay_cat[bf$crs_dep_time==bf$dep_time] <- 0
bf<-dplyr::filter(bf, is.na(delay_cat)==FALSE)


#using stepwise selection using a logistic regression first to see what features are significant
# in prediciting delays

# logit <- glm(delay_cat ~ origin_city, family = "binomial", data=bf)

# summary(logit)

  
#looks like saturday, sunday, Wednesday have high/significant coefs
#Frontier, SkyWest,  ExpressJet  have very high coefs
#Night has high coefs 
  
bf$airline_highdelay<- ifelse(bf$airline_name %in%  c('ExpressJet Airlines LLC','SkyWest Airlines Inc.','Frontier Airlines Inc.'),1,0)
bf$weekend<- ifelse(bf$day %in%  c('Saturday','Sunday'),1,0)

# bf <- dplyr::select(bf
#                    , long_flight
#                    , timeofday
#                    , month
#                    # , weekend
#                    , day
#                    , airline_highdelay
#                    , delay_cat)

#creating training/test set
bf$holdout = runif(nrow(bf))
train <- bf[bf$holdout < .7,]
test <- bf[bf$holdout >= .7,]

formula = as.formula(delay_cat~  long_flight + timeofday + month + weekend + airline_highdelay)

#trying interactions
# formula2 = as.formula(delay_cat~  (long_flight + timeofday + month + weekend + airline_highdelay)^2)

#tried formula with month*weekend, but it made AUC worse

mm <- model.matrix(formula,data=train)
mmtest <- model.matrix(formula, data=test)
mmall <- model.matrix(formula, data=bf)

plot(glmnet(mmall[ , -1], bf$delay_cat))

#train model
train.cvmn <- cv.glmnet(mm[ , -1], train$delay_cat, family="binomial", standardize=FALSE, type.measure="auc", nfold=10,alpha=1)
cvmnf <- glmnet(mm[ , -1], train$delay_cat,family="binomial",alpha=1,standardize = FALSE, lambda = train.cvmn$lambda.1se)
plot(train.cvmn) 

coef <- coef(cvmnf, s=cvmnf$lambda.min)[order(abs(coef(cvmnf, s=cvmnf$lambda.min)), decreasing = TRUE)][1:15]
names <- rownames(coef(cvmnf, s=cvmnf$lambda.min))[order(abs(coef(cvmnf, s=cvmnf$lambda.min)), decreasing = TRUE)][1:15]
for (iii in 1:15){
  cat(names[[iii]],"\t",coef[[iii]],"\n")
} 

calc_AUC <- function(obs, pred, plotROC=FALSE) {
  roc.pred<-prediction(pred, obs)
  if (plotROC) {
    roc.perf <- ROCR::performance(roc.pred, measure="tpr", x.measure="fpr")
    plot(roc.perf)
    abline(a=0,b=1)
  }
  return(ROCR::performance(roc.pred, "auc")@y.values[[1]])
}

train$delay_score <- as.numeric(predict(cvmnf, newx=mm[ , -1], type="response", s=cvmnf$lambda.1se))
calc_AUC(train$delay_cat, train$delay_score, "TRUE")

hist(train$delay_score)

#out of sample
test$delay_score <- as.numeric(predict(cvmnf, newx=mmtest[ , -1], type="response", s=cvmnf$lambda.1se))
calc_AUC(test$delay_cat, test$delay_score , "TRUE")
hist(test$delay_score)

test$delay_pred<-ifelse(test$delay_score>=0.17,1,0)
confusionMatrix( as.factor(test$delay_pred),as.factor(test$delay_cat), positive="1")


# 
# 2. Discuss the results of your fitted model. Here are a few things to describe:
# 
#   - The performance of your model---either its in-sample fit, or out-of-sample prediction
# accuracy (or both).
# - For a few interesting example flights or routes, use the model to estimate probability of
# a late departure or arrival and its uncertainty. Describe the results in plain English.
# - If your model is suited to estimating factors that affect delays
# (that is, it has interpretable coefficients), describe the estimated effects.
# 
# 
# My Lasso model has an out of sample AUC of 65, in sample 66.
# Using this model,  a test set flight on Frontier Airlines, from Laguardia to Atlanta, flying in June,
# on a Monday in the afternoon, has a 26% probability of being delayed. Using the threshold 17%,
# this flight will be predicted to be delayed.
# Give the AUC of 65 (out of sample), we understand that there is a 40% chance this is a False Positive,
# and a 60% chance this is a True positive.
# 
# According to this model, features that are likely to predict a delayed flight are:
# A night time flight, being on one of these airlines: ExpressJet Airlines LLC','SkyWest Airlines Inc.','Frontier Airlines Inc.',
# flying in July, August, and  to a lesser degree, January and  June.
# 
# Features that are less likely to predict a delayed flight:
# Being a weekend flight (Saturday and Sunday), flying in  March, September, and being a long flight
# 
# However, just noticing that with Lasso, it is not always very clear that coeficients represent
# the magnitude of the effect of features.
# 
# 3. Describe anything you feel might be missing from your model,
# or alternative approaches you might try with more time or data.
# If you think additional data would be helpful, describe what you would want.
# 
# Given more time, I would have focused a lot more on the seasonality element, and did more work
# to find origin/destination combinations that have delays. For example, if a route has many flights a day,
# that could  make flights more likely to be delayed, flights originating or heading to particular
# destinations have more delays?
# I probably would have consdiered doing a stepwise model, instead of throwing most things and the kitchen sink
# in a lasso, as well as an xgboost. I might have also used Facebook's Prophet package because
# it is good with seasonality. I would have also looked more into problems with the model
# like multicollinearity and outliers in the additional data I would have included.




