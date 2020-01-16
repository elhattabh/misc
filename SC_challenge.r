# install.packages('dplyr')
library(dplyr)
# install.packages('plyr')
library(plyr)
# install.packages('glmnet')
library(glmnet)
# install.packages('anytime')
library(anytime)
# install.packages("caret")
library(caret)
# install.packages('ROCR')
library(ROCR)
# install.packages('ggplo2')
library(ggplot2)

par(mfrow=c(1,1))

options(scipen=999)
#creating a function to calculate the area under the curve
calc_AUC <- function(obs, pred, plotROC=FALSE) {
  roc.pred<-prediction(pred, obs)
  if (plotROC) {
    roc.perf <- ROCR::performance(roc.pred, measure="tpr", x.measure="fpr")
    plot(roc.perf)
    abline(a=0,b=1)
  }
  return(ROCR::performance(roc.pred, "auc")@y.values[[1]])
}

# R was crashing when i was loading the json file, so i converted it to a csv in python
# then loaded it into R. Python i used is below:


# import pandas as pd
# import json
# import csv
# 
# with open('/Users/helhattab/Downloads/sc_data_science_challenge.json') as f:
#   lst = json.load(f)
# 
# tracks = pd.DataFrame(data = lst.get('data'), columns=['ts', 'country_code', 'client_version', 'listening_context', 'recommender_algorithm_name', \
#                                                        'track_id', 'track_genre_category', 'track_upload_date', 'track_duration', 'listen_duration', \
#                                                        'listener_id', 'listener_signup_date', 'listener_top_genre_category_listened', 'listener_prev_month_listening_time', \
#                                                        'listener_prev_month_avg_daily_tracks_listened'])
# tracks.to_csv(r'tracks_soundcloud.csv')

tracks <- read.csv("/Users/helhattab/Downloads/SoundCloud-master/tracks_soundcloud.csv", header = TRUE, stringsAsFactors = FALSE)

attach(tracks)

head(tracks)

#Exploring the data
#looking if the data has nulls 
apply(tracks,2,function(x){sum(is.na(x))})
# i see these two columns have nulls, might have to be dropped, or getting substituted with the mean
# if the features are important, will deaal with the features 
#listener_prev_month_listening_time 
#listener_prev_month_avg_daily_tracks_listened

#removing cases when the listen duration is longer than the track duration, probably a bug or dirty data
tracks<-dplyr::filter(tracks, listen_duration<=track_duration)
attach(tracks)

#looking at the frequency of duration in seconds and minutes
hist(track_duration*0.001)
hist(track_duration/60000)
# can see that there are some outliers for duration, so need to clean the data.

#Looking at the outliers of track duration, in minutes 
#creating minute and second variables for duration. first look at outliers, then establish what a skip means
tracks$duration_in_min <- round_any(tracks$track_duration/60000,1)
tracks$duration_in_sec <- round_any(tracks$track_duration*0.001,10)

#first the outliers
tracks$duration_in_min_tens <- round_any(track_duration/60000,10)
x<- dplyr::count(tracks,duration_in_min_tens)
x<-dplyr::arrange(x,-duration_in_min_tens)
# anything that has less than 15, will consider an outlier, but will double check with a box plot

#removing outliers, using a function
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}

#running the function, now the data frame has NA instead  of outliers, will fitler them out
outlierKD(tracks, duration_in_min) #need to say yes here, if running
tracks <- dplyr::filter(tracks,duration_in_min!='NA')
#looking at outliers for listener_prev_month_avg_daily_tracks_listened
outlierKD(tracks, listener_prev_month_avg_daily_tracks_listened) #need to say yes here, if running

#looking at outliers for listener_prev_month_listening_time
outlierKD(tracks, listener_prev_month_listening_time) #need to say yes here, if running

#checking that  the outliers are gone
dim(tracks)

#defining the skip behvaior, if the listening duration is longer than track duration
tracks$skip<- ifelse(tracks$listen_duration<tracks$track_duration,1,0)

#looking at the data to do some feature transformation
#look at listening_context xtabs
xtabs(~skip + listening_context, data=tracks)
logit <- glm(skip~ listening_context 
             , data = tracks, family = "binomial")
summary(logit)


#see some standouts, create dummy variables for them
tracks$listening_contextpersonal_recommended<- ifelse(tracks$listening_context=='personal-recommended',1,0)
tracks$listening_contextcharts<- ifelse(tracks$listening_context=='charts',1,0)
tracks$listening_contextsearch<- ifelse(tracks$listening_context=='search',1,0)
tracks$listening_contexttracks<- ifelse(tracks$listening_context=='tracks',1,0)


#look at recommender algorithm xtabs
xtabs(~skip + recommender_algorithm_name, data=tracks)
logit <- glm(skip~ recommender_algorithm_name 
             , data = tracks, family = "binomial")
summary(logit)
#looks like content-based has an effect, creating a dummy
tracks$recommender_algorithm_namecontentbased<-ifelse(tracks$recommender_algorithm_name=='content-based',1,0)
  

#look at track genre category xtabs
xtabs(~skip + track_genre_category, data=tracks)
logit <- glm(skip~ track_genre_category 
             , data = tracks, family = "binomial")
summary(logit)
#looks like a couple have an effect, creating a dummy
tracks$track_genre_categoryDanceElectronic<-ifelse(tracks$track_genre_category=='Dance & Electronic',1,0)
tracks$track_genre_categoryCountry <-ifelse(tracks$track_genre_category=='Country',1,0)
tracks$track_genre_categoryReggae<-ifelse(tracks$track_genre_category=='Reggae',1,0)
tracks$track_genre_categorySpeech<-ifelse(tracks$track_genre_category=='Speech',1,0)
tracks$track_genre_categoryUnknown<-ifelse(tracks$track_genre_category=='Unknown',1,0)

#look at client_version category xtabs
xtabs(~skip + client_version, data=tracks)
#okay many with very small n
x<- dplyr::count(tracks, client_version)
as.data.frame(dplyr::arrange(x,-n))
x<- dplyr::filter(x, n>1000)
#creating a version of trakcs with only the client versions with enough coverate
tracks_clientversion_covered<- dplyr::inner_join(tracks,x, by="client_version")
xtabs(~skip + client_version, data=tracks_clientversion_covered)

#given that client_version has so many levels, will look at a lasso for it now that we have kept only levels with coverage higher than a 1000
formula = as.formula(skip~client_version)
mmall <- model.matrix(formula, data=tracks_clientversion_covered)

plot(glmnet(mmall[ , -1], tracks_clientversion_covered$skip))

#train model, warning this takes a few minutes
train.cvmn <- cv.glmnet(mmall[ , -1], tracks_clientversion_covered$skip, family="binomial", standardize=FALSE, type.measure="auc", nfold=10,alpha=1)
cvmnf <- glmnet(mmall[ , -1], tracks_clientversion_covered$skip,family="binomial",alpha=1,standardize = FALSE, lambda = train.cvmn$lambda.min)

#arranging the coefficients in desc absolute value order to see if a few are standing out
coef <- coef(cvmnf, s=cvmnf$lambda.min)[order(abs(coef(cvmnf, s=cvmnf$lambda.min)), decreasing = TRUE)][1:50]
names <- rownames(coef(cvmnf, s=cvmnf$lambda.min))[order(abs(coef(cvmnf, s=cvmnf$lambda.min)), decreasing = TRUE)][1:50]
for (iii in 1:50){
  cat(names[[iii]],"\t",coef[[iii]],"\n")
}

#looks like a couple have an effect, creating a dummies for the highest
tracks$client_version24.8.20<-ifelse(tracks$client_version=='24.8.20',1,0)
tracks$client_version36.4.20<-ifelse(tracks$client_version=='36.4.20',1,0)
tracks$client_version176.0.0<-ifelse(tracks$client_version=='176.0.0',1,0)


#creating a feature if the date for signup and date for trackupload are close together
tracks$track_upload_date<- anydate(tracks$track_upload_date)
tracks$listener_signup_date<- anydate(tracks$listener_signup_date)
tracks$date_diff <- tracks$track_upload_date-tracks$listener_signup_date
head(dplyr::select(tracks, track_upload_date,listener_signup_date, date_diff))
tracks$signup_listen_within30days<- ifelse(abs(tracks$date_diff)<=30,1,0 )

# breaking down duration into four quartiles 
tracks$duration_quartile = cut(as.numeric(tracks$duration_in_sec), breaks=c(quantile(as.numeric(tracks$duration_in_sec), probs = seq(0, 1, by = 0.25),na.rm=TRUE)), include.lowest = TRUE)
levels(tracks$duration_quartile) = 1:4
aggregate(tracks$duration_in_sec, by=list(Category=tracks$duration_quartile), FUN=max)

#breaking down listener_prev_month_avg_daily_tracks_listened and listener_prev_month_listening_time 
#into ntiles
tracks$listener_prev_month_avg_daily_tracks_listened_quartile = cut(as.numeric(tracks$listener_prev_month_avg_daily_tracks_listened), breaks=c(quantile(as.numeric(tracks$listener_prev_month_avg_daily_tracks_listened), probs = seq(0, 1, by = 0.25),na.rm=TRUE)), include.lowest = TRUE)
levels(tracks$listener_prev_month_avg_daily_tracks_listened_quartile) = 1:4
aggregate(tracks$listener_prev_month_avg_daily_tracks_listened, by=list(Category=tracks$listener_prev_month_avg_daily_tracks_listened_quartile), FUN=min)
#found that the quartile 3,4 is significant re skipping so creating that feature
tracks$listener_prev_month_avg_daily_tracks_listened_quartile3_4<- ifelse(tracks$listener_prev_month_avg_daily_tracks_listened_quartile %in%c(3,4),1,0)

tracks$listener_prev_month_listening_time_quartile = cut(as.numeric(tracks$listener_prev_month_listening_time), breaks=c(quantile(as.numeric(tracks$listener_prev_month_listening_time), probs = seq(0, 1, by = 0.25),na.rm=TRUE)), include.lowest = TRUE)
levels(tracks$listener_prev_month_listening_time_quartile) = 1:4
aggregate(tracks$listener_prev_month_listening_time, by=list(Category=tracks$listener_prev_month_listening_time_quartile), FUN=min)
#found that the quartile 3,4 is significant re skipping so creating that feature
tracks$listener_prev_month_listening_time_quartile3_4<- ifelse(tracks$listener_prev_month_listening_time_quartile %in%c(3,4),1,0)


# if category of the track is the same 
tracks$track_cateogry_top <- ifelse(tracks$listener_top_genre_category_listened==tracks$track_genre_category,1,0)


#removing nulls for simplicity, could have also done mean substitution (possibly stratified)
tracks<-dplyr::filter(tracks,is.na(listener_prev_month_listening_time)==FALSE)
apply(tracks,2,function(x){sum(is.na(x))})


#creating a train and test set 
attach(tracks)
tracks$holdout = runif(nrow(tracks))
train <- tracks[tracks$holdout < .7,]
test <- tracks[tracks$holdout >= .7,]

# first,  running a lasso to get a better idea, after creating all the features,
# since lasso does feature selection
# not including listening_duration because obviously the longer the 
# listening duration the closer to the track duration the user is, so that variable shouldn't be used
#after running on training set to validate on test, running on all of data 

formula = as.formula(skip~
                     duration_quartile + 
                     signup_listen_within30days+
                     listener_prev_month_avg_daily_tracks_listened_quartile3_4 +
                     listener_prev_month_listening_time_quartile3_4+
                     track_cateogry_top+
                     listener_top_genre_category_listened+
                     listening_contextpersonal_recommended+
                     listening_contextcharts+
                     listening_contextsearch+
                     listening_contexttracks+
                     # listening_context+
                     recommender_algorithm_name +
                     # recommender_algorithm_namecontentbased+
                     track_genre_categoryDanceElectronic+
                     track_genre_categoryCountry+
                     track_genre_categorySpeech+
                     track_genre_categoryReggae+
                     track_genre_categoryUnknown+
                     client_version24.8.20+
                     client_version36.4.20+
                     client_version176.0.0+
                     country_code
)


mm <- model.matrix(formula,data=train)
mmtest <- model.matrix(formula, data=test)
mmall <- model.matrix(formula, data=tracks)

par(mfrow=c(1,1))
plot(glmnet(mmall[ , -1], tracks$skip))

#train model, warning this takes a while
train.cvmn <- cv.glmnet(mmall[ , -1], tracks$skip, family="binomial", standardize=FALSE, type.measure="auc", nfold=10,alpha=1)
cvmnf <- glmnet(mmall[ , -1], tracks$skip,family="binomial",alpha=1,standardize = FALSE, lambda = train.cvmn$lambda.min)

plot(train.cvmn) 
#auc is 0.75!, got it higher from initial AUC

coef <- coef(cvmnf, s=cvmnf$lambda.min)[order(abs(coef(cvmnf, s=cvmnf$lambda.min)), decreasing = TRUE)][1:15]
names <- rownames(coef(cvmnf, s=cvmnf$lambda.min))[order(abs(coef(cvmnf, s=cvmnf$lambda.min)), decreasing = TRUE)][1:15]
for (iii in 1:15){
  cat(names[[iii]],"\t",coef[[iii]],"\n")
}

df<-cbind(as.data.frame(names),as.data.frame(coef))
df$EffectOnSkip<- ifelse(coef>=0,'Inc Skip','Dec Skip')
df

ggplot(df, aes(x=`names`, y=coef, label=EffectOnSkip)) + 
  geom_bar(stat='identity', aes(fill=EffectOnSkip), width=.5) + 
  scale_y_continuous(breaks=seq(-1.5, 1.5, 0.25))+
  labs(title="Feature Importance for Prediciting Skip Behavior") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

#auc was originally 0.68 on test set, now it's 0.73!
#Based on this analysis, can see that the following are the most influential:
# names       coef pos_or_neg
# 1                      listening_contextpersonal_recommended -1.2554660   Dec Skip
# 2                                    listening_contexttracks  1.2543029   Inc Skip
# 3                                    listening_contextcharts -1.1587411   Dec Skip
# 4                                                (Intercept) -1.0424658   Dec Skip
# 5                                      client_version36.4.20  0.9400337   Inc Skip
# 6                                      client_version24.8.20  0.8244565   Inc Skip
# 7                                    listening_contextsearch  0.7656037   Inc Skip
# 8                                      client_version176.0.0 -0.6734581   Dec Skip
# 9             listener_prev_month_listening_time_quartile3_4 -0.6371562   Dec Skip
# 10 listener_prev_month_avg_daily_tracks_listened_quartile3_4  0.5376412   Inc Skip
# 11                 listener_top_genre_category_listenedWorld -0.4355613   Dec Skip
# 12                   recommender_algorithm_namecontent-based -0.3884566   Dec Skip
# 13                        recommender_algorithm_namefallback  0.2628606   Inc Skip
# 14                                track_genre_categoryReggae  0.2387483   Inc Skip
# 15                                        duration_quartile4  0.2136788   Inc Skip


#for fun just creating a score and seeing how it does
train$skip_score <- as.numeric(predict(cvmnf, newx=mm[ , -1], type="response", s=cvmnf$lambda.min))
calc_AUC(train$skip, train$skip_score, "TRUE")
test$skip_score <- as.numeric(predict(cvmnf, newx=mmtest[ , -1], type="response", s=cvmnf$lambda.min))
calc_AUC(test$skip, test$skip_score, "TRUE")



#did a double check using a logistic regression to make sure all these variables i used were significant

formula = as.formula(skip~ listening_context +country_code +
                       recommender_algorithm_name + track_genre_category +
                       track_cateogry_top + signup_listen_within30days  +
                       duration_quartile + listener_prev_month_listening_time_quartile3_4 + 
                       listener_prev_month_avg_daily_tracks_listened_quartile3_4)


logit <- glm(skip~ listening_context 
             # +
             # recommender_algorithm_name + track_genre_category +
             # track_cateogry_top + signup_listen_within30days  +
             # duration_quartile + listener_prev_month_listening_time_quartile + 
             # listener_prev_month_avg_daily_tracks_listened_quartile4
             , data = tracks, family = "binomial")

#not significant                     
#country_code
                  
#significant
# listening_context
# recommender_algorithm_name
# track_genre_category
# track_cateogry_top
# signup_listen_within30days
# duration_quartile 
# listener_prev_month_listening_time_quartile
# highest quartile of users  listener_prev_month_avg_daily_tracks_listened_quartile4






