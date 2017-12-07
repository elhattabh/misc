library(dplyr)
library(glmnet)
library(data.table)

calc_AUC <- function(obs, pred, plotROC=FALSE) {
  roc.pred<-prediction(pred, obs)
  if (plotROC) {
    roc.perf <- performance(roc.pred, measure="tpr", x.measure="fpr")
    plot(roc.perf)
    abline(a=0,b=1)
  }
  return(performance(roc.pred, "auc")@y.values[[1]])
}

data=read.csv('/Users/konafa/Downloads/bachelorette.csv')
head(data)
attach(data)

data$r1= ifelse(ELIMINATION.1=='R1', 1,0)
data$bachelor=ifelse(data$SHOW=='Bachelor',1,0)
data$bachelorette=ifelse(data$SHOW=='Bachelorette',1,0)


ddf= data.frame(dplyr::count(data,SHOW,as.integer(SEASON)) )
print (ddf)

bachelor <- dplyr::filter(data, SHOW =='Bachelor')
bachelorette= dplyr::filter(data, SHOW =='Bachelorette')

dplyr::count(data,r1)
dplyr::count(data, ELIMINATION.10)
#using bryan to check
dplyr::filter(data,CONTESTANT==toupper('13_Bryan_A'))


#feature extraction

#creating the dependent variable, a win
data$win= ifelse(data$ELIMINATION.10=='W' | data$ELIMINATION.9 =='W' | data$ELIMINATION.8 =='W' | data$ELIMINATION.7 =='W' |data$ELIMINATION.6 =='W' |data$ELIMINATION.5 =='W' |data$ELIMINATION.4 =='W' |data$ELIMINATION.3 =='W' |data$ELIMINATION.2 =='W' | data$ELIMINATION.1 =='W' , 1,0)

#whether you went on a 1:1
data$oneonone= ifelse(data$DATES.10=='D1' | data$DATES.9 =='D1' | data$DATES.8 =='D1' | data$DATES.7 =='D1' |data$DATES.6 =='D1' |data$DATES.5 =='D1' |data$DATES.4 =='D1' |data$DATES.3 =='D1' |data$DATES.2 =='D1' | data$DATES.1 =='D1' , 1,0)

#firstoneonone
data$firstoneonone= ifelse(data$DATES.2 =='D1', 1,0)

#whether you went on a 1:1 early 
data$oneononeearly= ifelse(data$DATES.4 =='D1' |data$DATES.3 =='D1' |data$DATES.2 =='D1', 1,0)

#twoonone
data$twoonone = ifelse(data$DATES.10=='D2' | data$DATES.9 =='D2' | data$DATES.8 =='D2' | data$DATES.7 =='D2' |data$DATES.6 =='D2' |data$DATES.5 =='D2' |data$DATES.4 =='D2' |data$DATES.3 =='D2' |data$DATES.2 =='D2' | data$DATES.1 =='D2' , 1,0)

#groupdaterose

#to get groupdate rose in python bec easier
#print """data$groupdaterose= ifelse((data$DATES.1!='D1' & data$DATES.1!='' & data$ELIMINATION.1 %like% "^R") |"""
#for x in range(2,11):
#  print """(data$DATES.%s!='D1' & data$DATES.%s!='' & data$ELIMINATION.%s %%like%% "^R") |""" %(x,x,x)
#print ''', 1,0)'''

data$groupdaterose= ifelse((data$DATES.1!='D1' & data$DATES.1!='' & data$ELIMINATION.1 %like% "^R") |
                             (data$DATES.2!='D1' & data$DATES.2!='' & data$ELIMINATION.2 %like% "^R") |
                             (data$DATES.3!='D1' & data$DATES.3!='' & data$ELIMINATION.3 %like% "^R") |
                             (data$DATES.4!='D1' & data$DATES.4!='' & data$ELIMINATION.4 %like% "^R") |
                             (data$DATES.5!='D1' & data$DATES.5!='' & data$ELIMINATION.5 %like% "^R") |
                             (data$DATES.6!='D1' & data$DATES.6!='' & data$ELIMINATION.6 %like% "^R") |
                             (data$DATES.7!='D1' & data$DATES.7!='' & data$ELIMINATION.7 %like% "^R") |
                             (data$DATES.8!='D1' & data$DATES.8!='' & data$ELIMINATION.8 %like% "^R") |
                             (data$DATES.9!='D1' & data$DATES.9!='' & data$ELIMINATION.9 %like% "^R") |
                             (data$DATES.10!='D1' & data$DATES.10!='' & data$ELIMINATION.10 %like% "^R") 
                             , 1,0)


#howmanydistinctiveroses
data$rose1 = ifelse(data$ELIMINATION.1 %like% "^R",1,0) 
data$rose2 = ifelse(data$ELIMINATION.2 %like% "^R",1,0)
data$rose3 = ifelse(data$ELIMINATION.3 %like% "^R",1,0)
data$rose4 = ifelse(data$ELIMINATION.4 %like% "^R",1,0)
data$rose5 = ifelse(data$ELIMINATION.5 %like% "^R",1,0)
data$rose6 = ifelse(data$ELIMINATION.6 %like% "^R",1,0)
data$rose7 = ifelse(data$ELIMINATION.7 %like% "^R",1,0)
data$rose8 = ifelse(data$ELIMINATION.8 %like% "^R",1,0)
data$rose9 = ifelse(data$ELIMINATION.9 %like% "^R",1,0)
data$rose10 = ifelse(data$ELIMINATION.10 %like% "^R",1,0)

data <- dplyr::select(data, - ndistroses)

data$ndistroses= dplyr::transmute(data, ndistroses= rose1
              + rose2 +
                + rose3 +
                + rose4 +
                + rose5 +
                + rose6 +
                + rose7 +
                + rose8 +
                + rose9 +
                + rose10)

data$ndistroses= data$rose1 + data$rose2 + + data$rose3 + data$rose4 + data$rose5 + data$rose6 + data$rose7 + data$rose8 + data$rose9 + data$rose10

dplyr::count(data,ndistroses)

#subsetting to bach/bachelorettes
bachelor=dplyr::filter(data,bachelor==1)
bachelor=subset(data, select=c("CONTESTANT", "firstoneonone","oneononeearly","twoonone","groupdaterose","ndistroses","win","r1"))

bachelorette=dplyr::filter(data,bachelorette==1)
bachelorette=subset(data, select=c("CONTESTANT", "firstoneonone","oneononeearly","twoonone","groupdaterose","ndistroses","win","r1"))

#looking for nulls
apply(winners,2,function(x){sum(is.na(x))})

#logit model
logit_bachelor=glm(win~firstoneonone+oneononeearly+twoonone+groupdaterose+ndistroses+r1, data=bachelor, family=binomial)
summary(logit_bachelor)

logit_bachelorette=glm(win~firstoneonone+oneononeearly+twoonone+groupdaterose+ndistroses+r1, data=bachelorette, family=binomial)
summary(logit_bachelorette)


#lasso
formula = as.formula( ~ firstoneonone+oneononeearly+twoonone+groupdaterose+ndistroses)
mm <- model.matrix(formula,data=bachelor)

train.cvmn <- cv.glmnet(mm[ , -1], winners$win, family="binomial", nlambda=60, standardize=FALSE, type.measure="auc", nfold=10,alpha=.5)
cvmn <- glmnet(mm[ , -1], winners$win,family="binomial",alpha=.5,standardize = FALSE, lambda = train.cvmn$lambda.min)



