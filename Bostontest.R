a. What statistical techniques did you apply to create your model? Why did you choose these
over other methods?
b. How did you select the three factors you used? How did you decide when to reject others? 
c. How accurate is your model? What metrics did you use to make this assessment?
d. What additional data do you think would make this model more accurate?
In addition to the written description, please submit all of the code you wrote for this exercise (verbose comments are highly encouraged).  You don’t need spend a great deal of time tuning your model once you have something you’re satisfied with conceptually  - the goal here is not for you to invest hours of your time in an overfitting exercise - rather, we want to see how you think about the process of ingesting a dataset, turning it into a predictive model, and reporting on the overall quality of the model you’ve created.

#The model chosen (rmse.rf), is a random forest model that has 3 independent variables to predict the median housing price in Boston.
#I used a lasso to look at all the coefficients of the features and also to look at the interactions that would be useful and 
#reached the best linear model (fit2) after different tries. Afterwards i tried the Random Forest which helps reduce over fitting,
#and while it wasn't a much better lift from from fit2, it still had a higher r2. That wasn't surprising since the three factors I found to be most predictive
#were rm, lstat, ptratio and their interactions, (black was predictive only in an interaction), so deeper interactions of those features led to the random forest model having the best fit.
#I selected the three factors I used using an iterative process. First I looked at a model using all the available features, and their interactions, looked at what had high p-values(and particularly high standard errors like with nox) and rejected those. Second, I looked at the variable inflation factors of the features, and if they were too high, I removed the feature. 
#Third, I played with the different features to determine which provided the best model. If i wasn't restricted to three factors I would probably try to use a quadratic for lstat, but in that case I would use lstat and lstat^2, so did not go with that idea. Overall, after trial and error, I chose what gave me the best fit for the linear model, then did the same for the random forest model.
#The model is relitively accurate with an R-squared of 0.78, and rmse of of 3.6. Also when looking at the residual plots from the linear model fit(2), there were no signs of collinearity or heteroscedasticity usign these features.
#In terms of additional data, population density, employment rate, a form of age breakdown, whether there are small business in the area, could all be features that would make the model more accurate.
#code is below!


#pulling in libraries
library(ISLR)
library(MASS)
library(car)
library(sciplot)

#attaching so i don't have to df$column
attach(Boston)
class(Boston)
?Boston

names(Boston)
#to break down the plot space into 4
par(mfrow=c(2,2))

#looking at the distribution of the housing prices to see if it is skewed
hist(medv)
#skewed to the right, log might be needed

#just playing with the data and looking relationships
plot(medv~lstat, pch=1)
plot(medv~age, pch=1)
plot(medv~tax, pch=1)
plot(medv~crim, pch=1)
plot(medv~rm, pch=1)

#looking at the correlation between each column and the median value of housing prices, ordered ascendingly
cor(Boston,medv)[order(cor(Boston,medv)),]

#creating training and testing sets
Boston$holdout = runif(nrow(Boston))
train <- Boston[Boston$holdout < .7, ]
test <- Boston[Boston$holdout > .7,]


fit1linear <- lm(medv~.,data = train)
summary(fit1linear)
coef(fit1linear)
#looking at the coefficients of all the features
sort(fit1linear$coefficients,decreasing = T)
plot(fit1linear)
#zn, chas, age are not statistically sig, nox has v high std. error
summary(fit1linear)
#tax and rad have v high variable inflation
vif(fit1linear)
#looking at the test set so we can further check the model
pred.fit1linear <- predict(fit1linear, newdata = test)
# looking at accuracy by checking the mse and r^2
sqrt(sum((pred.fit1linear - test$medv)^2)/
       length(test$medv))
summary(fit1linear)$r.squared


#also looking at a lasso and squaring the formula predictors to look at all possible interactions
#trying Lasso squared to see the interactions
formula = as.formula(medv ~ (.)^2)
#trying lasso not squaring to see all the features
formula = as.formula(medv ~ (.))

mm <- model.matrix(formula,data=train)
mmtest <- model.matrix(formula, data=test)
cv.glmnet(medv, dep0, family="gaussian", nfolds = 5, alpha = 0 )
train.cvmn <- cv.glmnet(mm[ , -1], train$medv, family="gaussian", nlambda=60, standardize=FALSE, type.measure="mse", nfold=10,alpha=.5)
cvmn <- glmnet(mm[ , -1], train$medv,family="gaussian",alpha=.5,standardize = FALSE, lambda = train.cvmn$lambda.min)
plot(cvmn)

coef <- coef(cvmn, s=cvmn$lambda.1se)[order(abs(coef(cvmn, s=cvmn$lambda.1se)), decreasing = TRUE)]
names <- rownames(coef(cvmn, s=cvmn$lambda.1se))[order(abs(coef(cvmn, s=cvmn$lambda.1se)), decreasing = TRUE)][1:30]
for (iii in 1:30){
  cat(names[[iii]],"\t",coef[[iii]],"\n")
}

#from the lasso i can see that rm:black, rm:tax, nox:tax, chas:tax, crim:lstat, ptratio:black, ptratio:lstat have the highest abs coef
#trying new linear model with the interactions i got from lasso, that are reasonable

#new linear model limiting to only reasonable features and interaction
fit2= lm(medv ~ rm*black + lstat + log(rm), data=train)
summary(fit2)
sort(fit2$coefficients,decreasing = T)
vif(fit2)

#looking at the test set so we can further check the model
pred.fit2 <- predict(fit2, newdata = test)
#sanity check comparing two models
anova(fit1linear,fit2)

# looking at accuracy by checking the mse and r^2
sqrt(sum((pred.fit2 - test$medv)^2)/
       length(test$medv))
summary(fit2)$r.squared
plot(fit2)


#Trying random forest and played with lots of the variables
library(randomForest)
# rfmodel <- randomForest(formula = log(medv) ~ rm + lstat + ptratio , data = train)
rfmodel <- randomForest(formula = medv ~ rm + lstat + ptratio , data = train)
pred.rfmodel <- predict(rfmodel, test)

sqrt(sum(((pred.rfmodel) - test$medv)^2)/
                  length(test$medv))
mean(rfmodel$rsq)


