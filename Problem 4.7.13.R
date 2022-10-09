library(ISLR)
library(corrplot)

head(Weekly)

# a)
summary(Weekly)
pairs(Weekly)
cor(Weekly[-9])
corrplot(cor(Weekly[,-9]), method="square")

# b)
Weekly.fit<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly,family=binomial)
summary(Weekly.fit)
# Lag2 is the only variable that was statistically significant at the α =0.05 level of significance. Otherwise, the additional variables are insufficient to reject the null hypothesis; β= 0.

# c)
logWeekly.prob= predict(Weekly.fit, type='response')
logWeekly.pred =rep("Down", length(logWeekly.prob))
logWeekly.pred[logWeekly.prob > 0.5] = "Up"
table(logWeekly.pred, Weekly$Direction)

mean(logWeekly.pred == Weekly$Direction)
# This shows that the model properly forecasted the weekly market trend 56.11% of the time. Differentiating between how well the model forecasts up and down trends. The model successfully anticipated the Up weekly trends 92.07% of the time. Down weekly trends, on the other hand, were predicted at a lower rate, with only 11.15% accurately predicted.

# d)
attach(Weekly)
train = (Year<2009)
Weekly.0910 <-Weekly[!train,]
Weekly.fit<-glm(Direction~Lag2, data=Weekly,family=binomial, subset=train)
logWeekly.prob= predict(Weekly.fit, Weekly.0910, type = "response")
logWeekly.pred = rep("Down", length(logWeekly.prob))
logWeekly.pred[logWeekly.prob > 0.5] = "Up"
Direction.0910 = Direction[!train]
table(logWeekly.pred, Direction.0910)

mean(logWeekly.pred == Direction.0910)
# When the entire Weekly dataset was divided into a training and test dataset, the model successfully predicted weekly trends at a rate of 62.5%, which is a moderate improvement over the model that used the entire dataset. This model, like the previous one, predicted upward trends better (91.80%) than downward trends (20.93%); nevertheless, this model was able to greatly improve in properly predicting downward trends.

# e)
library(MASS)
Weeklylda.fit<-lda(Direction~Lag2, data=Weekly,family=binomial, subset=train)
Weeklylda.pred<-predict(Weeklylda.fit, Weekly.0910)
table(Weeklylda.pred$class, Direction.0910)

mean(Weeklylda.pred$class==Direction.0910)
# Using Linear Discriminant Analysis to generate a classifying model produced comparable results as the logistic regression model created in section D.

# f)
Weeklyqda.fit = qda(Direction ~ Lag2, data = Weekly, subset = train)
Weeklyqda.pred = predict(Weeklyqda.fit, Weekly.0910)$class
table(Weeklyqda.pred, Direction.0910)

mean(Weeklyqda.pred==Direction.0910)
# Quadratic Linear Analysis produced a model with a lesser accuracy of 58.65% than the prior methods. Furthermore, this model only examined forecasting the validity of weekly rising trends while disregarding weekly downward trends.

# g)
library(class)
Week.train=as.matrix(Lag2[train])
Week.test=as.matrix(Lag2[!train])
train.Direction =Direction[train]
set.seed(1)
Weekknn.pred=knn(Week.train,Week.test,train.Direction,k=1)
table(Weekknn.pred,Direction.0910)

mean(Weekknn.pred == Direction.0910)
# The K-Nearest Neighbors algorithm produced a classifying model with a 50% accuracy rate, which is equal to random chance.

# h)
library(e1071)
nbayes=naiveBayes(Direction~Lag2 ,data=Weekly ,subset=train)
nbayes
nbayes.class=predict(nbayes ,Weekly.0910)
table(nbayes.class ,Direction.0910)
# The likelihood of down is 0.4477157, and the probability of up is 0.5522843. As we can see, our model correctly predicted that the market will rise on 61 days and fall on 0 days. This suggests that the naïve bayes model predicted correctly 58.65% of the time, the same as the qda model.

# i)
# Logistic Regression and Linear Discriminant Analysis have the highest accuracy rates, both with rates of 62.5%.