library(ISLR)

# a)
set.seed(1)
fit.glm = glm(default ~ income + balance, data = Default, family = "binomial")
summary(fit.glm)

# b)
# i)
train = sample(dim(Default)[1], dim(Default)[1] / 2)

# ii)
fit.glm = glm(default ~ income + balance, data = Default[train,], family = "binomial")
fit.glm = glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
#Both above formulas have the same outcome.
summary(fit.glm)

# iii)
glm.probs = predict(fit.glm, newdata = Default[-train, ], type="response")
glm.pred=rep("No",5000)
glm.pred[glm.probs>0.5] = "Yes"

# iv)
mean(glm.pred != Default[-train, ]$default)


# c)
train <- sample(dim(Default)[1], dim(Default)[1] / 2)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", length(probs))
pred.glm[probs > 0.5] <- "Yes"
mean(pred.glm != Default[-train, ]$default)

train <- sample(dim(Default)[1], dim(Default)[1] / 2)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", length(probs))
pred.glm[probs > 0.5] <- "Yes"
mean(pred.glm != Default[-train, ]$default)

train <- sample(dim(Default)[1], dim(Default)[1] / 2)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm <- rep("No", length(probs))
pred.glm[probs > 0.5] <- "Yes"
mean(pred.glm != Default[-train, ]$default)

# We can see that the validation estimate of the test error rate can vary based on which observations are in the training set and which observations are in the validation set.

# d)
train <- sample(dim(Default)[1], dim(Default)[1] / 2)
fit.glm <- glm(default ~ income + balance + student, data = Default, family = "binomial", subset = train)
pred.glm <- rep("No", length(probs))
probs <- predict(fit.glm, newdata = Default[-train, ], type = "response")
pred.glm[probs > 0.5] <- "Yes"
mean(pred.glm != Default[-train, ]$default)
# Adding the "student" dummy variable does not appear to reduce the validation set estimate of the test error rate.