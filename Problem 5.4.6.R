library(boot)

# a)
set.seed(1)
train <- sample(dim(Default)[1], dim(Default)[1] / 2)
fit.glm <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
summary(fit.glm)
# The standard errors estimated by glm() for the coefficients β0, β1 and β2 are respectively 0.4347564, 4.9851672 x 10^-6 and 2.2737314 x 10^-4.

# b)
boot.fn <- function(data, index) {
  fit <- glm(default ~ income + balance, data = data, family = "binomial", subset = index)
  return (coef(fit))
}

# c)
boot(Default, boot.fn, 1000)
# The bootstrap estimates of the standard errors for the coefficients β0, β1 and β2 are respectively 0.4239, 4.583 x 10^-6  and 2.268 x 10^-).

# d)
# The bootstrap standard errors appear to be very close to those calculated using the statistical formulae underlying the glm() function.