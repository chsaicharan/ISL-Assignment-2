library(MASS)

# a)
mu.hat = mean(Boston$medv)
mu.hat

# b)
se.hat = sd(Boston$medv) /sqrt(dim(Boston)[1])
se.hat

# c)
set.seed(1)
boot.fn <- function(data, index) {
  mu <- mean(data[index])
  return (mu)
}
boot(Boston$medv, boot.fn, 1000)

# The bootstrap yields an estimate of 0.406 for the standard error of. This is fairly close to the estimate from c).

# d)
t.test(Boston$medv)

CI.mu.hat <- c(22.53 - 2 * 0.4119, 22.53 + 2 * 0.4119)
CI.mu.hat
# The bootstrap confidence interval is pretty close to the t.test() function's.

# e)
med.hat <- median(Boston$medv)
med.hat
# 21.2

# f)
boot.fn <- function(data, index) {
  mu <- median(data[index])
  return (mu)
}
boot(Boston$medv, boot.fn, 1000)
# We acquire an estimated median value of 21.2, which is the same as the value obtained in (e), with a standard error of 0.3770, which is modest when compared to the median value.

# g)
percent.hat <- quantile(Boston$medv, c(0.1))
percent.hat
# 12.75

# h)
boot.fn <- function(data, index) {
  mu <- quantile(data[index], c(0.1))
  return (mu)
}
boot(Boston$medv, boot.fn, 1000)
# We receive an estimated tenth percentile value of 12.75, which is the same as the number obtained in (g), with a standard error of 0.4925, which is tiny when compared to the percentile value.