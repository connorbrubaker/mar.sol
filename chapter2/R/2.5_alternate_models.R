# exercise 2.5 - alternative models
set.seed(34)

# approximate slope and intercept of Model 1
beta0_1 <- -0.5; beta1_1 <- 1
# approximate slope and intercept of Model 2
beta0_2 <- 5.75; beta1_2 <- -0.125

# the variance of the error terms in Model 1 is 
# clearly much less than that in Model 2
sigma1 <- 0.75; sigma2 <- 2.5

# simulate data from the two population regression lines
n <- 100
x <- runif(n, min = 0, max = 10)
model1 <- beta0_1 + beta1_1 * x + rnorm(n, sd = sigma1)
model2 <- beta0_2 + beta1_2 * x + rnorm(n, sd = sigma2)

# plot the simulated data
par(mfrow = c(1, 2))
plot(x, model1, xlab = "x1", ylab = "y", main = "Model 1")
abline(a = beta0_1, b = beta1_1)
plot(x, model2, xlab = "x2", ylab = "y", main = "Model 2")
abline(a = beta0_2, b = beta1_2)

# fit a lm model to each data set
lm1 <- lm(model1 ~ x); lm2 <- lm(model2 ~ x)

# compute RSS for each model
rss1 <- sum((model1 - fitted(lm1))^2)
rss2 <- sum((model2 - fitted(lm2))^2)
print(paste("RSS Model 1 = ", rss1, "; RSS Model 2 = ", rss2, 
            sep = ""))
# [1] "RSS Model 1 = 61.3335369661467; ...
# RSS Model 2 = 668.428701788848"

# compute SSreg for each model
ssreg1 <- sum((fitted(lm1) - mean(model1))^2)
ssreg2 <- sum((fitted(lm2) - mean(model2))^2)
print(paste("SSreg Model 1 = ", ssreg1, "; SSreg Model 2 = ", ssreg2, 
            sep = ""))
# [1] "SSreg Model 1 = 896.784854943098; ...
# SSreg Model 2 = 18.6246686979669"