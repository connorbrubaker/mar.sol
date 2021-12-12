# exercise 2.1 - playbill
playbill <- read.csv("data/playbill.csv")
fit <- lm(CurrentWeek ~ LastWeek, data = playbill)

# a - confidence interval for slope
est.slope <- as.numeric(fit$coefficients[2])
est.slope.se <- as.numeric(summary(fit)$coefficients[2, 2])
slope.confint <- est.slope + c(-1, 1) * qt(0.975, df = 16) * est.slope.se
print(slope.confint)
# [1] 0.9514971 1.0126658

# b - hypothesis test on intercept
est.intercept <- as.numeric(fit$coefficients[1])
est.intercept.se <- as.numeric(summary(fit)$coefficients[1, 2])
test.statistic <- (est.intercept - 10000) / est.intercept.se
p.value <- 2 * pt(test.statistic, df = 16, lower.tail = TRUE)
print(p.value)
# [1] 0.7517807

# c - prediction interval
newdata <- data.frame(LastWeek = 400000)
print(predict.lm(fit, newdata = newdata, interval = "prediction", level = 0.95))
#        fit      lwr      upr
# 1 399637.5 359832.8 439442.2