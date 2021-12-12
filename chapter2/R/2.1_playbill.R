# exercise 2.1 - playbill
playbill <- read.csv("data/playbill.csv")
fit <- lm(CurrentWeek ~ LastWeek, data = playbill)

# a - confidence interval for slope
est.slope <- as.numeric(fit$coefficients[2])
est.slope.se <- as.numeric(summary(fit)$coefficients[2, 2])
slope.confint <- est.slope + 
    c(-1, 1) * qt(0.975, df = nrow(playbill) - 2) * est.slope.se
print(slope.confint)
# [1] 0.9514971 1.0126658

# b - hypothesis test on intercept
est.intercept <- as.numeric(fit$coefficients[1])
est.intercept.se <- as.numeric(summary(fit)$coefficients[1, 2])
test.statistic <- (est.intercept - 10000) / est.intercept.se
p.value <- 2 * pt(test.statistic, df = nrow(playbill) - 2)
print(p.value)
# [1] 0.7517807

# c - prediction interval
newdata <- data.frame(LastWeek = 400000)
print(predict.lm(fit, newdata = newdata, 
                 interval = "prediction", level = 0.95))
#        fit      lwr      upr
# 1 399637.5 359832.8 439442.2

# d - plot prediction interval for all values x
png("chapter2/figures/playbill_d.png")
plot(playbill$LastWeek, playbill$CurrentWeek,
     xlab = "Gross Box Office Results Previous Week",
     ylab = "Gross Box Office Results Current Week")
abline(fit, col = "black")
summary(playbill$LastWeek)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  105698  491210  590876  622187  801578 1202536 
newx <- seq(105698, 1202536, 100)
newdata.pred.int <- data.frame(LastWeek = newx)
pred.int <- predict(fit, newdata = newdata.pred.int, 
                    interval = "prediction", level = 0.95)
lines(newx, pred.int[, 2], col = "red", lty = 2)
lines(newx, pred.int[, 3], col = "red", lty = 2)
abline(a = 0, b = 1, col = "blue")
dev.off()