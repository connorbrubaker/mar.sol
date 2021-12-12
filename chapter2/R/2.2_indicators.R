# exercise 2.2 - indicators
# had to make modifications to the original data file to read in properly
indicators <- read.delim("data/indicators.txt", sep = "\t", header = TRUE)
fit <- lm(PriceChange ~ LoanPaymentsOverdue, data = indicators)

# a - confidence interval for slope
est.slope <- as.numeric(fit$coefficients[2])
est.slope.se <- as.numeric(summary(fit)$coefficients[2, 2])
slope.confint <- est.slope + 
    c(-1, 1) * qt(0.975, df = nrow(indicators) - 2) * est.slope.se
print(slope.confint)
# [1] -4.1634543 -0.3335853

# b - confidence interval for predicted average value at x = 4
xnew <- 4
x <- indicators$LoanPaymentsOverdue
sxx <- sum((x - mean(x))^2)
est.intercept <- as.numeric(fit$coefficients[1])
est.resid.se <- summary(fit)$sigma
pop.line.confint <- est.intercept + est.slope * xnew + 
    c(-1, 1) * qt(0.975, df = nrow(indicators) - 2) *
    est.resid.se * sqrt(
        (1 / nrow(indicators)) + ((xnew - mean(x))^2 / sxx)
    )
print(pop.line.confint)
# [1] -6.648849 -2.310322

# alternate - using predict
print(predict(fit, newdata = data.frame(LoanPaymentsOverdue = 4), 
              interval = "confidence"))
#         fit       lwr       upr
# 1 -4.479585 -6.648849 -2.310322