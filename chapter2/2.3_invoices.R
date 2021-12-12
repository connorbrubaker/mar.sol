# exercise 2.3 - invoices
# a - confidence interval for intercept
est.intercept <- 0.6417099
est.intercept.se <- 0.1222707
df <- 28
intercept.confint <- est.intercept + 
    c(-1, 1) * qt(0.975, df = df) * est.intercept.se
print(intercept.confint)
# [1] 0.3912497 0.8921701

# b - hypothesis test on slope
est.slope <- 0.0112916
est.slope.se <- 0.0008184
test.statistic <- (est.slope - 0.01) / est.slope.se
p.value <- 2 * pt(test.statistic, df = df, lower.tail = FALSE)
print(p.value)
# [1] 0.1257517

# c - point estimate and prediction interval
point.estimate <- est.intercept + est.slope * 130
est.resid.se <- 0.3298
sxx <- (est.resid.se / est.slope.se)^2
pred.int <- point.estimate + c(-1, 1) * qt(0.975, df = df) * est.resid.se * 
    sqrt((1 / 30) + ((130 - 130)^2 / sxx))
print(pred.int)
# [1] 0.6308769 3.5883589