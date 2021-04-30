setwd('~/Desktop/Evolution/Tasks/Task_11')
x <- rnorm(100, mean=5, sd=2)
x
y <- (x *5) + 2 + (rnorm(100, 0:0.1 ))
y
plot(x, y)
abline(lm(y~x), col='red')
coef(lm(y~x))
y intercept = 2.193123 x intercept = 4.989395
z <- c()
x <- rnorm(100, mean=5, sd=2)
