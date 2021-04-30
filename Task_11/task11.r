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
for(i in 1:100) {
  z[i]<-runif(1)
  y<- (x* z[i]) + 2 +(rnorm(100, 0:0.1))
  l<-coef(lm(z[1:100]~y))
}
pdf("2.pdf", height=4, width=4)
plot(z[1:100], y)
abline(lm(y~z[1:100]))
dev.off()
pdf("3.pdf", height=4, width=4)
plot(c(z, -0.03))
dev.off()
install.packages('meme')
library('meme')
u <-https://www.pinterest.com/pin/225602262557834822/
my_meme <- meme(u, lower= "when you get error again after the 10th try", color="white", size="2.0")  
