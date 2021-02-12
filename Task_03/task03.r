trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)
Size <- 50
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)
The samples and populations are different. 
boxplot(Sample1, Sample2)
source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma <- makeFounder("grandma_mom")
MatGrandpa <- makeFounder("grandpa_mom")
PatGrandma <- makeFounder("grandma_da")
PatGrandpa <- makeFounder("grandpa_da")
Alan <- makeBaby(PatGrandma, PatGrandpa)
Brenda <- makeBaby(MatGrandma, MatGrandpa)
Focus <- makeBaby(Brenda, Alan)
I think the number will be 20,000/2. 
ToMom <- length( grep("mom", Focus ) ) / length( Focus )
It didn't match exactly. 
ToMomMom <- length( grep( "grandma_mom", Focus ) ) / length( Focus )
ToMomDad <- length( grep( "grandpa_mom", Focus ) ) / length( Focus )
I sm guessing 0.25. The focus is not equally related. 
Sibling_01 <- makeBaby(Brenda, Alan)
I think they would share 50% of the DNA since they are siblings. 
ToSib <- length( intersect( Focus, Sibling_01 ) ) / length( Focus )
0.43
ManySiblings <- replicate( 1e3, length( intersect( Focus, makeBaby(Brenda, Alan) ) ) / length( Focus ) )
quantile(ManySiblings)
mean(ManySiblings)
plot(density(ManySiblings), main="", xlab="proportion shared genes")
There is a range because it is all random between the genes shared by focus and the siblings. 
HWE <- function(p) { 
aa <- p^2
ab <- 2 * p * (1 - p)
bb <- (1 - p)^2
return(c(aa=aa, ab=ab, bb=bb))
}
HWE(0.5)
plot(1, 1, type="n", xlim=c(0, 1), ylim=c(0, 1), xlab="freq. allele a", ylab="geno. freq")
p <- seq(from = 0, to = 1, by = 0.01)
GenoFreq <- t(sapply(p, HWE))
lines(p, GenoFreq[,"aa"], lwd=2, col="red")
Yes, The frequency of an allele increases and what we expect increases also.
lines(p, GenoFreq[,"ab"], lwd=2, col="purple")
lines(p, GenoFreq[, "bb"], lwd=2, col="blue")
legend("top", legend=c("aa", "ab", "bb"), col=c("red", "purple", "blue"), lty=1, lwd=2, bty="n")
Pop <- simPop(500
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")
The frequency of aa almost matches the expectation but not quiet.
Pop <- simPop(50)
points(Pop[,"freqa"], Pop[,"genotypes.aa"]/50, pch=22, bg="red")
The frequency increases because the popuation is smaller. 
install.packages("learnPopGen")
x <- genetic.drift(Ne=200, nrep=5, pause=0.01)
PopSizes <- 5:50
Samples <- rep(PopSizes, 5)
tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))
Line <- lm(tExt ~ Samples)
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)
As the population increases the points mave away from the line. This could mean they are less likely to go extinct. 
install.packages('lmtest')
install.packages('sandwich')
summary(Line)
bptest(Line)
bptest(Line2)
coeftest(Line, vcov = vcovHC(Line))
install.packages('robustbase')
Linerob <- lmrob(tExt~Samples)
summary(Linerob)
plot(Samples, tExt)
abline(Line)
abline(Linerob)
plot(Samples, tExt)
abline(Line, col='red')
abline(Linerob, col='blue')
legend(x=20, y=400, legend=c('Robust', 'Linear'), col=c('blue', 'red'), lwd=1.2, cex=0.8)
dev.off()
