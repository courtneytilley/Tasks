source("http://jonsmitchell.com/code/fxn05.R")
Pop1 <- simPop(Popsize = 50, nGenerations = 100, initial_p = 0.5, h = 1, s = 0)
plot(1:nrow(Pop1), Pop1[,1], ylim=c(0, 1), type = "l", xlab="generation", ylab="allele freq.", lwd='red'
lines(1:nrow(Pop1), Pop1[,2], lwd=2, col='red')
legend("topleft", legend = c("a", "b"), col = c("black", "red"), lwd = 2, bty="n")
plotFit( nruns = 10, n = 50, ngens = 100, init_p = 0.5, h = 1, s = 0)
Expectation <- c(10, 10, 10, 10)
Observed <- c(15, 15, 5, 5)
Chisq <- sum(((Expectation - Observed) ^ 2) / Expectation)
barplot(rbind(Expectation, Observed), beside = T, main = bquote(chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))
Observed <- c(5, 0, 0, 35)
Chisq <- sum(((Expectation - Observed) ^2) / Expectation)
barplot(rbind(Expectation, Observed), beside = T, , main = bquote(chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))
Observed <- c(2, 3, 10, 30)
Chisq <- sum(((Expectation - Observed) ^2) / Expectation)
barplot(rbind(Expectation, Observed), beside = T, , main = bquote(chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))
The x value when observed is 10. This number is same for all the observations in the category. The closer the bars on the bar graph are to the observed X^2 value, the bars are more even. 
results <- read.csv("http://jonsmitchell.com/data/biol112labresults.csv", stringsAsFactors=F)
counts <- results[,c("yellow", "red", "green", "blue", "black", "tan")]
backgrounds <- c("White", "Red", "Yellow", "Green", "Blue", "Black")
backgroundCol <- c("white", "#d53e4f", "#fee08b", "#abdda4", "#3288bd", "black")
calcChi(counts[1,])
Chisqs <- apply(counts, 1, calcChi)
plotChis(counts)
When the chi squared is high, the bars are more uneven. When the chi squared is low, the bars are even. plotChis() shows that the higher the chi squared value, the further away from the actual value when the lower the chi squared value the closer to the measurment that is actually observed.
Avg <- mean(Chisqs)
The mean average is higher than the critcal value.
The value differs by background of inputs that are different values for selection.
backgroundAvgs <- tapply(Chisqs, results[,3], mean)
propSig <- length( which( Chisqs > 11.70) ) / length(Chisqs)
percSig <- round(100 * propSig)
The number does not surprise me.
Because the number is so high I think natural selection and other factors may be playing into it. 
par(las = 1, mar = c(4, 4, 1, 1), mgp = c(2, 0.5, 0), tck = -0.01, cex.axis=1)
hist(Chisqs, main="", xlab="chi-squared values", ylab="frequency")
par(las = 1, mar = c(4, 4, 1, 1), mgp = c(2, 0.5, 0), tck = -0.01, cex.axis=1)
plot(1, 1, xlim=c(0, 400), ylim=c(1, 8.5), xlab="", ylab="", type="n", yaxt="n")
axis(2, at = 1:length(backgrounds), labels = backgrounds)
mtext(side=1, expression(chi^2), cex=1.75, line=2.5)
counter <- 1
for (i in backgrounds)  {
  Data <- Chisqs[which(results[,3] == i)]
  addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
  counter <- counter + 1
}
abline( v = 11.70, lyt=2, lwd = 2, col='black')
There was not many meaningful differences. 
Simulation <- simDraws(10000)
addHist(Y=7, Dat=Simulation, Color="lightgray")
mtext(side=2, at=7, line=0, "simulated")
abline(v = 11.70, lyt=2, lwd=2)

Fit <- c(1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation2 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0,0,0,0.25))
Fit <- c(0.1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation3 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation3, Color=rgb(0,0,0,0.25))
Fit <- c(0.5, 0.6, 0.7, 1, 1, 1)
names(Fit) <- 1:6
Simulation4 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation4, Color=rgb(0,0,0,0.25))
Fit <- c(0.1, 0.2, 0.3, 0.4, 0.5, 1)
names(Fit) <- 1:6
Simulation5 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation5, Color-rgb(0,0,0,0.25))
Fit <- c(0.1, 0.1, 0.1, 0.1, 0.1, 1)
names(Fit) <- 1:6
Simulation6 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation6, Color=rgb(0,0,0,0.25))
mtext(side=2, at=8, line=0, "sel. sim.")
Simulation7 <- c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist(Y=8, Dat=Simulation7, Color=rgb(0, 0, 1, 0.25))
The mixture compared to the student generated data is very similar. Most student groups do not show strong selection. I would say that there is natural selection happening with certain colors in regards to what colors get picked. 
The evolutionary processes at work in the lab-as-done-by-humans is variation of genes, natural selection and fitness in the selection of toothpicks. 
The evolutionary process at work in the lab-as-simulated-by-the-computer is natural selection.
The graph shows that the strengths of the processes are varied with strong and weak.
The student numbers when compared to the critical value are closer to the natural process than the simulated numbers.
Adding the possibility for a toothpick to mutate into a different type would increase the chi squared value with each mutation. 
data(simDraws)
simDraws <- function(nruns, ncols = 6, ndtart = 20, nrounds = 3, mu = 0, twoway = TRUE, w = NULL){
  Chiout <- c()
  for (j in 1:runs){
    if (is.null(w))
      Draws <- sample(Pop, 20, replace = T)
}
}
else if (!.null(w)) {
  if (length(setdiff(unique(Pop), names(w))) !=0)
    Draws <- sample(Pop, 20, replace = T, prob = w[Pop])
}
else if (length(setdiff(unique(Pop), names(w))) !=0) {
  cat("fitnessvalues", setdiff(unique(Pop), names(w)))
}
Pop <- sort (c(Draws,Draws,Draws))
Summary <- c()
for (k in 1:ncols)
Summary [k] <- length(which(Pop == k))
Chiout[j] <- sum(((Summary - nstart) ^2) / nstart)
return(Chiout)
Simulation8 <- simDraws(1e4, mu=2)
addHist(Y=8, Dat=Simulation8, Color=rgb(0, 0, 0, 0.25))
Simulation9 <- simDraws(1e4, mu=300)
addHist(Y=8, Dat=Simulation9, Color=rgb(0, 0, 0, 0.25))