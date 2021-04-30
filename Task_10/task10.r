setwd('~/Desktop/Evolution/Tasks/Task_10')
install.packages('diversitree')
library('diversitree')
transition_0to1 <- 0.1
transition_1to0 <- 0.1
speciation_0 <- 0.2
extinction_0 <- 0.15
speciation_1 <- 0.4
extinction_1 <- 0.1
maxN <- 1e3
maxT <- 50
Pars <- c(speciation_0, speciation_1, extinction_0, extinction_1, transition_0to1, transition_1to0)
simTree <- tree.bisse(Pars, max.taxa = maxN, max.t = maxT)
str(simTree)
#?tree.bisse()
stateTable <- table(simTree$tip.state)
stateTable / sum(stateTable)
Frequencies <- c('State0', 'State1')
Colors <- c('yellow', 'pink')
Data <- matrix(c(0.2, 0.25, 0.26, 0.29, 0.34, 0.39, 0.40, 0.3, 0.43, 0.33, 0.33, 0.49), nrow = 2, ncol = 6, byrow=TRUE)
Question 1: There would need to be an inverse relationship between the net diversification and frequency of state 1 and lower speciation rate for state 1 to be high with lower diversification.  
Difference <- c(0.02, 0.03, 0.05, 0.1, 0.15, 0.2) 
Freq1 <- c(0.2, 0.23, 0.24, 0.25, 0.31, 0.456)
Freq0 <- c(0.35, 0.45, 0.57, 0.657, 0.68, 0.70)
pdf('Q1.pdf', height=6, width=6) 
barplot(Data, names.arg=Difference,main = 'Changes in Frequency of States based on Variation in R Values',xlab = 'Difference in Diversification Rate',ylab = 'Frequency',beside=TRUE,col = c('yellow', 'red'))
legend('top', Frequencies, fill='yellow', 'red')
dev.off()
Question 2: When the net diversification rate of state 0 was increased the state 1 frequency was close but never 0. 
Freq <- c('State0', 'State1')
Data <- matrix(c(0.05, 0.0018, 0.0024, 0.039, 0.048, 0.18, 0.3, 0.46, 0.7, 0.95, 0.99))
Data
Difference <- c(0.05, 0, 0.2, 0.3, 0.4, 0.5)
pdf('Question2.pdf', height=8, width=8)
barplot(Data, names.arg=Difference,
        main='Close to Zero State 1 Gets When Transition Rate is Nonzero',
        xlab='Difference in Diversification Rate',
        ylab='Frequencies',
        col=c('yellow', 'pink')
)
legend('topright', frequencies, fill='green', 'pink')
dev.off()
Question 3: There was a little but not much variance in frequency. 
Data <- read.csv('/Users/courtneytilley/Desktop/Evolution/Tasks/Task_10/q3_data.csv', stringsAsFactors=F)
head(Data)
Freq1_Trial1 <- Data[,2]
Freq1_Trial2 <- Data[,5]
Freq1_Trial3<- Data[,8]
Variance1 <- var(Freq1_Trial1)
Variance2 <- var(Freq1_Trial2)
Variance3 <- var(Freq1_Trial3)
Variance1
Variance2
Variance3
VarianceMatrix <- c(Variance1, Variance2, Variance3)
VarianceMatrix
Trial <- c(1, 2, 3)
pdf('Question3.pdf', height=8, width=8)
barplot(TotalVarianceMatrix, names.arg=Trial, ylim=c(0, 0.5), xlab='Trial Number', ylab='Variance in Differnt Frequencies', col='blue')
dev.off()
Question 4: Drift and Selection can play a big role in altering frequency of state 1.
Data 
Freq0 <- Data[,2]
Freq0
NetDivR0 <-Data[,1]
pdf("Trend1.pdf", height=8, width=8)
plot(NetDivR0, Freq0, main='Change in Frequency of State 0 Based on Net Diversification Rate', xlab='Net Diversification Rate of State 0', ylab='Frequency of State 0')
abline(lm(Frequencyof0~NetDivR0), col='green', lty='dashed')
dev.off()
Freq1<-Data[,7]
NetDivR1<-Data[,5]
pdf("Trend2.pdf", height=8, width=8)
plot(NetDivR1, Freq1, main='Change in Frequency of State 1 Based on Net Diversification Rate', xlab='Net Diversification Rate of State 1', ylab='Frequency of State 1')
abline(lm(Freqof1~NetDivR1), col='red', lty='dashed')
dev.off()


