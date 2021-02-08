setwd('/Users/courtneytilley/Desktop/Evolution/Tasks/Task_02'')
Data <- read.csv('http://jonsmitchell.com/data/beren.csv', stringsAsFactors=F)
write.csv(Data, 'rawdata.csv', quote=F)
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)
Data[1 ,]
Data[2 ,]
Data[1:3 ,]
Data[1:3 , 4]
Data[1:5 , 1:3]
Data[257]
Feeds <- which(Data[,9] == 'bottle')
berenMilk <- Data[Feeds ,]
head(berenMilk)
There are 2 sets with 6 rows each, the rows are for the time and dates the child was given a bottle. The second set is the amount of formula and who gave the bottle. 
Feeds <- which(Data[,'event'] == 'bottle')
Feeds <- which(Data$event == 'bottle')
All functions use (which) as a function to mark certain chracteristics. Data and event afterwards targets specific set of data and the ==bottle tells R that we want to pinpoint the evnets that include a bottle. 
dayID <- apply(Data, 1, function(x) paste(x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
head(Data)
beren2 <- Data
beren3 <- beren2[order(beren2$age) ,]
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)
Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds]
beren <- read.csv("http://jonsmitchell.com/data/beren.csv", stringsAsFactors=F)
dayID <- apply(beren, 1, function(x) paste(x[1:3], collapse="-"))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
beren$age <- dateID - dateID[which(beren$event == "birth")]
beren2 <- beren[order(beren$age),]
beren2$value <- as.numeric(beren2$value)
beren3 <- beren2
Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds])
avgMilk
The unit for avgMilk is ounces.The value column was used to show the numerical value that went with the "Feeds" data. 
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
avgFeed
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
beren3$age[Feeds]
summary(berenCor)
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
boxplot(beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab = "who gave the bottle", ylab = "amount of milk consumed (oz)")
?par
las = edited axis labels
mar = margins that should be used 
mgp = margins that should be used for title and axis 
tck = edited length of tick marks
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab= "age in days", ylab= "ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
pdf("r02b-totalMilkByDay.pdf", height=4, width=4)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0,5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()
beren4 <- beren3[Naps,]
startHour <- (beren4$start_hour)
startMin <- (beren4$star_minute)
stopHour <- (beren4$end_hour)
stopMin <- (beren4$end_minute)
diaper <- which(Data[,9] == 'bowel')
avgdiaper <- mean(beren3$value[diaper])
sum(Feeds)
sum(diaper)
sum(diaper, Feeds)
totaldiaper <- sum(diaper, Feeds)
avgFeeds <- tapply(beren3$value[diaper], beren3$event[Feeds], mean)
diaper <- which(Data[,9] == 'diaper')
diaper
Feeds
totalFeeds <- sum(Feeds)
totaldiaper <- sum(diaper)
plot(totalFeeds, totaldiaper)
