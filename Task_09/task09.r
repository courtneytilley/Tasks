library ('phytools')
tree <- read.tree('https://jonsmitchell.com/data/anolis.tre')
plot(tree, type='fan')
tree$tip.label
Question 1 - There si 81 tree tips present and there are branch lengths present.
data <- read.csv('https://jonsmitchell.com/data/svl.csv', stringsAsFactors=F, row.names=1)
data
data[,1]
Question 2 - This shows the name/species of each individual lizard and the snout-vent length of each lizard. There is 100 dimensions 
dim(data)
typeof(data)
svl <- setNames(data$svl, rownames(data))
Ancestors <- fastAnc(tree, svl, vars=TRUE, CI=TRUE)
Ancestors
?fastAnc
Question 3 - The estimated values stored are in a list containing the estimates. The CI95 element is the 95% confidence interval of the estimates stored.
Question 4 - The two assumptions made about the estimate is the function re-roots the tree at the internal nodes and finds the contrast state at the root each time.
par(mar=c(0.1,0.1,0.1,0.1))
plot(tree, type="fan", lwd=2, show.tip.label=F)
tiplabels(pch=16, cex=0.25*Ancestors$ace)
nodellabels(pch=16, cex=0.25*svl[tree$tip.label])
obj <- contMap(tree, svl, plot=F)
plot(obj, type="fan", legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7,0.9))
fossilData <- data.frame(svl=log(c(25.4,23.2,17.7,19.7,24,31)), tip1=c("aliniger","aliniger","occultus","christophei","cristatellus","occultus"), tip2=c("chlorocyanus","coelestinus","monticola","cybotes","augusticeps","augusticeps"))
Question 5 - 
fossilNodes<-c()
nodeN <- c()
nodeN<-c()
{
  for(i in 1:nrow(fossilData))
    i<-1
  if(i==1) {
    print(Ancestors)
  }
}
Node <- fastMRCA(tree, fossilData[i, "tip1"], fossilData[i, "tip2"])
Node
fossilNodes[i] <- fossilData [i, "svl"]
fossilNodes[i]
nodeN[i] <- Node
names(fossilNodes)<- nodeN
Ancestors_withFossils<- fastAnc(tree, svl, anc.states=fossilNodes, CI=TRUE, var=TRUE)
Ancestors_withFossils
Ancestors_woFossils<- fastAnc(tree, svl,CI=TRUE, var=TRUE)
Ancestors_woFossils
plot(Ancestors_withFossils$ace, Ancestors_woFossils$ace, xlab='fossils', ylab='no fossils')
Question 7 - Fossils increase the estimated ancestral state estimates.
Question 8-10 
install.packages('geiger')
library('geiger')
?fitContinuous
fitContinuous(tree, svl, model='EB')
fitContinuous(tree, svl, model='OU')
fitContinuous(tree, svl, model='BM')
