install.packages("learnPopGen")
library("learnPopGen")
install.packages("coala")
library("coala")
install.packages("phytools")
library("phytools")
coalescent.plot()
coalescent.plot
It starts with 10 alleles that have been modified by the n=function from the plot above. 
pdf('r.05-Question1', height=5, width=5)
After 10 generations the allele goes to fixation.
pdf('r.05-Question2.pdf', height=5, width=5)
coalescent.plot(n=20, ngen=20, color=NULL)
dev.off()
2 was the average number of offspring.
pdf('r.05-Question3.pdf', height=5, width=5)
coalescent.plot(n=10, ngen=1, colors=NULL)
coalescent.plot(n=10, ngen=5, colors=NULL)
dev.off()
If an individual is unfit, they cannot mate many times are it will go into extinction.
yes 
model <- coal_model(sample_size = 5, loci_number = 10, loci_length = 500, ploidy = 2) +
  feat_mutation(10) +
  feat_recombination(10) +
  sumstat_trees() +
  sumstat_nucleotide_div()
stats <- simulate(model, nsim = 1)
Diversity <- stats$pi
Due to mutation, crossover, and recombination the numbers are not all the same because of the change in alleles.
Nloci <- length(stats$trees)
t1 <- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
There are two alleles for each individual so the number of tips differ.
Age1 <- read.tree(text=stats$trees[[2]][1])
t2 <- read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()
The most recent is not the same as the 1st
no 
par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()
compare.chronograms(t1, t2)
t1_1 <- read.tree(text=stats$trees[[1]][1])
t1_2 <- read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_1, t1_2)
for (loacus in 1:Nloci)  {
  ntrees <- length(stats$trees[[locus]])
  for (n in 1:ntrees) {
    if (locus == 1 && n == 1) {
      outPhy <- read.tree(text=stats$trees[[locus]][n])
    }
    else  {
      outPhy <- ape:::c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n]))
    }
  }
}
par(mfrow=c(1,1))
densityTree(outPhy)
model3 <- coal_model(10, 50) +
  feat_mutation(par_prior("theta", sample,int(100, 1))) +
  sumstat_nucleotide_div()
I expect a differnece in distribution due to mutations because of the changes in recombination frequency. We did see this happen.
model3 <- coal_model(10, 50) +
  feat_mutation(par_prior("theta", sample.int(100, 1))) +
  sumstat_nucleotide_div()
stats <- simulate(model3, nsim = 40)
mean_pi <- sapply(stats, function(x) mean(x$pi))
theta <- sapply(stats, function(x) x$pars[["theta"]])
pdf("r05-Model3Graph.pdf", height=5, width=5)
plot(mean_pi)
plot(theta)
plot(mean_pi, theta, xlab='Diversity', ylab='Mutation Rate', pch=16, cex=1.3, col='red', main='Diversity due to Varied Mutation Rate')
abline(lm(mean_pi ~ theta), col='black')
dev.off()
install.packages('phyclust')
library('phyclust')
activate_ms(priority = 200)
activate_msms(java = NULL, priority = 500, download = TRUE)
model4 <- coal_model(c(13, 18), loci_number = 10, loci_length = 1000, ploidy = 2) +
  feat_selection(10, time = 1, population = 1) +
  feat_selection(25, time = 1, population = 2) +
  feat_size_change(0.5,
                   population = 1,
                   time = '1', 
                   locus_group = 'all') +
  feat_size_change(0.1, 
                   population = 2, 
                   time = '2', 
                   locus_group = 'all') +
  feat_migration(1.2, 
                 pop_to = 1, 
                 pop_from = 2, 
                 symmetric = FALSE, time = '3', locus_group = 'all') +
  feat_mutation(15) +
  sumstat_trees() +
  sumstat_nucleotide_div()
list_simulators()
activate_scrm(priority = 400)
stat2 <- simulate(model4)
stat2 <- simulate(model4, nsim = 40)
Diversity2 <- stat2$pi
Diversity 2


