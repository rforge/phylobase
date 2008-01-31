## try to read NEXUS files
library(phylobase)
fn <- system.file("nexusfiles/treepluscharV01.nex",package="phylobase")
td<-NexusToPhylo4D(fn)
summary(td)
## would try plotting, but typically don't have enough room
## to plot data
## Error in .local(x, ...) : 
##    No room left to plot data; please try reducing ratio.tree or cex.label.
plot(as(td,"phylo4"))

