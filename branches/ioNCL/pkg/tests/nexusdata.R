## try to read NEXUS files

##  _0.2.1 extension is only necessary because I have two different
##   versions installed 
##
##
if (FALSE) {
    library(phylobase,version="0.2.1")
    fn <- system.file("nexusfiles/treepluscharV01.nex",package="phylobase_0.2.1")
    td<-NexusToPhylo4D(fn)
    file.copy(fn,"temp.nex")
    td<-NexusToPhylo4D("temp.nex")
    plot(td,ratio.tree=0.001)
}
