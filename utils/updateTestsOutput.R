### Script to recreate automatically all the output for the tests in 
###  pkg/tests/

setwd("~/Work/R-dev/phylobase/pkg/tests/")
listTests <- list.files(pattern=".R$")

for(eachTest in listTests) {
    out <- gsub("\\.R$", "\\.Rout.save", eachTest)
    cmd <- paste("R CMD BATCH --no-save", eachTest, out)
    system(cmd)
}
