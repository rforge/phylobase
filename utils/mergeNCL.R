### Script that allows to merge automatically .cpp and .h files from NCL

### IMPORTANT Notes: only works if the changes to libncl are committed (just) before the merging
###  in other words the difference between previous revision and current revision in libncl/current
###  represents the changes to NCL
### IMPORTANT Notes 2: if you run the script from within Emacs, in case of conflict during the merging
###   it's best to always choose "postpone" and do the modifications by hand.

### This script does not take care of files that could be added in the future development of NCL

### Procedure to follow:
## 0. extract NCL tarball in a folder outside of phylobase
## 1. copy the content of the folder (and overwrite) directly the content of libncl/current
## 2. run: find libncl/current -name ._* -delete (to remove Mac's fork files)
## 3. run: svn status | grep ^? (to identify the files that need to be added)
## 4. add/remove files with svn add/rm to include/remove all the files with version control
## 5. run: svn copy current 2.x.y (where 2.x.y represent the newest NCL version, to tag newest NCL version)
## 6. commit these changes to libncl folder
## 7. modify (to match your installation) and run the script below to merge all the files
## 8. commit changes


## Change this line to match your installation
setwd("~/Work/R-dev/phylobase/")

# run svn log to get latest revision number
system("svnversion > svn.log")
svnlog <- scan(file="svn.log", what="character")
revNb <- unlist(strsplit(svnlog, ":"))[2]
revNb <- gsub("[^0-9]", "", revNb)
revNb <- as.numeric(revNb)

## merge .cpp files
setwd("pkg/src/")
listCpp <- list.files(pattern="^nxs.+\\.cpp$")
for (eachCpp in listCpp) {
    cmd <- paste("svn merge -r", revNb-1, ":", revNb, " ../../libncl/current/ncl/", eachCpp, " ", eachCpp, sep="")
    cat("\n", cmd)
    system(cmd)
}

## merge .h files
setwd("ncl")
listH <- list.files(pattern="\\.h$")
for (eachH in listH) {
    cmd <- paste("svn merge -r", revNb-1, ":", revNb, " ../../../libncl/current/ncl/", eachH, " ", eachH, sep="")
    cat("\n", cmd, "\n")
    system(cmd)
}
