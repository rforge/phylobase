
NexusToPhylo4 <- function(fileToRead,multi=FALSE) {
#This returns a single phylo4 object if the nexus file has one tree; 
#it returns a list of phylo4 objects if the nexus file has more than one or multi==TRUE
#Note that a list of phylo4 objects is not the same as a multiphylo4d object
	trees<-c("Failure");
	filename<-fileToRead;
	params <- list(filename=fileToRead)
	
# Check that params is properly formatted.
	if(!is.list(params) || length(params) == 0) {
		stop("The params parameter must be a non-empty list");
	}
	
# Finally ready to make the call...
    intreesstring <- .Call("ReadTreesWithNCL", params,
						   PACKAGE="phylobase")
	
	intreesphylolist <- read.nexustreestring(intreesstring);
	if (length(intreesphylolist)>1 || multi) {
		trees<-list()
		for (i in 1:length(intreesphylolist)) {
			trees[[i]]<-as(intreesphylolist[[i]], "phylo4");
		}
	}
	else {
		trees<-as(intreesphylolist[[1]], "phylo4");
	}
	trees
}

NexusToDataFrame <- function(fileToRead,allchar=FALSE, polymorphictomissing=TRUE, levelsall=TRUE) {
#This returns a single phylo4D object if the nexus file has one tree; 
#it returns a list of phylo4D objects if the nexus file has more than one or multi==TRUE
#Note that a list of phylo4D objects is not the same as a multiphylo4d object
	output<-c("Failure")
	filename<-fileToRead
	params <- list(filename=fileToRead, allchar=allchar, polymorphictomissing=polymorphictomissing, levelsall=levelsall)
	
# Check that params is properly formatted.
	if(!is.list(params) || length(params) == 0) {
		stop("The params parameter must be a non-empty list")
	}
	
	incharsstring <- .Call("ReadCharsWithNCL",params,
						   PACKAGE="phylobase")
	print(incharsstring)
	tipdata<-eval(parse(text=incharsstring))
	tipdata
}


NexusToPhylo4D <- function(fileToRead,multi=FALSE,allchar=FALSE, polymorphictomissing=TRUE, levelsall=TRUE) {
#This returns a single phylo4D object if the nexus file has one tree; 
#it returns a list of phylo4D objects if the nexus file has more than one or multi==TRUE
#Note that a list of phylo4D objects is not the same as a multiphylo4d object
	output<-c("Failure")
	tipdata<-NexusToDataFrame(fileToRead,allchar, polymorphictomissing, levelsall);
	intreesphylolist<-NexusToPhylo4(fileToRead,multi=TRUE);
	if (length(intreesphylolist)>1 || multi) {
		output<-list()
		for (i in 1:length(intreesphylolist)) {
			output[[i]]<-phylo4d(as(intreesphylolist[[i]], "phylo4"), tip.data = tipdata)
		}
	}
	else {
		output<-phylo4d(as(intreesphylolist[[1]], "phylo4"), tip.data = tipdata)
	}
	output
}


read.nexustreestring <- function(X)
{
#Returns list of phylo objects (not multi.phylo, and always a list, even if there is only one element
#X is a character vector, each element is one line from a treefile
#This is based almost entirely on read.nexus from APE (Emmanuel Paradis).
	
		X<-unlist(strsplit(unlist(X),c("\n")))

## first remove all the comments

## BCO took out the "speedier removal of comments" code -- it keeps [&R] as a node label, replaced it with original APE code
## speedier removal of comments pc 13 April 2008
##X <- lapply(X, gsub, pattern = "\\[[^\\]]*\\]", replacement = "")

    LEFT <- grep("\\[", X)
    RIGHT <- grep("\\]", X)
    if (length(LEFT)) { # in case there are no comments at all
        w <- LEFT == RIGHT
        if (any(w)) { # in case all comments use at least 2 lines
            s <- LEFT[w]
            X[s] <- gsub("\\[[^]]*\\]", "", X[s])
            ## The above regexp was quite tough to find: it makes
            ## possible to delete series of comments on the same line:
            ##       ...[...]xxx[...]...
            ## without deleting the "xxx". This regexp is in three parts:
            ##       \\[      [^]]*       \\]
            ## where [^]]* means "any character, except "]", repeated zero
            ## or more times" (note that the ']' is not escaped here).
            ## The previous version was:
            ##       X[s] <- gsub("\\[.*\\]", "", X[s])
            ## which deleted the "xxx". (EP  2008-06-24)
        }
        w <- !w
        if (any(w)) {
            s <- LEFT[w]
            X[s] <- gsub("\\[.*", "", X[s])
            sb <- RIGHT[w]
            X[sb] <- gsub(".*\\]", "", X[sb])
            if (any(s < sb - 1))
                X <- X[-unlist(mapply(":", (s + 1), (sb - 1)))]
        }
    }
    endblock <- grep("END;|ENDBLOCK;", X, ignore.case = TRUE)
    semico <- grep(";", X)
    i1 <- grep("BEGIN TREES;", X, ignore.case = TRUE)
    i2 <- grep("TRANSLATE", X, ignore.case = TRUE)
    translation <- if (length(i2) == 1 && i2 > i1) TRUE else FALSE
    if (translation) {
        end <- semico[semico > i2][1]
        x <- X[(i2 + 1):end] # assumes there's a 'new line' after "TRANSLATE"
        ## x <- gsub("TRANSLATE", "", x, ignore.case = TRUE)
        x <- unlist(strsplit(x, "[,; \t]"))
        x <- x[nzchar(x)]
        TRANS <- matrix(x, ncol = 2, byrow = TRUE)
        TRANS[, 2] <- gsub("['\"]", "", TRANS[, 2])
        n <- dim(TRANS)[1]
    }
    start <-
        if (translation) semico[semico > i2][1] + 1
        else semico[semico > i1][1]
    end <- endblock[endblock > i1][1] - 1
    tree <- X[start:end]
    rm(X)
    tree <- gsub("^.*= *", "", tree)
    semico <- grep(";", tree)
    Ntree <- length(semico)
    ## are some trees on several lines?
    if (any(diff(semico) != 1)) {
        STRING <- character(Ntree)
        s <- c(1, semico[-Ntree] + 1)
        j <- mapply(":", s, semico)
        for (i in 1:Ntree)
            STRING[i] <- paste(tree[j[, i]], collapse = "")
    } else STRING <- tree
    rm(tree)
    STRING <- gsub(" ", "", STRING)
    colon <- grep(":", STRING)
    if (!length(colon)) {
    	#TODO: recode clado.build & .treeBuildWithTokens from ape to phylobase
        trees <- lapply(STRING, clado.build)
    } else if (length(colon) == Ntree) {
        trees <-
            if (translation) lapply(STRING, .treeBuildWithTokens)
            else lapply(STRING, tree.build)
    } else {
        trees <- vector("list", Ntree)
        trees[colon] <- lapply(STRING[colon], tree.build)
        nocolon <- (1:Ntree)[!1:Ntree %in% colon]
        trees[nocolon] <- lapply(STRING[nocolon], clado.build)
        if (translation) {
            for (i in 1:Ntree) {
                tr <- trees[[i]]
                for (j in 1:n) {
                    ind <- which(tr$tip.label[j] == TRANS[, 1])
                    tr$tip.label[j] <- TRANS[ind, 2]
                }
                if (!is.null(tr$node.label)) {
                    for (j in 1:length(tr$node.label)) {
                        ind <- which(tr$node.label[j] == TRANS[, 1])
                        tr$node.label[j] <- TRANS[ind, 2]
                    }
                }
                trees[[i]] <- tr
            }
            translation <- FALSE
        }
    }
    for (i in 1:Ntree) {
        tr <- trees[[i]]
        ## Check here that the root edge is not incorrectly represented
        ## in the object of class "phylo" by simply checking that there
        ## is a bifurcation at the root
        if (!translation) n <- length(tr$tip.label)
        ROOT <- n + 1
        if (sum(tr$edge[, 1] == ROOT) == 1 && dim(tr$edge)[1] > 1) {
            stop(paste("There is apparently two root edges in your file: cannot read tree file.\n  Reading NEXUS file aborted at tree no.", i, sep = ""))
        }
    }
    trees
}