formatData <- function(phy, dt, type=c("tip", "internal", "all"),
                       match.data=TRUE, rownamesAsLabels=FALSE,
                       label.type=c("rownames", "column"),
                       label.column=1, missing.data=c("fail", "warn", "OK"),
                       extra.data=c("warn", "OK", "fail"), keep.all=TRUE
                       ) {

    ## determine whether to return rows for all nodes, or just 'type'
    type <- match.arg(type)
    if (keep.all) {
        ids.out <- nodeId(phy, "all")
    } else {
        ids.out <- nodeId(phy, type)
    }

    ## if null, return empty data frame with node numbers as row names
    if (is.null(dt)) {
        return(data.frame(row.names=ids.out))
    }
    ## if vector, coerce to data.frame
    if (is.vector(dt) || is.factor(dt) || is.matrix(dt)) {
        dt <- as.data.frame(dt)
    }
    ## before proceeding, make sure that data provided are a data frame
    if (!is.data.frame(dt)) {
        stop(paste(deparse(substitute(dt)),
            "must be a vector, factor, matrix, or data frame"))
    }
    ## if lacking rows or columns, return a placeholder data frame with
    ## node numbers as row names
    if (any(dim(dt)==0)) {
        return(data.frame(row.names=ids.out))
    }

    label.type <- match.arg(label.type)
    ## Make sure the column specified for the labels is appropriate
    if (label.type == "column") {
        if (is.numeric(label.column))
            stopifnot(label.column %in% 1:ncol(dt))
        else
            stopifnot(label.column %in% names(dt))
    }

    missing.data <- match.arg(missing.data)
    extra.data <- match.arg(extra.data)

    if(match.data) {
        ## extract values to be matched to nodes
        ndNames <- switch(label.type,
                          rownames = rownames(dt),
                          column = dt[,label.column])
        if (rownamesAsLabels) {
            ids.in <- lapply(ndNames, function(ndnm) {
                getNode(phy, as.character(ndnm), missing="OK")
            })
        }
        else {
           ids.in <- lapply(ndNames, function(ndnm) {
                if (nchar(gsub("[0-9]", "", ndnm)) == 0) {
                    getNode(phy, as.integer(ndnm), missing="OK")
                }
                else {
                    getNode(phy, as.character(ndnm), missing="OK")
                }
            })
        }
        ids.list <- ids.in
        ids.in <- unlist(ids.in)

        ## Make sure that data are matched to appropriate nodes
        if (type=="tip" && any(na.omit(ids.in) %in% nodeId(phy,
            "internal"))) {
            stop("Your tip data are being matched to internal ",
                "nodes. Make sure that your data identifiers ",
                "are correct.")
        }
        if (type=="internal" && any(na.omit(ids.in) %in% nodeId(phy,
            "tip"))) {
            stop("Your node data are being matched to tip ",
                "nodes. Make sure that your data identifiers ",
                "are correct.")
        }

        ## Check differences between tree and data
        mssng <- setdiff(nodeId(phy, type), ids.in)
        if(length(mssng) > 0 && missing.data != "OK") {
            ## provide label if it exists and node number otherwise
            mssng <- getNode(phy, mssng)
            mssng <- ifelse(is.na(names(mssng)), mssng, names(mssng))
            msg <- "The following nodes are not found in the dataset: "
            msg <- paste(msg, paste(mssng, collapse=", "))
            switch(missing.data,
                   warn = warning(msg),
                   fail = stop(msg))
        }
        extra <- ndNames[is.na(ids.in)]
        if(length(extra) > 0 && extra.data != "OK") {
            msg <- "The following names are not found in the tree: "
            msg <- paste(msg, paste(extra, collapse=", "))
            switch(extra.data,
                   warn = warning(msg),
                   fail = stop(msg))
        }

        ## Format data to have correct dimensions
        ids.list <- ids.list[!is.na(ids.list)]
        dt <- dt[!is.na(ids.in), , drop=FALSE]
        if (hasDuplicatedLabels(phy)) {
            dtTmp <- array(, dim=c(length(ids.in[!is.na(ids.in)]), ncol(dt)),
                           dimnames=list(ids.in[!is.na(ids.in)], names(dt)))
            dtTmp <- data.frame(dtTmp)
            j <- 1
            for (i in 1:length(ids.list)) {
                for (k in 1:length(ids.list[[i]])) {
                    dtTmp[j, ] <- dt[i, , drop=FALSE]
                    j <- j + 1
                }
            }
            dt <- dtTmp
        }
        rownames(dt) <- ids.in[!is.na(ids.in)]
        dt.out <- dt[match(ids.out, rownames(dt)), , drop=FALSE]
        rownames(dt.out) <- ids.out
        if(label.type == "column") {
            dt.out <- subset(dt.out, select=-eval(parse(text=label.column)))
        }

    } else {
        ## Check if too many or not enough rows in input data
        expected.nrow <- length(nodeId(phy, type))
        diffNr <- nrow(dt) - expected.nrow
        if(nrow(dt) > expected.nrow && extra.data != "OK") {
            msg <- paste("There are", diffNr, "extra rows.")
            switch(extra.data,
                   warn = warning(msg),
                   fail = stop(msg))
        }
        if(nrow(dt) < expected.nrow && missing.data != "OK") {
            msg <- paste("There are", abs(diffNr), "missing rows.")
            switch(missing.data,
                   warn = warning(msg),
                   fail = stop(msg))
        }
        ## truncate rows of input data frame if necessary
        dt <- dt[1:min(nrow(dt), expected.nrow) ,, drop = FALSE]
        rownames(dt) <- nodeId(phy, type)[seq_len(nrow(dt))]
        dt.out <- dt[match(ids.out, rownames(dt)) ,, drop=FALSE]
        rownames(dt.out) <- ids.out
    }

    dt.out
}
