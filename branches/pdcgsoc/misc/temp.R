treePlot <- function(phy, type = 'phylogram', tip.order = NULL) {
    
    if (type = 'phylogram') {
        xxyy <- phyloXXYY(phy, tip.order)
    }
    
    if (type = 'unrooted') {
        xxyy <- unrootxxyy(phy)
    }
    
    edges <- edgechar(phy, params) ## TODO do these parameters even require a whole fun?
    
    tipplots <- tipPlot(...)
    
    nodeplot <- nodPlot(...)
    
    ## initialize canvas
    # call appropriate plot type
    ## grid calls Peter GSOC
    grid.newpage()
    if(show.tip.label) {
        treelayout <- grid.layout(nrow = 1, ncol = 2, 
            widths = unit(c(1, 1), c('null', 'strwidth'), list(NULL, 'seven')))
    } else {treelayout = NULL}
    
    pushViewport(viewport(
        x = 0.5, y = 0.5, 
        width = 0.8, height = 0.8, 
        layout = treelayout, name = 'treelayout', angle = -rot)) # rotataion set here
    
    if (show.tip.label) {
        pushViewport(viewport(
            layout = treelayout, 
            layout.pos.col = 2, 
            name = 'tip_labels'))
        grid.text(
            x@tip.label, 
            x = rep(0, length(x@tip.label)), 
            y = (yy/max(yy))[TIPS], 
            rot = rot, just = 'left'
            )
        popViewport()
    }
    pushViewport(viewport(
        layout = layout, layout.pos.col = 1, 
        name = 'tree'))
    grid.segments( # draws vertical lines
        x0 = x0v/xmax, y0 = y0v/ymax, 
        x1 = x0v/xmax, y1 = y1v/ymax, 
        name = "vert") #, gp = gpar(col = color.v, lwd = width.v)) 
    grid.segments(  # draws horizontal lines
        x0 = x0h/xmax, y0 = y0h/ymax, 
        x1 = x1h/xmax, y1 = y0h/ymax, 
        name = "horz") #, gp = gpar(col = edge.color, lwd = edge.width))
    popViewport()

}

####################################################    
    scratchYY <- function(phy, tip.order = NULL) {
        yy <- rep(NA, nrow(phy@edge))
        if(!is.null(tip.order)) { ## TODO do we need to acount for line weight when plotting close to edges?
            yy[which(phy@edge[, 2] == tip.order)] <- seq(0, 1, length.out = length(phy@tip.label)) ## TODO perhaps we want to use match here?
        } else {
            phy <- reorder.phylo4(phy)
            yy[phy@edge[, 2] <= length(phy@tip.label)] <- seq(0, 1, length.out = length(phy@tip.label)) 
        }
    
        calc.node.y <- function(x, phy, yy) {
            # recursive
            if(any(phy@edge[, 2] == x) == FALSE) {
                decdex <- which(phy@edge[, 1] == x)
                index <- 0 ## TODO hackish!
            } else {
                index <- which(phy@edge[, 2] == x)
                if(!is.na(yy[index])) { return(yy) }
                decdex <- which(phy@edge[, 1] == phy@edge[index, 2])
            }
            for(i in phy@edge[decdex, 2]) {
                yy <- calc.node.y(i, phy, yy)
            }
            yy[index] <- mean(yy[decdex])
            yy
        }
        yy <- calc.node.y(length(phy@tip.label) + 1, phy, yy)
        yy
####################################################
    }

    scratchXX <- function(phy) {
        ## xx <- rep(NA, nrow(phy@edge))
        calc.node.x <- function(node, phy, xx = numeric(nrow(phy@edge)), prevx = NULL) {
            ## recursive
            index <- which(phy@edge[, 2] == node)
            if(length(index) == 0) {
                newx <- 0
            } else {
                xx[index] <- phy@edge.length[index] + prevx
                newx <- xx[index]
            }
            for(i in phy@edge[phy@edge[, 1] == node, 2]) {
                xx <- calc.node.x(i, phy, xx, newx)
            }
            xx
        }
        calc.node.x(length(phy@tip.label) + 1, phy)
    }

phyloXXYY <- function(phy, tip.order = NULL) {
    xxyy = list(
        yy = rep(NA, nrow(phy@edge)), 
        xx = numeric(nrow(phy@edge)), 
        traverse = NULL) 
    
    ## TODO tip ordering should be dealt with at a higher level
    ## if(!is.null(tip.order)) { ## TODO do we need to acount for line weight when plotting close to edges?
    ##     yy[which(phy@edge[, 2] == tip.order)] <- seq(0, 1, length.out = length(phy@tip.label)) ## TODO perhaps we want to use match here?
    ## } else {
        phy <- reorder.phylo4(phy)
        xxyy$yy[phy@edge[, 2] <= length(phy@tip.label)] <- seq(
            0, 1, length.out = length(phy@tip.label)
        )
    ## }
    
    calc.node.xy <- function(node, phy, xxyy, prevx = 0) {
        if(any(phy@edge[, 2] == node) == FALSE) {
            decdex <- which(phy@edge[, 1] == node)
            index <- NULL
        } else {
            index <- which(phy@edge[, 2] == node)
            decdex <- which(phy@edge[, 1] == phy@edge[index, 2])
        }
        if(is.null(index)) {
            newx <- 0
        } else {
            newx <- xxyy$xx[index] <- phy@edge.length[index] + prevx
        }
        if(!is.null(index)) {
            if(!is.na(xxyy$yy[index])) { return(xxyy) }
        }
        for(i in phy@edge[decdex, 2]) {
            xxyy <- calc.node.xy(i, phy, xxyy, newx)
        }
        if(!is.null(index)) {
            xxyy$yy[index] <- mean(xxyy$yy[decdex])
        }
        ## TODO performance improvement here? rely on above ordering?
        xxyy$traverse <- c(xxyy$traverse, phy@edge[decdex, 2]) 
        xxyy
    }
    xxyy <- calc.node.xy(length(phy@tip.label) + 1, phy, xxyy)
    xxyy$xx <- xxyy$xx / max(xxyy$xx)
    xxyy
}

## How do we translate this info into a plot?
## Test code
## out <- scratch(foo <- as(rcoal(5), 'phylo4'))

