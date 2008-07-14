require(phylobase)
require(grid)
require(lattice)
treePlot <- function(phy, 
                     type = 'phylogram', 
                     show.tip.label = TRUE, 
                     tip.order = NULL,
                     plot.data = FALSE,
                     rot = 0,
                     tip.plot.fun = function() {grid.lines(1:10/10, rnorm(10, sd = .2, mean = .5))}
                     ## tip.plot.fun = function() {}
            )
    {
    
    if (type == 'phylogram') {
        xxyy <- phyloXXYY(phy, tip.order)
        ## because we may reoder the tip, we need to update the phy objec
        phy <- xxyy$phy
        segs <- segs(phy, XXYY = xxyy$xxyy)
    }
    
    ## TODO do these parameters even require a whole fun?
    ## edges <- edgechar(phy, params) 
    
    ## tipplots <- tipPlot(...)
    
    ## nodeplot <- nodPlot(...)
    
    ## initialize canvas
    # call appropriate plot type
    ## grid calls Peter GSOC
    grid.newpage()
    if(plot.data) {
        treelayout <- grid.layout(nrow = 1, ncol = 3,
            widths = unit(c(1, 1, .1), c('null', 'strwidth', 'npc'), 
            list(NULL, phy@tip.label, NULL)
            ))
    ## TODO handle showing data and labels better
    } else if(show.tip.label) {
        treelayout <- grid.layout(nrow = 1, ncol = 2, 
            ## TODO find the best way to get max label width
            widths = unit(c(1, 1), c('null', 'strwidth'), list(NULL, phy@tip.label)))
    } else {treelayout = NULL}
    
    pushViewport(viewport(
        x = 0.5, y = 0.5, 
        width = 0.8, height = 0.8, 
        # rotataion set here
        layout = treelayout, name = 'treelayout', angle = -rot))
    
    ## TODO handle better show label | data
    if (show.tip.label | plot.data) {
        pushViewport(viewport(
            layout = treelayout, 
            layout.pos.col = 2, 
            name = 'tip_labels'))
        grid.text(
            phy@tip.label, 
            x = rep(0, length(phy@tip.label)), 
            ## TODO yuck!!
            y = xxyy$xxyy$yy[which(phy@edge[, 2] <= length(phy@tip.label))], 
            rot = rot, just = 'left'
            )
        popViewport()
    }
    if (plot.data) {
        ## datalayout <- grid.layout(
        ##                 nrow = length(phy@tip.label), 
        ##                 ncol = 1,
        ##                 respect = TRUE)
        pushViewport(viewport(
            ## layout = datalayout, 
            layout.pos.col = 3, 
            name = 'data_plots'))
        ## TODO should plots float at tips, or only along edge?
        for(i in xxyy$xxyy$yy[which(phy@edge[, 2] <= length(phy@tip.label))]) {
            pushViewport(viewport(
                y = i, 
                height = unit(1, 'snpc'), 
                width = unit(1, 'snpc'), 
                name = paste('data_plot', i),
                just = "left"))
                tip.plot.fun()
            popViewport()
        }
        popViewport()
    }
    
    pushViewport(viewport(
        layout = treelayout, layout.pos.col = 1, 
        name = 'tree'))
    grid.segments( # draws vertical lines
        x0 = segs$v0x, y0 = segs$v0y, 
        x1 = segs$v1x, y1 = segs$v1y, 
        name = "vert") #, gp = gpar(col = color.v, lwd = width.v)) 
    grid.segments(  # draws horizontal lines
        x0 = segs$h0x, y0 = segs$h0y, 
        x1 = segs$h1x, y1 = segs$h1y, 
        name = "horz") #, gp = gpar(col = edge.color, lwd = edge.width))
    popViewport()
}


phyloXXYY <- function(phy, tip.order = NULL) {
    ## initalize the output
    xxyy = list(
        yy = rep(NA, nrow(phy@edge)), 
        xx = numeric(nrow(phy@edge)), 
        ## record the order that nodes are visited in
        traverse = NULL) 
    
    # TODO tip ordering should be dealt with at a higher level
    # if(!is.null(tip.order)) { 
        ## TODO do we need to acount for line weight when plotting close to edges?
    #     yy[which(phy@edge[, 2] == tip.order)] <- seq(
        ## TODO perhaps we want to use match here?
        ## 0, 1, length.out = length(phy@tip.label)) 
    # } else {
        ## reoder the phylo and assign even y spacing to the tips
        phy <- reorder.phylo4(phy)
        xxyy$yy[phy@edge[, 2] <= length(phy@tip.label)] <- seq(
            0, 1, length.out = length(phy@tip.label)
        )
    # }
    
    ## a recurvise preorder traversal 
    ## node  -- initalized to be root, is the starting point for the traversal
    ## phy   -- the phylogeny
    ## xxyy  -- the list initalized below that holds the output
    ## prevx -- the sum of ancestral branch lengths
    calc.node.xy <- function(node, phy, xxyy, prevx = 0) {
        ## if node == root node, and there is no root edge set get descendants
        ## and set index to NULL index is used for indexing output
        if(any(phy@edge[, 2] == node) == FALSE) {
            decdex <- which(phy@edge[, 1] == node)
            index <- NULL
            ## if root node start at x = 0
            newx <- 0
        } else {
            ## non-root node behavior
            ## get row in edge matrix corresponding to node, get descendants
            index <- which(phy@edge[, 2] == node)
            decdex <- which(phy@edge[, 1] == phy@edge[index, 2])
            ## non-root node x location 
            newx <- xxyy$xx[index] <- phy@edge.length[index] + prevx
            ## if the x value is already set we are at a tip and we return
            if(!is.na(xxyy$yy[index])) { return(xxyy) }
        }
        for(i in phy@edge[decdex, 2]) {
            ## for each decendant call the function again
            xxyy <- calc.node.xy(i, phy, xxyy, newx)
        }
        if(!is.null(index)) {
            ## set y value by averaging the decendants
            xxyy$yy[index] <- mean(xxyy$yy[decdex])
        }
        ## TODO performance improvement here? rely on above ordering?
        ## keep track of the nodes and order we visited them
        xxyy$traverse <- c(xxyy$traverse, phy@edge[decdex, 2]) 
        xxyy
    }
    ## call function for the first time
    xxyy <- calc.node.xy(length(phy@tip.label) + 1, phy, xxyy)
    ## scale the x values
    xxyy$xx <- xxyy$xx / max(xxyy$xx)
    list(xxyy = xxyy, phy = phy)
}

segs <- function(phy, XXYY) {
    treelen <- rep(NA, nrow(phy@edge) + 1)
    segs <- list(v0x = treelen, v0y = treelen, v1x = treelen, v1y = treelen,
                 h0x = treelen, h0y = treelen, h1x = treelen, h1y = treelen)
    troot <- length(phy@tip.label) + 1

    get.coor <- function(node, segs) {
        if(any(phy@edge[, 2] == node) == FALSE) {
            decdex <- which(phy@edge[, 1] == node)
            index <- length(treelen)
            segs$v0x[index] <- segs$v1x[index] <- 0
            
            segs$h0y[index] <- segs$h1y[index] <- NA
            segs$h0x[index] <- segs$h1x[index] <- NA
            segs$h0x[decdex] <- 0            
        } else {
            index <- which(phy@edge[, 2] == node)
            if(!any(phy@edge[, 1] == node)) {
                return(segs)
            }
            decdex <- which(phy@edge[, 1] == phy@edge[index, 2])
            segs$v0x[index] <- segs$v1x[index] <- XXYY$xx[index]
            segs$h0x[decdex] <- XXYY$xx[index]
        }
        segs$h1x[decdex] <- XXYY$xx[decdex]
        segs$h0y[decdex] <- segs$h1y[decdex] <- XXYY$yy[decdex]
        
        segs$v0y[index] <- min(XXYY$yy[decdex])
        segs$v1y[index] <- max(XXYY$yy[decdex])
        
        for(i in phy@edge[decdex, 2]) {
            segs <- get.coor(i, segs)
        }
        segs
    }
    get.coor(troot, segs)
}

## How do we translate this info into a plot?
## Test code
out <- phyloXXYY(foo <- as(rcoal(3), 'phylo4'))
data(geospiza)
## TODO true arbitary functions with data from associated data frames
## grid.points(
##     x = rep(1:ncol(geospiza@tip.data), 
##     nrow(geospiza@tip.data))/ncol(geospiza@tip.data) - .2, 
##     y = scale(geospiza@tip.data))

treePlot(geospiza, plot.data = TRUE)
