treePlot <- function(phy, type = 'phylogram', tip.order = NULL) {
    
    if (type = 'phylogram') {
        xxyy <- phyloXXYY(phy, tip.order)
    }
    
    if (type = 'unrooted') {
        xxyy <- unrootxxyy(phy)
    }
    
    ## TODO do these parameters even require a whole fun?
    edges <- edgechar(phy, params) 
    
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
        # rotataion set here
        layout = treelayout, name = 'treelayout', angle = -rot))
    
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
        } else {
            ## non-root node behavior
            ## get row in edge matrix corresponding to node, get descendants
            index <- which(phy@edge[, 2] == node)
            decdex <- which(phy@edge[, 1] == phy@edge[index, 2])
        }
        if(is.null(index)) {
            ## if root node start at x = 0
            newx <- 0
        } else {
            ## non-root node x location 
            newx <- xxyy$xx[index] <- phy@edge.length[index] + prevx
        }
        if(!is.null(index)) {
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
    xxyy
}

## How do we translate this info into a plot?
## Test code
## out <- scratch(foo <- as(rcoal(5), 'phylo4'))

