require(phylobase)
require(grid)
require(lattice)
treePlot <- function(phy, 
                     type = 'phylogram', 
                     show.tip.label = TRUE, 
                     tip.order = NULL,
                     plot.data = FALSE,
                     rot = 0,
                     tip.plot.fun = 'bubbles',
                     edge.color = 'black', ## TODO colors for branhes and nodes seperately?
                     node.color = 'black',
                     tip.color  = 'black', 
                     edge.width = 1 ## TODO currently only one width is allowed allow many?
            )
{
    phy.orig <- phy
    Nedges   <- nrow(phy@edge)
    Ntips    <- length(phy@tip.label)
    tindex <- phy@edge[phy@edge[, 2] <= Ntips, 2]
    if (type == 'phylogram') {
        xxyy <- phyloXXYY(phy, tip.order)
        ## because we may reoder the tip, we need to update the phy objec
        phy <- xxyy$phy
    }
    
    if(plot.data) {
        phyplotlayout <- grid.layout(nrow = 1, ncol = 2,
            widths = unit(c(1, 1), c('null', 'null'), list(NULL, NULL))
            )
    } else {
        phyplotlayout <- grid.layout(nrow = 1, ncol = 1)
    }
    ## TODO handle showing data and labels better
    grid.newpage()
    pushViewport(viewport(
        x = 0.5, y = 0.5, 
        width = 0.9, height = 0.9, 
        # rotataion set here
        layout = phyplotlayout, name = 'phyplotlayout', angle = -rot))
    pushViewport(viewport(layout.pos.col = 1))
        tree.plot(xxyy, show.tip.label, edge.color, node.color, tip.color, edge.width, rot)
    upViewport()
    ## TODO handle better show label | data
    if (plot.data) {
        tindex <- phy@edge[phy@edge[, 2] <= Ntips, 2]
        if(length(tip.color) != Ntips) {
            tip.color <- rep(tip.color, length.out = Ntips)
        }
        if (tip.plot.fun == 'bubbles') {
            pushViewport(viewport(layout.pos.col = 2))
                phylobubbles(xxyy)
            popViewport()
        } else {
            ## datalayout <- grid.layout(
            ##                 nrow = Ntips, 
            ##                 ncol = 1,
            ##                 respect = TRUE)
            pushViewport(viewport(
                ## layout = datalayout, 
                layout.pos.col = 2, 
                name = 'data_plots'))
            ## TODO should plots float at tips, or only along edge?
            for(i in xxyy$yy[which(phy@edge[, 2] <= Ntips)]) {
                pushViewport(viewport(
                    y = i, 
                    height = unit(1, 'snpc'), 
                    width = unit(1, 'snpc'), 
                    name = paste('data_plot', i),
                    just = "left"))
                    # tip.plot.fun()
                popViewport()
            }
            popViewport()
        }
    }
}

tree.plot <- function(xxyy, show.tip.label, edge.color, 
                        node.color, tip.color, edge.width, rot) 
{

    # TODO switch to phylobase abstractions
    phy <- xxyy$phy
    Nedges   <- nrow(phy@edge)
    Ntips    <- length(phy@tip.label)
    tindex <- phy@edge[phy@edge[, 2] <= Ntips, 2]
    eindex <- match(phy@edge[,2], xxyy$phy.orig@edge[,2])
    segs <- segs(phy, XXYY = xxyy)

    ## TODO check that colors are valid?
    ## TODO edge colors are required to be in the order of edge matrix
    if(length(edge.color) != Nedges) {
        edge.color <- rep(edge.color, length.out = Nedges)
    }
    edge.color <- edge.color[eindex]

    ## TODO check that colors are valid?
    nindex <- sort(eindex[phy@edge[, 2] > Ntips], index.return = TRUE)$ix
    if(length(node.color) != length(nindex)) {
        node.color <- rep(node.color, length.out = length(nindex))
    }
    node.color <- node.color[nindex]

    if(show.tip.label) {
        labw <- max(stringWidth(phy@tip.label))
        # print(convertUnit(labw, 'inches'))
        treelayout <- grid.layout(nrow = 1, ncol = 2,
            widths = unit.c(unit(1, 'null', NULL), labw)
            )
        tindex <- phy@edge[phy@edge[, 2] <= Ntips, 2]
        if(length(tip.color) != Ntips) {
            tip.color <- rep(tip.color, length.out = Ntips)
        }
    } else {
        treelayout <- grid.layout(nrow = 1, ncol = 1)
    }
    # grid.show.layout(treelayout)
    pushViewport(viewport(
        x = 0.5, y = 0.5, 
        width = 1, height = 1, 
        # rotataion set here
        layout = treelayout, name = 'treelayout', angle = -rot))
    pushViewport(viewport(
        layout = treelayout, layout.pos.col = 1, 
        name = 'tree'))
    vseg <- grid.segments( # draws vertical lines
        x0 = segs$v0x, y0 = segs$v0y, 
        x1 = segs$v1x, y1 = segs$v1y, 
        name = "vert", gp = gpar(col = node.color, lwd = 1)) 
    hseg <- grid.segments(  # draws horizontal lines
        x0 = segs$h0x, y0 = segs$h0y, 
        x1 = segs$h1x, y1 = segs$h1y, 
        name = "horz", gp = gpar(col = edge.color, lwd = 1))
    upViewport()
    if(show.tip.label) {
        pushViewport(viewport(
            layout = treelayout, layout.pos.col = 1, 
            ))
        labtext <- grid.text(
            phy@tip.label[tindex], 
            x = xxyy$xx[phy@edge[, 2] %in% tindex] + 0.02, 
            ## TODO yuck!!
            y = xxyy$yy[phy@edge[, 2] %in% tindex], 
            default.units = 'npc', 
            rot = rot, just = 'left', gp = gpar(col = tip.color[tindex])
        )
        upViewport()
    }
    # grobTree(vseg, hseg, labtext)
}


phyloXXYY <- function(phy, tip.order = NULL) {
    ## initalize the output
    Nedges <- nrow(phy@edge)
    phy.orig <- phy
    Ntips  <- length(phy@tip.label)
    xxyy = list(
        yy = rep(NA, Nedges), 
        xx = numeric(Nedges), 
        ## record the order that nodes are visited in
        traverse = NULL) 
    
    # TODO tip ordering should be dealt with at a higher level
    # if(!is.null(tip.order)) { 
        ## TODO do we need to acount for line weight when plotting close to edges?
    #     yy[which(phy@edge[, 2] == tip.order)] <- seq(
        ## TODO perhaps we want to use match here?
        ## 0, 1, length.out = Ntips) 
    # } else {
        ## reoder the phylo and assign even y spacing to the tips
        phy <- reorder(phy, 'pruningwise')
        xxyy$yy[phy@edge[, 2] <= Ntips] <- seq(
            0, 1, length.out = Ntips
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
    xxyy <- calc.node.xy(Ntips + 1, phy, xxyy)
    ## scale the x values
    xxyy$xx <- xxyy$xx / max(xxyy$xx)
    # TODO return an index vector instead of a second phy object
    c(xxyy, phy = list(phy), phy.orig = list(phy.orig))
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

phylobubbles <- function(XXYY) {
    phy <- XXYY$phy
    
    tys <- XXYY$yy[phy@edge[, 2] <= nTips(phy)]
    
    traits <- phy@tip.data

    maxr <- ifelse(ncol(traits) > nTips(phy), .75/ncol(traits), .75/nTips(phy))

    tnames <- names(traits)
    traits <- scale(traits)
    traits <- apply(traits, 2, function(x) maxr * x / max(abs(x), na.rm = T))
    names(traits) <- tnames
    print(colnames(traits))
    
    if(ncol(traits) == 1) {
        xpos <- 0.5
    } else {
        xpos <- seq(0+maxr, 1-maxr, length.out = ncol(traits))
    }
    tys <- tys * (1 - (2 * maxr)) + maxr
    
    xrep <- rep(xpos, each = length(tys))
    ccol <- ifelse(traits < 0, 'black', 'white')
    nays <- tys[apply(traits, 1, function(x) any(is.na(x)))]
    naxs <- xpos[apply(traits, 2, function(x) any(is.na(x)))]
    traits[is.na(traits)] <- 0
    
    bublayout <- grid.layout(nrow = 2, ncol = 2,
        widths = unit(c(1, 1), c('null', 'strwidth'), 
            list(NULL, phy@tip.label)), 
        heights = unit(c(1, 1), c('null', 'strwidth'), 
            list(NULL, colnames(traits)))
        )
    pushViewport(viewport(
        x = 0.5, y = 0.5, 
        width = 1, height = 1, 
        layout = bublayout, name = 'bublayout'))
    pushViewport(viewport( 
        name = 'bubble_plots', 
        layout = bublayout, 
        layout.pos.col = 1, 
        layout.pos.row = 1
    ))
    grid.segments(x0 = 0,  x1 = 1, y0 = tys, y1 = tys, gp = gpar(col = 'grey'))
    grid.segments(x0 = xpos,  x1 = xpos, y0 = 0, y1 = 1, gp = gpar(col = 'grey'))
    grid.text('x', naxs, nays)
    grid.circle(xrep, tys, r = unlist(traits), gp = gpar(fill = ccol)
        )
    popViewport()
    pushViewport(viewport( 
        name = 'bubble_tip_labels', 
        layout = bublayout, 
        layout.pos.col = 2, 
        layout.pos.row = 1
    ))
    grid.text(phy@tip.label, 0.2, tys, just = 'left')
    popViewport()
    pushViewport(viewport( 
        name = 'bubble_data_labels', 
        layout = bublayout, 
        layout.pos.col = 1, 
        layout.pos.row = 2
    ))
    grid.text(colnames(traits), xpos, .8, rot = 90, just = 'right')
    popViewport()

    popViewport()
}

## How do we translate this info into a plot?
## Test code
# out <- phyloXXYY(foo <- as(rcoal(3), 'phylo4'))
data(geospiza)
# foo <- phyloXXYY(geospiza)
# phylobubbles(foo)
## TODO true arbitary functions with data from associated data frames

# p1 <- treePlot(
#     geospiza, 
#     plot.data = TRUE, 
#     show.tip.label = FALSE, 
#     # edge.color = rainbow(nrow(geospiza@edge)),  
#     tip.color = c('red',  'black', 'blue')
# )

tree1 <- as(rtree(10), 'phylo4')
tree1@tip.label <- replicate(10, paste(sample(LETTERS, sample(2:20, 1)), collapse = ""))

p2 <- treePlot(
    tree1, #, plot.data = TRUE
)

# pushViewport(viewport(
#     width = unit(1, 'grobwidth', list(p2))
#     ))
