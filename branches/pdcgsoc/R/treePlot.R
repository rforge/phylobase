treePlot <- function(phy, 
                     type = c('phylogram', 'cladogram', 'fan'), 
                     show.tip.label = TRUE,
                     show.node.label = FALSE, 
                     tip.order = NULL,
                     plot.data = is(phy, 'phylo4d'),
                     rot = 0,
                     tip.plot.fun = 'bubbles',
                     edge.color = 'black', ## TODO colors for branhes and nodes seperately?
                     node.color = 'black',
                     tip.color  = 'black', 
                     edge.width = 1,  ## TODO currently only one width is allowed allow many?
                     ...
            )
{
    if (!isRooted(phy)) stop("treePlot function requires a rooted tree.")
    width <- height <- 0.9
    type <- match.arg(type)
    phy.orig <- phy
    Nedges   <- nrow(phy@edge)
    Ntips    <- length(phy@tip.label)
    if(is.null(edgeLength(phy)) || type == 'cladogram') {
        # TODO there should be an abstraction for assigning branch lengths
        phy@edge.length <- rep(1, nrow(phy@edge))
    }
    xxyy <- phyloXXYY(phy, tip.order)
    phy <- xxyy$phy
    # TODO this is pointless no? simply returns 1:Ntips
    tindex <- phy@edge[phy@edge[, 2] <= Ntips, 2]
    if(type == 'cladogram') {
        xxyy$xx[phy@edge[, 2] <= Ntips] <- 1
    }
    # TODO add symbols at the nodes, allow coloirng and sizing downViewport approach?
    # TODO cladogram methods incorrect
    # TODO abstract, make ultrametric? good algorithms for this?
    grid.newpage()
    ## because we may reoder the tip, we need to update the phy objec
    
    if(!plot.data) {
        phyplotlayout <- grid.layout(nrow = 1, ncol = 1)
        # TODO for very long plots, alternative margin setting useful
        pushViewport(viewport(width = width, height = height, 
                            layout = phyplotlayout, 
                            name = 'phyplotlayout', angle = -rot))
        pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 1))
            tree.plot(xxyy, type, show.tip.label, show.node.label, 
                edge.color, node.color, tip.color, 
                edge.width, rot)
        upViewport()
        upViewport()
        # TODO should return something useful
        return(invisible())
    }
    
    if(plot.data) {
        if(is.function(tip.plot.fun)) {
            datalayout <- grid.layout(ncol = 2,
                width = unit(c(1, 1/Ntips), c('null', 'null')) 
                )
                # TODO this is done multiple times, 
                pushViewport(viewport(width = width, height = height, 
                                    layout = datalayout, 
                                    name = 'datalayout', angle = -rot))
                pushViewport(viewport(layout.pos.col = 1))
                    tree.plot(xxyy, type, show.tip.label, show.node.label, 
                        edge.color, node.color, tip.color, 
                        edge.width, rot)
                upViewport()
                
                pushViewport(viewport(
                    yscale = c(-0.5/Ntips, 1 + 0.5/Ntips), 
                    xscale = c(0, 1 + 1/Ntips), 
                    layout.pos.col = 1, 
                    name = 'data_plots'))
                ## TODO should plots float at tips, or only along edge?
                hc <- convertY(unit(1/Ntips, 'snpc'), 'npc')
                for(i in 1:Ntips) {
                    pushViewport(viewport(
                        y = xxyy$yy[phy@edge[, 2] == i],
                        x = 1, # xxyy$xx[phy@edge[, 2] == i], 
                        height = hc, 
                        width = hc, 
                        # default.units = 'native', 
                        name = paste('data_plot', i),
                        just = "left"
                        ))
                        grid.rect()
                        tip.plot.fun(t(tdata(phy, which = 'tip')[i, ]))
                    upViewport()
                }
                upViewport()
                upViewport()
        } else {
            # use phylobubbles as default
            dlabwdth <- max(stringWidth(colnames(phy@tip.data)))
            phyplotlayout <- grid.layout(nrow = 2, ncol = 2, 
                heights = unit.c(unit(1, 'null', NULL), dlabwdth), 
                widths = unit(c(1, 1), c('null', 'null'), list(NULL, NULL))
                )
                pushViewport(viewport(width = width, height = height, 
                                    layout = phyplotlayout, 
                                    name = 'phyplotlayout', angle = -rot))
                pushViewport(viewport(layout.pos.row = 1:2, layout.pos.col = 2,
                                    height = unit(1, 'npc', NULL) + 
                                                convertUnit(dlabwdth, 'npc'), 
                                    default.units = 'native'))
                    phylobubbles(xxyy, ...)
                upViewport()
                pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 1))
                    tree.plot(xxyy, type, show.tip.label, show.node.label, 
                        edge.color, node.color, tip.color, 
                        edge.width, rot)
                upViewport()
            upViewport()
        }
    }
}

tree.plot <- function(xxyy, type, show.tip.label, show.node.label, edge.color, 
                        node.color, tip.color, edge.width, rot) 
{
    # TODO switch to phylobase abstractions
    phy <- xxyy$phy
    Nedges   <- nrow(phy@edge)
    Ntips    <- length(phy@tip.label)
    tindex <- phy@edge[phy@edge[, 2] <= Ntips, 2]
    eindex <- match(phy@edge[,2], xxyy$phy.orig@edge[,2])
    segs <- segs(XXYY = xxyy)

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
        treelayout <- grid.layout(nrow = 1, ncol = 2,
            widths = unit.c(unit(1, 'null', NULL), labw + unit(0.02, 'npc'))
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
    if (identical(type,"fan")) {
        dseg <- grid.segments( # draws vertical lines
            x0 = segs$v0x, y0 = segs$v0y, 
            x1 = segs$h1x, y1 = segs$h1y, 
            name = "diag", gp = gpar(col = edge.color, lwd = edge.width))     
    }
    else
    {
        vseg <- grid.segments( # draws vertical lines
            x0 = segs$v0x, y0 = segs$v0y, 
            x1 = segs$v1x, y1 = segs$v1y, 
            name = "vert", gp = gpar(col = edge.color, lwd = edge.width)) 
        hseg <- grid.segments(  # draws horizontal lines
            x0 = segs$h0x, y0 = segs$h0y, 
            x1 = segs$h1x, y1 = segs$h1y, 
            name = "horz", gp = gpar(col = edge.color, lwd = edge.width))
    }
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
    # TODO probably want to be able to adjust the location of these guys
    if(show.node.label) {
        pushViewport(viewport(
            layout = treelayout, layout.pos.col = 1, 
            ))
        labtext <- grid.text(
            phy@node.label[nindex], 
            x = xxyy$xx[phy@edge[, 2] > Ntips], 
            ## TODO yuck!!
            y = xxyy$yy[phy@edge[, 2] > Ntips], 
            default.units = 'npc', 
            rot = rot, just = 'left', gp = gpar(col = node.color[nindex])
        )
        upViewport()
    }
    upViewport()
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
    if(is.null(edgeLength(phy))) {
        # TODO there should be an abstraction for assigning branch lengths
        stop('Phylogeny has no branch lengths, cannot calculate x coordinates')
    }
    
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

segs <- function(XXYY) {
    phy <- XXYY$phy
    treelen <- rep(NA, nrow(phy@edge) + 1)
    segs <- list(v0x = treelen, v0y = treelen, v1x = treelen, v1y = treelen,
                 h0x = treelen, h0y = treelen, h1x = treelen, h1y = treelen)
    troot <- length(phy@tip.label) + 1

    get.coor <- function(node, segs) {
        if(any(phy@edge[, 2] == node) == FALSE) {
            #root
            decdex <- which(phy@edge[, 1] == node)
            index <- length(treelen)
            segs$v0y[index] <- segs$v1y[index] <- NA
            segs$v0x[index] <- segs$v1x[index] <- NA
            segs$h0y[index] <- segs$h1y[index] <- NA
            segs$h0x[index] <- segs$h1x[index] <- NA
            segs$v0x[decdex] <- segs$v1x[decdex] <- segs$h0x[decdex] <- 0            
            segs$v0y[decdex] <- mean(XXYY$yy[decdex])            
        } else {
            #not root
            index <- which(phy@edge[, 2] == node)
            segs$h1x[index] <- XXYY$xx[index]
            segs$h0y[index] <- segs$h1y[index] <- segs$v1y[index] <- XXYY$yy[index]
            if(!any(phy@edge[, 1] == node)) {
                return(segs)
            }
            decdex <- which(phy@edge[, 1] == phy@edge[index, 2])
            segs$v0x[decdex] <- segs$v1x[decdex] <- segs$h0x[decdex] <- XXYY$xx[index]            
            segs$v0y[decdex] <- mean(XXYY$yy[decdex])           
        }
        
        for(i in phy@edge[decdex, 2]) {
            segs <- get.coor(i, segs)
        }
        segs
    }
    get.coor(troot, segs)
}

phylobubbles <- function(XXYY, square = FALSE) {
    phy <- XXYY$phy
    
    tys <- XXYY$yy[phy@edge[, 2] <= nTips(phy)]
    
    traits <- phy@tip.data

    maxr <- ifelse(ncol(traits) > nTips(phy), 1/ncol(traits), 1/nTips(phy))

    tnames <- names(traits)
    traits <- scale(traits)
    traits <- apply(traits, 2, function(x) maxr * x / max(abs(x), na.rm = T))
    names(traits) <- tnames
    
    if(ncol(traits) == 1) {
        xpos <- 0.5
    } else {
        xpos <- seq(0+maxr, 1-maxr, length.out = ncol(traits))
    }
    tys <- tys # * (1 - (2 * maxr)) + maxr
    xrep <- rep(xpos, each = length(tys))
    ccol <- ifelse(traits < 0, 'black', 'white')
    nays <- tys[apply(traits, 1, function(x) any(is.na(x)))]
    naxs <- xpos[apply(traits, 2, function(x) any(is.na(x)))]
    traits[is.na(traits)] <- 0
    
    datalabwidth <- max(stringWidth(colnames(traits)))
    tiplabwidth  <- max(stringWidth(phy@tip.label))
    
    bublayout <- grid.layout(nrow = 2, ncol = 2,
        widths = unit.c(unit(1, 'null', NULL), tiplabwidth), 
        heights = unit.c(unit(1, 'null', NULL), datalabwidth))
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
    if(square) {
        # to keep the squares square, yet resize nicely use the square npc
        sqedge <- unit(unlist(traits), 'snpc')
        grid.rect(x = xrep, y = tys, 
            width = sqedge, 
            height = sqedge, 
            gp=gpar(fill = ccol))
    } else {
        grid.circle(xrep, tys, r = unlist(traits), gp = gpar(fill = ccol))
    }
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

