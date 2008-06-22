## Peter Cowan 2008-06-11 Heavily modified from Emmanuel Paradis plot.phylo
## original copyright below

## plot.phylo.R (2008-05-08)

##   Plot Phylogenies

## Copyright 2002-2008 Emmanuel Paradis

## This file is part of the R-package `ape'.
## See the file ../COPYING for licensing issues.

myorder <- function(edge, tips, root = tips + 1) {
    ## if(is.null(root)) {
    ##     root <- tips + 1
    ## }
    ## if(root <= tips) {return()}
    index <- edge[, 1] == root
    nextr <- edge[index, 2]
    ## paths <- apply(as.matrix(nextr), 1, reorder, edge = edge, tips = tips)
    nord <- NULL
    for(i in nextr) {
        if(i <= tips) {next()}
        nord <- c(nord, myorder(edge, tips, root = i))
    }
    c(nord, which(index))
}

myplot <- function(x, type = "phylogram", use.edge.length = TRUE,
                       node.pos = NULL, show.tip.label = TRUE,
                       show.node.label = FALSE, edge.color = "black",
                       edge.width = 1, font = 3, cex = par("cex"),
                       adj = NULL, srt = 0, no.margin = FALSE,
                       root.edge = FALSE, label.offset = 0, underscore = FALSE,
                       x.lim = NULL, y.lim = NULL, direction = "rightwards",
                       lab4ut = "horizontal", tip.color = "black", rot = 0,  ...)
{
    require(grid)
    Ntip <- length(x@tip.label)
    
    if (Ntip == 1) stop("found only one tip in the tree!")
    
    Nedge <- dim(x@edge)[1]
    Nnode <- x@Nnode
    
    ROOT <- Ntip + 1
        
    direction <- match.arg(direction, c("rightwards", "leftwards",
                                        "upwards", "downwards"))
    
    if (is.null(x@edge.length)) use.edge.length <- FALSE
    if (!use.edge.length) root.edge <- FALSE

    xe <- x@edge
    x@edge <- x@edge[myorder(x@edge, Ntip), ]
    ## TODO does is make sense to pile edge and node data into a phylo4d object?
    ## Fix from Klaus Schliep (2007-06-16):
    ## fix from Li-San Wang (2007-01-23):

    ereorder <- match(x@edge[, 2], xe[, 2])
    edge.color <- edge.color[ereorder]
    edge.width <- edge.width[ereorder]

    edge.color <- rep(edge.color, length.out = Nedge)
    edge.width <- rep(edge.width, length.out = Nedge)

    yy <- numeric(Ntip + Nnode)
    TIPS <- x@edge[x@edge[, 2] <= Ntip, 2]
    yy[TIPS] <- 1:Ntip

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
    
    if (is.null(node.pos)) {
        node.pos <- 1
    }
    if (node.pos == 1)
      yy <- .C("node_height", as.integer(Ntip), as.integer(Nnode),
               as.integer(x@edge[, 1]), as.integer(x@edge[, 2]),
               as.integer(Nedge), as.double(yy),
               DUP = FALSE, PACKAGE = "ape")[[6]]
    else {
      ## node_height_clado requires the number of descendants
      ## for each node, so we compute `xx' at the same time
      ans <- .C("node_height_clado", as.integer(Ntip),
                as.integer(Nnode), as.integer(x@edge[, 1]),
                as.integer(x@edge[, 2]), as.integer(Nedge),
                double(Ntip + Nnode), as.double(yy),
                DUP = FALSE, PACKAGE = "ape")
      xx <- ans[[6]] - 1
      yy <- ans[[7]]
    }
    if (!use.edge.length) {
        if(node.pos != 2)
          xx <- .C("node_depth", as.integer(Ntip), as.integer(Nnode),
                   as.integer(x@edge[, 1]), as.integer(x@edge[, 2]),
                   as.integer(Nedge), double(Ntip + Nnode),
                   DUP = FALSE, PACKAGE = "ape")[[6]] - 1
        xx <- max(xx) - xx
    } else  {
          xx <- .C("node_depth_edgelength", as.integer(Ntip),
                   as.integer(Nnode), as.integer(x@edge[, 1]),
                   as.integer(x@edge[, 2]), as.integer(Nedge),
                   as.double(x@edge.length), double(Ntip + Nnode),
                   DUP = FALSE, PACKAGE = "ape")[[7]]
    }
        
    if (is.null(x.lim)) {
        x.lim <- c(0, NA)
        tmp <-
          if (show.tip.label) nchar(x@tip.label) * 0.018 * max(xx) * cex
          else 0
        x.lim[2] <- max(xx[1:Ntip] + tmp)
    } else if (length(x.lim) == 1) {
        x.lim <- c(0, x.lim)
    }

    if (is.null(y.lim)) {
        y.lim <- c(1, Ntip)
    } else if (length(y.lim) == 1) {
        y.lim <- c(0, y.lim)
        y.lim[1] <- 1
    }

    ## fix by Klaus Schliep (2008-03-28):
    if (is.null(adj))
      adj <- 0
    
    ## Grid calls Peter GSOC
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
    phylogram.plot2(x@edge, Ntip, Nnode, xx, yy,
                       edge.color, edge.width, 
                       xlim = x.lim, ylim = y.lim, layout = treelayout)
    
    if (root.edge) {
        grid.segments(0, yy[ROOT], x@root.edge, yy[ROOT])
    }
    
    L <- list(type = type, use.edge.length = use.edge.length,
              node.pos = node.pos, show.tip.label = show.tip.label,
              show.node.label = show.node.label, font = font,
              cex = cex, adj = adj, srt = srt, no.margin = no.margin,
              label.offset = label.offset, x.lim = x.lim, y.lim = y.lim,
              direction = direction, tip.color = tip.color,
              Ntip = Ntip, Nnode = Nnode)
    assign("last_plot.phylo", c(L, list(edge = xe, xx = xx, yy = yy)),
           envir = .PlotPhyloEnv)
    invisible(L)
}

phylogram.plot2 <- function(edge, Ntip, Nnode, xx, yy,
                            edge.color, edge.width, xlim, ylim, layout)
{
    nodes <- (Ntip + 1):(Ntip + Nnode)

    ## un trait vertical à chaque noeud...
    x0v <- xx[nodes]
    y0v <- y1v <- numeric(Nnode)
    for (i in nodes) {
        j <- edge[which(edge[, 1] == i), 2]
        y0v[i - Ntip] <- min(yy[j])
        y1v[i - Ntip] <- max(yy[j])
    }
    ## ... et un trait horizontal partant de chaque tip et chaque noeud
    ##  vers la racine
    sq <- if (Nnode == 1) 1:Ntip else c(1:Ntip, nodes[-1])
    y0h <- yy[sq]
    x1h <- xx[sq]
    ## match() is very useful here becoz each element in edge[, 2] is
    ## unique (not sure this is so useful in edge[, 1]; needs to be checked)
    ## `pos' gives for each element in `sq' its index in edge[, 2]
    pos <- match(sq, edge[, 2])
    x0h <- xx[edge[pos, 1]]

    e.w <- unique(edge.width)
    if (length(e.w) == 1) width.v <- rep(e.w, Nnode)
    else {
        width.v <- rep(1, Nnode)
        for (i in 1:Nnode) {
            br <- edge[which(edge[, 1] == i + Ntip), 2]
            width <- unique(edge.width[br])
            if (length(width) == 1) width.v[i] <- width
        }
    }
    e.c <- unique(edge.color)
    if (length(e.c) == 1) color.v <- rep(e.c, Nnode)
    else {
        color.v <- rep("black", Nnode)
        for (i in 1:Nnode) {
            br <- which(edge[, 1] == i + Ntip)
            #br <- edge[which(edge[, 1] == i + Ntip), 2]
            color <- unique(edge.color[br])
            if (length(color) == 1) color.v[i] <- color
        }
    }

    ## we need to reorder `edge.color' and `edge.width':
    edge.width <- edge.width[pos]
    edge.color <- edge.color[pos]
    xmax <- xlim[2]
    ymax <- ylim[2]
    ## grid calls Peter GSOC
    pushViewport(viewport(
        x = xmax/(xmax * 2), y = ymax / (ymax * 2), 
        width = xmax, height = ymax, 
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

cladogram.plot <- function(edge, xx, yy, edge.color, edge.width)
  segments(xx[edge[, 1]], yy[edge[, 1]], xx[edge[, 2]], yy[edge[, 2]],
           col = edge.color, lwd = edge.width)

unrooted.xy <- function(Ntip, Nnode, edge, edge.length)
{
    foo <- function(node, ANGLE, AXIS) {
        ind <- which(edge[, 1] == node)
        sons <- edge[ind, 2]
        start <- AXIS - ANGLE/2
        for (i in 1:length(sons)) {
            h <- edge.length[ind[i]]
            angle[sons[i]] <<- alpha <- ANGLE*nb.sp[sons[i]]/nb.sp[node]
            axis[sons[i]] <<- beta <- start + alpha/2   
            start <- start + alpha
            xx[sons[i]] <<- h*cos(beta) + xx[node]
            yy[sons[i]] <<- h*sin(beta) + yy[node]
        }
        for (i in sons)
          if (i > Ntip) foo(i, angle[i], axis[i])
    }
    root <- Ntip + 1
    Nedge <- dim(edge)[1]
    yy <- xx <- numeric(Ntip + Nnode)
    nb.sp <- .C("node_depth", as.integer(Ntip), as.integer(Nnode),
                as.integer(edge[, 1]), as.integer(edge[, 2]),
                as.integer(Nedge), double(Ntip + Nnode),
                DUP = FALSE, PACKAGE = "ape")[[6]]
    ## `angle': the angle allocated to each node wrt their nb of tips
    ## `axis': the axis of each branch
    axis <- angle <- numeric(Ntip + Nnode)
    ## start with the root...
    ## xx[root] <- yy[root] <- 0 # already set!
    foo(root, 2*pi, 0)

    M <- cbind(xx, yy)
    axe <- axis[1:Ntip] # the axis of the terminal branches (for export)
    axeGTpi <- axe > pi
    ## insures that returned angles are in [-PI, +PI]:
    axe[axeGTpi] <- axe[axeGTpi] - 2*pi
    list(M = M, axe = axe)
}

node.depth <- function(phy)
{
    n <- length(phy@tip.label)
    m <- phy@Nnode
    N <- dim(phy@edge)[1]
    phy@edge <- phy@edge[myorder(phy@edge, n), ]
    .C("node_depth", as.integer(n), as.integer(m),
       as.integer(phy@edge[, 1]), as.integer(phy@edge[, 2]),
       as.integer(N), double(n + m), DUP = FALSE, PACKAGE = "ape")[[6]]
}

## testing
## require(phylobase)
## bar <- rcoal(7)
## bar$tip.label <- c("one", "two", "three", "four", "five", "six", "seven")
## bar <- as(bar, 'phylo4')
## myplot(bar, show.tip.label = TRUE)
