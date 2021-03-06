#
# --- Test readNexus.R ---
#

### Get all the test files
if (Sys.getenv("RCMDCHECK") == FALSE) {
    pth <- file.path(getwd(), "..", "inst", "nexusfiles")
} else {
    pth <- system.file(package="phylobase", "nexusfiles")
}
## co1.nex -- typical output from MrBayes. Contains 2 identical trees, the first
## one having posterior probabilities as node labels
co1File <- file.path(pth, "co1.nex")

## MultiLineTrees.nex -- 2 identical trees stored on several lines
multiLinesFile <- file.path(pth, "MultiLineTrees.nex")

## treeWithDiscreteData.nex -- Mesquite file with discrete data
treeDiscDt <- file.path(pth, "treeWithDiscreteData.nex")

## treeWithPolyExcludedData.nex -- Mesquite file with polymorphic and excluded
##  characters
treePolyDt <- file.path(pth, "treeWithPolyExcludedData.nex")

## treeWithContinuousData.nex -- Mesquite file with continuous characters
treeContDt <- file.path(pth, "treeWithContinuousData.nex")

## treeWithDiscAndContData.nex -- Mesquite file with both discrete and
##    continuous data
treeDiscCont <- file.path(pth, "treeWithDiscAndContData.nex")

## Contains correct (as of 2010-03-08) phylo4 representation of one of the tree
## stored in the nexus file
mlFile <- file.path(pth, "multiLines.Rdata")

## Contains representation of data associated with continuous data
ExContDataFile <- file.path(pth, "ExContData.Rdata")

stopifnot(file.exists(co1File))
stopifnot(file.exists(treeDiscDt))
stopifnot(file.exists(multiLinesFile))
stopifnot(file.exists(mlFile))
stopifnot(file.exists(treePolyDt))
stopifnot(file.exists(treeContDt))
stopifnot(file.exists(treeDiscCont))
stopifnot(file.exists(ExContDataFile))

op <- phylobase.options()

test.readNexus <- function() {
    ## function (file, simplify=TRUE, type=c("all", "tree", "data"),
    ##   char.all=FALSE, polymorphic.convert=TRUE, levels.uniform=TRUE,
    ##   check.node.labels=c("keep", "drop", "asdata"))

    ## ########### CO1 -- MrBayes file -- tree only
    ## Tree properties
    ## Labels
    labCo1 <- c("Cow", "Seal", "Carp", "Loach", "Frog", "Chicken", "Human",
                "Mouse", "Rat", "Whale", NA, NA, NA, NA, NA, NA, NA, NA)
    names(labCo1) <- 1:18
    ## Edge lengths
    eLco1 <- c(0.143336, 0.225087, 0.047441, 0.055934, 0.124549, 0.204809, 0.073060, 0.194575,
               0.171296, 0.222039, 0.237101, 0.546258, 0.533183, 0.154442, 0.134574, 0.113163,
               0.145592)
    names(eLco1) <- c("11-1", "11-2", "11-12", "12-13", "13-14", "14-15", "15-16", "16-17", "17-3",
                      "17-4", "16-5", "15-6", "14-7", "13-18", "18-8", "18-9", "12-10")
    ## Node types
    nTco1 <-  c("tip", "tip", "tip", "tip", "tip", "tip", "tip", "tip", "tip",
                "tip", "internal", "internal", "internal", "internal", "internal",
                "internal", "internal", "internal")
    names(nTco1) <- 1:18
    ## Label values
    lVco1 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.93, 0.88, 0.99, 1.00,
               0.76, 1.00, 1.00)
    ## Read trees
    co1 <- readNexus(file=co1File, check.node.labels="asdata")
    ## Tree 1
    co1Tree1 <- co1[[1]]
    checkIdentical(labels(co1Tree1), labCo1)     # check labels
    checkIdentical(edgeLength(co1Tree1), eLco1)  # check edge lengths
    checkIdentical(nodeType(co1Tree1), nTco1)    # check node types
    checkIdentical(as(co1Tree1, "data.frame")$labelValues, lVco1) # check label values
    ## Tree 2
    co1Tree2 <- co1[[2]]
    checkIdentical(labels(co1Tree2), labCo1)     # check labels
    checkIdentical(edgeLength(co1Tree2), eLco1)  # check edge lengths
    checkIdentical(nodeType(co1Tree2), nTco1)    # check node types

    ## Check option simplify
    co1 <- readNexus(file=co1File, check.node.labels="asdata", simplify=TRUE)
    checkIdentical(length(co1), as.integer(1))   # make sure there is only one tree
    checkIdentical(labels(co1), labCo1)          # check labels
    checkIdentical(edgeLength(co1), eLco1)       # check edge lengths
    checkIdentical(nodeType(co1), nTco1)         # check node type
    checkIdentical(as(co1, "data.frame")$labelValues, lVco1)  # check label values

    ## Check option check.node.labels
    phylobase.options(allow.duplicated.labels="fail")
    checkException(readNexus(file=co1File, check.node.labels="keep")) # fail because labels aren't unique
    phylobase.options(op)
    phylobase.options(allow.duplicated.labels="ok")
    co1 <- readNexus(file=co1File, check.node.labels="keep", simplify=TRUE)
    checkIdentical(nodeLabels(co1), setNames(c(NA, "0.93", "0.88", "0.99", "1.00", "0.76", "1.00", "1.00"),
                                             11:18))
    phylobase.options(op)
    co1 <- readNexus(file=co1File, check.node.labels="drop", simplify=TRUE)
    checkIdentical(labels(co1), labCo1)          # check labels
    checkIdentical(edgeLength(co1), eLco1)       # check edge lengths
    checkIdentical(nodeType(co1), nTco1)         # check node type
    checkIdentical(as(co1, "data.frame")$labelValues, NULL)  # check label values don't exist

    ## ########### Mutli Lines -- tree only
    multiLines <- readNexus(file=multiLinesFile)
    ## load correct representation and make sure that the trees read
    ## match it
    load(mlFile)
    checkIdentical(multiLines[[1]], ml1)
    checkIdentical(multiLines[[2]], ml1)
    rm(ml1)

    ## ########### Tree + data -- file from Mesquite
    ## tree properties
    labTr <-  c("Myrmecocystussemirufus", "Myrmecocystusplacodops",
                "Myrmecocystusmendax", "Myrmecocystuskathjuli",
                "Myrmecocystuswheeleri", "Myrmecocystusmimicus",
                "Myrmecocystusdepilis", "Myrmecocystusromainei",
                "Myrmecocystusnequazcatl", "Myrmecocystusyuma",
                "Myrmecocystuskennedyi", "Myrmecocystuscreightoni",
                "Myrmecocystussnellingi", "Myrmecocystustenuinodis",
                "Myrmecocystustestaceus", "Myrmecocystusmexicanus",
                "Myrmecocystuscfnavajo", "Myrmecocystusnavajo",
                NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    names(labTr) <- 1:35
    eTr <- c(1.699299, 0.894820, 0.836689, 4.524387, 0.506099, 0.198842, 0.689044,
             2.926053, 1.724765, 1.724765, 4.650818, 4.255993, 1.083870, 1.083870,
             0.802512, 2.027251, 2.708942, 2.708942, NA, 0.284767, 2.257581,
             2.193845, 2.193845, 4.451425, 6.044804, 10.569191, 8.635503, 2.770378,
             2.770378, 12.300701, 8.275077, 5.724923, 2.855375, 2.869547, 2.869547)
    names(eTr) <- c("19-20","20-21","21-22","22-23","23-24","24-25","25-26","26-27",
                    "27-1", "27-2","26-3","25-28","28-4","28-5","24-29","29-30",
                    "30-6","30-7","0-19","29-31","31-32","32-8","32-9","31-10",
                    "23-11","22-12","21-33","33-13","33-14","20-15","19-34","34-16",
                    "34-35","35-17","35-18")
    nTtr <- c("tip", "tip", "tip", "tip", "tip", "tip", "tip", "tip", "tip",
              "tip", "tip", "tip", "tip", "tip", "tip", "tip", "tip", "tip",
              "root", "internal", "internal", "internal", "internal", "internal",
              "internal", "internal", "internal", "internal", "internal",
              "internal", "internal", "internal", "internal", "internal",
              "internal")
    names(nTtr) <- 1:35
    ## data to test against
    dtTest1 <- data.frame(time = factor(c(2,1,0,0,0,0,2,0,2,0,0,0,0,1,1,1,0,1)),
                          subgenus = factor(c(2,1,0,0,0,0,2,0,2,0,0,0,0,1,1,2,0,1)))
    row.names(dtTest1) <- c("Myrmecocystuscfnavajo","Myrmecocystuscreightoni",
                            "Myrmecocystusdepilis","Myrmecocystuskathjuli",
                            "Myrmecocystuskennedyi","Myrmecocystusmendax",
                            "Myrmecocystusmexicanus","Myrmecocystusmimicus",
                            "Myrmecocystusnavajo","Myrmecocystusnequazcatl",
                            "Myrmecocystusplacodops","Myrmecocystusromainei",
                            "Myrmecocystussemirufus","Myrmecocystussnellingi",
                            "Myrmecocystustenuinodis","Myrmecocystustestaceus",
                            "Myrmecocystuswheeleri","Myrmecocystusyuma")
    dtTest2 <- dtTest1
    levels(dtTest2$time) <- c("diurnal", "crepuscular", "nocturnal")
    levels(dtTest2$subgenus) <- c("Endiodioctes", "Eremnocystus", "Myrmecocystus")
    p4 <- "phylo4"
    p4d <- "phylo4d"
    attributes(p4) <- attributes(p4d) <- list(package="phylobase")
    ## Tree only
    tr <- readNexus(file=treeDiscDt, type="tree")
    checkIdentical(labels(tr), labTr)   # check labels
    checkIdentical(edgeLength(tr), eTr) # check edge lengths
    checkIdentical(nodeType(tr), nTtr)  # check node types
    checkIdentical(class(tr), p4)       # check class
    ## Data only
    dt1 <- readNexus(file=treeDiscDt, type="data", return.labels=FALSE,
                     levels.uniform=FALSE)
    checkIdentical(dt1, dtTest1)
    dt2 <- readNexus(file=treeDiscDt, type="data", return.labels=TRUE,
                     levels.uniform=FALSE)
    checkIdentical(dt2, dtTest2)
    ## Tree + Data
    trDt1 <- readNexus(file=treeDiscDt, type="all", return.labels=FALSE,
                       levels.uniform=FALSE)
    checkIdentical(labels(trDt1), labTr)   # check labels
    checkIdentical(edgeLength(trDt1), eTr) # check edge lengths
    checkIdentical(nodeType(trDt1), nTtr)  # check node types
    checkIdentical(class(trDt1), p4d)      # check class
    checkIdentical(tdata(trDt1, type="tip")[rownames(dtTest1), ], dtTest1)
    trDt2 <- readNexus(file=treeDiscDt, type="all", return.labels=TRUE,
                       levels.uniform=FALSE)
    checkIdentical(labels(trDt2), labTr)   # check labels
    checkIdentical(edgeLength(trDt2), eTr) # check edge lengths
    checkIdentical(nodeType(trDt2), nTtr)  # check node types
    checkIdentical(class(trDt2), p4d)      # check class
    checkIdentical(tdata(trDt2, type="tip")[rownames(dtTest2), ], dtTest2)

    ## ########## Tree + Data -- Test for polymorphic.convert, levels.uniform and char.all
    ## data to test against
    ## dtTest 3 -- levels.uniform=FALSE, return.labels=FALSE, polymorphic.convert=FALSE
    dtPoly1 <- data.frame(Test1=factor(c(0,0,1,1,0,NA,1,1,1,0,0,NA,1,1,NA,0,1,
                          NA)),
                          Test2=factor(c(0,0,0,0,0,NA,0,1,0,1,1,
                          "{0,1}",NA,0,NA,0,"{0,1}",1)),
                          Test3=factor(c(1,1,1,0,0,0,2,
                          "{0,1,2}",0,NA,0,"{0,1}",0,1,0,0,"{0,1,2}",1)),
                          row.names=c("Myrmecocystussemirufus","Myrmecocystusplacodops",
                          "Myrmecocystusmendax","Myrmecocystuskathjuli",
                          "Myrmecocystuswheeleri","Myrmecocystusmimicus",
                          "Myrmecocystusdepilis","Myrmecocystusromainei",
                          "Myrmecocystusnequazcatl","Myrmecocystusyuma",
                          "Myrmecocystuskennedyi","Myrmecocystuscreightoni",
                          "Myrmecocystussnellingi","Myrmecocystustenuinodis",
                          "Myrmecocystustestaceus","Myrmecocystusmexicanus",
                          "Myrmecocystuscfnavajo","Myrmecocystusnavajo"))
    ## dtPoly2 -- levels.uniform=FALSE, return.labels=FALSE, polymorphic.convert=TRUE
    dtPoly2 <- dtPoly1
    dtPoly2[c(12,17),2] <- NA
    dtPoly2[c(8,12,17),3] <- NA
    dtPoly2$Test1 <- factor(dtPoly2$Test1)
    dtPoly2$Test2 <- factor(dtPoly2$Test2)
    dtPoly2$Test3 <- factor(dtPoly2$Test3)
    ## dtPoly3 -- levels.uniform=FALSE, return.labels=TRUE, polymorphic.convert=TRUE
    dtPoly3 <- dtPoly2
    levels(dtPoly3$Test1) <- c("test1A", "test1B")
    levels(dtPoly3$Test2) <- c("test2A", "test2B")
    levels(dtPoly3$Test3) <- c("test3A", "test3B", "test3C")
    ## dtPoly4 -- levels.uniform=FALSE, return.labels=TRUE, polymorphic.convert=FALSE
    ##    not yet implemented

    ## dtPoly5 -- levels.uniform=TRUE, return.labels=FALSE, polymorphic.convert=FALSE
    dtPoly5 <- dtPoly1
    levels(dtPoly5$Test1) <- levels(dtPoly5$Test2) <- levels(dtPoly5$Test3) <-
        union(levels(dtPoly1$Test1), c(levels(dtPoly1$Test2), levels(dtPoly1$Test3)))
    ## dtPoly6 -- levels.uniform=TRUE, return.labels=FALSE, polymorphic.convert=TRUE
    dtPoly6 <- dtPoly2
    levels(dtPoly6$Test1) <- levels(dtPoly6$Test2) <- levels(dtPoly6$Test3) <-
        union(levels(dtPoly2$Test1), c(levels(dtPoly2$Test2), levels(dtPoly2$Test3)))
    ## dtPoly7 -- levels.uniform=TRUE, return.labels=TRUE, polymorphic.convert=FALSE
    ##    not yet implemented

    ## dtPoly8 -- levels.uniform=TRUE, return.labels=TRUE, polymorphic.convert=TRUE
    dtPoly8 <- dtPoly3
    levels(dtPoly8$Test1) <- levels(dtPoly8$Test2) <- levels(dtPoly8$Test3) <-
        union(levels(dtPoly3$Test1), c(levels(dtPoly3$Test2), levels(dtPoly3$Test3)))
    ## dtPoly5F -- char.all=FALSE, levels.uniform=TRUE, return.labels=FALSE, polymorphic.convert=FALSE
    dtPoly5F <- dtPoly1[, 1:2]
    levels(dtPoly5F$Test1) <- levels(dtPoly5F$Test2) <-
        union(levels(dtPoly1$Test1), levels(dtPoly1$Test2))
    ## dtPoly6F -- char.all=FALSE, levels.uniform=TRUE, return.labels=FALSE, polymorphic.convert=TRUE
    dtPoly6F <- dtPoly2[, 1:2]
    levels(dtPoly6F$Test1) <- levels(dtPoly6F$Test2) <-
        union(levels(dtPoly2$Test1), levels(dtPoly2$Test2))
    ## dtPoly8F -- char.all=FALSE, levels.uniform=TRUE, return.labels=TRUE, polymorphic.convert=TRUE
    dtPoly8F <- dtPoly3[, 1:2]
    levels(dtPoly8F$Test1) <- levels(dtPoly8F$Test2) <-
        union(levels(dtPoly3$Test1), levels(dtPoly3$Test2))

    ## char.all=TRUE, levels.uniform=FALSE, return.labels=FALSE, polymorphic.convert=FALSE
    trChr1 <- readNexus(file=treePolyDt, type="all", polymorphic.convert=FALSE,
                        levels.uniform=FALSE, char.all=TRUE, return.labels=FALSE)
    checkIdentical(labels(trChr1), labTr)   # check labels
    checkIdentical(edgeLength(trChr1), eTr) # check edge lengths
    checkIdentical(nodeType(trChr1), nTtr)  # check node types
    checkIdentical(class(trChr1), p4d)      # check class
    checkIdentical(tdata(trChr1, "tip"), dtPoly1)
    ## char.all=TRUE, levels.uniform=FALSE, return.labels=FALSE, polymorphic.convert=TRUE
    trChr2 <- readNexus(file=treePolyDt, type="all", polymorphic.convert=TRUE,
                        levels.uniform=FALSE, return.labels=FALSE, char.all=TRUE)
    checkIdentical(labels(trChr2), labTr)   # check labels
    checkIdentical(edgeLength(trChr2), eTr) # check edge lengths
    checkIdentical(nodeType(trChr2), nTtr)  # check node types
    checkIdentical(class(trChr2), p4d)      # check class
    checkIdentical(tdata(trChr2, "tip"), dtPoly2)
    ## char.all=TRUE, levels.uniform=FALSE, return.labels=TRUE, polymorphic.convert=TRUE
    trChr3 <- readNexus(file=treePolyDt, type="all", polymorphic.convert=TRUE,
                        levels.uniform=FALSE, char.all=TRUE, return.labels=TRUE)
    checkIdentical(labels(trChr3), labTr)   # check labels
    checkIdentical(edgeLength(trChr3), eTr) # check edge lengths
    checkIdentical(nodeType(trChr3), nTtr)  # check node types
    checkIdentical(class(trChr3), p4d)      # check class
    checkIdentical(tdata(trChr3, "tip"), dtPoly3)
    ## char.all=TRUE, levels.uniform=FALSE, return.labels=TRUE, polymorphic.convert=FALSE
    ##   not yet implemented
    ## trChr4 <-
    checkException(readNexus(file=treePolyDt, type="all",
                             levels.uniform=FALSE,
                             return.labels=TRUE,
                             polymorphic.convert=FALSE))
    ## char.all=TRUE, levels.uniform=TRUE, return.labels=FALSE, polymorphic.convert=FALSE
    trChr5 <- readNexus(file=treePolyDt, type="all", polymorphic.convert=FALSE,
                        levels.uniform=TRUE, char.all=TRUE, return.labels=FALSE)
    checkIdentical(labels(trChr5), labTr)   # check labels
    checkIdentical(edgeLength(trChr5), eTr) # check edge lengths
    checkIdentical(nodeType(trChr5), nTtr)  # check node types
    checkIdentical(class(trChr5), p4d)      # check class
    checkIdentical(tdata(trChr5, "tip"), dtPoly5)
    ## char.all=TRUE, levels.uniform=TRUE, return.labels=FALSE, polymorphic.convert=TRUE
    trChr6 <- readNexus(file=treePolyDt, type="all", polymorphic.convert=TRUE,
                        levels.uniform=TRUE, char.all=TRUE, return.labels=FALSE)
    checkIdentical(labels(trChr6), labTr)   # check labels
    checkIdentical(edgeLength(trChr6), eTr) # check edge lengths
    checkIdentical(nodeType(trChr6), nTtr)  # check node types
    checkIdentical(class(trChr6), p4d)      # check class
    checkIdentical(tdata(trChr6, "tip"), dtPoly6)
    ## char.all=TRUE, levels.uniform=TRUE, return.labels=TRUE, polymorphic.convert=FALSE
    ##    not yet implemented
    ## trChr7 <-
    checkException(readNexus(file=treePolyDt, type="all", char.all=TRUE,
                             levels.uniform=TRUE,
                             return.labels=TRUE,
                             polymorphic.convert=FALSE))
    ## char.all=TRUE, levels.uniform=TRUE, return.labels=TRUE, polymorphic.convert=TRUE
    trChr8 <- readNexus(file=treePolyDt, type="all", char.all=TRUE,
                        levels.uniform=TRUE,
                        return.labels=TRUE,
                        polymorphic.convert=TRUE)
    checkIdentical(labels(trChr8), labTr)   # check labels
    checkIdentical(edgeLength(trChr8), eTr) # check edge lengths
    checkIdentical(nodeType(trChr8), nTtr)  # check node types
    checkIdentical(class(trChr8), p4d)      # check class
    checkIdentical(tdata(trChr8, "tip"), dtPoly8)

    ## -- with char.all=FALSE
    ## char.all=FALSE, levels.uniform=FALSE, return.labels=FALSE, polymorphic.convert=FALSE
    trChr1F <- readNexus(file=treePolyDt, type="all", polymorphic.convert=FALSE,
                        levels.uniform=FALSE, char.all=FALSE, return.labels=FALSE)
    checkIdentical(labels(trChr1F), labTr)   # check labels
    checkIdentical(edgeLength(trChr1F), eTr) # check edge lengths
    checkIdentical(nodeType(trChr1F), nTtr)  # check node types
    checkIdentical(class(trChr1F), p4d)      # check class
    checkIdentical(tdata(trChr1F, "tip"), dtPoly1[, 1:2])
    ## char.all=FALSE, levels.uniform=FALSE, return.labels=FALSE, polymorphic.convert=TRUE
    trChr2F <- readNexus(file=treePolyDt, type="all", polymorphic.convert=TRUE,
                        levels.uniform=FALSE, return.labels=FALSE, char.all=FALSE)
    checkIdentical(labels(trChr2F), labTr)   # check labels
    checkIdentical(edgeLength(trChr2F), eTr) # check edge lengths
    checkIdentical(nodeType(trChr2F), nTtr)  # check node types
    checkIdentical(class(trChr2F), p4d)      # check class
    checkIdentical(tdata(trChr2F, "tip"), dtPoly2[, 1:2])
    ## char.all=FALSE, levels.uniform=FALSE, return.labels=TRUE, polymorphic.convert=TRUE
    trChr3F <- readNexus(file=treePolyDt, type="all", polymorphic.convert=TRUE,
                        levels.uniform=FALSE, char.all=FALSE, return.labels=TRUE)
    checkIdentical(labels(trChr3F), labTr)   # check labels
    checkIdentical(edgeLength(trChr3F), eTr) # check edge lengths
    checkIdentical(nodeType(trChr3F), nTtr)  # check node types
    checkIdentical(class(trChr3F), p4d)      # check class
    checkIdentical(tdata(trChr3F, "tip"), dtPoly3[, 1:2])
    ## char.all=FALSE, levels.uniform=FALSE, return.labels=TRUE, polymorphic.convert=FALSE
    ##   not yet implemented
    ## trChr4F <-
    checkException(readNexus(file=treePolyDt, type="all",
                             levels.uniform=FALSE,
                             return.labels=TRUE,
                             polymorphic.convert=FALSE))
    ## char.all=FALSE, levels.uniform=TRUE, return.labels=FALSE, polymorphic.convert=FALSE
    trChr5F <- readNexus(file=treePolyDt, type="all", polymorphic.convert=FALSE,
                        levels.uniform=TRUE, char.all=FALSE, return.labels=FALSE)
    checkIdentical(labels(trChr5F), labTr)   # check labels
    checkIdentical(edgeLength(trChr5F), eTr) # check edge lengths
    checkIdentical(nodeType(trChr5F), nTtr)  # check node types
    checkIdentical(class(trChr5F), p4d)      # check class
    checkIdentical(tdata(trChr5F, "tip"), dtPoly5F)
    ## char.all=FALSE, levels.uniform=TRUE, return.labels=FALSE, polymorphic.convert=TRUE
    trChr6F <- readNexus(file=treePolyDt, type="all", polymorphic.convert=TRUE,
                        levels.uniform=TRUE, char.all=FALSE, return.labels=FALSE)
    checkIdentical(labels(trChr6F), labTr)   # check labels
    checkIdentical(edgeLength(trChr6F), eTr) # check edge lengths
    checkIdentical(nodeType(trChr6F), nTtr)  # check node types
    checkIdentical(class(trChr6F), p4d)      # check class
    checkIdentical(tdata(trChr6F, "tip"), dtPoly6F)
    ## char.all=FALSE, levels.uniform=TRUE, return.labels=TRUE, polymorphic.convert=FALSE
    ##    not yet implemented
    ## trChr7F <-
    checkException(readNexus(file=treePolyDt, type="all", char.all=FALSE,
                             levels.uniform=TRUE,
                             return.labels=TRUE,
                             polymorphic.convert=FALSE))
    ## char.all=FALSE, levels.uniform=TRUE, return.labels=TRUE, polymorphic.convert=TRUE
    trChr8F <- readNexus(file=treePolyDt, type="all", char.all=FALSE,
                         levels.uniform=TRUE,
                         return.labels=TRUE,
                         polymorphic.convert=TRUE)
    checkIdentical(labels(trChr8F), labTr)   # check labels
    checkIdentical(edgeLength(trChr8F), eTr) # check edge lengths
    checkIdentical(nodeType(trChr8F), nTtr)  # check node types
    checkIdentical(class(trChr8F), p4d)      # check class
    checkIdentical(tdata(trChr8F, "tip"), dtPoly8F)

    ## ########## Tree + Data -- test with continuous Characters
    DtCont <- readNexus(file=treeContDt, type="data")
    trDtCont <- readNexus(file=treeContDt, type="all")
    load(ExContDataFile)
    checkIdentical(DtCont, ExContData[rownames(DtCont), ])
    checkIdentical(tdata(trDtCont, "tip"), ExContData)
    rm(ExContData)
    checkIdentical(labels(trDtCont), labTr)   # check labels
    checkIdentical(edgeLength(trDtCont), eTr) # check edge lengths
    checkIdentical(nodeType(trDtCont), nTtr)  # check node types
    checkIdentical(class(trDtCont), p4d)      # check class

    ## ########## Tree + Data -- both types (Discrete & Continuous)
    dtDiscCont <- readNexus(file=treeDiscCont, type="data", levels.uniform=FALSE)
    trDtDiscCont <- readNexus(file=treeDiscCont, type="all", levels.uniform=FALSE)
    load(ExContDataFile)
    dtDiscContTest <- cbind(ExContData, dtTest2[rownames(ExContData), ])
    rm(ExContDataFile)
    checkIdentical(dtDiscCont, dtDiscContTest[rownames(dtDiscCont), ])
    checkIdentical(tdata(trDtDiscCont, "tip"), dtDiscContTest)
    checkIdentical(labels(trDtDiscCont), labTr)   # check labels
    checkIdentical(edgeLength(trDtDiscCont), eTr) # check edge lengths
    checkIdentical(nodeType(trDtDiscCont), nTtr)  # check node types
    checkIdentical(class(trDtDiscCont), p4d)      # check class
}

