.First.lib <- function(lib, pkg) {
	require(ape)
        if (.Platform$OS.type!="windows") {
            library.dynam("phylobase", pkg, lib )
        } else {
            warning("no NCL built for Windows")
        }
    }


