'sort.ursaRaster' <- function(x,decreasing=FALSE,...) {
   x[sort(names(x),decreasing=decreasing,...)]
}

