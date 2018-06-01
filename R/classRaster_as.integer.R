'as.integer.ursaRaster' <- function(x,...) {
   clname <- class(x$value)
   dimx <- dim(x$value)
   x$value <- as.integer(x$value,...)
   ignorevalue(x) <- .optimal.nodata(x) ## added 20161206
   dim(x$value) <- dimx
   class(x$value) <- clname
   x
}
'.is.integer.ursaRaster' <- function(x,...) {
   print("needs Generic")
   NA
}
