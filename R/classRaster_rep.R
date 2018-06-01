'.rep.ursaRaster.hide' <- function(x,...) {
   args <- list(...)
   if (is.null(names(args)))
      n <- args[[1]]
   else
      n <- args[[match("times",names(args))]]
   if ((!is.numeric(n))||(n<0))
   {
      op <- options(warn=0)
      warning("incorrect parameter for 'rep()' function. Return arg#1")
      options(op)
      return(x)
   }
   res <- ursa_new(bandname=rep(bandname(x),n),nodata=ignorevalue(x))
   ind <- match(bandname(res),bandname(x))
   for (i in seq(res))
      res[i] <- x[ind[i]]
   res
}
'rep.ursaRaster' <- function(x,...) {
   arglist <- list(...)
  # a <- c(list(bandname(x)),arglist)
  # str(a)
   res <- ursa_new(bandname=do.call("rep",c(list(bandname(x)),arglist))
                  ,nodata=ignorevalue(x))
   ind <- match(bandname(res),bandname(x))
   for (i in seq(res))
      res[i] <- x[ind[i]]
   class(res$value) <- class(x$value)
   ursa_colortable(res) <- ursa_colortable(x)
   res
}
