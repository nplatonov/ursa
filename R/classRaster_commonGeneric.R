'diff.ursaRaster' <- function(x,lag=1,differences=1,...) {
   as.ursa(t(diff(t(x$value),lag=lag,differences=differences,...))
          ,nband=nband(x)-1L)
}
'duplicated.ursaRaster' <- function(x,incomparables=FALSE,MARGIN=2
                                   ,fromLast=FALSE,...) {
   duplicated(unclass(x$value),incomparables=incomparables,MARGIN=2
             ,fromLast=fromLast,...)
}
