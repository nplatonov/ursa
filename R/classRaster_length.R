'length.ursaRaster' <- function(x) .syn('nband',0,x)
#'band_length' <- function(...) .syn('nband',0,...)
'nband' <- function(x)
{
   if (is.null(x))
      return(0L)
   if (!is.ursa(x))
      return(NULL)
   z <- x$con$posZ
   if (!length(z))
      ret <- 0L
   else if (all(is.na(z)))
      ret <- x$dim[2]
   else
      ret <- length(z)
   ret
}
