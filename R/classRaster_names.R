'names.ursaRaster' <- function(x) .syn('bandname',0,x)
'names<-.ursaRaster' <- function(x,value) .syn('bandname<-',0,x,value)
#'band_names' <- function(x) .syn('bandname',0,x)
#'band_names<-' <- function(x,value) .syn('bandname<-',0,x,value)
'bandname' <- function(x)
{
   if (is.ursa(x))
   {
      z <- x$con$posZ
      if (all(is.na(z)))
         return(x$name)
      return(x$name[z])
   }
   if ((is.character(x))&&(envi_exists(x)))
   {
      envi <- open_envi(x,decompress=FALSE)
      res <- envi$name
      if (!is.na(envi$con$posZ))
        res <- res[envi$con$posZ]
      close(envi)
      return(res)
   }
   NULL
}
'bandname<-' <- function(x,value)
{
   if (!is.ursa(x))
      return(NULL)
   if (is.null(value))
      value <- character()
   z <- x$con$posZ
   if (all(is.na(z))) {
     # n1 <- length(x$name)
      n2 <- x$dim[3]
      x$name <- rep(value,length=n2)
   }
   else
      x$name[z] <- rep(value,length=length(z))
   x
}
