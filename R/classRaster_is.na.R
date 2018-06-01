'is.na.ursaRaster' <- function(x) 
{
   if (is.na(x$con$posZ[1]))
      val <- rep(NA,prod(x$dim))
   else
      val <- rep(NA,x$dim[1]*length(x$con$posZ))
   val[which(is.na(c(x$value)))] <- 1L
   x$value[] <- val
   if (!is.na(ignorevalue(x)))
      ignorevalue(x) <- 127L
   rm(val)
   .gc()
   x
}
'is.infinite.ursaRaster' <- function(x) 
{
   if (is.na(x$con$posZ[1]))
      val <- rep(NA,prod(x$dim))
   else
      val <- rep(NA,x$dim[1]*length(x$con$posZ))
   val[which(is.infinite(c(x$value)))] <- 1L
   x$value[] <- val
   if (!is.na(ignorevalue(x)))
      ignorevalue(x) <- 127L
   rm(val)
   .gc()
   x
}
'is.nan.ursaRaster' <- function(x) 
{
   if (is.na(x$con$posZ[1]))
      val <- rep(NA,prod(x$dim))
   else
      val <- rep(NA,x$dim[1]*length(x$con$posZ))
   val[which(is.nan(c(x$value)))] <- 1L
   x$value[] <- val
   if (!is.na(ignorevalue(x)))
      ignorevalue(x) <- 127L
   rm(val)
   .gc()
   x
}
'is.na<-.ursaRaster' <- function(x,value)
{
   x$value[is.na(x$value)] <- value
   x
}
