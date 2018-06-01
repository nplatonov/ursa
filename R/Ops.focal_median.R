'focal_median' <- function(x,size=3,cover=1e-6,fillNA=FALSE,saveMargin=TRUE
                          ,verbose=0L)
{
   fun <- 'focalMedian' # as.character(match.call()[1])
  # print(fun)
   if (!is.ursa(x))
      return(NULL)
   sparse <- attr(x$value,"sparse")
   if ((!is.null(sparse))&&(any(na.omit(sparse)!=0)))
      stop("TODO: expand compression")
   if (!is.na(x$con$posZ[1]))
      nb <- length(x$con$posZ)
   else
      nb <- x$dim[2]
   dimy <- with(x$grid,c(columns,rows,nb))
   dim(x$value) <- dimy
   x$value <- as.numeric(x$value)
   if (verbose>2)
      .elapsedTime(paste0("start:nodata:",fun))
   nodata <- x$con$nodata
   if (is.na(nodata))
      nodata <- max(x$value,na.rm=TRUE)+1
   x$value[is.na(x$value)] <- nodata
   if (verbose>2)
      .elapsedTime(paste0("finish:nodata:",fun))
   if (verbose>1)
      print(summary(x$value))
   if (verbose)
      .elapsedTime(paste0("start:",fun))
   x$value <- .Cursa("focalMedian",x=x$value,nodata=as.numeric(nodata)
                ,dim=as.integer(dimy),size=as.integer(size)
                ,fillNA=as.integer(fillNA),saveMargin=as.integer(saveMargin)
                ,cover=as.numeric(cover),verbose=as.integer(verbose)
                ,res=numeric(prod(dimy)),NAOK=FALSE)$res
   if (verbose)
      .elapsedTime(paste0("finish:",fun))
   if (verbose>1)
      print(summary(x$value))
   if (verbose>2)
      .elapsedTime(paste0("start:nodata:",fun))
   if (abs(nodata)<1)
      x$value[abs(x$value-nodata)<1e-27] <- NA
   else
      x$value[abs(x$value/nodata-1)<1e-6] <- NA
   if (verbose>2)
      .elapsedTime(paste0("finish:nodata:",fun))
   dim(x$value) <- with(x$grid,c(columns*rows,nb))
   class(x$value) <- "ursaNumeric"
   x
}
