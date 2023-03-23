'focal_min' <- function(x,size=3,cover=1e-6,fillNA=FALSE,saveMargin=TRUE
                       ,verbose=0L) {
   focal_extrem(x=x,kind="min",size=size,cover=cover,fillNA=fillNA
               ,saveMargin=saveMargin,verbose=verbose)
}
'focal_max' <- function(x,size=3,cover=1e-6,fillNA=FALSE,saveMargin=TRUE
                       ,verbose=0L) {
   focal_extrem(x=x,kind="max",size=size,cover=cover,fillNA=fillNA
               ,saveMargin=saveMargin,verbose=verbose)
}
'focal_extrem' <- function(x,kind=c("min","max"),size=3,cover=1e-6
                     ,fillNA=FALSE,saveMargin=TRUE,verbose=0L)
{
   kind <- match.arg(kind)
   fun <- 'focalExtrem' # as.character(match.call()[1])
   if (!is.ursa(x))
      return(NULL)
   sparse <- attr(x$value,"sparse")
   if ((!is.null(sparse))&&(any(na.omit(sparse)!=0)))
      stop("TODO: expand compression")
   if (!is.na(x$con$posZ[1]))
      nb <- length(x$con$posZ)
   else
      nb <- x$dim[2]
   if (!is.na(x$con$posR[1]))
      nr <- length(x$con$posR)
   else
      nr <- x$grid$rows
   if (!is.na(x$con$posC[1]))
      nc <- length(x$con$posC)
   else
      nc <- x$grid$columns
   dimy <- c(nc,nr,nb)
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
   x$value <- .Cursa(C_focalExtrem,x=x$value
                ,kind=switch(kind,min=-1L,max=1L)
                ,nodata=as.numeric(nodata)
                ,dim=as.integer(dimy)
                ,size=as.integer(size)
                ,cover=as.numeric(cover)
                ,fillNA=as.integer(fillNA)
                ,saveMargin=as.integer(saveMargin)
                ,verbose=as.integer(verbose)
                ,res=numeric(prod(dimy)),NAOK=FALSE)$res
  # str(x$value,digits=7)
  # x$value <- x$value$res
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
   dim(x$value) <- c(dimy[1]*dimy[2],dimy[3])#with(x$grid,c(columns*rows,nb))
   class(x$value) <- "ursaNumeric"
   x
}
