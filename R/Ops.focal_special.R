'focal_special' <- function(x,type=c("custom","gaussian","laplacian","osisaf"
                                    ,"hires","correl","LoG","sobel","sobelG")
                           ,fmask=NULL,size=3,alpha=0.5,sigma=(size-1)/4,cover=1-1e-6
                           ,fillNA=FALSE,saveMargin=FALSE,verbose=0L)
{
   type <- match.arg(type)
   if (!is.ursa(x))
      return(NULL)
   sparse <- attr(x$value,"sparse")
   if ((!is.null(sparse))&&(any(na.omit(sparse)!=0)))
      stop("TODO: expand compression")
   if (!any(!is.na(x$value)))
      return(x)
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
   nodata <- x$con$nodata
   if (is.na(nodata))
      nodata <- max(x$value,na.rm=TRUE)+1
   x$value[is.na(x$value)] <- nodata
   if (type!="custom")
      fmask <- NULL
   if (!is.null(fmask)) {
      size <- unique(dim(fmask))
      if (length(size)!=1)
         stop("Only squared filter mask is allowed")
   }
   if (type %in% c("custom")) {
      if (is.null(fmask)) {
         fmask=1
         size=1L
      }
      x$value <- .Cursa(C_focalCommon
                   ,x=as.numeric(x$value)
                   ,dim=as.integer(dimy)
                   ,nodata=as.numeric(nodata)
                   ,H=as.numeric(fmask)
                   ,size=as.integer(size)
                   ,cover=as.numeric(cover)
                   ,fillNA=as.integer(fillNA)
                   ,saveMargin=as.integer(saveMargin)
                   ,verbose=as.integer(verbose)
                   ,res=numeric(prod(dimy)),NAOK=FALSE)$res
   }
   else {
     # fun <- gsub("^(\\w)(\\w+)","focal\\U\\1\\E\\2",type,perl=TRUE) #paste0(type,"4")
      fun <- switch(type,gaussian=C_focalGaussian
                        ,laplacian=C_focalLaplacian
                        ,osisaf=C_focalOsisaf
                        ,hires=C_focalHires
                        ,correl=C_focalCorrel
                        ,LoG=C_focalLoG
                        ,sobel=C_focalSobel
                        ,sobelG=C_focalSobelG
                        )
      x$value <- .Cursa(fun
                   ,x=as.numeric(x$value)
                   ,dim=as.integer(dimy)
                   ,nodata=as.numeric(nodata)
                   ,size=as.numeric(size)
                   ,sigma=as.numeric(sigma)
                   ,alpha=as.numeric(alpha)
                   ,cover=as.numeric(cover)
                   ,fillNA=as.integer(fillNA)
                   ,saveMargin=as.integer(saveMargin)
                   ,verbose=as.integer(verbose)
                   ,res=numeric(prod(dimy)),NAOK=FALSE)$res
   }
   if (abs(nodata)<1)
      x$value[abs(x$value-nodata)<1e-27] <- NA
   else
      x$value[abs(x$value/nodata-1)<1e-6] <- NA
   dim(x$value) <- c(dimy[1]*dimy[2],dimy[3])#with(x$grid,c(columns*rows,nb))
   class(x$value) <- "ursaNumeric"
   .gc()
   x
}
