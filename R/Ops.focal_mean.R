'focal_mean' <- function(x,size=3.0,cover=1e-6,fillNA=FALSE,saveMargin=TRUE
                        ,noNA=TRUE,verbose=0L)
{
   fun <- "focalMean" # as.character(match.call())[1]
   if (!is.ursa(x))
      return(NULL)
  # x$data <- aperm(x$data,c(2,1,3))
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
   dimy <- c(x$grid$columns,nr,nb)
  # dim(x$value) <- dimy
   x$value <- as.numeric(x$value)
   if (noNA)
   {
      if (verbose>2)
         .elapsedTime(paste0("start:nodata:",fun))
      nodata <- x$con$nodata
      if (is.na(nodata))
         nodata <- max(x$value,na.rm=TRUE)+1
      x$value[is.na(x$value)] <- nodata
      if (verbose>2)
         .elapsedTime(paste0("finish:nodata:",fun))
   }
   if (verbose>1)
      print(summary(x$value))
   if (verbose)
      .elapsedTime(paste0("start:",fun))
   if (noNA) {# C-code is more quick 
      x$value <- .Cursa("focalMean",x=x$value,nodata=as.numeric(nodata)
                   ,dim=as.integer(dimy),size=as.numeric(size)
                   ,cover=as.numeric(cover),fillNA=as.integer(fillNA)
                   ,saveMargin=as.integer(saveMargin)
                   ,verbose=as.integer(verbose)
                   ,res=numeric(prod(dimy)),NAOK=FALSE)$res
   }
   else {
      x$value <- .Cursa("focalMeanWithNA",x=x$value
                   ,dim=as.integer(dimy),size=as.numeric(size)
                   ,cover=as.numeric(cover),fillNA=as.integer(fillNA)
                   ,verbose=as.integer(verbose)
                   ,res=numeric(prod(dimy)),NAOK=TRUE)$res
   }
   if (verbose)
      .elapsedTime(paste0("finish:",fun))
   if (verbose>1)
      print(summary(x$value))
   if (noNA)
   {
      if (verbose>2)
         .elapsedTime(paste0("start:nodata:",fun))
      if (abs(nodata)<1)
         x$value[abs(x$value-nodata)<1e-27] <- NA
      else
         x$value[abs(x$value/nodata-1)<1e-6] <- NA
      if (verbose>2)
         .elapsedTime(paste0("finish:nodata:",fun))
   }
   dim(x$value) <- c(dimy[1]*dimy[2],dimy[3])#with(x$grid,c(columns*rows,nb))
   class(x$value) <- "ursaNumeric"
   x
}
'.focal_mean.big' <- function(x,fname,memory=100,verbose=FALSE,...)
{
   fun <- "focal_mean.big"
   if (!is.ursa(x))
      return(NULL)
   nr <- x$grid$rows
   res <- if (missing(fname)) create_envi(x,...) else create_envi(x,fname,...)
   rel <- as.list(match.call())[-1]
   rel$fname <- NULL
   size <- rel[[.grep("size",names(rel))]]
   s1 <- ceiling(size)
   if (!(s1%%2))
      s1 <- s1+1
   s2 <- as.integer(floor(s1/2))
   chunk <- chunk_line(x,memory)
   isPb <- verbose & length(chunk)>1
   if (isPb)
      pb <- ursaProgressBar(min=0,max=length(chunk),tail=TRUE)
   for (i in seq_along(chunk)) {
     # if (verbose)
         print(range(i))
      r1 <- chunk[[i]]
      r2 <- c(min(r1)-rev(seq(s2)),r1,max(r1)+seq(s2))
      r2 <- r2[r2>=1 & r2<=nr]
      r3 <- na.omit(match(r1,r2))
      rel[["x"]] <- quote(x[,r2])
      res[,r1] <- do.call("focal_mean",rel)[,r3]
      if (isPb)
         setUrsaProgressBar(pb,i)
   }
   if (isPb)
      close(pb)
   res
}
