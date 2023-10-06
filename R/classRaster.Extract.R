'[.ursaRaster' <- function(x,i,j,...,drop=FALSE)
{
   verbose <- isTRUE(getOption("ursaDevel"))
   if (verbose) {
      cat("----------\n")
      on.exit(cat("==========\n"))
      print(match.call())
      ##~ a <- as.list(match.call())$i
      ##~ print(class(a))
      ##~ print(is.symbol(a))
      ##~ print(is.language(a))
      ##~ print(is.call(a))
      ##~ q()
   }
   dimx <- dim(x$value)
   clValue <- class(x$value)
   res <- x
   missingJ <- missing(j) ## read all lines and (selected) bands
   missingI <- missing(i) ## read all bands and (selected) lines
   if ((missingJ)&&(missingI))
   {
      if (length(dimx)==2L)
         return(x) ## return(x$value) identiical(x$value,as.matrix(x))==TRUE
   }
   if (!is.null(dimx))
   {
      if ((!missingI)&&(is.ursa(i)))
      {
        # return(res*i) ## removed 20160805
         if (nband(i)==1)
            res$value[which(is.na(i$value)),] <- NA
         else
            res$value[which(is.na(i$value))] <- NA
         return(res) ## added 20160805
      }
      if (missingJ)
         j <- seq(dimx[1])
      if (missingI)
         i <- seq(dimx[2])
      if ((is.numeric(i))&&(any(abs(i)>dimx[2])))
         i <- as.character(i)
      if (is.character(i))
      {
         args <- list(...)
         regexp <- if ("regexp" %in% names(args)) args[["regexp"]] else FALSE
         i1 <- .getBand(res,i,regexp=regexp)
         if (is.null(i1)) {
            ind <- .grep(i,ursa(res,"category"))
            if (length(ind) == -10000+1) {
               return(res[res==ind-1L])
            }
            else {
               if (length(ind)) {
                  res$value[!(x$value %in% (ind-1L))] <- NA
                  return(res)
               }
               else
                  return(res[integer()])
            }
         }
         else
            i <- i1
         j <- seq(dimx[1])
         missingJ <- TRUE
         missingI <- FALSE
      }
     # if (is.con(res$con))
     #    res$con$indexZ <- res$con$indexZ[i]
     # print(c(j=range(j),i=i))
     # res$name <- make.unique(res$name[i],"_")
      if (all(i<0))
      {
         if (!is.na(res$con$posZ[1]))
            i <- seq(res$con$posZ)[i]
         else
            i <- seq(res$dim[2])[i]
      }
     # res$dim[2] <- length(i)
      if (!missingJ)
      {
         if (verbose)
            .elapsedTime("expandIndex:start")
         if (is.na(x$con$indexC)[1])
            s <- x$grid$columns
         else
            s <- length(x$con$indexC)
         j2 <- j
         j <- as.integer(c(t(col(array(NA,dim=c(length(j2),s)))+(j2-1)*s)))
         if (max(j)>x$dim[1])
         {
            opW <- options(warn=0)
            warning(paste("Expected",length(j),"bytes, but found",max(j),"bytes"))
            options(opW)
            j <- j[j<=x$dim[1]]
         }
         if (verbose)
            .elapsedTime("expandIndex:finish")
      }
      if (.is.con(res$con))
      {
         if (!missingJ) {
            if (is.na(res$con$posR[1]))
               res$con$posR <- j2
            else
               res$con$posR <- res$con$posR[j2]
         }
         if (!missingI)
         {
            if (is.na(res$con$posZ[1]))
            {
               if (is.logical(i))
                  res$con$posZ <- which(i)
               else
                  res$con$posZ <- i
            }
            else
               res$con$posZ <- res$con$posZ[i]
         }
      }
      else
         res$name <- res$name[i]
      cl <- class(res$value)
      sp <- attr(res$value,"sparse")
      res$value <- res$value[j,i,drop=drop]
      class(res$value) <- cl
      attr(res$value,"sparse") <- sp
      res$dim <- dim(res$value)
      return(res)
   }
   res$con$compress <- 0L
   grid <- res$grid
   con <- res$con
   indF <- length(con$fname)
   opW <- options(warn=-1)
   intOverflow <- is.na(with(con,samples*lines*bands*sizeof))
   options(opW)
   if (verbose & intOverflow) {
      print(c(intOverflow=with(con,as.double(samples)*as.double(lines)*
                                          as.double(bands)*as.double(sizeof))))
     # intOverflow <- FALSE
   }
   intOverflow <- FALSE
   internalReading <- intOverflow | con$connection %in% c("gzfile")
   externalReading <- !internalReading
   if ((1)&&(!missingJ)&&(is.character(j)))
      stop("TODO: is.character(j)")
   if ((1)&&(!missingI)&&(is.character(i)))
   {
      args <- list(...)
      regexp <- if ("regexp" %in% names(args)) args[["regexp"]] else FALSE
      i <- .getBand(res,i,regexp=regexp)
      if (is.null(i))
      {
         op <- options(warn=1)
         warning(paste("Speciefied name",paste0("'",i,"'"),"is not in bandname"))
         options(op)
         return(NULL)
      }
      missingJ <- TRUE
      missingI <- FALSE
   }
   if ((!missingI)&&(is.logical(i))) {
      i <- which(i)
   }
   if ((!missingI)&&(all(i<0)))
   {
      if (!is.na(res$con$posZ[1]))
         i <- seq(res$con$posZ)[i]
      else
         i <- seq(res$dim[2])[i]
   }
   if ((missingJ)&&(missingI)) ## read full
   {
      if (con$driver %in% c("ENVI","EGDAL")) {
         n <- prod(with(con,samples*lines*bands))
         xdim <- with(con,c(lines,samples,bands))
         if ((con$seek)&&(con$interleave %in% c("bsq","bil"))&&
             (externalReading)&&(TRUE)) {
            seek(con,where=0L,origin="start",rw="r")
            if (con$interleave=="bsq") {
               if (con$mode=="integer") {
                  res$value <- with(con,.Cursa(C_readBsqBandInteger
                                   ,fname=con$fname[indF],dim=xdim,index=seq(bands),n=bands
                                   ,datatype=datatype,swap=swap
                                   ,res=integer(bands*samples*lines)))$res
               }
               else
                  res$value <- with(con,.Cursa(C_readBsqBandDouble
                                   ,fname=con$fname[indF],dim=xdim,index=seq(bands),n=bands
                                   ,datatype=datatype,swap=swap
                                   ,res=double(bands*samples*lines)))$res
            }
            else if (con$interleave=="bil") {
               if (con$mode=="integer") {
                  res$value <- with(con,.Cursa(C_readBilLineInteger2
                                   ,fname=con$fname[indF],dim=xdim,index=seq(lines),n=lines
                                   ,datatype=datatype,swap=swap
                                   ,res=integer(bands*samples*lines)))$res
               }
               else {
                  res$value <- with(con,.Cursa(C_readBilLineDouble2
                                   ,con$fname[indF],dim=xdim
                                   ,lines=seq(lines)
                                   ,nline=lines,datatype=datatype,swap=swap
                                   ,res=double(with(con,lines*samples*bands))))$res
               }
            }
            dim(res$value) <- with(con,c(samples,lines,bands))
         }
         else {
            res$value <- with(con,.readline(handle,datatype,n,endian))
            if (con$interleave=="bil") ##bil[col,band,row] -> R[col,row,band]
            {
               dim(res$value) <- with(con,c(samples,bands,lines))
               res$value <- aperm(res$value,c(1,3,2)) ##(3,1,2)
            }
            else if (con$interleave=="bip") ##bip[band,col,row] -> R[col,row,band]
            {
               dim(res$value) <- with(con,c(bands,samples,lines))
               res$value <- aperm(res$value,c(2,3,1)) ##c(3,2,1)
            }
            else if (con$interleave=="bsq") ##bsq[col,row,band] -> R[col,row,band]
            {
               dim(res$value) <- with(con,c(samples,lines,bands))
              # res$value <- aperm(res$value,c(1,2,3)) ##c(2,1,3)
            }
            else
               stop("read ENVI: Error in input header file ",con$interleave
                   ," incorrect interleave type")
         }
      }
      else if (con$driver=="RGDAL") { ## read full
         res$value <- .rgdal_getRasterData(con$handle)
         dim(res$value) <- with(con,c(samples,lines,bands))
      }
      else if (con$driver=="GDALRASTER") { ## read full
         if (con$datatype %in% c(4,5))
            res$value <- array(NA_real_,dim=res$dim)
         else
            res$value <- array(NA_integer_,dim=res$dim)
         if (verbose)
            cat("read")
         for (b2 in seq_len(con$bands)) {
            if (verbose)
               cat(".")
            res$value[,b2] <- con$handle$read(band=b2
                                            ,xoff=0,yoff=0
                                            ,xsize=con$samples,ysize=con$lines
                                            ,out_xsize=con$samples,out_ysize=con$lines
                                            )
         }
         if (verbose)
            cat(" done!\n")
         dim(res$value) <- with(con,c(samples,lines,bands))
      }
      else if (con$driver=="SF") { ## read full
         if (con$datatype %in% c(4,5)) {
            res$value <- attr(sf::gdal_read(con$handle$filename,read_data=TRUE),"data")
            attr(res$value,"units") <- NULL
         }
         else {
            res$value <- as.integer(attr(sf::gdal_read(con$handle$filename
                                                      ,read_data=TRUE),"data"))
         }
         dim(res$value) <- with(con,c(samples,lines,bands))
      }
      else if (con$driver=="NCDF") { ## read full
        # stop("NCDF -- read full")
         nc <- ncdf4::nc_open(con$fname[indF])
         varName <- con$handle
         flip <- attr(varName,"flip")
         permute <- attr(varName,"permute")
         indT <- attr(varName,"temporal")
         indS <- attr(varName,"spatial")
         indV <- c(indS,indT)
         indL <- seq_along(con$offset)[-indV]
         w <- attr(varName,"weight")
        # level <- attr(varName,"level")
         attributes(varName) <- NULL
        # print(data.frame(var=varName,flip=flip,permute=permute,time="???"))
         nc.start <- rep(1,length(con$offset))
         nc.count <- con$offset
         if (length(w)) {
            ind <- which(w>0)
            isW <- all(diff(ind))==1
            if (isW) {
               nc.start[indL] <- ind[1]
               nc.count[indL] <- length(ind)
               w <- w[ind]
            }
         }
         else
            isW <- FALSE
         res$value <- ncdf4::ncvar_get(nc,varName,start=nc.start,count=nc.count
                                ,collapse_degen=!FALSE)
         if (length(dim(res$value))==1)
            return(res$value)
         dim(res$value) <- nc.count
         if (length(indL)==1) {
            if (nc.count[indL]>1) { #(con$offset[indL]>1)
               if (FALSE)
                  res$value <- apply(res$value,indV,function(x) sum(x*w))
               else {
                  val <- aperm(res$value,c(indV,indL))
                  dima <- dim(val)
                  dim(val) <- c(prod(dima[1:3]),dima[4])
                  val <- .average(val,weight=w)
                  dim(val) <- nc.count[indV]
                  res$value <- val
                  rm(val)
               }
            }
            else
               dim(res$value) <- con$offset[indV]
         }
         if (permute) {
            dima <- length(dim(res$value))
            if (dima==2)
               res$value <- aperm(res$value,c(2,1))
            else if (dima==3)
               res$value <- aperm(res$value,c(2,1,3))
            else if (dima==4)
               res$value <- aperm(res$value,c(2,1,3,4))
            else
               stop(paste("NCDF: transpose is not implemented for dimension:",dima))
         }
         if (flip) {
            if (length(dim(res$value))>2)
               res$value <- res$value[,rev(seq(dim(res$value)[2])),,drop=FALSE]
            else
               res$value <- res$value[,rev(seq(dim(res$value)[2])),drop=FALSE]
         }
         dim(res$value) <- with(con,c(samples,lines,bands))
         ncdf4::nc_close(nc)
      }
      else
         stop(paste("unknown driver:",con$driver))
      if ((con$samples!=res$grid$columns)||(con$lines!=res$grid$rows))
         res$value <- res$value[con$indexC,con$indexR,,drop=FALSE]
      dimy <- dim(res$value)
      dim(res$value) <- c(dimy[1]*dimy[2],dimy[3]) ## t()
      res$dim <- dim(res$value)
      if (!is.na(con$nodata)) {
         if (abs(con$nodata)<1)
            res$value[abs(res$value-con$nodata)<1e-27] <- NA
         else
            res$value[abs(res$value/con$nodata-1)<1e-6] <- NA
      }
      class(res$value) <- clValue
      return(res)
   }
   else if ((missingJ)&&(!missingI)) ## read band
   {
      if (verbose)
         cat("read bands ",min(i),":",max(i),"\n")
      if (is.list(i))
         i <- unlist(i)
      nb <- length(i)
      i <- as.integer(i)
      nline <- if (!is.na(con$indexR[1L])) length(con$indexR) else con$lines
      if (is.na(con$indexR)[1])
         minJ <- 0L
      else
         minJ <- min(con$indexR)-1L
      minI <- min(i)
      toWarp <- with(con,(!is.na(indexR)[1])&&(length(indexR)!=lines)||
                         (!is.na(indexC)[1])&&(length(indexC)!=samples))
      if (con$driver %in% c("ENVI","EGDAL")) {
         if (con$interleave=="bil")
         {
            if (externalReading)
            {
               if (con$seek)
                  seek(con,where=0L,origin="start",rw="r")
               xdim <- with(con,c(lines,samples,bands))
               if (con$mode=="integer")
                  val <- .Cursa(C_readBilBandInteger,con$fname[indF],dim=xdim,index=i
                           ,n=nb,datatype=con$datatype,swap=con$swap
                           ,res=integer(with(con,nb*samples*lines)))$res
               else {
                  val <- .Cursa(C_readBilBandDouble,con$fname[indF],dim=xdim,index=i
                           ,n=nb,datatype=con$datatype,swap=con$swap
                           ,res=double(with(con,nb*samples*lines)))$res
               }
            }
            else
            {
               n <- nb*con$samples
               val <- array(NA,dim=c(n,nline))
               conseq <- all(diff(sort(i))==1)
               if (conseq) {
                  for (r in seq(nline)) {
                    # print(with(con,c(minJ=minJ,rsi=bands*(r-1)+minI-1
                    #                 ,samples=samples,sizeof=sizeof)))
                     pos <- with(con,(minJ+bands*(r-1)+minI-1)*samples*sizeof)
                    # print(c(r=r,pos=pos,n=n))
                     if (con$seek)
                        seek(con,where=pos,origin="start",rw="r")
                     val[,r] <- with(con,.readline(handle,datatype,n,endian))
                  }
               }
               else {
                  for (r in seq(nline))
                  {
                     for (s in seq_along(i)) {
                        pos <- with(con,(minJ+bands*(r-1)+i[s]-1)*samples*sizeof)
                        if (con$seek)
                           seek(con,where=pos,origin="start",rw="r")
                        s2 <- seq((s-1)*con$samples+1,s*con$samples)
                        val[s2,r] <- with(con,.readline(handle,datatype,con$samples,endian))
                     }
                  }
               }
            }
            if (toWarp) ## added 2013-06-14
            {
               dim(val) <- with(con,c(samples,nb,lines))
               val <- val[,,con$indexR,drop=FALSE]
            }
            dim(val) <- with(con,c(samples,nb,nline))
            val <- aperm(val,c(1,3,2))
         }
         else if (con$interleave=="bip")
         {
            n <- with(con,samples*bands)
            val <- array(NA,dim=c(nb*con$samples,nline))
            ind <- which(with(con,(seq(bands*samples)-1)%%bands+1) %in% i)
            for (r in seq(nline))
            {
               pos <- with(con,(minJ+(r-1)*samples*bands)*sizeof)
               if (con$seek)
                  seek(con,where=pos,origin="start",rw="r")
               val[,r] <- with(con,.readline(handle,datatype,n,endian))[ind]
            }
            dim(val) <- with(con,c(nb,samples,nline))
            val <- aperm(val,c(2,3,1))
         }
         else if (con$interleave=="bsq")
         {
            isSeq <- identical(i,min(i):max(i))
           # val <- array(NA,dim=c(con$samples*nline,nb))
            if ((externalReading)&&(TRUE))
            {
               if (con$seek)
                  seek(con,where=0L,origin="start",rw="r")
               xdim <- with(con,c(lines,samples,bands))
              # str(list(i=i,dim=xdim,nb=nb,fname=con$fname))
               if (con$mode=="integer")
               {
                  val <- .Cursa(C_readBsqBandInteger,fname=con$fname[indF],dim=xdim,index=i
                           ,n=nb,datatype=con$datatype,swap=con$swap
                           ,res=integer(with(con,nb*samples*lines)))$res
               }
               else
               {
                  val <- .Cursa(C_readBsqBandDouble,fname=con$fname[indF],dim=xdim,index=i
                           ,n=nb,datatype=con$datatype,swap=con$swap
                           ,res=double(with(con,nb*samples*lines)))$res
               }
            }
            else
            {
               val <- array(NA,dim=c(con$samples*nline,nb))
               if ((1)&&(isSeq))
               {
                  n <- with(con,nline*samples*nb)
                  pos <- with(con,(minJ+(minI-1)*lines*samples)*sizeof)
                  if (con$seek)
                     seek(con,where=pos,origin="start",rw="r")
                  val[] <- with(con,.readline(handle,datatype,n,endian))
               }
               else
               {
                  n <- with(con,nline*samples)
                  for (r in seq(along=i))
                  {
                     pos <- with(con,(minJ+(i[r]-1)*lines*samples)*sizeof)
                     if (con$seek)
                        seek(con,where=pos,origin="start",rw="r")
                     val[,r] <- with(con,.readline(handle,datatype,n,endian))
                  }
               }
            }
            if (toWarp)
            {
               dim(val) <- with(con,c(samples,lines,nb))
               val <- val[,con$indexR,,drop=FALSE] ## before 2012-12-23
            }
            dim(val) <- with(con,c(samples,nline,nb))
           # val <- aperm(val,c(1,2,3))
         }
         else
            stop("Error in input header file ",con$interleave
                ," incorrect interleave type")
      }
      else if (con$driver=="RGDAL") { ## read band
         val <- .rgdal_getRasterData(con$handle,band=i)
         dim(val) <- with(con,c(samples,nline,nb))
      }
      else if (con$driver=="GDALRASTER") { ## read band
         if (con$datatype %in% c(4,5))
            val <- array(NA_real_,dim=c(res$dim[1],length(i)))
         else
            val <- array(NA_integer_,dim=c(res$dim[1],length(i)))
         if (verbose)
            cat("read chunk band")
         for (b2 in seq_along(i)) {
            if (verbose)
               cat(".")
            val[,b2] <- con$handle$read(band=i[b2]
                                       ,xoff=0,yoff=0
                                       ,xsize=con$samples,ysize=con$lines
                                       ,out_xsize=con$samples,out_ysize=con$lines
                                       )
         }
         if (verbose)
            cat(" done!\n")
         dim(val) <- with(con,c(samples,nline,nb))
      }
      else if (con$driver=="SF") { ## read band
         rasterio <- list(bands=i)
         if (con$datatype %in% c(4,5)) {
            val <- attr(sf::gdal_read(con$handle$filename,read_data=TRUE
                                           ,RasterIO_parameters=rasterio),"data")
            attr(val,"units") <- NULL
         }
         else {
            val <- as.integer(attr(sf::gdal_read(con$handle$filename,read_data=TRUE
                                           ,RasterIO_parameters=rasterio),"data"))
         }
         dim(val) <- with(con,c(samples,nline,nb))
      }
      else if (con$driver=="NCDF") { ## read band
        # stop("NCDF -- read band")
        # str(con)
        # print(c(i=i))
         di <- diff(i)
         isC <- ((!length(di))||(abs(unique(di))==1))
         nc <- ncdf4::nc_open(con$fname)
         varName <- con$handle
         flip <- attr(varName,"flip")
         permute <- attr(varName,"permute")
         indT <- attr(varName,"temporal")
         indS <- attr(varName,"spatial")
         indV <- c(indS,indT)
         indL <- seq_along(con$offset)[-indV]
        # print(c(indS=indS,indT=indT,indL=indL))
         w <- attr(varName,"weight")
         attributes(varName) <- NULL
         nc.start <- rep(1,length(con$offset))
         nc.count <- con$offset
         if (length(w)) {
            ind <- which(w>0)
            isW <- all(diff(ind))==1
            if (isW) {
               nc.start[indL] <- ind[1]
               nc.count[indL] <- length(ind)
               w <- w[ind]
            }
         }
         else
            isW <- FALSE
        # level <- attr(varName,"level")
        # print(data.frame(var=varName,flip=flip,permute=permute,time=time))
         if (isC) { #(isC) {
            nc.start[indT] <- min(i)
            nc.count[indT] <- length(i)
            val <- ncdf4::ncvar_get(nc,varName,start=nc.start,count=nc.count
                                   ,collapse_degen=FALSE)
           # if (length(indL)==1)
           #    val <- apply(val,c(1,2,indT),mean)
         }
         else {
            nc.count[indT] <- 1
            dima <- con$offset
            dima[indT] <- length(i)
            val <- array(NA_real_,dim=dima)
            for (i2 in seq_along(i)) {
               nc.start[indT] <- i[i2]
               if (length(indL)==1) {
                  if (indT==4)
                     val[,,,i2] <- ncdf4::ncvar_get(nc,varName,start=nc.start
                                           ,count=nc.count,collapse_degen=FALSE)
                  else
                     stop("NCDF column 'time' index?")
               }
               else { 
                  if (indT==3)
                     val[,,i2] <- ncdf4::ncvar_get(nc,varName,start=nc.start
                                           ,count=nc.count,collapse_degen=FALSE)
                  else if (indT==1)
                     val[i2,,] <- ncdf4::ncvar_get(nc,varName,start=nc.start
                                           ,count=nc.count,collapse_degen=FALSE)
               }
            }
         }
         if (length(indL)==1) {
            if (nc.count[indL]>1) { ## con$offset[indL]>1
               if (FALSE)
                  val <- apply(val,indV,function(x) sum(x*w))
               else {
                  val <- aperm(val,c(indV,indL))
                  dima <- dim(val)
                  dim(val) <- c(prod(dima[1:3]),dima[4])
                  val <- .average(val,weight=w)
                  dim(val) <- nc.count[indV]
               }
            }
            else {
               dim(val) <- nc.count[indV]
            }
         }
         if (permute) {
            dima <- length(dim(val))
            if (dima==2)
               val <- aperm(val,c(2,1))
            else if (dima==3)
               val <- aperm(val,c(2,1,3))
            else if (dima==4)
               val <- aperm(val,c(2,1,3,4))
            else
               stop(paste("NCDF: transpose is not implemented for dimension:",dima))
         }
         if (flip) {
            if (length(dim(val))>2)
               val <- val[,rev(seq(dim(val)[2])),,drop=FALSE]
            else
               val <- val[,rev(seq(dim(val)[2])),drop=FALSE]
         }
         dim(val) <- with(con,c(samples,nline,nb))
         ncdf4::nc_close(nc)
      }
      if (!is.na(con$indexC[1]))
         res$value <- val[con$indexC,,,drop=FALSE]
      else
         res$value <- val
      dimy <- dim(res$value)
      dim(res$value) <- c(dimy[1]*dimy[2],dimy[3])
      if (!is.na(con$nodata)) {
         if (abs(con$nodata)<1)
            res$value[abs(res$value-con$nodata)<1e-27] <- NA
         else {
           # print(abs(res$value/con$nodata-1))
            res$value[abs(res$value/con$nodata-1)<1e-6] <- NA
         }
      }
      res$con$posZ <- i
      class(res$value) <- clValue
      return(res)
   }
   else if ((!missingJ)&&(missingI)) ## read line
   {
      if (is.list(j))
         j <- unlist(j)
      j <- as.integer(seq(min(j),max(j)))
      nline <- length(j)
      minJ <- (min(j)-1L)+min(con$indexR-1L)
      if (con$driver %in% c("ENVI","EGDAL")) {
         if (con$interleave=="bil") ##bil[col,band,row] -> R[col,row,band]
         {
            if ((externalReading)&&(TRUE))
            {
               xdim <- with(con,c(lines,samples,bands))
               if (con$mode=="integer")
                  val <- .Cursa(C_readBilLineInteger,con$fname[indF],dim=xdim
                           ,lines=j+as.integer(min(con$indexR-1L))
                           ,nline=nline,datatype=con$datatype,swap=con$swap
                           ,res=integer(with(con,nline*samples*bands)))$res
               else
                  val <- .Cursa(C_readBilLineDouble,con$fname[indF],dim=xdim
                           ,lines=j+as.integer(min(con$indexR-1L))
                           ,nline=nline,datatype=con$datatype,swap=con$swap
                           ,res=double(with(con,nline*samples*bands)))$res
              # print(summary(val))
            }
            else
            {
               n <- with(con,nline*bands*samples)
              # print(sprintf("seek=%d",with(con,minJ*bands*sizeof*samples)))
               if (con$seek)
                  seek(con,where=with(con,minJ*bands*sizeof*samples)
                      ,origin="start",rw="r")
               val <- with(con,.readline(handle,datatype,n,endian))
              # print(summary(val))
            }
            dim(val) <- with(con,c(samples,bands,nline))
            val <- aperm(val,c(1,3,2))
         }
         else if (con$interleave=="bip") ##bip[band,col,row] -> R[col,row,band]
         {
            n <- with(con,nline*bands*samples)
            seek(con,where=with(con,minJ*bands*sizeof*samples),origin="start",rw="r")
            val <- with(con,.readline(handle,datatype,n,endian))
            dim(val) <- with(con,c(bands,samples,nline))
            val <- aperm(val,c(2,3,1))
         }
         else if (con$interleave=="bsq") ##bsq[col,row,band] -> R[col,row,band]
         {
            if ((externalReading)&&(TRUE))
            {
               xdim <- with(con,c(lines,samples,bands))
               if (con$mode=="integer")
                  val <- .Cursa(C_readBsqLineInteger,con$fname[indF],dim=xdim,lines=j
                           ,nline=nline,datatype=con$datatype,swap=con$swap
                           ,res=integer(with(con,nline*samples*bands)))$res
               else
                  val <- .Cursa(C_readBsqLineDouble,con$fname[indF],dim=xdim,lines=j
                           ,nline=nline,datatype=con$datatype,swap=con$swap
                           ,res=double(with(con,nline*samples*bands)))$res
            }
            else
            {
               n <- nline*con$samples
               val <- array(NA,dim=c(n,con$bands))
               for (nb in seq(con$bands))
               {
                  pos <- minJ+(nb-1)*con$lines
                  if (con$seek)
                     seek(con,where=with(con,pos*sizeof*samples),origin="start",rw="r")
                  val[,nb] <- with(con,.readline(handle,datatype,n,endian))
               }
            }
            dim(val) <- with(con,c(samples,nline,bands))
           # val <- aperm(val,c(1,2,3))
         }
      }
      else if (con$driver=="RGDAL") { ## read line
         nline <- length(j)
         minJ <- (min(j)-1L)+min(con$indexR-1L)
         val <- .rgdal_getRasterData(con$handle,offset=c(minJ,0)
                               ,region.dim=c(nline,con$samples))
         dim(val) <- with(con,c(samples,nline,bands))
      }
      else if (con$driver=="GDALRASTER") { ## read line
         nline <- length(j)
         minJ <- (min(j)-1L)+min(con$indexR-1L)
         if (con$datatype %in% c(4,5))
            val <- array(NA_real_,dim=c(con$samples*nline,res$dim[2]))
         else
            val <- array(NA_integer_,dim=c(con$samples*nline,res$dim[2]))
         if (verbose)
            cat("read chunk line")
         for (b2 in seq_len(con$bands)) {
            if (verbose)
               cat(".")
            val[,b2] <- con$handle$read(band=b2
                                       ,xoff=0,yoff=minJ
                                       ,xsize=con$samples,ysize=nline
                                       ,out_xsize=con$samples,out_ysize=nline
                                       )
         }
         if (verbose)
            cat(" done!\n")
         dim(val) <- with(con,c(samples,nline,bands))
      }
      else if (con$driver=="SF") { ## read line
         nline <- length(j)
         minJ <- (min(j)-1L)+min(con$indexR-1L)
         rasterio <- list(nXOff=1,nYOff=minJ+1L,nXSize=con$samples,nYSize=nline
                        # ,nBufXSize=2,nBufYSize=2
                         )
         if (con$datatype %in% c(4,5)) {
            val <- attr(sf::gdal_read(con$handle$filename,read_data=TRUE
                                           ,RasterIO_parameters=rasterio),"data")
            attr(val,"units") <- NULL
         }
         else {
            val <- as.integer(attr(sf::gdal_read(con$handle$filename,read_data=TRUE
                                           ,RasterIO_parameters=rasterio),"data"))
         }
         dim(val) <- with(con,c(samples,nline,bands))
      }
      else if (con$driver=="NCDF") { ## read line
         nc <- ncdf4::nc_open(con$fname[indF])
         varName <- con$handle
        # str(con$offset)
         flip <- attr(varName,"flip")
         permute <- attr(varName,"permute")
         indT <- attr(varName,"temporal")
         indS <- attr(varName,"spatial")
         indV <- c(indS,indT)
         indL <- seq_along(con$offset)[-indV]
         indC <- indS[ifelse(permute,2,1)]
         indR <- indS[ifelse(permute,1,2)]
        # indR <- ifelse(permute,1L,2L)
        # indC <- 3L-indR
         w <- attr(varName,"weight")
         if (flip)
            j <- rev(rev(seq(con$offset[[indR]]))[j])
         nline <- length(j)
         dj <- diff(j)
         isC <- ((!length(dj))||(abs(unique(dj))==1))
        # level <- attr(varName,"level")
         attributes(varName) <- NULL
         nc.start <- rep(1,length(con$offset))
         nc.count <- con$offset
         ##~ print(nc.start)
         ##~ print(nc.count)
         if (isC) { #(isC) {
            nc.start[indR] <- min(j)
            nc.count[indR] <- length(j)
           # print(c(start=nc.start))
           # print(c(count=nc.count))
            val <- ncdf4::ncvar_get(nc,varName,start=nc.start,count=nc.count
                                   ,collapse_degen=FALSE)
         }
         else { ## slow!
            nc.count[indR] <- 1
            dima <- con$offset
            dima[indR] <- length(j)
            val <- array(NA_real_,dim=dima)
            for (j2 in seq_along(j)) {
               nc.start[indR] <- j[j2]
              # print(nc.start)
              # print(nc.count)
               if (length(indL)==1) {
                  if (indR==2)
                     val[,j2,,] <- ncdf4::ncvar_get(nc,varName,start=nc.start
                                           ,count=nc.count,collapse_degen=FALSE)
                  else
                     stop("NCDF column 'line' index? (multi-level)")
               }
               else {
                 # print(indV)
                  if (indR==2)
                     val[,j2,] <- ncdf4::ncvar_get(nc,varName,start=nc.start
                                              ,count=nc.count,collapse_degen=FALSE)
                  else
                     stop("NCDF column 'line' index? (single-level)")
               }
            }
         }
         if (length(indL)==1) {
            if (con$offset[indL]>1) {
              # val <- apply(val,indV,mean)
               if (FALSE)
                  val <- apply(val,indV,function(x) sum(x*w))
               else {
                  val <- aperm(val,c(indV,indL))
                  dima <- dim(val)
                  dim(val) <- c(prod(dima[1:3]),dima[4])
                  val <- .average(val,weight=w)
                  dim(val) <- nc.count[indV]
               }
            }
            else {
               dim(val) <- nc.count[indV] # con$offset[indV]
            }
         }
         if (permute) {
            dima <- length(dim(val))
            if (dima==2)
               val <- aperm(val,c(2,1))
            else if (dima==3)
               val <- aperm(val,c(2,1,3))
            else if (dima==4)
               val <- aperm(val,c(2,1,3,4))
            else
               stop(paste("NCDF: transpose is not implemented for dimension:",dima))
         }
         if (flip) {
            if (length(dim(val))>2)
               val <- val[,rev(seq(dim(val)[2])),,drop=FALSE]
            else
               val <- val[,rev(seq(dim(val)[2])),drop=FALSE]
         }
         dim(val) <- with(con,c(samples,nline,bands))
         ncdf4::nc_close(nc)
      }
      else
         stop("read ENVI: Error in input header file ",con$interleave
             ," incorrect interleave type")
      res$value <- val[con$indexC,,,drop=FALSE]
      dimy <- dim(res$value)
      dim(res$value) <- c(dimy[1]*dimy[2],dimy[3]) ## t()
      if (!is.na(con$nodata)) {
         if (abs(con$nodata)<1)
            res$value[abs(res$value-con$nodata)<1e-27] <- NA
         else
            res$value[abs(res$value/con$nodata-1)<1e-6] <- NA
      }
      res$con$posR <- j
      res$dim <- dim(res$value)
      class(res$value) <- clValue
      return(res)
   }
   else
      stop("TODO")
   stop("UNREACHABLE CODE")
}
'.readline' <- function(Fin,datatype,size,endian)
{
   if (FALSE) ## benchmark is needed
      datatype <- datatype+1000L
   if (datatype==1001) ## data.type==1 -> 1-byte short 
      val <- readBin(Fin,integer(),n=size,size=1,signed=FALSE)
   else if (datatype==1) ## data.type==1 -> 1-byte short, unsigned
      val <- readBin(readBin(Fin,raw(),n=size*1)
                    ,integer(),n=size,size=1,signed=FALSE)
   else if (datatype==1002) ## data.type==2 -> 2-byte short, signed
      val <- readBin(Fin,integer(),n=size,size=2,endian=endian,signed=TRUE)
   else if (datatype==2) ## data.type==2 -> 2-byte short, unsigned
      val <- readBin(readBin(Fin,raw(),n=size*2)
                    ,integer(),n=size,size=2,endian=endian,signed=TRUE)
   else if (datatype==1003) ## data.type==3 -> 4-byte int
      val <- readBin(Fin,integer(),n=size,endian=endian,signed=TRUE)
   else if (datatype==3) ## data.type==3 -> 4-byte int
      val <- readBin(readBin(Fin,raw(),n=size*4)
                    ,integer(),n=size,endian=endian,signed=TRUE)
   else if (datatype==1004) ## data.type==4 -> 4-byte float 
      val <- readBin(Fin,double(),n=size,size=4,endian=endian)
   else if (datatype==4) ## data.type==4 -> 4-byte float 
      val <- readBin(readBin(Fin,raw(),n=size*4)
                    ,double(),n=size,size=4,endian=endian)
   else if (datatype==5) ## data.type==5 -> 8-byte double
      val <- readBin(Fin,double(),n=size,endian=endian)
   else if (datatype %in% c(11,1011)) ## data.type==11 -> 1-byte signed short integer
      val <- readBin(Fin,integer(),n=size,size=1,endian=endian,signed=TRUE)
   else if (datatype %in% c(12,1012)) ## data.type==12 -> 2-byte unsigned short integer
      val <- readBin(Fin,integer(),n=size,size=2,endian=endian,signed=FALSE)
   else if (datatype %in% c(13,1013)) ## data.type==13 -> 4-byte unsigned integer
   {
      val <- readBin(Fin,integer(),n=size,size=4,endian=endian,signed=!FALSE) ## need signed=FALSE!
      ind <- which(val>(2^31-1))
      if (length(ind)) {
         opW <- options(warn=1)
         warning("Datatype 'UInt32' is supported with restriction. See '?readBin'.")
         options(opW)
         val[ind] <- val[ind]-2^32
      }
   }
   else
      stop("Unsupported data data type: ",datatype)
   gc(reset=TRUE)
   val
}
