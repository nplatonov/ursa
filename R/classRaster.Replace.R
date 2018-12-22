'[<-.ursaRaster' <- function(x,i,j,...,value)
{
   verbose <- isTRUE(getOption("ursaDevel"))
   if (verbose) {
      cat("----------\n")
      on.exit(cat("==========\n"))
   }
  # if (verbose)
  #    str(match.call())
   dimx <- dim(x$value)
   con <- x$con
   if (!.is.con(con))
      stop("no connnecton")
   missingJ <- missing(j)
   missingI <- missing(i)
   if (!is.null(dimx))
   {
      obj <- x
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
         i <- .getBand(obj,i,regexp=regexp)
         j <- seq(dimx[1])
      }
      if (is.ursa(i))
      {
         debug <- FALSE
         if (debug)
         {
            print(head(obj))
            print(head(i))
            print(head(value))
         }
         if (!is.ursa(value))
            obj$value[!is.na(i$value)] <- value
         else
         {
            ind <- !is.na(i$value)
            if (proposed <- TRUE) {
               if ((length(obj)>1)&&(length(obj)==length(i))) {
                  k <- rep(seq_along(value),length.out=length(obj))
                  for (m in seq_along(k))
                     obj$value[ind[,m],m] <- value$value[ind[,m],k[m]]
               }
               else ## common
                  obj$value[ind] <- value$value[ind]
            }
            else {
               obj$value[ind] <- value$value[ind]
            }
         }
         if (debug)
         {
            print(head(obj))
            stop("HERE")
         }
         return(obj)
      }
      if (is.ursa(value))
      {
         if (FALSE)
         {
            print(str(obj))
            print(str(value))
            print(i)
         }
         if (FALSE) ## 2011-08-07
         {
            if (length(obj$name[i])==length(value$name[seq(i)]))
               obj$name[i] <- value$name[seq(i)]
         }
         posR <- value$con$posR
         posZ <- value$con$posZ
         value <- value$value ## length(dim(val))==2 should be!
         isUrsa <- TRUE
      }
      else {
         posR <- NA
         posZ <- NA
         isUrsa <- FALSE
      }
      if (TRUE) { ## added 20160327
         bg <- ignorevalue(x)
         if (!is.na(bg))
            value[.is.eq(value,bg)] <- NA
      }
      if (verbose) {
         str(value)
         print(is.null(value))
         print(dim(value))
         print(length(value))
      }
      if (missingJ) {
         if (is.null(value))
            return(obj[-i])
         if ((is.null(dim(value)))&&(!length(value)))
            return(obj[-i])
      }
      if (is.array(value))
      {
         if (!isUrsa) { ## 20170712 ++
            d <- prod(obj$dim)/prod(dim(value))
            d2 <- prod(dim(value))==prod(c(obj$dim[1],obj$dim[1]/prod(dim(value))))
           ## 2013-02-25 added '&&(prod(dim(value))==prod(obj$dim))'
           ## 2013-10-07 added '&&(d==round(d))'
           ## 2013-11-07 added '&&(d2)'
            if ((dim(value)[1]!=obj$dim[1])&&(d==round(d))&&(d2))
               dim(value) <- c(obj$dim[1],obj$dim[1]/prod(dim(value)))
         }
         j2 <- if (!missingJ) .expandLineIndex(x,j) else j
        # print(range(j))
        # print(range(j2))
        # print(i)
        # str(value)
         for (m in seq(along=i))
         {
           # obj$data[j,i[m]] <- value[,m]
            val <- value[,m,drop=FALSE]
           # val[val==con$nodata] <- NA
            if (FALSE)
            {
               print(c(m=m,j=range(j2),i=i,im=i[m]))
               print(str(val))
               print(str(obj$data[j,]))
            }
            obj$value[j2,i[m]] <- val
         }
      }
      else if ((is.numeric(value))||(is.na(value)))
      {
         if (!missingJ)
            j2 <- .expandLineIndex(x,j)
         if ((!missingJ)&&(length(value)==length(j2)))
         {
            for (m in seq_along(i))
            {
               obj$value[j2,i[m]] <- value
            }
         }
         else
         {
            dim1 <- obj$dim[1]
            lenv <- length(value)
            leni <- length(i)
           # if (length(value)==1L)
           #    value <- rep(value,length(i))
            if (lenv==dim1*leni)
               obj$value[,i] <- value
            else if (lenv<=leni) {
               value <- rep(value,len=leni)
               for (m in seq(i))
                  obj$value[,i[m]] <- value[m]
            }
            else if (.is.integer(lenv/dim1))
               obj$value[,i] <- rep(value,len=dim1*leni)
            else {
               str(value)
               warning("#101 Cannot recognize value structure. Assigning is skipped")
            }
         }
      }
      else
      {
         print(str(value))
         warning("#102 Cannot recognize value structure. Assigning is skipped")
      }
     # obj$con <- con
     # print(str(obj));q()
      return(obj)
   }
   if (is.array(value))
   {
      message("**************************** Not debugged *********************************")
      message("** 'is.array(v)'. Try 'x[] <- ursa_new(value=v)' or 'ursa_value(x) <- v' **")
      message("**************************** Not debugged *********************************")
      value <- ursa_new(value=value,flip=FALSE,permute=FALSE) ## introduced 2012-10-14
     # val <- value
     # value <- x
     # print(str(x))
   }
   else if ((!is.ursa(value))&&((is.numeric(value))||(all(is.na(value))))) {
      value <- ursa_new(value=value,flip=FALSE,permute=FALSE)
   }
   if (is.ursa(value))
   {
      dimy <- dim(value$value)
      dimx <- x$dim
      if (length(con$nodata)==1L)
         value$value[is.na(value$value)] <- con$nodata ## uncomment 2012-05-06
      toSeek <- as.integer(con$seek)
      if (missing(j))
         j <- seq(dimy[1])
      else if (toSeek)
         toSeek <- toSeek+1
      if (missing(i))
         i <- seq(dimy[2])
      else if (toSeek)
         toSeek <- toSeek+2
      if (is.list(j))
         j <- unlist(j)
      if (is.list(i))
         i <- unlist(i)
      if (toSeek)
         toSeek <- toSeek-1
      if (is.character(i))
      {
         i <- .getBand(x,i,new=TRUE)
         j <- seq(dimy[1])
      }
      if (all(i %in% seq(dimx[2])))
         NULL
      else ##if (all(!(i %in% seq(dimx[2]))))
      {
         message("append new band(s)")
         if (x$con$interleave!="bsq")
            stop("Only BSQ interleave is effective for band rebuild")
         nb <- x$con$bands
         nb2 <- max(nb,i)
         x$con$bands <- nb2
         z <- x$con$indexZ
         x$con$indexZ <- seq(nb2)
         indZ <- match(z,i)
         x$dim[2] <- x$dim[2]+nb2-nb
         myname <- rep("",nb2)
         myname[z] <- x$name
         name2 <- value$name
         if (!is.na(value$con$posZ)[1])
            name2 <- name2[value$con$posZ]
         myname[i] <- name2
         ind <- which(!nchar(myname))
         if (length(ind))
            myname[ind] <- paste0("band",ind)
         x$name <- myname
         dimx <- x$dim
         if (x$con$driver=="ENVI") {
            .write.hdr(x)
            if (TRUE) { ## added 20161226
               nb <- ifelse(is.na(x$con$posZ[1]),x$con$bands,length(con$posZ))
               if (toSeek)
                  seek(x$con,origin="start"
                      ,where=with(x$con,(lines*samples*nb2-1)*sizeof+offset),rw="w")
               val <- 0L
               storage.mode(val) <- con$mode
               with(x$con,writeBin(val,size=sizeof,endian=endian,handle))
            }
         }
         con <- x$con
      }
     ## else stop("Cannot apply mixed assignment for existing and missing bands")
      if (!TRUE) ## TRUE before 2012-09-07
      {
         j <- seq(min(j),max(j))
         i <- seq(min(i),max(i))
      }
     # isRound <- if (con$datatype %in% c(1,2,3,12)) TRUE else FALSE
      toRound <- !(is.integer(value$value)) && (con$mode=="integer")
      bands <- x$dim[2]
      if (length(i)>bands)
      {
         cat(sprintf("writting %d bands into file with %d bands. Truncated\n"
            ,length(i),bands))
         stop("Unpredictive result. Check your code!")
         i <- i[seq(bands)]
      }
      if (verbose)
         print(c(j=range(j),i=range(i),dim=dimx,toSeek=toSeek))
     # print(c(j=all(j %in% seq(dimx[1])),i=all(i %in% seq(dimx[2]))))
      if ((length(j)<dimx[1])&&(all(j %in% seq(dimx[1])))) ## write lines
      {
         if (verbose)
            print("write lines")
         toSeek <- toSeek %in% c(1,3)
         if (any(is.na(value$con$posR)))
            nl <- as.integer(dimy[1]/con$samples)
         else
            nl <- length(value$con$posR)
         sparse <- attr(value$value,"sparse")
         if ((!is.null(sparse))&&(any(sparse!=0)))
         {
            val <- array(NA,dim=with(con,c(samples*nl,dimy[2])))
            cl <- class(value$value)
            val[sparse,] <- value$value
            value$value <- val
            class(value$value) <- cl
            rm(val)
            gc(reset=TRUE)
         }
         dim(value$value) <- with(con,c(samples,nl,bands))
         dimz <- dim(value$value)
         if (con$driver=="ENVI") {
            if (con$interleave=="bil")
            {
               if ((0)&&(TRUE))
               {
                  stop("HERE")
               }
               else
               {
                  if (toSeek)
                     seek(con,origin="start"
                         ,with(con,(min(j)-1)*samples*bands*sizeof+offset),rw="w")
                  if (toRound)
                     for (r in seq(dimz[2]))
                        writeBin(as.vector(.round(value$value[,r,],0),con$mode)
                                ,size=con$sizeof,endian=con$endian,con$handle)
                  else
                     for (r in seq(dimz[2]))
                        writeBin(as.vector(value$value[,r,],con$mode)
                                ,size=con$sizeof,endian=con$endian,con$handle)
               }
            }
            else if (con$interleave=="bip")
            {
               if (toSeek)
                  seek(con,origin="start"
                      ,with(con,(min(j)-1)*samples*bands*sizeof+offset),rw="w")
               if (toRound)
                  for (r in seq(dimz[2]))
                     writeBin(as.vector(t(.round(value$value[,r,],0)),con$mode)
                             ,size=con$sizeof,endian=con$endian,con$handle)
               else
                  for (r in seq(dimz[2]))
                     writeBin(as.vector(t(value$value[,r,]),con$mode)
                             ,size=con$sizeof,endian=con$endian,con$handle)
            }
            else if (con$interleave=="bsq")
            {
               posR <- value$con$posR
               nl <- length(posR)
               minI <- min(posR)
               myoffset <- with(con,(lines-nl)*samples*sizeof)
               for (r in seq(dimz[3]))
               {
                 # print(with(con,((r-1)*lines+minI-1)*samples))
                  if (toSeek)
                     seek(con,origin="start"
                         ,with(con,((r-1)*lines+minI-1)*samples*sizeof+offset)
                         ,rw="w")
                 # print(c(r=r,beg=with(con,seek(handle,w=0,pos="current"))))
                  if (toRound)
                     writeBin(as.vector(.round(value$value[,,r],0),con$mode)
                             ,size=con$sizeof,endian=con$endian,con$handle)
                  else
                     writeBin(as.vector(value$value[,,r],con$mode)
                             ,size=con$sizeof,endian=con$endian,con$handle)
                 # print(c(r=r,end=with(con,seek(handle,w=0,pos="current"))))
                  if ((!toSeek)&&(r<dimz[3])) {
                     seek(con$handle,where=myoffset,origin="current",rw="w")
                  }
               }
               if (!toSeek)
               {
                  seek(con$handle,origin="current"
                      ,where=-with(con,lines*samples*(dimz[3]-1)*sizeof),rw="w")
               }
              # print(c(onexit=with(con,seek(handle,w=0,pos="current"))))
            }
         }
         else if (con$driver=="GDAL") { # write lines
            minJ <- min(j)-1
            if (toRound) {
               for (b in seq(dimz[3])) {
                  rgdal::putRasterData(con$handle
                                      ,.round(value$value[,,b,drop=TRUE])
                                      ,offset=c(minJ,0),band=b)
               }
            }
            else {
               for (b in seq(dimz[3])) {
                  rgdal::putRasterData(con$handle,value$value[,,b,drop=TRUE]
                                      ,offset=c(minJ,0),band=b)
               }
            }
         }
      }
      else if ((length(i)<dimx[2])&&(all(i %in% seq(dimx[2])))) ## write bands
      {
         if (verbose)
            print("write bands")
         toSeek <- (toSeek %in% c(1,2)) ## c(2,3) 2012-08-27
         sparse <- attr(value$value,"sparse")
         if ((!is.null(sparse))&&(any(sparse!=0)))
         {
            val <- array(NA,dim=with(con,c(samples*lines,dimy[2])))
            cl <- class(value$value)
            val[sparse,] <- value$value
            value$value <- val
            class(value$value) <- cl
            rm(val)
            gc(reset=TRUE)
         }
         else 
            dim(value$value) <- with(con,c(samples,lines,dimy[2]))
         dimz <- dim(value$value)
         if (con$driver=="ENVI") {
            if (con$interleave=="bil")
            {
               posZ <- as.integer(value$con$posZ)
              # .elapsedTime("write:start")
               if ((0)&&(TRUE))
               {
                  xdim <- with(con,c(lines,samples,bands))
                  if (toRound)
                     val <- as.vector(.round(value$value),con$mode)
                  else
                     val <- as.vector(value$value,con$mode)
                  if (!is.na(con$nodata))
                     val[is.na(val)] <- con$nodata
                  if (con$mode=="integer")
                     val <- .Cursa("writeBilBandInteger",con$fname,val=val,dim=xdim
                              ,index=posZ,n=length(posZ),datatype=con$datatype
                              ,swap=con$swap)
                  else
                     val <- .Cursa("writeBilBandDouble",con$fname,dim=xdim,index=i
                              ,n=bands,datatype=con$datatype,swap=con$swap
                              ,res=double(with(con,bands*samples*lines)))$res
               }
               else
               {
                  if (!toSeek)
                  {
                     if (!is.na(posZ[1]))
                        i <- posZ
                     else if (FALSE)
                        stop("unable define file position. Please specify 'band'")
                     toSeek <- con$seek
                     if (is.na(posZ[1]))
                        nb <- con$bands
                     else
                     {
                        nb <- length(posZ)
                        i <- seq(length(posZ))
                     }
                     nb <- ifelse(is.na(con$posZ[1]),con$bands,length(con$posZ))
                  }
                  else
                     nb <- con$bands
                  dj <- which(diff(i)!=1)
                  nj <- 1+length(dj)
                  listJ <- vector("list",nj)
                  j1 <- 1
                  for (j3 in seq_along(listJ))
                  {
                     j2 <- ifelse(j3==nj,length(i),dj[j3])
                     listJ[[j3]] <- i[j1:j2]
                     j1 <- j2+1
                  }
                  for (j0 in seq_along(listJ))
                  {
                     j1 <- listJ[[j0]]
                     minJ <- min(j1)-1
                     j2 <- match(j1,i)
                     for (r in seq(dimz[2]))
                     {
                        if (toSeek)
                        {
                           pos <- with(con,((r-1)*nb+minJ)*samples*sizeof+offset)
                           seek(con$handle,pos,origin="start",rw="w")
                        }
                        if (toRound)
                           writeBin(as.vector(.round(value$value[,r,j2],0),con$mode)
                                   ,size=con$sizeof,endian=con$endian,con$handle)
                        else
                           writeBin(as.vector(value$value[,r,j2],con$mode)
                                   ,size=con$sizeof,endian=con$endian,con$handle)
                     }
                  }
               }
              # .elapsedTime("write:finish")
            }
            else if (con$interleave=="bip")
            {
               posZ <- value$con$posZ
               minJ <- min(posZ)-1
               for (r in seq(dimz[2]))
               {
                  val <- t(value$value[,r,])
                  if (toRound)
                     val <- .round(val,0)
                  for (c in seq(dimz[1]))
                  {
                     pos <- with(con,(((r-1)*samples+c-1)*bands+minJ)*sizeof+offset)
                     seek(con$handle,pos,origin="start",rw="w")
                     writeBin(as.vector(val[,c],con$mode)
                             ,size=con$sizeof,endian=con$endian,con$handle)
                  }
               }
            }
            else if (con$interleave=="bsq")
            {
               toSeek <- con$seek ## introdiced 2012-08-27
               for (r in seq(along=i))
               {
                  if (toSeek)
                     seek(con$handle,origin="start"
                         ,with(con,(i[r]-1)*lines*samples*sizeof+offset),rw="w")
                  if (toRound)
                     writeBin(as.vector(.round(value$value[,,r],0),con$mode)
                             ,size=con$sizeof,endian=con$endian,con$handle)
                  else
                     writeBin(as.vector(value$value[,,r],con$mode)
                             ,size=con$sizeof,endian=con$endian,con$handle)
                 # if ((!toSeek)&&(r<dimy[3]))
                 #    seek(con$handle,where=myoffset,origin="current")
               }
               if ((0)&&(!toSeek))
                  seek(con$handle,origin="current"
                      ,where=-with(con,(lines)*samples*(dimy[3]-1)*sizeof),rw="w")
            }
            else
               stop("unknown interleave")
         }
         else if (con$driver=="GDAL") { ## write bands
            if (toRound) {
               for (r in seq_along(i)) {
                  rgdal::putRasterData(con$handle
                                      ,.round(value$value[,,r,drop=TRUE])
                                      ,band=i[r])
               }
            }
            else {
               for (r in seq_along(i)) {
                  rgdal::putRasterData(con$handle,value$value[,,r,drop=TRUE]
                                      ,band=i[r])
               }
            }
         }
      }
      else if ((length(j)==dimx[1])&&(length(i)==dimx[2])) ## write full
      {
         if (verbose)
            print("write full")
        # .writeall(data=value$value,con=con,split=TRUE,con$handle)
         if (!is.na(con$nodata))
         {
           # storage.mode(con$nodata) <- con$mode
            if (TRUE)
            {
               tmp <- value$value
               tmp[is.na(tmp)] <- con$nodata
               value$value[] <- tmp
            }
            else if (FALSE)
               value$value[is.na(value$value)] <- con$nodata
            else if (FALSE)
               is.na(value$value) <- con$nodata ## don't trust
         }
         value <- decompress(value)
         dim(value$value) <- with(con,c(samples,lines,bands))
         if (con$driver=="ENVI") {
            Fout <- con$handle
            if (con$seek)
               seek(con,origin="start",where=con$offset,rw="w")
            if (toRound)
            {
               if (con$interleave=="bil") ## R's [col,row,band] -> bil [col,band,row]
               {
                  for (r in seq(dim(value$value)[2]))
                     writeBin(as.vector(.round(value$value[,r,],0),con$mode)
                             ,size=con$sizeof,endian=con$endian,Fout)
               }
               else if (con$interleave=="bip") ## R's [col,row,band] -> bip [band,col,row]
               {
                  for (r in seq(dim(value$value)[2]))
                     writeBin(as.vector(t(.round(value$value[,r,],0)),con$mode)
                             ,size=con$sizeof,endian=con$endian,Fout)
               }
               else if (con$interleave=="bsq") ## R's [col,row,band] -> bsq [col,row,band] 
               {
                  for (r in seq(dim(value$value)[3]))
                     writeBin(as.vector(.round(value$value[,,r],0),con$mode)
                             ,size=con$sizeof,endian=con$endian,Fout)
               }
            }
            else
            {
               if (con$interleave=="bil") ## R's [col,row,band] -> bil [col,band,row]
               {
                  for (r in seq(dim(value$value)[2]))
                     writeBin(as.vector(value$value[,r,],con$mode)
                             ,size=con$sizeof,endian=con$endian,Fout)
               }
               else if (con$interleave=="bip") ## R's [col,row,band] -> bip [band,col,row]
               {
                  for (r in seq(dim(value$value)[2]))
                     writeBin(as.vector(t(value$value[,r,]),con$mode)
                             ,size=con$sizeof,endian=con$endian,Fout)
               }
               else if (con$interleave=="bsq") ## R's [col,row,band] -> bsq [col,row,band] 
               {
                  for (r in seq(dim(value$value)[3]))
                     writeBin(as.vector(value$value[,,r],con$mode)
                             ,size=con$sizeof,endian=con$endian,Fout)
               }
            }
         }
         else if (con$driver=="GDAL") { # write full
            dimz <- dim(value$value)
            if (toRound) {
               for (b in seq(dimz[3]))
                  rgdal::putRasterData(con$handle
                                      ,.round(value$value[,,b,drop=TRUE])
                                      ,band=b)
            }
            else {
               for (b in seq(dimz[3])) {
                  rgdal::putRasterData(con$handle,value$value[,,b,drop=TRUE]
                                      ,band=b)
               }
            }
         }
      }
      else
      {
         print(dimx)
         print(length(j))
         print(length(i))
         stop("TODO#1")
      }
      if (x$con$compress==-1)
         x$con$compress <- -2
      return(x)
   }
   else
      stop("TODO#2")
}
'.expandLineIndex' <- function(x,j) ## cadidate for applying in '[.' function
{
   if (!is.ursa(x))
      return(NULL)
   if (is.na(x$con$indexC)[1])
      s <- x$grid$columns
   else
      s <- length(x$con$indexC)
   i <- as.integer(c(t(col(array(NA,dim=c(length(j),s)))+(j-1)*s)))
   if (max(j)>x$dim[1])
   {
      print(c(maxi=max(j),leni=length(j)))
      i <- i[i<=x$dim[1]]
   }
   i
}
