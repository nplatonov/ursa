'.read_nc' <- function(fname,var="",level=NA,verbose=FALSE) {
   if (!is.character(fname))
      return(NULL)
   if (!requireNamespace("ncdf4",quietly=.isPackageInUse())) {
      stop("package 'ncdf4' is required for reading NetCDF")
     # return(NULL)
   }
   if (isZip <- .lgrep("\\.zip$",fname)>0) {
      ziplist <- unzip(fname,exdir=tempdir());on.exit(file.remove(ziplist))
      fname <- .grep("\\.(nc)$",ziplist,value=TRUE)
   }
   else if ((nchar(Sys.which("gzip")))&&
            (isZip <- (isZip1 <- .lgrep("\\.gz$",fname)>0) ||
                      (isZip2 <- file.exists(paste0(fname,".gz"))))) {
     # print(c(isZip1=isZip1,isZip2=isZip2))
     # stop("A")
      if (isZip1)
         fname0 <- fname
      else if (isZip2)
         fname0 <- paste0(fname,".gz")
      if (FALSE) {
         fname <- tempfile()
        # on.exit(file.remove(fname))
         system2("gzip",c("-f -d -c",.dQuote(fname0)),stdout=fname,stderr=FALSE)
      }
      else {
         fname <- .ursaCacheRaster(fname0,unpack="gzip")
      }
   }
   else if ((nchar(Sys.which("bzip2")))&&
            (isZip <- (isZip1 <- .lgrep("\\.bz2$",fname)>0)||
                      (isZip2 <- file.exists(paste0(fname,".bz2"))))) {
      if (isZip1)
         fname0 <- fname
      else if (isZip2)
         fname0 <- paste0(fname,".bz2")
      if (FALSE) {
         fname <- tempfile()
        # on.exit(file.remove(fname))
         system2("bzip2",c("-f -d -c",.dQuote(fname0)),stdout=fname,stderr=FALSE)
      }
      else {
         fname <- .ursaCacheRaster(fname0,unpack="bzip2")
      }
   }
   else if (.lgrep("^(https|http|ftp)\\://",fname)) {
      fname <- .ursaCacheDownload(fname,quiet=FALSE,mode="wb")
   }
   nc <- try(ncdf4::nc_open(fname,suppress_dimvals=FALSE))
   if (inherits(nc,"try-error")) {
     # cat(geterrmessage())
      return(NULL) 
   }
   on.exit({
      ncdf4::nc_close(nc)
     # if (isZip)
     #    file.remove(fname)
   })
   level0 <- level
   var0 <- var
   varList <- names(nc$var)
   if (length(varList)<0) ## ==1
      varName <- varList
   else if (nchar(var))
      varName <- .grep(var,varList,value=TRUE)
   else
      stop(paste("","-----","Please specify variable (argument 'var'):"
                ,paste(paste(seq(along=varList),". ",varList,sep=""),collapse="\n")
                ,"-----",sep="\n"))
   toggle <- FALSE
   if (!length(varName)) {
      return(list())
   }
   res <- vector("list",length(varName))
   names(res) <- varName
   for (i in seq_along(res)) {
      if (verbose)
         print(varName[i])
     # nc2 <- nc$var[[varName[i]]]
      b <- .open_nc(nc,var=varName[i],grid=TRUE,verbose=FALSE)
      indS <- attr(b,"spatial")
      indT <- attr(b,"temporal")
      bname <- names(b)
     # indL <- .grep("^(lon|lat|x$|y$|west|east|south|north|time|proj4)"
     #              ,bname[-c(1,2)],inv=TRUE)
      indL <- which(is.na(match(seq_along(bname),c(indS,indT))))
      if (length(indL)>1)
         stop("extra dimenstion?")
      ##~ str(b)
      ##~ str(indT)
      ##~ q()
      if (!length(indL)) {
        # print("B1")
         res[[i]] <- .open_nc(fname,var=varName[i],verbose=verbose)[]
      }
      else {
        # indL <- indL+2
         if (i>1)
            level <- level0
         if ((length(level)==1)&&(is.na(level)))
            level <- b[[indL]]
         if ((length(indL))&&(length(level)>1)&&(!identical(level,level0))) {
           # print("B2")
            res2 <- vector("list",length(level))
            names(res2) <- level
            for (j in seq_along(level)) {
               res2[[j]] <- .open_nc(fname,var=varName[i],level=level[j]
                                      ,verbose=verbose)[]
            }
            res[[i]] <- res2
            rm(res2)
            toggle <- TRUE
         }
         else {
           # print("B3")
            res[[i]] <- .open_nc(fname,var=varName[i],level=level
                                  ,verbose=verbose)[]
         }
      }
   }
   if ((length(varName)==1)&&(toggle))
      return(res[[1]])
   if (isDF <- all(sapply(res,inherits,"data.frame"))) {
      res <- as.data.frame(unlist(unname(res),recursive=FALSE),check.names=FALSE)
      name1 <- names(res)
      name2 <- unique(name1)
      ind <- match(name1,name2)
      count <- table(ind) # names(count) <- name2
      sameValue <- TRUE
      for (i in seq_along(name2)) {
         if (count[i]==1)
            next
         val <- unique(apply(as.matrix(res[ind==i]),1,function(x)
                                                         length(unique(x))))
         if (length(val)>1) {
            sameValue <- FALSE
            break
         }
      }
      if (!sameValue)
         stop("variables have not the same dimension; not implemented")
      ind <- match(name2,name1)
      res <- res[,ind]
      return(res)
   }
   res
}
'.open_nc' <- function(fname,var="",level=NA,grid=FALSE,verbose=FALSE) {
   if (!requireNamespace("ncdf4",quietly=.isPackageInUse())) {
      stop("package 'ncdf4' is required for reading NetCDF")
     # return(NULL)
   }
   if (inherits(fname,"ncdf4"))
      nc <- fname
   else if (is.character(fname)) {
      if (isZip <- .lgrep("\\.zip$",fname)>0) {
         ziplist <- unzip(fname,exdir=tempdir());on.exit(file.remove(ziplist))
         dsn <- .grep("\\.(nc)$",ziplist,value=TRUE)
      }
      else if ((nchar(Sys.which("gzip")))&&(isZip <- .lgrep("\\.gz$",fname)>0)) {
         fname0 <- fname
         fname <- tempfile();on.exit(file.remove(fname))
         system2("gzip",c("-f -d -c",.dQuote(fname0)),stdout=fname,stderr=FALSE)
      }
      else if ((nchar(Sys.which("bzip2")))&&(isZip <- .lgrep("\\.bz2$",fname)>0)) {
         fname0 <- fname
         fname <- tempfile();on.exit(file.remove(fname))
         system2("bzip2",c("-f -d -k",.dQuote(fname0)),stdout=fname,stderr=FALSE)
      }
      else if (.lgrep("^(http|https|ftp)://",fname)) {
         fname <- .ursaCacheDownload(fname,mode="wb")
      }
      nc <- try(ncdf4::nc_open(fname,suppress_dimvals=FALSE))
      if (inherits(nc,"try-error")) {
        # cat(geterrmessage())
         return(NULL) 
      }
      on.exit({
         ncdf4::nc_close(nc)
         if (isZip)
            file.remove(fname)
      })
   }
   else
      return(NULL)
  # opW <- options(warn=0-!verbose) ## to prevent 'GeoTransform values not available'
   varList <- names(nc$var)
   ##~ a <- ncvar_get(nc,"Times")
   ##~ print(a)
   ##~ q()
   if (length(varList)==1)
      varName <- varList
   else if (!is.na(var)) {
      varName <- .grep(var,varList,value=TRUE)
      if (length(varName)>1)
         varName <- .grep(paste0("^",var),varList,value=TRUE)
      if (length(varName)>1)
         varName <- .grep(paste0("^",var,"$"),varList,value=TRUE)
   }
   if (length(varName)!=1) {
      if (!grid)
         stop(paste("","-----","Please specify variable (argument 'var='):"
                   ,paste(paste(seq(along=varList),". ",varList,sep=""),collapse="\n")
                   ,"-----",sep="\n"))
      else
         varName <- varList[1]
   }
   att <- ncdf4::ncatt_get(nc,varName)
   md <- nc$var[[varName]][c("size","dimids","prec","unlim")]
   md$id <- md$dim <- NULL
   if (length(att))
      md <- c(md,'-------------'=NA,att)
   if ((verbose)&&(1)) {
      str(md)
   }
   if (FALSE) {
      nc2 <- nc$var[[varName]]
      val2 <- ncdf4::ncvar_get(nc,varName)
      att2 <- ncdf4::ncatt_get(nc,varName) ## $flag_values $flag_meanings -> colortable
      str(nc2)
      str(val2)
      str(att2)
      q()
   }
   a <- nc$var[[varName]]$dim
   if (is.null(a)) {
      if (verbose)
         message("scalar value")
      if (grid) 
         return(list())
      if (md$prec=="char") {
         val2 <- att
      }
      else
         val2 <- ncdf4::ncvar_get(nc,varName)
     # attr(val2,"metadata") <- md
     # ncdf4::nc_close(nc)
      return(val2)
   }
   b <- vector("list",length(a))
   names(b) <- lapply(a,function(x) x$name)
   b[] <- lapply(a,function(x){
      isTime <- ((length(x$units)>0)&&(.lgrep("\\ssince\\s",x$units)))
      if ((!FALSE)&&(isTime)) {
         y <- .ncdf_timeunits(x$val,x$units)
      }
      else
         y <- x$val
      y
   })
  # b$proj4 <- "" #ncdf4.helpers::nc.get.proj4.string(nc,varName)
   if (length(md$dimids)<2) {
      if (verbose)
         message("one-dimensional variable")
      val3 <- ncdf4::ncvar_get(nc,varName)
     # attr(val3,"metadata") <- md
     # ncdf4::nc_close(nc)
     # print(val3)
      return(val3)
   }
   bname <- names(b)
   indX <- 1
   indY <- 2
   if (length(ind <- .grep("^(lon|x$|west|east)",bname)))
      indX <- ind
   else
      indX <- 1L
   if (length(ind <- .grep("^(y$|lat|south|north)",bname)))
      indY <- ind
   else
      indY <- 2L
   if ((length(indX)!=1)||(length(indY)!=1)||(indX[1]==indY[1])) {
      if (verbose)
         message('dimensional variables are not detected')
      indX <- 1
      indY <- 2
      checkDim <- TRUE
   }
   else
      checkDim <- FALSE
   nonstandard <- !((.lgrep("^(x|y)$",bname)==2) |
                 (.lgrep("^(lon|lat)",bname)==2) |
                 (.lgrep("^(west|south)",bname)==2) |
                 (.lgrep("^(east|north)",bname)==2))
   # att <- ncdf4::ncatt_get(nc,varName,attname=NA,verbose=FALSE)
  # str(att)
   if ((nonstandard)&&(verbose))
      message(paste("non-standard dim names:"
        ,paste(sQuote(grep("^proj4$",bname,invert=TRUE,value=TRUE)),collapse=",")))
   flip <- TRUE
   permute <- FALSE
   bY <- b[[indY]]
   if ((is.numeric(bY))&&((all(diff(bY)<0))||(nonstandard))) {
      flip <- FALSE
      if (verbose)
         message("flip - reverse second dim")
      b[[indY]] <- rev(bY)
   }
   if (indX>indY) {
      permute <- TRUE
      if (verbose)
         message("permute - transpose coordinates")
      names(b)[c(indX,indY)] <- bname[c(indY,indX)]
      b[c(indX,indY)] <- b[c(indY,indX)]
      .ind <- indX
      indX <- indY
      indY <- .ind
      rm(.ind)
   }
   indS <- c(indX,indY)
   indT <- which(sapply(b,inherits,c("POSIXt","Date")))
   if (!length(indT))
      indT <- .grep("(time|^zzzday$)",bname)
   indL <- which(is.na(match(seq_along(b),c(indS,indT))))
   if ((!length(indT))&&(length(indL))) {
      lev <- b[[indL]]
      if (length(lev)>2) {
         dl <- unique(diff(lev))
         if (length(dl)==1) { ## considering that levels are not regular
            indT <- indL
            indL <- integer()
         }
      }
   }
   if (grid) {
     # ncdf4::nc_close(nc)
      attr(b,"spatial") <- indS
      attr(b,"temporal") <- indT
      return(b)
   }
   cond1 <- length(dim(b[[indX]]))>1 | length(dim(b[[indY]]))>1
   cond2 <- (length(b[[indX]])<=2 | length(b[[indY]])<=2) & nonstandard
   if (cond2) {
      if (verbose)
         message("truncated to two-columns/rows matrix")
      val6 <- t(ncdf4::ncvar_get(nc,varName))
      if (is.character(val6)) {
         val6 <- c(val6)
         names(val6) <- b[[indY]]
      }
      else {
         dimnames(val6) <- list(as.character(b[[indY]]),as.character(b[[indX]]))
      }
      if (length(indT)) {
         dimv <- dimnames(val6)
         dima <- dim(val6)
         val6 <- as.character(.ncdf_timeunits(val6,attr(b[[indT]],"units")))
         dim(val6) <- dima
         dimnames(val6) <- dimv
      }
     # if (length(indT))
     #    attr(val6,"time") <- b[[indT]]
      return(val6)
   }
   if ((cond1)||(cond2)) {
      if (verbose)
         message("gridded dimension coordinates; coercing to data frame")
     # x <- ncdf4::ncvar_get(nc,bname[1])
      val5 <- ncdf4::ncvar_get(nc,varName)
      da <- data.frame(x=c(b[[indX]]),y=c(b[[indY]]),z=c(val5))
      names(da) <- c(bname[c(indX,indY)],varName)
      if (length(indT))
         attr(da,"time") <- b[[indT]]
      return(da)
   }
   con <- .con.skeleton()
   con$driver <- "NCDF"
   con$offset <- nc$var[[varName]]$size
   con$seek <- FALSE
   if ((length(indT))&&(indT<=2)&&(length(indS)!=2)) {
      if (verbose)
         message("spatial coordinates cannot be detected")
      val4 <- ncdf4::ncvar_get(nc,varName)
      if (!TRUE) {
         dima <- dim(val4)
         val4 <- as.character(.ncdf_timeunits(val4,attr(b[[indT]],"units")))
         dim(val4) <- dima
      }
     # attr(val2,"metadata") <- md
     # ncdf4::nc_close(nc)
      return(val4)
   }
   con$fname <- nc$filename
   con$handle <- varName ## temporal hiding
  # attr(con$handle,"var") <- varName
   attr(con$handle,"flip") <- flip
   attr(con$handle,"permute") <- permute
   attr(con$handle,"spatial") <- indS
   attr(con$handle,"temporal") <- indT
   attr(con$handle,"level (proposed)") <- indL
   #indL <- which(is.na(match(seq_along(b),c(indX,indY,indT,length(b)))))
  # if ((!any(is.na(level)))&&(length(indL <- .grep("(level|zlev)",bname)))) {
  # if ((!any(is.na(level)))&&(length(indL))) {
   if (length(indL)) {
      lev <- b[[indL]]
     # print(c(levels=length(lev)))
      w <- rep(0,length(lev))
      if (anyNA(level))
         level <- lev
      if (length(level)>length(lev)) ## interval
         lev2 <- lev[lev>=min(level) & lev<=max(level)]
      else ## match
         lev2 <- lev[match(level,lev)]
      ind <- match(lev,lev2)
      ind2 <- which(!is.na(ind))
      if (length(ind2)>1) {
         dl <- rowSums(matrix(c(0,rep(abs(diff(lev2)),times=2),0),ncol=2))
         w[ind2] <- dl/sum(dl)
      }
      else if (length(ind2)==1)
         w[ind2] <- 1
      else {
         lev1 <- tail(lev[lev>=level],1)
         lev2 <- head(lev[lev<=level],1)
         dl <- abs(diff(c(lev1,level,lev2)))
         ind <- match(lev,c(lev1,lev2))
         w[which(!is.na(ind))] <- rev(dl/sum(dl))
      }
      names(w) <- lev
      attr(con$handle,"weight") <- w
   }
   if (verbose)
      print(object.size(nc))
   if (!is.null(b)) {
      b$proj4 <- ""
      g1 <- .grid.skeleton()
      aname <- names(b)
      indx <- .grep("^(x$|lon|west|east)",aname)
      indy <- .grep("^(y$|lat|south|north)",aname)
      if (((!length(indx))||(!length(indy)))&&(length(aname)>=2)) {
         indx <- 1L
         indy <- 2L
      }
      proj4 <- .grep("proj4",aname)
      aname <- .grep("proj4",aname,value=TRUE,invert=TRUE)
      indz <- which(is.na(match(seq_along(aname),c(indx,indy))))
      if ((length(indx))&&(length(indy))) {
         x <- b[[indx]]
         y <- b[[indy]]
         ##~ print(sd(diff(x)))
         ##~ print(sd(diff(y)))
         g1$resx <- mean(unique(diff(x)))
         g1$resy <- mean(unique(diff(y)))
         g1$minx <- min(x)-g1$resx/2
         g1$maxx <- max(x)+g1$resx/2
         g1$miny <- min(y)-g1$resy/2
         g1$maxy <- max(y)+g1$resy/2
         g1$columns <- length(x)
         g1$rows <- length(y)
         if ((length(proj4))&&(nchar(b[[proj4]]))) {
            g1$proj4 <- b[[proj4]] # code lost: 'g1$proj4 <- p'
         }
         else if (.lgrep("(lon|lat)",aname)==2)
            g1$proj4 <- " +proj=longlat +datum=WGS84 +no_defs"
         if (TRUE)
            g1 <- with(g1,regrid(g1,setbound=c(minx,miny,maxx,maxy)
                                   ,dim=c(rows,columns)))
         session_grid(g1)
         con$samples <- g1$columns
         con$lines <- g1$rows
         if (length(indT)==1)
            con$bands <- length(b[[indT]])
         else
            con$bands <- 1
         con$indexC <- seq(g1$columns)
         con$indexR <- seq(g1$rows)
         con$indexZ <- seq(con$bands)
      }
   }
  # a <- nc$var[[varName]]
   con$nodata <- nc$var[[varName]]$missval
   if (verbose)
      str(b,digits=5)
   res <- .raster.skeleton()
   res$grid <- g1
   res$con <- con
   res$dim <- c(con$samples*con$lines,con$bands)
   detail <- FALSE
   if (length(indT)==1)
      res$name <- as.character(b[[indT]])
   else if (length(indL)==1) {
      w2 <- w[w>0]
      if (length(w2)==1)
         res$name <- names(w2)
      else
         res$name <- varName
   }
   else {
     # res$name <- as.character(b[[indL]])
      if (length(ind <- .grep("long_name",names(att)))) {
         res$name <- att[[ind]]
         if (length(ind <- .grep("units",names(att))))
            res$name <- paste0(res$name,", ",att[[ind]])
      }
      else
         res$name <- varName
   }
   res
}
'.ncdf_timeunits' <- function(x,units) {
   if (is.null(units))
      return(x)
   or <- .gsub2("\\D(\\d+-\\d+-\\d+)\\D+(\\d+:\\d+:\\d+(\\.\\d+)*)","\\1 \\2",units)
   if (or==units)
      or <- .gsub2("\\D(\\d+-\\d+-\\d+)","\\1",units)
   if (or==units)
      stop(paste("incorrect parsing for 'origin' detection:",dQuote(units)))
   if (.lgrep("second(s)* since",units))
      NULL
   else if (.lgrep("minute(s)* since",units))
      x <- x*60
   else if (.lgrep("hour(s)* since",units))
      x <- x*60*60
   else if (.lgrep("day(s)* since",units))
      x <- x*60*60*24
   else if (.lgrep("since",units))
      stop(paste("incorrect parsing for 'origin' detection:",dQuote(units)))
   y <- as.POSIXct(x,origin=or,tz="UTC")
   attr(y,"units") <- units
   attr(y,"origin") <- or
   class(y) <- c(class(y),"ursaTimeUnits")
   y
}
