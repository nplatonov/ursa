'.gdal_sfinfo' <- function(fname) {
   md <- list(nodata=NA,scale=NA,offset=NA)
   if (forceInfo <- TRUE) {
      gi <- sf::gdal_utils("info",fname,quiet=TRUE)
      gi <- strsplit(gi,split="\\n")[[1]]
      nd <- grep("NoData Value",gi,value=TRUE)
      if (length(nd)>0) {
         md$nodata <- as.numeric(gsub("^.*=(\\s*(\\S+))$","\\1",nd))
        # if (typeof(ursa_value(res))=="integer")
        #    ignorevalue(res) <- as.integer(unique(nodata))
        # else
        #    ignorevalue(res) <- as.numeric(unique(nodata))
      }
      sc <- grep("(Offset:|Scale:)",gi,value=TRUE)
      if (length(sc)>0) {
         md$offset <- as.numeric(gsub(".*Offset:\\s*((-)*\\d(\\.\\d+)*)(\\D.*|$)","\\1",sc))
         md$scale <- as.numeric(gsub(".*Scale:\\s*((-)*\\d(\\.\\d+)*)(\\D.*|$)","\\1",sc))
      }
      if ((!is.na(md$nodata[1]))&&(!is.na(md$scale))&&(!is.na(md$offset))) {
         md$nodata <- md$nodata*md$scale+md$offset
      }
   }
   md
}
'.open_sfgdal' <- function(fname,engine="sf",verbose=FALSE) {
   if (!requireNamespace("sf",quietly=.isPackageInUse()))
      stop("Package 'sf' is required for this operation")
   if (verbose)
      print(c('open_gdal'="`sf::gdal_read()` without data"))
   obj <- sf::gdal_read(fname,read_data=FALSE)
   md <- .gdal_sfinfo(fname)
   columns <- obj$cols[2]
   rows <- obj$rows[2]
   bands <- length(obj$bands)
   if (length(ind <- grep("(^crs$|^wkt|wkt$)",names(obj))))
      crs <- obj[[ind]]
   if (is.character(crs))
      crs <- sf::st_crs(crs)$proj4string
   if (inherits(crs,"crs")) {
      crs <- .WKT(crs)
     # crs <- .proj4string(crs)
   }
   if (is.na(crs))
      crs <- ""
   if (all(is.na(obj$geotransform))) {
      resx <- 1
      resy <- 1
      minx <- 0
      miny <- 0
      maxx <- columns
      maxy <- rows
   }
   else {
      resx <- obj$geotransform[2]
      resy <- -obj$geotransform[6]
      minx <- obj$geotransform[1]
      maxy <- obj$geotransform[4]
      maxx <- minx+columns*resx
      miny <- maxy-rows*resy
   }
   ##~ a <- list(minx=minx,maxx=maxx,miny=miny,maxy=maxy,columns=columns,rows=rows
               ##~ ,crs=crs # sf::st_crs(obj$crs)$proj4string
               ##~ )
   ##~ str(a,digits=16)
   g1 <- regrid(minx=minx,maxx=maxx,miny=miny,maxy=maxy,columns=columns,rows=rows
               ,crs=.ursaCRS(crs) # sf::st_crs(obj$crs)$proj4string
               )
   res <- .raster.skeleton()
   res$grid <- g1
   con <- .con.skeleton()
   con$samples <- columns
   con$lines <- rows
   con$bands <- bands
   con$driver <- "SF"
   con$indexC <- seq(columns)
   con$indexR <- seq(rows)
   con$indexZ <- seq_along(bands)
   con$seek <- FALSE
   con$fname <- fname
   con$datatype <- switch(obj$datatype,byte=1L,integer=2L,real=4L,float=4L
                         ,Byte=1L,UInt8=1L,Int8=11
                         ,Int16=2L,UInt16=12,UInt32=13,Int32=3
                         ,Float32=4L,Float64=5L
                         ,CInt16=21L,CInt32=22L,CFloat32=23L,CFloat64=24L
                     ,NULL)
   con$handle <- obj
   res$con <- con
   isClass <- length(obj$attribute_tables[[1]])>0
   isColor <- length(obj$color_tables[[1]])>0
   isCat <- (isClass)||(isColor)
   if (isCat) {
      if (isClass) {
         ctName <- obj$attribute_tables[[1]][["category"]]
         if (is.null(ctName))
            isCat <- FALSE
      }
   }
   if (isCat) {
      if (isColor) {
         ctCol <- obj$color_tables[[1]]
         ct <- rgb(ctCol[,1],ctCol[,2],ctCol[,3],ctCol[,4],maxColorValue=255)
         if (all(substr(ct,8,9)=="FF"))
            ct <- substr(ct,1,7)
         if (isClass)
            if (length(ct)>length(ctName))
               ct <- ct[seq_len(length(ctName))]
      }
      else
         ct <- rep(NA,length(ctName))
      if (isClass)
         names(ct) <- ctName
      class(ct) <- "ursaColorTable"
      if (length(ind <- which(names(ct)==""))) {
         if (length(ind <- ind[ind>1])>0) {
            if (length(ctCol <- unique(substr(ct[ind],1,7)))==1) {
               ct <- ct[-ind]
            }
         }
      }
   }
   if (.lgrep("float",obj$datatype))
      vmode <- "numeric"
   else
      vmode <- "integer"
   if ((isCat)||(vmode=="integer")) {
      if ((!is.na(md$scale))&&(!is.na(md$offset))) {
        # .elapsedTime("F")
        # v <- as.integer(v)
        # dim(v) <- dimv
        # storage.mode(v) <- "integer"
         vmode <- "numeric"
         res$con$datatype <- 4L
        # .elapsedTime("G")
      }
   }
  # .elapsedTime("as.ursa -- before")
  # res <- as.ursa(v) ## RECURSIVE
  # res <- ursa_new(v)
  # .elapsedTime("as.ursa -- after")
   if (isCat) {
      ursa_colortable(res) <- ct
      class(res$value) <- "ursaCategory"
   }
   bname <- obj$description
   if (any(nchar(bname)>0)) {
      names(res) <- gsub("\\t","",bname) ## patch for ENVI 'band name'
   }
   else {
      patt <- "^Band_(\\d+)=(.+)$"
      j <- grep(patt,obj$meta)
      ind <- as.integer(gsub(patt,"\\1",obj$meta[j]))
      bname <- gsub(patt,"\\2",obj$meta[j])
      bname[ind] <- bname
      if (!length(bname))
         bname <- paste("Band",seq(bands))
      names(res) <- bname
   }
   res$dim <- as.integer(c(columns*rows,bands))
   if (!is.na(md$nodata[1])) {
      if (vmode!=mode(md$nodata))
         mode(md$nodata) <- vmode
      ignorevalue(res) <- unique(md$nodata)
   }
   mode(res$value) <- vmode
   res
}
'.read_stars' <- function(fname) {
   if (!requireNamespace("sf",quietly=.isPackageInUse()))
      stop("Package 'sf' is required for this operation")
   a <- sf::gdal_read(fname,read_data=FALSE)
   columns <- a$cols[2]
   rows <- a$rows[2]
   bands <- a$bands[2]
  # patt <- "^Band_(\\d+)=(.+)$"
   patt <- "^Band_(\\d+)=\\t*(.+)$"
   bname <- grep(patt,a$meta,value=TRUE)
   b1 <- .grep(patt,a$meta,value=TRUE)
   bname <- .gsub(patt,"\\2",b1)
   bname[as.integer(.gsub(patt,"\\1",b1))] <- bname
   if (all(is.na(a$geotransform))) {
      resx <- 1
      resy <- 1
      minx <- 0
      miny <- 0
      maxx <- columns
      maxy <- rows
   }
   else {
      resx <- a$geotransform[2]
      resy <- -a$geotransform[6]
      minx <- a$geotransform[1]
      maxy <- a$geotransform[4]
      maxx <- minx+columns*resx
      miny <- maxy-rows*resy
   }
   g1 <- regrid(minx=minx,maxx=maxx,miny=miny,maxy=maxy,columns=columns,rows=rows
               ,crs=sf::st_crs(a$crs)$proj4string)
   session_grid(g1)
   res <- ursa(attr(sf::gdal_read(fname,read_data=TRUE),"data"),flip=TRUE)
   if (length(bname)==length(res))
      names(res) <- bname
   res
}
'as_stars' <- function(obj) {
   if (inherits(obj,"stars"))
      return(obj)
   if (!inherits(obj,"ursaRaster"))
      return(NULL)
   g <- ursa_grid(obj)
   crs <- ursa_crs(obj)
   if ((FALSE)&&(requireNamespace("sf",quietly=.isPackageInUse())))
      crs <- sf::st_crs(crs)
   md <- list(x=NULL
             ,y=NULL
             ,band=NULL
             )
   md$x <- list(from=1L
               ,to=g$columns
               ,offset=g$minx
               ,delta=g$resx
               ,refsys=crs
               ,point=NA
               ,values=NULL
               )
   md$y <- list(from=1L
               ,to=g$rows
               ,offset=g$maxy
               ,delta=-g$resy
               ,refsys=crs
               ,point=NA
               ,values=NULL
               )
   md$band <- list(from=1L
                  ,to=unname(length(obj))
                  ,offset=NA_real_
                  ,delta=NA_real_
                  ,refsys=NA_character_
                  ,point=NA,values=names(obj)
                  )
   class(md$x) <- class(md$y) <- class(md$band) <- "dimension"
   band <- list(affine=c(0,0)
               ,dimensions=c("x","y")
               ,curvilinear=FALSE
               ,blocksizes=NULL
               )
   class(band) <- "stars_raster"
   attr(md,"raster") <- band
   class(md) <- "dimensions"
   ret <- list(imported=as.array(obj,flip=TRUE,permute=FALSE))
   if (T & length(ct <- ursa_colortable(obj))>0) {
      dima <- dim(ret[[1]])[-3]
      ret[[1]] <- factor(ret[[1]],levels=seq_along(ct)-0L,labels=names(ct))
      dim(ret[[1]]) <- dima
      attr(ret[[1]],"colors") <- unclass(unname(ct))
      attr(ret[[1]],"exclude") <- rep(FALSE,length(ct))
      md$band <- NULL
   }
   attr(ret,"dimensions") <- md
  # attr(ret,"geotransform") <- c(g$minx,g$resx,0,g$maxy,-g$resy,0)
   class(ret) <- "stars"
   ret
}
# 'as.stars' <- function(obj) as_stars(obj=obj) ## if 'stars' is class, then 'as' is function
