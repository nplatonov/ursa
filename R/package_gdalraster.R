'.open_gdalraster' <- function(fname,engine="gdalraster",verbose=FALSE) {
   if (!is.character(fname))
      return(NULL)
   engine <- match.arg(engine)
   requireNamespace("gdalraster",quietly=.isPackageInUse())
   opW <- options(warn=0-!verbose,show.error.messages=TRUE)
   on.exit(options(opW))
   ds <- methods::new(gdalraster::GDALRaster,filename=fname,read_only=TRUE)
   # str(ds)
   if (verbose) {
      md <- list(ncols=ds$getRasterXSize()
                ,nrows=ds$getRasterYSize()
                ,bbox=ds$bbox()
                ,nodata=ds$getNoDataValue(band=1)
                ,dim=ds$dim()
                ,res=ds$res()
               # ,md=ds$getMetadata(band=1)
                ,transform=ds$getGeoTransform()
                ,crs=ds$getProjectionRef()
                ,datatype=ds$getDataTypeName(band=1)
               # ,info=ds$info()
               # ,jinfo=ds$infoAsJSON() |> jsonlite::fromJSON()
                ,md0=ds$getMetadata(band=0,domain="")
               # ,md1=ds$getMetadata(band=1,domain="")
               # ,mdi0=ds$getMetadataItem(band=0,domain="",mdi_name="")
                ,ct=ds$getColorTable(band=1) ## ver 1.4.1
                )
      str(md)
   }
   dima <- ds$dim()
   datatype <- ds$getDataTypeName(band=1)
   nodata <- ds$getNoDataValue(band=1)
   md0 <- ds$getMetadata(band=0,domain="")
   bbox <- ds$bbox()
   res <- ds$res()
   if (TRUE) {
      g1 <- regrid(bbox=bbox,res=res
                  ,crs=sf::st_crs(ds$getProjectionRef())$proj4string
                 # ,crs=ds$getProjectionRef()
                  )
   }
   else {
      g1 <- .grid.skeleton()
      g1$minx <- bbox[1]
      g1$maxx <- bbox[3]
      g1$miny <- bbox[2]
      g1$maxy <- bbox[4]
      g1$columns <- dima[1]
      g1$rows <- dima[2]
      g1$resx <- res[1]
      g1$resy <- res[2]
     # g1$crs=sf::st_crs(ds$getProjectionRef())$proj4string
      g1$crs=ds$getProjectionRef()
   }
  # session_grid(g1)
   res <- .raster.skeleton()
   res$dim <- c(prod(dima[1:2]),dima[3])
   con <- .con.skeleton()
   con$driver <- "GDALRASTER"
   con$samples <- g1$columns
   con$lines <- g1$rows
   con$bands <- dima[3]
   con$indexC <- seq(g1$columns)
   con$indexR <- seq(g1$rows)
   con$indexZ <- seq_len(dima[3])
   con$seek <- FALSE
   con$fname <- fname
   con$handle <- ds
   con$datatype <- switch(datatype,byte=1L,integer=2L,real=4L,float=4L
                                    ,Byte=1L,UInt8=1L,Int8=11
                                    ,Int16=2L,UInt16=12,UInt32=13,Int32=3
                                    ,Float32=4L,Float64=5L
                                    ,NA_integer_)
   res$con <- con
   ursa_grid(res) <- g1
   if (!is.null(datatype))
      ignorevalue(res) <- ifelse(con$datatype %in% c(1L,11L,2L,12L,3L,13L)
                                ,as.integer(nodata),nodata)
   ct <- NULL ## CURRENTLY UNEMPLEMENTED
   ursa_grid(res) <- g1
   patt <- "^Band_(\\d+)=(.+)$"
   j <- grep(patt,md0)
   bname <- gsub(patt,"\\2",md0[j])
  # bname <- gsub("^Band_\\d+=","",grep("^Band",md$md0,value=TRUE))
   if (!length(bname))
      names(res) <- paste("Band",seq_along(con$bands))
   else {
      ind <- as.integer(gsub(patt,"\\1",md0[j]))
      names(res)[ind] <- bname
   }
  # names(res) <- bname
   ursa_colortable(res) <- ct
  # if (!is.null(a$nodata_value))
  #    ignorevalue(res) <- a$nodata_value
   class(res$value) <- ifelse(length(ct),"ursaCategory","ursaNumeric")
   res
}
'.read_gdalraster' <- function(fname,resetGrid=TRUE,band=NULL
                       ,engine=c("gdalraster"),verbose=FALSE,...) { ## ,...
   engine <- match.arg(engine)
   requireNamespace("gdalraster",quietly=.isPackageInUse())
   a <- .open_gdalraster(fname,engine=engine,verbose=verbose)
   ds <- methods::new(gdalraster::GDALRaster,filename=fname,read_only=TRUE)
   if (verbose) {
      md <- list(ncols=ds$getRasterXSize()
                ,nrows=ds$getRasterYSize()
                ,bbox=ds$bbox()
                ,nodata=ds$getNoDataValue(band=1)
                ,dim=ds$dim()
                ,res=ds$res()
               # ,md=ds$getMetadata(band=1)
                ,transform=ds$getGeoTransform()
                ,crs=ds$getProjectionRef()
               # ,info=ds$info()
               # ,jinfo=ds$infoAsJSON() |> jsonlite::fromJSON()
                ,md0=ds$getMetadata(band=0,domain="")
               # ,md1=ds$getMetadata(band=1,domain="")
               # ,mdi0=ds$getMetadataItem(band=0,domain="",mdi_name="")
                ,ct=ds$getColorTable(band=1) ## ver 1.4.1
                )
      str(md)
   }
   dima <- ds$dim()
   nodata <- ds$getNoDataValue(band=1)
   md0 <- ds$getMetadata(band=0,domain="")
   g1 <- regrid(bbox=ds$bbox(),res=ds$res()
               ,crs=sf::st_crs(ds$getProjectionRef())$proj4string
              # ,crs=ds$getProjectionRef()
               )
   session_grid(g1)
   patt <- "^Band_(\\d+)=(.+)$"
   j <- grep(patt,md0)
   bname <- gsub(patt,"\\2",md0[j])
  # bname <- gsub("^Band_\\d+=","",grep("^Band",md$md0,value=TRUE))
   if (!length(bname))
      bname <- paste("Band",seq_along(dima[3]))
   else {
      ind <- as.integer(gsub(patt,"\\1",md0[j]))
      bname[ind] <- bname
   }
   out <- ursa_new(bandname=bname,nodata=nodata)
   if (verbose)
      cat("read")
   for (i in seq(out)) {
      if (verbose)
         cat(".")
     # print(i)
     # ursa_value(out)[,i] <- 
      a <- ds$read(band=i
                  ,xoff=0,yoff=0
                  ,xsize=dima[1],ysize=dima[2]
                  ,out_xsize=dima[1],out_ysize=dima[2]
                  )  
      out$value[,i] <- a
   }
   if (verbose)
      cat(" done!\n")
   out
}
