'ursa_read' <- function(fname,verbose=FALSE) { ## ,resetGrid=TRUE
   if (envi_exists(fname)) {
      return(read_envi(fname)) # ,resetGrid=resetGrid
   }
   if (!.lgrep("\\.zip$",fname)) {
      return(read_gdal(fname=fname,verbose=verbose)) ## ,resetGrid=resetGrid
     # return(.read_gdal(fname=fname,verbose=verbose))
   }
   list1 <- unzip(fname,exdir=tempdir());on.exit(file.remove(list1))
   ind <- .grep("\\.tif(f)*$",list1)
   if (length(ind)) {
      aname <- .gsub("\\.tif(f)*","",basename(list1[ind]))
      if (TRUE) {
         res <- vector("list",length(aname))
         names(res) <- aname
         res <- lapply(list1[ind],.read_gdal,verbose=verbose)
         names(res) <- aname
         g <- lapply(res,ursa_grid)
         if (all(sapply(head(g,-1),function(g2) all.equal(g[[1]],g2))))
            res <- as.ursa(res)
         return(res)
      }
      for (i in sample(seq_along(aname))) {
         a <- .read_gdal(fname=list1[ind][i],verbose=verbose)
         if (!exists("res"))
            res <- ursa(bandname=aname)
         res[i] <- a
      }
      return(res)
   }
   NULL
}
'read_gdal' <- function(fname,resetGrid=TRUE,band=NULL
                       ,engine=c("native","rgdal","sf"),verbose=FALSE,...) { ## ,...
  # if (resetGrid)
  #    session_grid(NULL)
   if (!file.exists(fname)) {
      list1 <- dir(path=dirname(fname),pattern=paste0("^",basename(fname)),full.names=TRUE)
      list1 <- list1[.grep("\\.(tif|tiff|img|hfa)$",basename(list1))]
      if (length(list1)==1)
         fname <- list1
   }
   engine <- match.arg(engine)
   loaded <- loadedNamespaces() #.loaded()
   forceSF <- isTRUE(getOption("ursaForceSF"))
   if (accepted_changes <- TRUE) {
      if ((is.null(band))&&(engine %in% "native")) {
         if ((!forceSF)&&(("sp" %in% loaded)||("rgdal" %in% loaded)))
            isSF <- FALSE
         if ((forceSF)||("sf" %in% loaded))
            isSF <- TRUE
         else
            isSF <- FALSE
      }
      else if (engine %in% c("native","sf")[2]) { 
         isSF <- TRUE
      }
     # else if ((!is.null(band))||(engine %in% c("native","rgdal")[1:2])) {
     #    isSF <- FALSE
     # }
      else
         isSF <- FALSE
      ##~ else {
         ##~ loaded <- loadedNamespaces() #.loaded()
         ##~ if ("sf" %in% loaded)
            ##~ isSF <- TRUE
         ##~ else if (("sp" %in% loaded)||("rgdal" %in% loaded))
            ##~ isSF <- FALSE
         ##~ else
            ##~ isSF <- requireNamespace("sf",quietly=.isPackageInUse())
      ##~ }
   }
   else
      isSF <- FALSE
   if ((isSF)&&(!("sf" %in% loaded)))
      isSF <- requireNamespace("sf",quietly=.isPackageInUse())
   if (verbose)
      print(c(isSF=isSF))
   if (isSF) {
     # str(md <- sf::gdal_metadata(fname,parse=!FALSE))
     # str(ds <- sf::gdal_subdatasets(fname,name=TRUE))
      opW <- options(warn=ifelse(.isPackageInUse(),-1,1))
      res <- as_ursa(sf::gdal_read(fname))
      options(opW)
      if (!is.null(band))
         res <- res[band]
   }
   else {
      obj <- open_gdal(fname,verbose=verbose)
      if (is.null(obj))
         return(NULL)
      res <- if (!is.null(band)) obj[band] else obj[]
      close(obj)
   }
   if (T & length(grep("^\\d{8}\\.s1ab\\.1km\\.n\\.mos[13]d\\.jpg$",basename(fname)))) {
     ## patch to seaice.dk Sentinel-1 mosaic
      g0 <- ursa_grid(res)
      if ((g0$columns==4500L)&&(g0$rows==5500L)) {
         xy <- .project(c(-176.682000,61.327000),spatial_crs(3413))
         g1 <- .grid.skeleton()
         g1$resx <- g1$resy <- 1004.1
         g1$crs <- spatial_crs(3413)
         g1$columns <- g0$columns
         g1$rows <- g0$rows
         g1$minx <- round(xy[,1])-g1$resx/2
         g1$maxy <- round(xy[,2])#+g1$resy/2
         g1$maxx <- g1$minx+g1$resx*g1$columns
         g1$miny <- g1$maxy-g1$resy*g1$rows
         ursa_grid(res) <- g1
      }
   }
   if (resetGrid)
      session_grid(res)
   res
}
'.read_gdal' <- function(fname,fileout=NULL,verbose=!FALSE,...) {
   if (!is.character(fname))
      return(NULL)
  # suppressMessages(require("rgdal"))
   requireNamespace("rgdal",quietly=.isPackageInUse())
   if (verbose)
      .elapsedTime("rgdal has been loaded")
  # print(geterrmessage())
   op <- options(warn=0-!verbose)
   a <- try(rgdal::GDALinfo(fname,returnStats=FALSE,returnRAT=FALSE
                ,returnColorTable=TRUE,returnCategoryNames=TRUE))
   options(op)
   if (inherits(a,"try-error")) {
      fname <- normalizePath(fname)
      op <- options(warn=0-!verbose)
      a <- try(rgdal::GDALinfo(fname,returnStats=FALSE,returnRAT=FALSE
                   ,returnColorTable=TRUE,returnCategoryNames=TRUE))
      options(op)
      if (verbose)
         str(a)
      if (inherits(a,"try-error")) {
         if (verbose) {
            message("It looks like file ",.dQuote(fname)
                   ," is not found or not GDAL-recognized")
         }
         return(NULL)
      }
   }
   a1 <- as.numeric(a)
   g1 <- regrid()
   g1$rows <- as.integer(a1[1])
   g1$columns <- as.integer(a1[2])
   nl <- as.integer(a1[3])
   g1$minx <- a1[4]
   g1$miny <- a1[5]
   g1$resx <- a1[6]
   g1$resy <- a1[7]
   g1$maxx <- with(g1,minx+resx*columns)
   g1$maxy <- with(g1,miny+resy*rows)
   g1$crs <- attr(a,"projection")
   if (is.na(g1$crs))
      g1$crs <- ""
   b1 <- attr(a,"mdata")
   ln <- .gsub("^Band_\\d+=\\t*(.+)$","\\1",.grep("band",b1,value=TRUE))
   c1 <- attr(a,"df")
   hasndv <- unique(c1$hasNoDataValue)
   nodata <- unique(c1$NoDataValue)
   nodata <- if ((length(hasndv)==1)&&(length(nodata)==1)&&(hasndv)) nodata
             else NA
  # print(length(attr(a,"ColorTable")))
   ct <- attr(a,"ColorTable")
   if ((length(ct))&&(!is.null(ct[[1]]))) {
      ct <- ct[[1]]
      ca <- attr(a,"CATlist")
      if ((length(ca))&&(!is.null(ca[[1]]))) {
         nval <- ca[[1]]
         ct <- ct[seq(length(nval))]
      }
      else
         nval <- NULL #seq(length(ct))
      names(ct) <- nval
   }
   else
      ct <- character()
   class(ct) <- "ursaColorTable"
   session_grid(g1)
   dset <- methods::new("GDALReadOnlyDataset",fname)
   if (!length(ln)) {
      dima <- dim(dset)
      ln <- paste("Band",if (length(dima)==3) seq(dima[3]) else 1L)
   }
   if (!is.character(fileout)) {
      val <- rgdal::getRasterData(dset)
      dima <- dim(val)
      if (length(dima)==2)
         dim(val) <- c(dima,1L)
      val <- val[,rev(seq(dim(val)[2])),,drop=FALSE] ## added 20160330
      res <- as.ursa(value=val,bandname=ln,ignorevalue=nodata)
   }
   else {
      res <- create_envi(fileout,bandname=ln,ignorevalue=nodata,...)
      for (i in seq_along(ln))
      {
         res[i]$value[] <- rgdal::getRasterData(dset,band=i)
      }
   }
   rgdal::closeDataset(dset)
   res$colortable <- ct
   class(res$value) <- ifelse(length(ct),"ursaCategory","ursaNumeric")
   res
}
