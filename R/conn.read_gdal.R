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
         res <- lapply(list1[ind],read_gdal,verbose=verbose) ## '.read_gdal'
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
                       ,engine=c("native","sf","gdalraster","vapour")
                       ,verbose=FALSE,...) { ## ,...
  # if (resetGrid)
  #    session_grid(NULL)
   engList <- as.character(as.list(match.fun("read_gdal"))[["engine"]])[-1]
   if (length(engine)<length(engList)) {
      if (!.isPackageInUse()) {
         engList <- c(engList,"rgdal")
      }
   }
   engine <- match.arg(engine,engList)
   fname <- gsub("\\.$","",fname)
   if (!file.exists(fname)) {
      list1 <- dir(path=dirname(fname),pattern=paste0("^",basename(fname)),full.names=TRUE)
      list1 <- list1[.grep("\\.(tif|tiff|img|hfa)$",basename(list1))]
      if (length(list1)==1)
         fname <- list1
   }
   if ((engine=="vapour")&&(requireNamespace("vapour",quite=!.isPackageInUse()))) {
      return(.read_vapour(fname,resetGrid=resetGrid,band=band
                         ,engine=engine,verbose=verbose))
   }
   if ((engine=="gdalraster")&&(requireNamespace("gdalraster",quite=!.isPackageInUse()))) {
      return(.read_gdalraster(fname,resetGrid=resetGrid,band=band
                         ,engine=engine,verbose=verbose))
   }
   if (engine %in% c("vapour","gdalraster"))
      engine <- "native"
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
      if (forcedNoData <- TRUE) {
         gi <- sf::gdal_utils("info",fname,quiet=!FALSE)
         gi <- strsplit(gi,split="\\n")[[1]]
         gi <- grep("NoData Value",gi,value=TRUE)
         if (length(gi)>0) {
            nodata <- gsub("^.*=(\\s*(\\S+))$","\\1",gi)
            if (typeof(ursa_value(res))=="integer")
               ignorevalue(res) <- as.integer(unique(nodata))
            else
               ignorevalue(res) <- as.numeric(unique(nodata))
         }
      }
      if (!is.null(band))
         res <- res[band]
   }
   else {
      obj <- open_gdal(fname,engine=engine,verbose=verbose)
      if (is.null(obj))
         return(NULL)
      res <- if (!is.null(band)) obj[band] else obj[]
      close(obj)
   }
   if (T & length(grep("^(\\d{8}\\.s1ab\\.1km\\.n\\.mos[13]d|.+sentinel1-n-[13]daymos)\\.jpg$"
                      ,basename(fname)))) {
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
