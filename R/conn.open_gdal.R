'ursa_open' <- function(fname,verbose=FALSE) open_gdal(fname=fname,verbose=verbose)
'open_gdal' <- function(fname,verbose=FALSE) {
  ## 20170116 removed '...' argument
   if (!is.character(fname))
      return(NULL)
   requireNamespace("rgdal",quietly=.isPackageInUse())
  # if (verbose)
  #    .elapsedTime("rgdal has been loaded")
   opW <- options(warn=0-!verbose,show.error.messages=TRUE) ## to prevent 'GeoTransform values not available'
   on.exit(options(opW))
   if (devel <- T) {
      a <- vapour:::gdalinfo_internal(fname,json=TRUE,stats=FALSE)
      a <- jsonlite::fromJSON(a)
   }
   a <- try(rgdal::GDALinfo(fname,returnStats=FALSE,returnRAT=FALSE
                ,returnColorTable=TRUE,returnCategoryNames=TRUE),silent=TRUE)
   if (inherits(a,"try-error")) {
      fname <- normalizePath(fname)
      a <- try(rgdal::GDALinfo(fname,returnStats=FALSE,returnRAT=FALSE
                   ,returnColorTable=TRUE,returnCategoryNames=TRUE),silent=TRUE)
      if (inherits(a,"try-error")) {
         if ((TRUE)||(!.isPackageInUse()))
            cat(geterrmessage())
        # * using R version 3.4.0 Patched (2017-05-16 r72684)
        # * using platform: x86_64-pc-linux-gnu (64-bit)
        # > ### Name: open_gdal
        # > ### Title: Open GDAL file
        # > ### Aliases: open_gdal
        # > ### Keywords: connection
        # > 
        # > ### ** Examples
        # > 
        # > session_grid(NULL)
        # > fname1 <- system.file("pictures/cea.tif",package="rgdal")
        # > message(fname1)
        # /tmp/RtmpdrKt9J/RLIBS_310d3548ed74/rgdal/pictures/cea.tif
        # > a1 <- open_gdal(fname1)
        # > print(a1)
        # NULL
        # > print(a1[])
        # NULL
        # > close(a1)
        # Error in UseMethod("close") : 
        #   no applicable method for 'close' applied to an object of class "NULL"
        # Calls: close
        ## 20170529 patch for failure with 'rgdal' at r-forge
         if (.Platform$OS.type=="unix"){
            message(paste("Unable to open GDAL file. Failure for"
                         ,"R-forge buildig machine (Unix OS) since May 2017"))
            if ((!.lgrep("\\.(rds)$",fname))&&(file.exists(fname))) {## 20170529
               return(ursa_new())
            }
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
   b1 <- .grep("band",attr(a,"mdata"),value=TRUE)
   patt <- "^Band_(\\d+)=\\t*(.+)$"
   bname <- .gsub(patt,"\\2",b1)
   bname[as.integer(.gsub(patt,"\\1",b1))] <- bname
   c1 <- attr(a,"df")
   hasndv <- unique(c1$hasNoDataValue)
   nodata <- unique(c1$NoDataValue)
   nodata <- if ((length(hasndv)==1)&&(length(nodata)==1)&&(hasndv)) nodata
             else NA
  # print(length(attr(a,"ColorTable")))
   ct <- attr(a,"ColorTable")
   ca <- attr(a,"CATlist")
   if ((length(ct))&&(!is.null(ct[[1]]))) {
      ct <- ct[[1]]
      if ((length(ca))&&(!is.null(ca[[1]]))) {
         nval <- ca[[1]]
         ct <- ct[seq(length(nval))]
      }
      else
         nval <- NULL #seq(length(ct))
      names(ct) <- nval
   }
   else if ((length(ca))&&(!is.null(ca[[1]]))) {
      nval <- ca[[1]]
      ct <- rep(NA,length(nval))
      names(ct) <- nval
   }
   else
      ct <- character()
   class(ct) <- "ursaColorTable"
   dset <- methods::new("GDALReadOnlyDataset",fname)
   dima <- dim(dset)
   if (length(dima)==2)
      dima <- c(dima,1L)
   if (!length(bname)) {
      bname <- paste("Band",if (length(dima)==3) seq(dima[3]) else 1L)
   }
   session_grid(g1)
   res <- .raster.skeleton()
   res$dim <- c(dima[1]*dima[2],dima[3])
   con <- .con.skeleton()
   con$driver <- "GDAL"
   con$samples <- g1$columns
   con$lines <- g1$rows
   con$bands <- length(bname)
   con$indexC <- seq(g1$columns)
   con$indexR <- seq(g1$rows)
   con$indexZ <- seq_along(bname)
   con$seek <- FALSE
   con$fname <- fname
   con$handle <- dset
   res$con <- con
   ursa_grid(res) <- g1
   ursa_colortable(res) <- ct
   class(res$value) <- ifelse(length(ct),"ursaCategory","ursaNumeric")
   ursa_nodata(res) <- nodata
   names(res) <- bname
   res
}
