'.open_rgdal' <- function(fname,engine="sf",verbose=FALSE) {
   .rgdal_requireNamespace()
  # if (verbose)
  #    .elapsedTime("rgdal has been loaded")
   opW <- options(warn=0-!verbose,show.error.messages=TRUE) ## to prevent 'GeoTransform values not available'
   on.exit(options(opW))
   if (devel <- F) {
     # a <- vapour:::gdalinfo_internal(fname,json=TRUE,stats=FALSE)
     # a <- jsonlite::fromJSON(a)
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
  # names(a1) <- attr(a,"names")
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
   g1$crs <- .ursaCRS(g1$crs)
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
   con$driver <- "RGDAL"
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
'.shp.read' <- function(fname,reproject=TRUE,encoding="1251",resetGrid=FALSE
                       ,verbose=0L,...)
{
  ## b <- sf::st_read("gde-1-1-15.shp",quiet=TRUE)
  ## b <- sf::st_transform(b,ursa_proj(a))
  # print(fname)
   .rgdal_requireNamespace()
   if (resetGrid)
      reproject <- FALSE
  # require(methods)
   if (.lgrep("\\.zip$",basename(fname)))
      fname <- .gsub("\\.zip$","",fname)
   fpath <- dirname(fname)
   z <- file.path(fpath,paste0(.shp.layer(fname),".zip")) # ,.
   if (!file.exists(z))
      z <- file.path(fpath,paste0(.shp.layer(fname),".shp.zip"))
   if (file.exists(z))
   {
      a <- utils::unzip(z,exdir=fpath,junkpaths=TRUE)
      on.exit(file.remove(a))
   }
   if (verbose>1)
      .elapsedTime("readOGR:start")
   cpgname <- file.path(fpath,paste0(.shp.layer(fname),".cpg"))
   e_opt <- if (file.exists(cpgname)) readLines(cpgname,warn=FALSE) else ""
   i_opt <- if (grepl("UTF(-)*8",e_opt)) TRUE else FALSE
  # print(data.frame(e_opt=e_opt,i_opt=i_opt))
   opW <- options(warn=0)
   res <- rgdal::readOGR(.shp.file(fname),.shp.layer(fname),pointDropZ=TRUE
                        ,encoding=e_opt,use_iconv=i_opt
                        ,verbose=as.logical(verbose),...)
   options(opW)
   if (verbose>1)
      .elapsedTime("readOGR:finish")
   proj4 <- session_grid()$crs
   if ((reproject)&&(nchar(proj4))&&(!is.na(sp::proj4string(res)))) {
      if (verbose>1)
         .elapsedTime("spTransform:start")
      res <- sp::spTransform(res,proj4)
      if (verbose>1)
         .elapsedTime("spTransform:finish")
   }
   if (resetGrid)
      session_grid(NULL)
   res
}
'.shp.write' <- function(obj,fname,compress=FALSE,zip=NULL)
{
   requireNamespace("methods",quietly=.isPackageInUse()) 
  ## Error: inherits(obj, "Spatial") is not TRUE
  # require("methods") 
   .rgdal_requireNamespace()
  # suppressMessages(require(rgdal)) ## should be already loaded
   if (!is.null(zip))
      compress <- zip
   fpath <- dirname(fname)
   layer <- .shp.layer(fname)
   suppressWarnings({
      first <- TRUE
      op <- options(warn=2)
      repeat({
         if (!file.exists(.shp.file(fname)))
            break
         if (file.remove(.shp.file(fname)))
            break
         if (first) {
            cat(paste("Waiting for permitted writting",.sQuote(basename(fname))))
            first <- FALSE
         }
         cat(".")
         Sys.sleep(1)
      })
      if (!first)
         cat(" ok!\n")
      options(op)
   })
   opW <- options(warn=0)
   rgdal::writeOGR(obj,fpath,layer,driver="ESRI Shapefile"
                 # ,encoding=encoding
                  ,overwrite=TRUE)
   options(opW)
   writeLines("1251",file.path(fpath,paste0(layer,".cpg")))
   if (!compress)
      return(NULL)
   f <- .dir(path=dirname(fname)
            ,pattern=paste0("^",.shp.layer(fname),"\\.(cpg|dbf|prj|qpj|shp|shx)$")
            ,full.names=TRUE)
   z <- paste0(.shp.file(fname),".zip")
   opW <- options(warn=-1)
   first <- TRUE
   while(file.exists(z)) {
      if (file.remove(z))
         break
      if (first) {
         cat(paste("Waiting for deleting",.sQuote(z)))
         first <- FALSE
      }
      cat(".")
      Sys.sleep(1)
   }
   if (!first)
      cat(" ok!\n")
   options(opW)
   utils::zip(z,f,flags="-qm9j") ## verbose output ## 'myzip(z,f,keys="-m -9 -j")'
}
'.rgdal_close_Transient' <- function(con,bname) {
   .rgdal_requireNamespace()
   dr <- rgdal::getDriverName(rgdal::getDriver(con$handle))
   opt <- con$compress
   if (length(opt))
      opt <- opt[[.grep("options",names(opt))]]
   if (!length(opt)) {
      op <- NULL
      if (dr=="GTiff")
         op=c(paste0("COMPRESS=",c("DEFLATE","ZSTD","LZW")[1])
             ,paste0("PREDICTOR=",ifelse(con$mode=="numeric",3,2))
             ,"TILED=NO"
             ,"ZLEVEL=9"
             ,"ZSTD_LEVEL=9"
             ,paste0("INTERLEAVE=",switch(con$interleave,bil="PIXEL","BAND")))
      else if (dr=="HFA") {
         op=c("COMPRESSED=YES")
      }
      else if (dr=="ENVI") {
        # print(con$interleave)
         op <- paste0("INTERLEAVE=",toupper(con$interleave))
      }
   }
   else {
      if (is.character(opt)) {
         oname <- names(opt)
         if (is.null(oname))
            op <- do.call(c,strsplit(opt,split="\\s+"))
         else
            op <- paste0(oname,"=",opt)
      }
      else if (is.list(opt))
         op <- paste0(names(opt),"=",sapply(opt,\(x) x))
      else
         op <- character()
   }
   rgdal::saveDataset(con$handle,con$fname,options=op)
  # rgdal::closeDataset(con$handle)
   rgdal::GDAL.close(con$handle)
   con$handle <- NA
  # if (FALSE) {
   standardname <- paste("Band",seq_along(bname))
   if ((TRUE)&&(!is.na(bname[1]))&&(!identical(standardname,bname))) {
      metafile <- paste0(con$fname,".aux.xml")
      if (!is.na(con$posZ[1]))
         bname <- bname[con$posZ]
      added3 <- rep("",length(bname))
         for (i in seq_along(bname))
            added3[i] <- paste0("    <MDI key=",.dQuote(paste0("Band_",i))
                              ,">",bname[i],"</MDI>")
      added2 <- c("  <Metadata>",added3,"  </Metadata>")
      added1 <- c("<PAMDataset>",added2,"</PAMDataset>")
      if (!file.exists(metafile)) {
         Fmeta <- file(metafile,"wt")
         writeLines(added1,Fmeta)
         close(Fmeta)
      }
      else {
         meta <- readLines(metafile)
        # i1 <- .grep("<Metadata>",meta)
         ##~ i2 <- .grep("</Metadata>",meta)
         ##~ i2 <- i2[i2>i1][1]
         i3 <- .grep("</PAMDataset>",meta)
         metaBefore <- meta[1:(i3-1)]
         metaAfter <- meta[i3:length(meta)]
         writeLines(c(metaBefore,added2,metaAfter),metafile)
        # op <- options(warn=0)
        # warning("Band names was not written. TODO insert lines to *.aux.xml")
        # options(op)
      }
   }
   invisible(NULL)
}
'.rgdal_close_ReadOnly' <- function(con) {
   .rgdal_requireNamespace()
  # print(class(con$handle))
  # rgdal::closeDataset(con$handle)
   rgdal::GDAL.close(con$handle)
}
'.read_gdal' <- function(fname,fileout=NULL,verbose=!FALSE,...) {
   if (!is.character(fname))
      return(NULL)
  # suppressMessages(require("rgdal"))
   .rgdal_requireNamespace()
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
'.rgdal_goodies' <- function() {
  # if (!length(obj$colortable)) {
  #    rgdal::setCPLConfigOption("GDAL_PAM_ENABLED","FALSE") ## doesnt work 20180327
  # }
   invisible(NULL)
}
'.rgdal_prepare_con' <- function(x) {
   .rgdal_requireNamespace()
   opW <- options(warn=ifelse(.isPackageInUse(),0,1)) # 
   rgdal::GDALcall(x$con$handle,"SetProject",x$grid$crs)
   options(opW)
   rgdal::GDALcall(x$con$handle,"SetGeoTransform"
                               ,with(x$grid,c(minx,resx,0,maxy,0,-resy)))
   nodata <- ursa_nodata(x)
   ct <- ursa_colortable(x)
   isCT <- (nband(x)==1)&&(length(ct)>0)
   hasColor <- (isCT)&&(all(!is.na(ct)))
   hasNames <- (isCT)&&(all(!is.na(names(ct))))
  # print(c(has_nodata=!is.na(nodata),isCT=isCT,hasColor=hasColor,hasNames=hasNames))
   if (any(!is.na(nodata),isCT,hasColor,hasNames)) {
      for (i in seq(nband(x))) {
         bset <- methods::new("GDALRasterBand",x$con$handle,i)
         if (!is.na(nodata))
            rgdal::GDALcall(bset,"SetNoDataValue",nodata)
         if (hasColor)
            rgdal::GDALcall(bset,"SetRasterColorTable",as.character(ct))
         if (hasNames)
            rgdal::GDALcall(bset,"SetCategoryNames",names(ct))
        # if (isCT)
        #    rgdal::putRasterData(dset,as.array(colorize(obj[i]),flip=TRUE,drop=TRUE),band=i)
        # else
        #    rgdal::putRasterData(dset,as.array(obj[i],flip=TRUE,drop=TRUE),band=i)
      }
   }
   invisible(NULL)
}
'.rgdal_showWKT' <- function(...) {
   if (isTRUE(getOption("ursaPackageInUse")))
      .DeadEnd()
   rgdal::showWKT(...)
}
'.rgdal_showP4' <- function(...) {
   if (isTRUE(getOption("ursaPackageInUse")))
      .DeadEnd()
   rgdal::showP4(...)
}
'.rgdal_writeOGR' <- function(...) {
   if (isTRUE(getOption("ursaPackageInUse")))
      .DeadEnd()
   rgdal::writeOGR(...)
}
'.rgdal_getRasterData' <- function(...) {
   if (isTRUE(getOption("ursaPackageInUse")))
      .DeadEnd()
   rgdal::getRasterData(...)
}
'.rgdal_putRasterData' <- function(...) {
   if (isTRUE(getOption("ursaPackageInUse")))
      .DeadEnd()
   rgdal::putRasterData(...)
}
'.rgdal_CRSargs' <- function(...) {
   if (isTRUE(getOption("ursaPackageInUse")))
      .DeadEnd()
   rgdal::CRSargs(...)
}
'.rgdal_ogrListLayers' <- function(...) {
   if (isTRUE(getOption("ursaPackageInUse")))
      .DeadEnd()
   rgdal::ogrListLayers(...)
}
'.rgdal_readOGR' <- function(...) {
   if (isTRUE(getOption("ursaPackageInUse")))
      .DeadEnd()
   rgdal::readOGR(...)
}
'.rgdal_ogrInfo' <- function(...) {
   if (isTRUE(getOption("ursaPackageInUse")))
      .DeadEnd()
   rgdal::ogrInfo(...)
}
'.rgdal_CRSargs' <- function(...) {
   if (isTRUE(getOption("ursaPackageInUse")))
      .DeadEnd()
   rgdal::CRSargs(...)
}
'.rgdal_project' <- function(...) {
   if (isTRUE(getOption("ursaPackageInUse")))
      .DeadEnd()
   rgdal::project(...)
}
'.rgdal_loadedNamespaces' <- function(...) {
   if (isTRUE(getOption("ursaPackageInUse")))
      .DeadEnd()
  "rgdal" %in% loadedNamespaces()
}
'.rgdal_requireNamespace' <- function(...) {
   if (isTRUE(getOption("ursaPackageInUse")))
      .DeadEnd()
   requireNamespace("rgdal",quietly=.isPackageInUse())
}
'.rgeos_requireNamespace' <- function(...) {
   if (isTRUE(getOption("ursaPackageInUse")))
      .DeadEnd()
   requireNamespace("rgeos",quietly=.isPackageInUse())
}
'.rgeos_readWKT' <- function(...) {
   if (isTRUE(getOption("ursaPackageInUse")))
      .DeadEnd()
   rgeos::readWKT(...)
}
'.rgeos_gLength' <- function(...) {
   if (isTRUE(getOption("ursaPackageInUse")))
      .DeadEnd()
   rgeos::gLength(...)
}
'.rgeos_gIntersection' <- function(...) {
   if (isTRUE(getOption("ursaPackageInUse")))
      .DeadEnd()
   rgeos::gIntersection(...)
}
'.rgeos_gDifference' <- function(...) {
   if (isTRUE(getOption("ursaPackageInUse")))
      .DeadEnd()
   rgeos::gDifference(...)
}
'.rgeos_gSymdifference' <- function(...) {
   if (isTRUE(getOption("ursaPackageInUse")))
      .DeadEnd()
   .rgeos::gSymdifference(...)
}
'.rgeos_gBuffer' <- function(...) {
   if (isTRUE(getOption("ursaPackageInUse")))
      .DeadEnd()
   rgeos::gBuffer(...)
}
'.rgeos_gUnaryUnion' <- function(...) {
   if (isTRUE(getOption("ursaPackageInUse")))
      .DeadEnd()
   rgeos::gUnaryUnion(...)
}
'.rgeos_gUnion' <- function(...) {
   if (isTRUE(getOption("ursaPackageInUse")))
      .DeadEnd()
   rgeos::gUnion(...)
}
'.rgeos_gSimplify' <- function(...) {
   if (isTRUE(getOption("ursaPackageInUse")))
      .DeadEnd()
   rgeos::gSimplify(...)
}
'.rgeos_gIsValid' <- function(...) {
   if (isTRUE(getOption("ursaPackageInUse")))
      .DeadEnd()
   rgeos::gIsValid(...)
}
