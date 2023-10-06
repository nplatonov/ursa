# 'ursa_write' <- function(...) .syn('.write_gdal',2,...)
'ursa_write' <- function(obj,fname) { ## proposed: compress=TRUE for DEFLATE, ZSTD, etc
   if (!.lgrep("\\..+$",basename(fname))) {
      return(write_envi(obj,fname))
   }
  # stop("B")
   if (.lgrep("\\.zip$",basename(fname))) {
      aname <- paste0(names(obj),".tif")
      td <- file.path(tempdir(),basename(.maketmp()))
      dir.create(td)
      wd <- setwd(td)
      for (i in seq(obj)) {
         ursa_write(obj[i],aname[i]) ## RECURSIVE!!!
        # write_gdal(obj[i],aname[i])
      }
      if (!.is.colortable(obj))
         file.remove(dir(pattern="\\.aux\\.xml$"))
      zname <- file.path(wd,fname)
      if (file.exists(zname))
         file.remove(zname)
      utils::zip(zname,dir(),"-qmj9")
      setwd(wd)
      return(invisible(integer()))
   }
   if ((FALSE)&&(.lgrep("\\.(tif|img)$",basename(fname)))&&(nchar(Sys.which("gdal_translate")))) {
     # print("interim ENVI, then system GDAL")
      ftmp <- .maketmp()
      ret <- write_envi(obj,paste0(ftmp,"."))
      pr <- ifelse(ret %in% c(1L,2L,3L,11L,12L,13L),2L,3L)
      fpath <- dirname(fname)
      if (!dir.exists(fpath))
         dir.create(fpath,recursive=TRUE)
      proj_lib <- Sys.getenv("PROJ_LIB")
      Sys.setenv(PROJ_LIB=file.path(dirname(dirname(Sys.which("gdal_translate")))
                                   ,"share/proj"))
      if (.lgrep("\\.(tif)$",basename(fname)))
         system2("gdal_translate",c("-q","-of","GTiff"
                                   ,"-co",.dQuote(paste0("COMPRESS=",c("DEFLATE","ZSTD")[1]))
                                   ,"-co",.dQuote(paste0("PREDICTOR=",pr))
                                   ,"-co",.dQuote("ZSTD_LEVEL=9")
                                   ,"-co",.dQuote("ZLEVEL=9")
                                   ,"-co",.dQuote("TILED=NO")
                                   ,"-co",.dQuote(paste0("INTERLEAVE="
                                                        ,ifelse(length(obj)<2,"PIXEL","BAND")))
                                   ,.dQuote(ftmp),.dQuote(fname)))
      else if (.lgrep("\\.(img)$",basename(fname)))
         system2("gdal_translate",c("-q","-of","HFA"
                                   ,"-co",.dQuote("COMPRESSED=YES")
                                   ,.dQuote(ftmp),.dQuote(fname)))
      Sys.setenv(PROJ_LIB=proj_lib)
      envi_remove(ftmp)
      return(invisible(ret))
   }
   ##~ if (("sf" %in% loadedNamespaces())&&
                     ##~ (requireNamespace("stars",quietly=.isPackageInUse()))) {
      ##~ ret <- .write_sfgdal(obj,fname)
   ##~ }
   if ((!"sf" %in% loadedNamespaces())&&(isTRUE(getOption("ursaForceSF"))))
      requireNamespace("sf",quietly=.isPackageInUse())
   ftmp <- .maketmp()
   ret <- write_envi(obj,paste0(ftmp,"."))
   ret2 <- .envi2gdal(src=ftmp,dst=fname,datatype=ret,bands=length(obj))
   envi_remove(ftmp)
   if (ret==ret2)
      return(invisible(ret))
  # stop("Failed to write raster file ",dQuote(fname))
   return(write_gdal(obj=obj,fname=fname))
}
'.envi2gdal' <- function(src,dst,datatype,bands) {
   fpath <- dirname(dst)
   if (!dir.exists(fpath))
      dir.create(fpath,recursive=TRUE)
   if (file.exists(dst))
      file.remove(dst)
   op <- character()
   if (.lgrep("\\.(tif(f)*)$",basename(dst))) {
      pr <- ifelse(datatype %in% c(4L,5L),3L,2L)
      op <- c("-of","GTiff"
             ,"-co",paste0("COMPRESS=",c("DEFLATE","ZSTD")[1])
             ,"-co",paste0("PREDICTOR=",pr)
             ,"-co",paste0("ZSTD_LEVEL=9")
             ,"-co",paste0("ZLEVEL=9")
             ,"-co",paste0("TILED=NO")
             ,"-co",paste0("INTERLEAVE="
                                  ,ifelse(bands<2,"PIXEL","BAND"))
             )
   }
   else if (.lgrep("\\.(img|hfa)$",basename(dst))) {
      op <- c("-of","HFA"
             ,"-co",paste("COMPRESSED=YES")
             )
   }
   else {
      return(invisible(-98L))
   }
  # print(paste(op,collapse=" "))
   gd <- sf::gdal_utils(util="translate"
                       ,source=src
                       ,destination=dst
                       ,quiet=TRUE
                       ,options=op
                       )
   if (file.exists(dst))
      return(invisible(datatype))
   invisible(-97L)   
}
'write_gdal' <- function (obj,...) {
   if (.isPackageInUse()) {
      res <- try(.write_sfgdal(obj,...))
     # ret <- .try(res <- .write_sfgdal(obj,...))
      if (!inherits(res,"try-error"))
         return(invisible(res))
      message(as.character(res))
      warning("File creation is failed; writting is failed")
      return(invisible(NULL))
   }
   res <- create_gdal(obj,...)
   if (is.null(res))
      return(invisible(-99L))
   res[] <- obj
   close(res)
   return(invisible(res$con$datatype))
}
'.write_sfgdal' <- function(obj,fname,driver) {
   if ((!"sf" %in% loadedNamespaces())&&(T | isTRUE(getOption("ursaForceSF"))))
      requireNamespace("sf",quietly=.isPackageInUse())
   if (!requireNamespace("stars",quietly=.isPackageInUse()))
      warning("Package `stars` is required for raster writting")
   datatype <- .optimal.datatype(obj)
   nodata <- ignorevalue(obj)
   dtName <- switch(as.character(datatype)
                   ,'1'="Byte",'2'="Int16",'4'="Float32"
                   ,'11'="Int8",'12'="UInt16",'13'="UInt32",'3'="Int32"
                   ,'5'="Float64",stop("cannot recognize datatype"))
   if (missing(driver)) {
      driver <- NULL
      bname <- basename(fname)
      if (.lgrep("\\.tif(f)*$",bname))
         driver <- "GTiff"
      else if (.lgrep("\\.img$",bname))
         driver <- "HFA" # https://gdal.org/frmt_hfa.html
      else if (.lgrep("\\.png$",bname))
         driver <- "PNG"
      else if (.lgrep("\\.jp(e)*g$",bname))
         driver <- "JPEG"
      else if (.lgrep("\\.bmp$",bname))
         driver <- "BMP"
     # else if (.lgrep("\\.sdat$",fname))
     #    driver <- "SAGA"
      if (is.null(driver))
         driver <- "ENVI"
   }
   if (driver=="GTiff") {
      opt <- c("COMPRESS=DEFLATE"
              ,paste0("PREDICTOR=",ifelse(datatype %in% c(4,5),"3","2"))
              ,paste0("INTERLEAVE=",ifelse(length(obj)==1,"PIXEL","BAND"))
              ,"TILED=NO"
              )
   }
   else
      opt <- character()
   ret <- sf::gdal_write(as_stars(obj),driver=driver
                 ,file=fname,type=dtName,NA_value=nodata,options=opt
                 ,geotransform=with(ursa_grid(obj),c(minx,resx,0,maxy,0,-resy))
                 )
   return(invisible(datatype))
}
