# 'ursa_write' <- function(...) .syn('.write_gdal',2,...)
'ursa_write' <- function(obj,fname) {
   if (!.lgrep("\\..+$",basename(fname))) {
      return(write_envi(obj,fname))
   }
  # stop("B")
   if (.lgrep("\\.zip$",basename(fname))) {
      aname <- paste0(names(obj),".tif")
      td <- file.path(tempdir(),basename(.maketmp()))
      dir.create(td)
      wd <- setwd(td)
      for (i in seq(obj))
         write_gdal(obj[i],aname[i])
      if (!.is.colortable(obj))
         file.remove(dir(pattern="\\.aux\\.xml$"))
      zname <- file.path(wd,fname)
      if (file.exists(zname))
         file.remove(zname)
      utils::zip(zname,dir(),"-qmj9")
      setwd(wd)
      return(invisible(integer()))
   }
   if ((TRUE)&&(.lgrep("\\.(tif|img)$",basename(fname)))&&(nchar(Sys.which("gdal_translate")))) {
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
                                                        ,ifelse(length(obj)>=2,"PIXEL","BAND")))
                                   ,.dQuote(ftmp),.dQuote(fname)))
      else if (.lgrep("\\.(img)$",basename(fname)))
         system2("gdal_translate",c("-q","-of","HFA"
                                   ,"-co",.dQuote("COMPRESSED=YES")
                                   ,.dQuote(ftmp),.dQuote(fname)))
      Sys.setenv(PROJ_LIB=proj_lib)
      envi_remove(ftmp)
      return(invisible(ret))
   }
   return(write_gdal(obj=obj,fname=fname))
}
'write_gdal' <- function (obj,...) {
   requireNamespace("rgdal",quietly=.isPackageInUse())
   if (!length(obj$colortable)) {
      rgdal::setCPLConfigOption("GDAL_PAM_ENABLED","FALSE") ## doesnt work 20180327
   }
   res <- create_gdal(obj,...)
   if (is.null(res))
      return(invisible(-99L))
   res[] <- obj
   close(res)
   return(invisible(res$con$datatype))
}
