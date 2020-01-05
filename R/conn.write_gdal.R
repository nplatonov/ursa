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
   if ((TRUE)&&(.lgrep("\\.tif$",basename(fname)))&&(nchar(Sys.which("gdal_translate")))) {
      ftmp <- .maketmp()
      ret <- write_envi(obj,paste0(ftmp,"."))
      proj_lib <- Sys.getenv("PROJ_LIB")
      Sys.setenv(PROJ_LIB=file.path(dirname(dirname(Sys.which("gdal_translate")))
                                   ,"share/proj"))
      system2("gdal_translate",c("-q","-of","GTiff"
                                ,"-co",.dQuote("COMPRESS=DEFLATE")
                                ,"-co",.dQuote("PREDICTOR=2")
                                ,"-co",.dQuote("TILED=NO")
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
