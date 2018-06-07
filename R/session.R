'session_grid' <- function(obj)
{
   if (missing(obj)) ## 'Extract'
   {
      ref <- getOption("ursaSessionGrid")
      if ((is.null(ref))||(!.is.grid(ref)))
      {
        # fname <- system.file("template","default.hdr",package="ursa")
         fname <- file.path(getOption("ursaRequisite"),"template.hdr")
         if (file.exists(fname)) {
            fname <- system.file("requisite/template.hdr",package="ursa")
            if (file.exists(fname))
               ref <- .read.hdr(fname)$grid
            else
               ref <- .read.hdr("default")$grid ## read.idr
         }
         options(ursaSessionGrid=ref)
      }
     # return(invisible(ref))
      return(ref)
   }
  # above - 'Extract' (visible), below - 'Replace' (invisible)
   if (is.null(obj))
      return(options(ursaSessionGrid=NULL))
   if (.is.grid(obj)) {
      options(ursaSessionGrid=obj)
      return(invisible(obj))
   }
   if (.is.ursa_stack(obj))
      obj <- obj[[1]]
   if (is.ursa(obj)) {
      options(ursaSessionGrid=obj$grid)
      return(invisible(obj$grid))
   }
   if (is_spatial(obj)) {
      bbox <- spatial_bbox(obj)
      nc <- (bbox["xmax"]-bbox["xmin"])
      nr <- (bbox["ymax"]-bbox["ymin"])
      res <- max(nc,nr)/640
      p <- pretty(res)
      res <- p[which.min(abs(res-p))]
      g1 <- regrid(setbound=unname(bbox),proj4=spatial_proj4(obj),res=res)
      return(session_grid(g1))
   }
   if ((!envi_exists(obj))&&(nchar(Sys.getenv("R_IDRISI")))&&(exists("read.idr"))) {
      g1 <- do.call("read.idr",list((obj)))$grid
      options(ursaSessionGrid=g1)
      return(invisible(g1))
   }
   if (is.character(obj)) {
      a <- try(open_envi(obj,resetGrid=TRUE,decompress=FALSE))
      if (inherits(a,"try-error")) {
         a <- open_gdal(obj)
      }
      g1 <- a$grid
      close(a)
      if (!.is.grid(g1))
         return(NULL)
      options(ursaSessionGrid=g1)
      return(invisible(g1))
   }
   str(obj)
   stop('Unable to recognize paramaters for new grid')
}
## .Unable to implement 'session_grid() <- val' for missing object
#'session_grid<-' <- function(value) {
#   stop("<-s")
#  # options(ursaSessionGrid=value)
#   session_grid(value)
#  # return(session_grid())
#}
'session_proj4' <- 'session_crs' <- function() session_grid()$proj4
'session_cellsize' <- function() with(session_grid(),sqrt(resx*resy))
'session_bbox' <- function() with(session_grid()
                                 ,c(minx=minx,miny=miny,maxx=maxx,maxy=maxy))
'session_pngviewer' <- function(allow=NA) {
   opV <- getOption("ursaAllowPngViewer")
   if ((is.na(allow))||(!is.logical(allow))) {
      if (is.logical(opV))
         return(opV)
      allow <- .isRscript() | .isKnitr() | .isJupyter() | .isShiny()

   }
   opA <- options(ursaAllowPngViewer=allow)[[1]]
   if (is.null(opV))
      opA <- allow
  # invisible(getOption("ursaAllowPngViewer"))
  # invisible(allow) ## RC
   invisible(opA)
}
'session_tempdir' <- function(dst=character()) {
   opD <- getOption("ursaTempDir")
   if ((is.character(dst))&&(length(dst))) {
      if ((file.exists(dst))&&(file.info(dst)$isdir)) {
         options(ursaTempDir=dst)
         return(invisible(dst))
      }
   }
   if (length(opD))
      return(opD)
   dst <- ifelse(.isRscript(),getwd(),tempdir())
   options(ursaTempDir=dst)
   return(dst)
}
'session_use_experimental_functions' <- function() {
   list1 <- readLines(system.file("NAMESPACE",package="ursa"))
   list1 <- grep("^export\\(",list1,value=TRUE)
   list1 <- gsub("^export\\(\\\"(.+)\\\"\\)","\\1",list1)
   ns <- asNamespace("ursa")
   list2 <- ls(envir=ns)
   list2 <- grep("^[A-Za-z]",list2,value=TRUE)
   list2 <- grep("\\.(ursa(Raster|Grid|ColorTable|Connection|Numeric|Category|Stack))"
                ,list2,value=TRUE,invert=TRUE)
   list2 <- grep("^(as\\.Raster|djqwotrhfndh)\\.",list2,value=TRUE,invert=TRUE)
   list2 <- list2[which(is.na(match(list2,list1)))]
   for (v in list2) {
     # global env set hack (function(key, val, pos) assign(key,val, envir=as.environment(pos)))(myKey, myVal, 1L) `
     # assign(v,get(v,envir=ns),envir=as.environment(1)) ## 'as.environment(1)' '.GlobalEnv'
      do.call("assign",list(v,get(v,envir=ns),envir=as.environment(1)))
   }
   invisible(list2)
}
