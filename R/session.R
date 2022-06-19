'session_grid' <- function(obj,...) {
   arglist <- list(...)
   ref <- getOption("ursaSessionGrid")
   if (missing(obj)) { ## 'Extract'
      if ((is.null(ref))||(!.is.grid(ref))) {
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
      if (!length(arglist)) {
        # return(invisible(ref))
         return(ref)
      }
      else {
         obj <- do.call(regrid,c(list(ref),arglist))
         arglist <- NULL
      }
   }
  # above - 'Extract' (visible), below - 'Replace' (invisible)
   options(ursaSessionGrid_prev=ref)
   if (is.null(obj))
      return(options(ursaSessionGrid=NULL))
   if (length(arglist)) {
     # if (is_spatial(obj))
      obj <- do.call(regrid,c(list(spatial_grid(obj)),arglist))
     # else
     #    obj <- do.call(regrid,c(list(ursa_grid(obj)),arglist))
   }
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
      return(session_grid(spatial_grid(obj)))
   }
   if ((length(obj)==1)&&(!envi_exists(obj))&&
       (nchar(Sys.getenv("R_IDRISI")))&&(exists("read.idr"))) {
      g1 <- do.call("read.idr",list((obj)))$grid
      options(ursaSessionGrid=g1)
      return(invisible(g1))
   }
   if (is.character(obj)) {
     # print(obj)
     # print(spatial_dir(pattern=obj,recursive=FALSE))
      if (T & length(spatial_dir(path=dirname(obj),pattern=basename(obj),recursive=FALSE))==1) {
         a <- spatial_read(obj)
         g1 <- spatial_grid(a)
         rm(a)
      }
      else {
         opW <- options(warn=2)
         a <- try(open_envi(obj,resetGrid=TRUE,decompress=FALSE))
         options(opW)
         if (inherits(a,"try-error")) {
            if (file.exists(obj)) {
               a <- open_gdal(obj)
            }
         }
         g1 <- a$grid
         if (is_ursa(a))
            close(a)
      }
      if (!.is.grid(g1))
         return(NULL)
      options(ursaSessionGrid=g1)
      return(invisible(g1))
   }
   if ((is.numeric(obj))&&(length(obj)==2)) {
      obj <- unname(obj)
      ref <- round(obj)
      g1 <- .grid.skeleton()
      g1$columns <- as.integer(ref[2])
      g1$rows <- as.integer(obj[1])
      g1$minx <- 0
      g1$miny <- 0
      g1$maxx <- obj[2]
      g1$maxy <- obj[1]
      g1$resx <- with(g1,(maxx-minx)/columns)
      g1$resy <- with(g1,(maxy-miny)/rows)
      if (!FALSE) {
         retina <- getOption("ursaRetina")
         if ((is.numeric(retina))&&(retina>1))
            g1$retina <- retina
      }
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
# .syn('session_crs',0)
#'.session_crs<-' <- function(x,value) {
#   a <- session_grid()
#   a$crs <- .epsg2proj4(value,force=TRUE)
#   session_grid(a)
#}
'session_proj' <- 'session_proj4' <- 'session_crs' <- function() session_grid()$crs
'session_cellsize' <- function() with(session_grid(),sqrt(as.numeric(resx)*as.numeric(resy)))
'session_dim' <- function() with(session_grid(),c(lines=rows,samples=columns))
'session_bbox' <- function() with(session_grid()
                                 ,c(minx=minx,miny=miny,maxx=maxx,maxy=maxy))
'session_pngviewer' <- function(allow=NA) {
   opV <- getOption("ursaAllowPngViewer")
  # str(list(allow=allow,opV=opV,isRscript=.isRscript()))
   if ((is.na(allow))||(!is.logical(allow))) {
      if (is.logical(opV))
         return(opV)
      allow <- interactive() | .isRscript() | .isKnitr() | .isJupyter() | .isShiny()

   }
   opA <- options(ursaAllowPngViewer=allow)[[1]]
   if (is.null(opV))
      opA <- allow
  # invisible(getOption("ursaAllowPngViewer"))
  # invisible(allow) ## RC
   invisible(opA)
}
'session_tempdir' <- function(dst=character()) {
   if ((is.character(dst))&&(length(dst))) {
      if (!dir.exists(dst)) {
         opW <- options(warn=2)
         dir.create(dst)
         options(opW)
      }
     # options(ursaTempDir=normalizePath(dst,winslash="/",mustWork=FALSE))
      options(ursaTempDir=dst)
      return(invisible(dst))
   }
   opD <- getOption("ursaTempDir")
   if (length(opD))
      return(opD)
   dst <- ifelse(.isRscript()
                ,ifelse(T,.ursaCacheDir(),getwd())
               # ,normalizePath(tempdir(),winslash="/")
                ,tempdir()
                ) ## "." <-> 'getwd()'
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
