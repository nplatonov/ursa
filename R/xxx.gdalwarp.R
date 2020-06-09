# https://gis.stackexchange.com/questions/37790/how-to-reproject-raster-from-0-360-to-180-180-with-cutting-180-meridian
# gdalwarp -t_srs WGS84 ~/0_360.tif 180.tif  -wo SOURCE_EXTRA=1000 --config CENTER_LONG 0

'.gdalwarp' <- function(src,dst=NULL,grid=NULL,resample="near",nodata=NA
                       ,resetGrid=FALSE,opt=NULL,close=FALSE,verbose=0L) {
   if (is.null(grid)) {
      if (is.ursa(dst,"grid")) {
         grid <- dst
         dst <- NULL
      }
      else if (is.ursa(dst)) {
         grid <- ursa(dst,"grid")
         dst <- NULL
      }
      else
         grid <- getOption("ursaSessionGrid")
   }
   else
      grid <- ursa_grid(grid)
   if (!nchar(Sys.which("gdalwarp"))) {
      withRaster <- requireNamespace("raster",quietly=.isPackageInUse())
      if (withRaster) {
         r1 <- as.Raster(src)
         session_grid(grid)
         r2 <- as.Raster(ursa_new(0L))
         r3 <- try(raster::resample(r1,r2,method=c("bilinear","ngb")[1]))
         if (inherits(r3,"try-error")) {
            if (verbose)
               message('reprojection is failed')
            return(src)
         }
      }
      else if (verbose)
         message(paste("'gdalwarp' is not found; package 'raster' is not found."
                      ,"Reprojection is failed."))
      return(src)
   }
  # a <- open_envi(src)
  # ct <- ursa_colortable(a)
  # close(a)
   if (is.ursa(src)) {
      removeSrc <- TRUE
      .src <- src
      nodata <- ignorevalue(src)
      src <- .maketmp(ext=".")
      write_envi(.src,src)
   }
   else {
      removeSrc <- FALSE
     # nodata <- NA
   }
   inMemory <- is.null(dst)
   if (inMemory) {
      dst <- .maketmp(ext="")
      driver <- "ENVI"
   }
   else {
     # driver <- .gsub("^.+(\\.(.+))$","\\2",tolower(basename(dst)))
      driver <- switch(.gsub("^.+(\\.(.+))$","\\2",tolower(basename(dst)))
                      ,tif="GTiff",tiff="GTiff",envi="ENVI",img="HFA",hfa="HFA"
                      ,"ENVI")
   }
   if (verbose)
      print(c(inMemory=inMemory,removeSrc=removeSrc,isNullGrid=is.null(grid)))
   proj4 <- ursa_proj4(grid)
   if (!nchar(proj4)) {
      opt <- c(opt,to="SRC_METHOD=NO_GEOTRANSFORM",to="DST_METHOD=NO_GEOTRANSFORM")
   }
   if (!("co" %in% names(opt))) {
      if (driver=="GTiff")
         opt <- c(opt,co="COMPRESS=DEFLATE",co="PREDICTOR=2",co="TILED=NO")
      else if (driver=="HFA") {
         opt <- c(opt,co="COMPRESSED=YES")
      }
   }
   if (is.null(opt)) {
      optF <- ""
   }
   else if (!is.null(names(opt))) {
      optS <- unlist(opt)
      optF <- paste(paste0("-",names(optS)," ",.dQuote(unname(optS))),collapse=" ")
      optF <- gsub("\\s*\"TRUE\"","",optF)
      optF <- .gsub("\\s\\\"\\\"","",optF)
   }
   else
      optF <- ""
   if (!("r" %in% names(opt))) {
      optF <- paste(optF,"-r",resample)
   }
   if (is.null(grid))
      cmd <- paste("gdalwarp -overwrite -of",driver
                  ,ifelse(is.na(nodata),"",paste("-srcnodata",nodata,"-dstnodata",nodata))
                  ,ifelse(verbose==0L,"-q","")
                  ,optF,src,dst)
   else
      cmd <- with(grid,paste("gdalwarp -overwrite -of",driver
                        ,ifelse(nchar(proj4),paste("-t_srs",.dQuote(proj4)),"")
                        ,"-nosrcalpha"
                        ,"-tr",resx,resy,"-te",minx,miny,maxx,maxy
                        ,ifelse(is.na(nodata),"",paste("-srcnodata",nodata,"-dstnodata",nodata))
                        ,ifelse(verbose==0L,"-q","")
                        ,optF,src,dst))
   if (verbose)
      message(cmd)
   if (verbose>1)
      return(NULL)
   proj_lib <- Sys.getenv("PROJ_LIB")
   Sys.setenv(PROJ_LIB=file.path(dirname(dirname(Sys.which("gdalwarp"))),"share/proj"))
  # Sys.setenv(PROJ_LIB="")
  # print(Sys.getenv("PROJ_LIB"))
   system(cmd)
   Sys.setenv(PROJ_LIB=proj_lib)
   session_grid(NULL)
   if (inMemory) {
      ret <- if (driver=="ENVI") read_envi(dst) else read_gdal(dst)
   }
   else if (!close)
      ret <- if (driver=="ENVI") open_envi(dst) else open_gdal(dst)
   else
      ret <- NULL
   if (!is.na(nodata)) {
      ignorevalue(ret) <- nodata
      if (inMemory)
         ret[ret==nodata] <- NA
   }
   if (inMemory) {
      envi_remove(dst)
   }
   if (removeSrc) {
      envi_remove(src)
   }
   if (resetGrid)
      session_grid(ret)
   ret
}
