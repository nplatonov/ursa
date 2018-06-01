## 'caTools::read.ENVI'
##~ '.read_envi' <- function(fname,subset=NULL,nodata=NaN
                       ##~ ,ref=NULL,resetGrid=FALSE,...)
'read_envi' <- function(fname,...)
{
   arglist <- list(...)
   subset <- .getPrm(arglist,name="(^$|band|subset)",class=c("integer","character")
                    ,default=NULL)
   nodata <- .getPrm(arglist,name="(nodata|ignorevalue)",default=NaN)
   cache <- .getPrm(arglist,name="cache",class=c("integer","logical"),default=0L)
   ref <- .getPrm(arglist,class=c("ursaRaster","ursaGrid"),default=NULL)
   resetGrid <- .getPrm(arglist,name="reset(Grid)*",default=FALSE)
   verbose <- .getPrm(arglist,name="verb(ose)*",default=FALSE)
   if (!is.null(ref)) {
      if (is.ursa(ref))
         ref <- ref$grid
   }
   g0 <- getOption("ursaSessionGrid")
   g1 <- if (is.null(ref)) g0 else ref
   if (is.null(g1))
      theSame <- TRUE
   else {
      g2 <- ursa_grid(fname)
     # .c1 <- match(ursa_seqx(g2),ursa_seqx(g1));print(.c1);q()
      x1 <- ursa_seqx(g1)
      x2 <- ursa_seqx(g2)
      y1 <- ursa_seqy(g1)
      y2 <- ursa_seqy(g2)
      condX <- na.omit(.is.near(x2,x1))
      ind <- attr(condX,"na.action")
      condY <- na.omit(.is.near(y2,y1))
      condX <- ((length(condX))&&(all(diff(condX)==1)))
      condY <- ((length(condY))&&(all(diff(condY)==1)))
     # condG <- .grid.equal(g1,g2)
     # print(c(condX=condX,condY=condY,condG=condG))
      theSame <- condX & condY # & condG
   }
   if (!theSame) {
      ref <- g1
      ##~ resetGrid <- TRUE
   }
   if (verbose) {
      str(list(subset=subset,nodata=nodata,ref=ref,grid=ursa_grid(fname)
              ,theSame=theSame,resetGrid=resetGrid))
   }
   if ((!is.null(ref))&&(theSame)) {
      session_grid(ref)
   }
  # if (!theSame) { ## removed 20161002
   if ((resetGrid)||(!theSame)) { ## added 20161002
      session_grid(NULL)
   }
   obj <- open_envi(fname,cache=cache,decompress=!is.null(subset)) ## ,nodata=nodata
   bname <- bandname(obj)
   if (.is.grid(obj))
      return(NULL)
   if (!is.nan(nodata)) {
      storage.mode(nodata) <- obj$con$mode
      obj$con$nodata <- nodata
   }
   if ((!is.null(subset))&&(obj$con$connection=="file")) { ## if (obj$con$seek)
      ret <- obj[subset]
   }
   else
   {
     # if ((is.character(obj$con$fname))&&(nchar(obj$con$fname)>0)&&
     #     (!file.exists(obj$con$fname)))
      if (is.na(obj$con$handle)) {
        # ursa_value(obj) <- NA
         ret <- ursa_new(bandname=bandname(obj))
      }
      else {
         ret <- obj[]
      }
      if (!is.null(subset))
         ret <- obj[subset]
   }
   if (FALSE) { ## --20180312
     # print(summary(as.numeric(obj$value)));q()
      if (!is.na(obj$con$handle))
         close(obj$con$handle)
      obj$con$handle <- NA
   }
   else
      close(obj)
  # .gc() ## 20180312 does it required?
  # if (!theSame & !resetGrid) {
   if (!theSame & !resetGrid) {
      ret <- regrid(ret,grid=ref,...)
   }
   if ((!resetGrid)&&(!is.null(g0)))
         session_grid(g0)
   ret
}
