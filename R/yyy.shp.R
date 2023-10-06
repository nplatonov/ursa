'.shp.layer' <- function(fname)
{
   f <- basename(fname)
   if (!.lgrep("\\.shp$",f))
      return(f)
   .gsub("\\.shp$","",f)
}
'.shp.file' <- function(fname)
{
   if (.lgrep("\\.shp$",fname))
      return(fname)
   paste0(fname,".shp")
}
'.shp.geometry' <- function(fname,verbose=FALSE) {
   if (.lgrep("\\.zip$",basename(fname)))
      fname <- .gsub("\\.zip$","",fname)
   fpath <- dirname(fname)
   lname <- .shp.layer(fname)
   z <- file.path(fpath,paste0(lname,".zip"))
   if (!file.exists(z))
      z <- file.path(fpath,paste0(lname,".shp.zip"))
   if (file.exists(z))
   {
      a <- utils::unzip(z,exdir=fpath,junkpaths=TRUE)
      on.exit(file.remove(a))
   }
   info <- system(paste("ogrinfo",.dQuote(fname)),intern=TRUE)
   patt <- paste0(".+",lname," \\((.+)\\)")
   res <- .gsub(patt,"\\1",.grep(patt,info,value=TRUE))
   if (verbose)
      names(res) <- fname
   res
}
'.shp.remove' <- function(fname) {
   if (.lgrep("\\.shp$",fname))
      fname <- .gsub("\\.shp$","",fname)
   list1 <- .dir(path=dirname(fname)
                ,pattern=paste0("^",basename(fname),"\\.(cpg|dbf|prj|shp|shx)$")
                ,full.names=TRUE)
   file.remove(list1)
}
'.sf.read' <- function(fname,reproject=TRUE,encoding="1251",verbose=0L,...)
{
  ## b <- sf::st_read("gde-1-1-15.shp",quiet=TRUE)
  ## b <- sf::st_transform(b,ursa_proj(a))
  # print(fname)
   if (!requireNamespace("sf",quietly=.isPackageInUse()))
      return(NULL)
   if (!file.exists(fname)) {
      aname <- paste0(fname,".zip")
      if (isZip <- file.exists(aname)) {
         ziplist <- unzip(aname);on.exit(file.remove(ziplist))
         fname <- .grep("\\.shp$",ziplist,value=TRUE)
      }
   }
   else {
      if (isZip <- .lgrep("\\.zip$",fname)>0) {
         ziplist <- unzip(fname);on.exit(file.remove(ziplist))
         fname <- .grep("\\.shp$",ziplist,value=TRUE)
      }
   }
   if (verbose>1)
      .elapsedTime("st_read:start")
   res <- sf::st_read(fname,quiet=TRUE)
   if (isZip)
      cpgname <- .grep("\\.cpg$",ziplist,value=TRUE)
   else
      cpgname <- .gsub("\\.shp$",".cpg")
   if ((length(cpgname))&&(file.exists(cpgname))) {
      cpg <- readLines(cpgname,warn=FALSE)
      if (cpg=="UTF-8")
         cpg <- NULL
   }
   if (verbose>1)
      .elapsedTime("st_read:finish")
   proj4 <- session_grid()$crs
   if ((reproject)&&(nchar(proj4))&&(!is.na(sf::st_crs(res)))) {
      if (verbose>1)
         .elapsedTime("st_transform:start")
      res <- sf::st_transform(res,proj4)
      if (verbose>1)
         .elapsedTime("st_transform:finish")
   }
   res
}
