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
'.shp.read' <- function(fname,reproject=TRUE,encoding="1251",resetGrid=FALSE
                       ,verbose=0L,...)
{
  ## b <- sf::st_read("gde-1-1-15.shp",quiet=TRUE)
  ## b <- sf::st_transform(b,ursa_proj(a))
  # print(fname)
   requireNamespace("rgdal",quietly=.isPackageInUse())
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
   proj4 <- session_grid()$proj4
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
   requireNamespace("rgdal",quietly=.isPackageInUse())
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
   file.remove(.dir(paste0("^",fname,"\\.(cpg|dbf|prj|shp|shx)$")))
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
   proj4 <- session_grid()$proj4
   if ((reproject)&&(nchar(proj4))&&(!is.na(sf::st_crs(res)))) {
      if (verbose>1)
         .elapsedTime("st_transform:start")
      res <- sf::st_transform(res,proj4)
      if (verbose>1)
         .elapsedTime("st_transform:finish")
   }
   res
}
