# https://github.com/ecohealthalliance/fasterize
'.gdal_rasterize' <- '.rasterize' <- function(obj,...) {
   if (!nchar(Sys.which("gdal_rasterize")))
      return(NULL)
   on.exit(NULL)
   arglist <- list(...)
   verbose <- .getPrm(arglist,name="verb(ose)*",default=FALSE)
  # obj <- .getPrm(arglist,name=".*",class=list("character","ursaVectorExternal"))
  # inMemory <- inherits(obj,c("sf","Spatial"))
   inMemory <- (.isSF(obj))||(.isSP(obj))
   if (inMemory) {
      if (.isSF(obj)) {
         mname <- try(names(sf::st_agr(obj)),silent=TRUE)
         if (inherits(mname,"try-error"))
            mname <- character()
      }
      else if (.isSP(obj)) {
         mname <- try(colnames(methods::slot(obj,"data")),silent=TRUE)
         if (inherits(mname,"try-error"))
            mname <- character()
      }
      Ftemp <- .maketmp(ext=ifelse(spatial_dim(obj)==2,".shp",".gpkg"))
      spatial_write(obj,Ftemp,verbose=verbose)
      obj <- Ftemp
      isShp <- .lgrep("\\.shp$",basename(Ftemp))>0
      if (isShp)
         on.exit(.shp.remove(Ftemp),add=TRUE)
      else
         on.exit(file.remove(Ftemp),add=TRUE)
   }
   external <- is.character(obj)
   if (external) {
      dsnE <- obj
      if (verbose)
         .elapsedTime("read vector -- start")
      obj <- spatialize(obj,...)
      if (verbose)
         .elapsedTime("read vector -- finish")
   }
   if (external)
      dsn <- dsnE
   else
      dsn <- attr(obj,"dsn")
  # print(c(dsn=dsn,dsnE=dsnE))
   g0 <- attr(obj,"grid")
   dname <- attr(obj,"colnames")
   dmask <- .getPrm(arglist,name="(attr|field)",default=".+")
   feature <- .getPrm(arglist,name="feature",valid=c("field","geometry","FID"))
   where <- .getPrm(arglist,name="subset",default="")
   ogropt <- .getPrm(arglist,name="ogropt",default="")
   optF <- .getPrm(arglist,name="^opt$",class="character",default="")
   clipsrc <- .getPrm(arglist,name="clipsrc",default=FALSE)
   clipdst <- .getPrm(arglist,name="clipdst",default=FALSE)
   wrapdateline <- .getPrm(arglist,name="wrapdateline",default=FALSE)
   layer <- .getPrm(arglist,name="layer",default=".*")
   isZip <- .lgrep("\\.zip$",dsn)>0
   fname1 <- .gsub("\\.zip$","",dsn)
   fname2 <- paste0(fname1,".zip")
   fname3 <- gsub("(\\..+$)",".zip",fname1)
   fname4 <- paste0(fname1,".gz")
   fname5 <- paste0(fname1,".bz2")
   cond1 <- file.exists(fname1)
   cond2 <- file.exists(fname2)
   cond3 <- file.exists(fname3)
   cond4 <- file.exists(fname4)
   cond5 <- file.exists(fname5)
  # print(dsn)
  # print(c(fname1=fname1,fname2=fname2,fname3=fname3))
  # print(c(isZip=isZip,cond1=cond1,cond2=cond2,cond3=cond3,cond4=cond4,cond5=cond5))
   if (cond1)
      dsn <- fname1
   else if (cond2 | cond3) {
      if (cond2)
         ziplist <- unzip(fname2,exdir=tempdir())
      if (cond3)
         ziplist <- unzip(fname3,exdir=tempdir())
      on.exit(file.remove(ziplist),add=TRUE)
      dsn <- .grep("\\.shp$",ziplist,value=TRUE)
   }
   else if ((cond4)&&(nchar(Sys.which("gzip")))) {
      dsn0 <- dsn
      dsn <- tempfile();on.exit(file.remove(dsn))
      system2("gzip",c("-f -d -c",.dQuote(dsn0)),stdout=dsn,stderr=FALSE)
   }
   else if ((cond5)&&(nchar(Sys.which("bzip2")))) {
      dsn0 <- dsn
      dsn <- tempfile();on.exit(file.remove(dsn))
      system2("bzip2",c("-f -d -c",.dQuote(dsn0)),stdout=dsn,stderr=FALSE)
   }
   else
      dsn <- NA
   isSF <- inherits(obj,"sf")
   isSP <- !isSF
  # print(dsn)
  # str(obj)
   if ((nchar(ogropt)>1)||(nchar(where)>0)) {
      shpname <- .maketmp()
      cmd <- paste("ogr2ogr"
              ,ogropt 
              ,"-where",dQuote(where)
              ,"-f",.dQuote(c("ESRI Shapefile","SQLite","GeoJSON")[1])
             # ,ifelse(verbose,"-progress","")
              ,.dQuote(paste0(shpname,".shp")),.dQuote(dsn)
              )
      if (verbose)
         message(cmd)
      system(cmd)
      list3 <- .dir(path=dirname(shpname)
                   ,pattern=paste0(basename(shpname),"\\.(cpg|dbf|prj|shp|shx)")
                   ,recursive=FALSE,full.names=TRUE)
      on.exit(file.remove(list3),add=TRUE)
      lname <- basename(shpname)
      dsn <- .grep("\\.shp$",list3,value=TRUE)
   }
   if (isZip <- .lgrep("\\.zip$",dsn)>0) {
      ziplist <- unzip(dsn,exdir=tempdir());on.exit(file.remove(ziplist))
      dsn <- .grep("\\.(shp|sqlite|gpkg|geojson)$",ziplist,value=TRUE)
   }
   else if ((nchar(Sys.which("gzip")))&&(isZip <- .lgrep("\\.gz$",dsn)>0)) {
      dsn0 <- dsn
      dsn <- tempfile();on.exit(file.remove(dsn))
      system2("gzip",c("-f -d -c",.dQuote(dsn0)),stdout=dsn,stderr=FALSE)
   }
   cmd <- paste("gdalsrsinfo -o proj4",.dQuote(dsn))
   if (verbose)
      message(cmd)
   proj4 <- system(cmd,intern=TRUE)
   proj4 <- .gsub("'","",proj4)
   proj4 <- .gsub("(^\\s|\\s$)","",proj4)
   if (noProj <- !length(proj4))
      proj4 <- "+init=epsg:4326"
   ftemp <- .maketmp() # .maketmp() #tempfile(pattern="") # ".\\tmp1"
   cmd <- paste("ogrinfo","-q",.dQuote(dsn))
   if (verbose)
      message(cmd)
   lname <- system(cmd,intern=TRUE)
   lname <- .gsub("(\\s\\(.+\\))*$","",lname)
   lname <- .gsub("^\\d+:\\s(.+)$","\\1",lname)
   .lname <- .grep(layer,lname,value=TRUE)
   if (length(.lname)>1)
      .lname <- lname[match(layer,lname)]
   if (length(.lname)!=1) {
      print(paste("Select only one layer:",paste(paste0(seq(.lname),")")
                                 ,.sQuote(.lname),collapse=", ")),quote=FALSE)
      return(NULL)
   }
   lname <- .lname
   rm(.lname)
   if (proj4!=g0$proj4) {
     # if (verbose)
     #   message("REPROJECT")
      shpname <- .maketmp()
      bb1 <- with(regrid(g0,expand=1.1),matrix(c(minx,miny,maxx,maxy),ncol=2))
      bb1 <- matrix(bb1[c(1,2,1,4,3,4,3,2,1,2)],ncol=2,byrow=TRUE)
      if (TRUE) {
         x <- bb1[,1]
         y <- bb1[,2]
         n <- 256
         x <- c(seq(x[1],x[2],len=n),seq(x[2],x[3],len=n)
               ,seq(x[3],x[4],len=n),seq(x[4],x[5],len=n))
         y <- c(seq(y[1],y[2],len=n),seq(y[2],y[3],len=n)
               ,seq(y[3],y[4],len=n),seq(y[4],y[5],len=n))
         bb1 <- cbind(x,y)
      }
      if (isSF) {
         bb1 <- sf::st_sfc(sf::st_multilinestring(list(bb1)),crs=g0$proj4)
         bb2 <- sf::st_transform(bb1,proj4)
         bb2 <- sf::st_bbox(bb2)
      }
      else if (isSP) {
         bb1 <- sp::SpatialLines(list(sp::Lines(sp::Line(bb1),1L))
                                ,proj4string=sp::CRS(g0$proj4))
         bb2 <- sp::spTransform(bb1,proj4)
         bb2 <- c(sp::bbox(bb2))
         if (length(bb2)==6)
            bb2 <- bb2[c(1,2,4,5)]
      }
     # print(proj4)
      if (.lgrep("\\+proj=longlat",proj4)) {
         bb2[1][bb2[1]<(-179)] <- -180
         bb2[3][bb2[3]>(179)] <- 180
      }
     ## http://www.gdal.org/ogr2ogr.html http://gis-lab.info/qa/ogr2ogr-examples.html
      g1 <- regrid(g0,border=5)
      cmd <- paste("ogr2ogr","-t_srs",.dQuote(g0$proj4)
              ,ifelse(noProj,paste("-s_srs",.dQuote(proj4)),"")
              ,"-sql",.dQuote(paste("select FID,* from",.dQuote(.dQuote(lname))))
              ,"-dialect",c("SQLITE","OGRSQL")[2]
              ,"-select FID"
              ,"-f",.dQuote(c("ESRI Shapefile","SQLite")[1])
              ,ifelse(verbose,"-progress","")
             # ,"-skipfailures"
              ,ifelse(wrapdateline,"-wrapdateline","")
             # ,"-datelineoffset 180"
              ,ifelse(clipsrc,paste("-clipsrc",paste(bb2,collapse=" ")),"")
              ,ifelse(clipdst,paste("-clipdst"
                       ,with(g1,paste(c(minx,miny,maxx,maxy),collapse=" "))),"")
              ,.dQuote(paste0(shpname,".shp")),.dQuote(dsn)
              )
      if (verbose)
         message(cmd)
     # .elapsedTime("a")
      system(cmd)
     # .elapsedTime("b")
      list2 <- .dir(path=dirname(shpname)
                   ,pattern=paste0(basename(shpname),"\\.(cpg|dbf|prj|shp|shx)")
                   ,recursive=FALSE,full.names=TRUE)
      on.exit(file.remove(list2),add=TRUE)
      lname <- basename(shpname)
      shpname <- .grep("\\.shp$",list2,value=TRUE)
   }
   else
      shpname <- dsn
   if (is.null(dname)) {
      md <- system(paste("ogrinfo -al -so",dsn),intern=TRUE)
      patt <- "^(.+): \\S+ \\(.+\\)$"
      md <- .grep(patt,md,value=TRUE)
      dname <- .grep(dmask,.gsub(patt,"\\1",md),value=TRUE)
   }
   fext <- .gsub(".+\\.(.+)$","\\1",basename(dsn))
   fromZero <- .lgrep("^(sqlite|gpkg)$",fext)==0 & .lgrep("^fid$",dname)==0
   if (verbose)
      print(c(FID_starts_from_zero=fromZero))
   if (feature=="field") {
      if (inMemory)
         dname <- mname ## restore dbf coersion
     # opt <- paste()
     # dname <- .grep(dmask,dname,value=TRUE)
      cmd <- with(g0,paste("gdal_rasterize"
              ,"-a FID",optF
              ,"-sql",.dQuote(paste("select FID,* from",.dQuote(.dQuote(lname))))
              ,"-dialect",c("SQLITE","OGRSQL")[2]
              ,"-init -1 -a_nodata -1"
              ,"-a_srs",.dQuote(proj4)
              ,"-tr",resx,resy
             # ,"-where",dQuote(subset)
              ,"-te",minx,miny,maxx,maxy
              ,"-of ENVI -ot Int32",ifelse(verbose,"","-q")
              ,.dQuote(shpname),ftemp))
      if (verbose) {
         .elapsedTime(paste("rasterize",.sQuote(feature),"-- start"))
         message(cmd)
      }
      system(cmd)
      if (verbose)
         .elapsedTime(paste("rasterize",.sQuote(feature),"-- finish"))
      va <- read_envi(ftemp,resetGrid=TRUE)
      envi_remove(ftemp)
      ta <- ursa(va,"table")
      tavalue <- as.integer(names(ta))
      if (TRUE) { ## ++ 20171127
        # print(ursa_table(va))
         if (fromZero)
            va <- va+1L
      }
      else {
         minva <- global_min(va)
         if ((!is.na(minva))&&(minva==0))
            va <- va+1L
      }
      va[va==0] <- NA
      res <- lapply(dname,function(x) {
         isFID <- .lgrep("fid",x)
         if ((isFID)||(identical(tavalue,obj[[x]])))
            a <- va
         else {
            a <- reclass(va,src=seq(nrow(obj)),dst=obj[[x]])
         }
         if (.is.category(a))
            ursa(a,"nodata") <- length(ursa(a,"colortable"))
         names(a) <- x
         a
      })
      names(res) <- dname ## or, comment it
   }
   else if (feature=="geometry") {
      writeValue <- FALSE
      if (length(dname)==1) {
         if (is.numeric(obj[[dname]])) {
            value <- obj[[dname]]
            writeValue <- TRUE
         }
      }
      if (verbose)
         .elapsedTime(paste("rasterize",.sQuote(feature),"-- start"))
      nr <- nrow(obj)
      res <- ursa_new(badname=paste("FID",seq(nr)))
      for (i in seq(nr)) {
         cmd <- with(g0,paste("gdal_rasterize"
              ,"-a FID",optF
              ,"-sql",.dQuote(paste("select FID,* from",.dQuote(.dQuote(lname))
                             ,"where",paste0(.dQuote("FID"),"=",i-fromZero)))
              ,"-dialect",c("SQLITE","OGRSQL")[2]
             # ,"-where",paste0(.dQuote("FID"),"=",i)
              ,"-tr",resx,resy
              ,"-te",minx,miny,maxx,maxy
              ,"-of ENVI -ot Int16",ifelse(verbose,"","-q")
              ,"-a_nodata -1 -init -1"
              ,shpname,ftemp))
         if (verbose)
            message(cmd)
         system(cmd)
         if (writeValue) {
            va <- read_envi(ftemp)
            va[!is.na(va)] <- value[i]
            res[i] <- va
         }
         else
            res[i] <- read_envi(ftemp)
         envi_remove(ftemp)
      }
      if (verbose)
         .elapsedTime(paste("rasterize",.sQuote(feature),"-- finish"))
   }
   else if (feature=="FID") {
      cmd <- with(g0,paste("gdal_rasterize"
              ,"-a FID",optF
              ,"-sql",.dQuote(paste("select FID,* from",.dQuote(.dQuote(lname))))
              ,"-dialect",c("SQLITE","OGRSQL")[2]
              ,"-init -1 -a_nodata -1"
              ,"-a_srs",.dQuote(proj4)
              ,"-tr",resx,resy
             # ,"-where",dQuote(subset)
              ,"-te",minx,miny,maxx,maxy
              ,"-of ENVI -ot Int32",ifelse(verbose,"","-q")
              ,.dQuote(shpname),ftemp))
      if (verbose) {
         .elapsedTime(paste("rasterize",.sQuote(feature),"-- start"))
         message(cmd)
      }
      system(cmd)
      if (verbose)
         .elapsedTime(paste("rasterize",.sQuote(feature),"-- finish"))
      res <- c(FID=read_envi(ftemp,resetGrid=TRUE)+1*fromZero)
      envi_remove(ftemp)
   }
   else
      stop(paste("Unimplemented feature:",feature))
   res
}
