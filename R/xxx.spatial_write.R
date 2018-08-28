'spatial_write' <- function(obj,fname,layer,driver=NA,compress=""
                        ,ogr2ogr=nchar(Sys.which("ogr2ogr"))>0
                        ,transform=NA,verbose=FALSE) {
  # obj <- head(obj,100)
   bname <- basename(fname)
   dname <- dirname(fname)
   fext <- .gsub("^.+\\.(.+)$","\\1",bname)
   wait <- 60
   interimExt <- c("gpkg","geojson","shp","sqlite")[4]
   driverList <- c(shp="ESRI Shapefile"
                  ,sqlite="SQLite",json="GeoJSON",geojson="GeoJSON",gpkg="GPKG"
                  ,tab="Mapinfo File",kml="KML")
   if (!nchar(compress)) {
      packPatt <- "^(zip|bz(ip)*2|gz(ip)*|xz)$"
      if (.lgrep(packPatt,fext)>0) {
         compress <- .gsub(packPatt,"\\1",fext)
         bname <- gsub(paste0("\\.",compress),"",bname)
         fext <- .gsub("^(.+)\\.(.+)$","\\2",bname)
         fname <- file.path(dname,bname)
      }
   }
   lname <- .gsub("^(.+)\\.(.+)$","\\1",bname)
   isList <- is.list(obj)
   if (isList) {
      cl <- sapply(obj,inherits,c("sf","sfc","SpatialLinesDataFrame"
                          ,"SpatialPointsDataFrame","SpatialPolygonsDataFrame"
                          ,"SpatialLines","SpatialPoints","SpatialPolygons"))
      cl2 <- sapply(obj,function(x) {
         if (.isSF(x))
            return("sf")
         if (.isSP(x))
            return("s")
         return("unknown")
      })
      allSF <- all(cl2 %in% "sf")
      allSP <- all(cl2 %in% "sp")
      if (any(!cl))
         isList <- FALSE
   }
   if (!is.character(driver)) {
      driver <- driverList[fext]
      if (is.na(driver)) {
         if (isList)
            driver <- driverList["sqlite"]
         else
            driver <- driverList["sqlite"]
         fext <- names(driver)
         lname <- bname
         bname <- paste0(lname,".",fext)
         fname <- file.path(dname,bname)
      }
   }
   if ((!missing(layer))&&((is.character(layer))&&(nchar(layer))))
      lname <- layer
   ext <- switch(driver,'ESRI Shapefile'="(cpg|dbf|prj|qpj|shp|shx)"
                       ,'MapInfo File'="(mif|mid)",fext)
   if (isList) {
      if ((!allSF)&&(!ogr2ogr))
         stop("'ogr2ogr' is requires to merge layers")
     # fname1 <- paste0("res",seq_along(obj),".gpkg")
      fname1 <- .maketmp(length(obj),ext=interimExt)
      if (!allSF)
         pb <- ursaProgressBar(min=0,max=2*length(obj))
      p4s <- sapply(obj,function(x){
         res <- if (inherits(x,c("sf","sfc"))) sf::st_crs(x)$proj4string
                else sp::proj4string(x)
         res
      })
      p4s <- unname(p4s)
      keepCRS <- ifelse(length(p4s)==1,TRUE,do.call("all.equal",as.list(p4s)))
      if (verbose)
         .elapsedTime("list:0")
      iname <- vector("list",length(fname1))
      lname <- names(obj)
      if (is.null(lname))
         lname <- rep("",length(obj))
      if (any(ind <- !nchar(lname))) {
         lname[ind] <- as.character(as.list(match.call())$obj)[-1][ind]
        # lname[ind] <- aname[ind]
      }
      options(ursaSpatialMultiLayer=0L)
      for (i in seq_along(obj)) {
         o <- obj[[i]]
         isSF <- inherits(o,c("sf","sfc"))
         isSP <- !isSF
         if (isSF) {
            ind <- match(attr(o,"sf_column"),colnames(o))
            jname <- colnames(o)[-ind]
            if (interimExt=="shp")
               colnames(o)[-ind] <- sprintf("fld%03d",seq_along(jname))
         }
         else if (isSP) {
            jname <- colnames(methods::slot(o,"data"))
            if (interimExt=="shp")
               colnames(methods::slot(o,"data")) <- sprintf("fld%03d",seq_along(jname))
         }
         iname[[i]] <- jname
         options(ursaSpatialMultiLayer=getOption("ursaSpatialMultiLayer")+1L)
         if (allSF)
            spatial_write(o,fname,layer=lname[i],verbose=verbose) ## RECURSIVE
         else
            spatial_write(o,fname1[i],verbose=verbose) ## RECURSIVE
         if (!allSF)
            setUrsaProgressBar(pb)
         if (verbose)
            .elapsedTime(paste0("list:",i))
      }
      options(ursaSpatialMultiLayer=NULL)
      if (allSF)
         return(invisible(0L))
      dopt <- character()
      lopt <- character()
      if (driver=="ESRI Shapefile")
         lopt <- c(lopt,"ENCODING=UTF-8")
      if (driver=="MapInfo File")
         dopt <- c(dopt,"FORMAT=MIF")
      if (driver=="SQLite") {
        # dopt <- c(dopt,"SPATIALITE=yes")
         lopt <- c(lopt,"LAUNDER=NO")#,"SPATIAL_INDEX=YES")
      }
      suppressWarnings({
         first <- TRUE
         op <- options(warn=2)
         for (i in seq(wait)) {
            if (!file.exists(fname))
               break
            if (file.remove(fname))
               break
            if (first) {
               cat(paste("Waiting",wait,"seconds for permitted writting"
                        ,.sQuote(bname)))
               first <- FALSE
            }
            cat(".")
            Sys.sleep(1)
         }
         options(op)
         if (!first) {
            if (i==wait)
               cat(" FAILURE!\n")
            else
               cat(" ok!\n")
         }
      })
      for (i in seq_along(fname1)) {
         b <- paste(fname,fname1[i],"-nln",lname[i])
         if (length(dopt))
            b <- paste(paste("-dco",dopt),b)
         if (length(lopt))
            b <- paste(paste("-lco",lopt),b)
         if ((i==1)&&(keepCRS))
            b <- paste(b,"-t_srs",.dQuote(p4s[1]))
         if ((interimExt=="shp")&&(!is.null(iname[[i]]))) {
            aname <- sprintf("fld%03d",seq_along(iname[[i]]))
            s <- paste("select",paste(aname,"as",paste0("\\\"",iname[[i]],"\\\"")
                                     ,collapse=",")
                      ,"from",paste0("\\\""
                                    ,.gsub("\\.shp$","",basename(fname1[i]))
                                    ,"\\\""))
            s <- paste("-sql",.dQuote(s))
            ##~ cat(s)
            ##~ cat("\n")
            ##~ s <- ""
         }
         else
            s <- ""
         if (i==1)
            cmd <- paste("ogr2ogr",s,"-f",.dQuote(driver),b)
         else
            cmd <- paste("ogr2ogr",s,"-update -append",b)
         if (verbose)
            message(cmd)
         if (!system(cmd)) {
            if (.lgrep("\\.shp$",basename(fname1[i])))
               .shp.remove(fname1[i])
            else if (file.exists(fname1[i]))
               file.remove(fname1[i])
         }
         setUrsaProgressBar(pb)
         if (verbose)
            .elapsedTime(paste0("append:",i))
      }
      if (!allSF)
         close(pb)
      return(invisible(0L))
   }
   appendlayer <- getOption("ursaSpatialMultiLayer")
   appendlayer <- ifelse(is.null(appendlayer),FALSE,appendlayer>1)
   if ((ogr2ogr)&&(interimExt!="sqlite")&&(driver %in% "SQLite")) { ## sf>=0.6.3 great improvement
      interim <- TRUE
      driver0 <- driver
      fext0 <- fext
      fname0 <- fname
      fext <- interimExt
      driver <- driverList[fext]
     # fname <- .gsub(paste0("\\.",fext0,"$"),paste0(".",fext),fname)
      fname <- .maketmp(ext=interimExt)
      bname <- basename(fname)
      p4s <- if (inherits(obj,c("sf","sfc"))) sf::st_crs(obj)$proj4string
             else sp::proj4string(obj)
   }
   else
      interim <- FALSE
   dopt <- character()
   lopt <- character()
   if (driver=="ESRI Shapefile")
      lopt <- c(lopt,"ENCODING=UTF-8")
   if (driver=="MapInfo File")
      dopt <- c(dopt,"FORMAT=MIF")
   if (driver=="SQLite") {
     # dopt <- c(dopt,"SPATIALITE=yes")
      lopt <- c(lopt,"LAUNDER=NO")#,"SPATIAL_INDEX=YES")
   }
   if ((is.logical(compress))&&(compress)) {
      compress <- if (driver %in% c("GeoJSON","SQLite","GPKG","KML")) "gz" 
                  else "zip"
   }
   if (verbose)
      print(data.frame(fname=fname,pack=compress,bname=bname,layer=lname
                      ,ext=fext,driver=driver))
   if (!appendlayer) {
      suppressWarnings({
         first <- TRUE
         op <- options(warn=2)
         for (i in seq(wait)) {
            if (!file.exists(fname))
               break
            if (file.remove(fname))
               break
            if (first) {
               cat(paste("Waiting",wait,"seconds for permitted writting"
                        ,.sQuote(bname)))
               first <- FALSE
            }
            cat(".")
            Sys.sleep(1)
         }
         options(op)
         if (!first) {
            if (i==wait)
               cat(" FAILURE!\n")
            else
               cat(" ok!\n")
         }
      })
   }
   isSF <- inherits(obj,c("sf","sfc"))
   isSP <- !isSF
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF)
      aname <- try(names(sf::st_agr(obj)),silent=TRUE)
   else if (isSP)
      aname <- try(colnames(methods::slot(obj,"data")),silent=TRUE)
   if (inherits(aname,"try-error"))
      aname <- character()
   for (a in aname) {
      if (is.ordered(obj[[a]]))
         obj[[a]] <-  factor(obj[[a]],ordered=FALSE)
   }
   if (driver %in% c("ESRI Shapefile")) {
      for (a in aname) {
         if (!inherits(obj[[a]],"POSIXt"))
            next
         v <- as.character(as.POSIXct(as.numeric(obj[[a]])
                          ,origin="1970-01-01",tz="UTC"))
         obj[[a]] <- paste0(.gsub("(^.+\\d)(\\s)(\\d.+$)","\\1\\T\\3",v),"Z")
      }
   }
   if (isSP) {
      if (driver %in% c("GeoJSON","KML","GPX")) {
         obj <- sp::spTransform(obj,sp::CRS("+init=epsg:4326"))
      }
      opW <- options(warn=1)
     # dsn <- if (driver %in% c("zzzESRI Shapefile")) dname else fname
      if ((interim)&&(interimExt=="shp")) {
         colnames(methods::slot(obj,"data")) <- iname <- sprintf("fld%03d",seq_along(colnames(methods::slot(obj,"data"))))
      }
      lch <- getOption("encoding")
      rgdal::writeOGR(obj,dsn=fname,layer=lname,driver=driver
                     ,dataset_options=dopt,layer_options=lopt
                     ,encoding=if (lch=="UTF-8") NULL else localeToCharset()
                     ,overwrite_layer=TRUE,morphToESRI=FALSE
                     ,verbose=verbose)
      if ((TRUE)&&(driver=="ESRI Shapefile")) {
         prj <- sp::proj4string(obj)
         prj1 <- rgdal::showWKT(prj,morphToESRI=TRUE)
         prj2 <- rgdal::showWKT(prj,morphToESRI=FALSE)
         if (!identical(prj1,prj2)) {
            writeLines(prj2,gsub("\\.shp$",".prj",fname))
         }
      }
      options(opW)
   }
   else if (isSF) {
      if (FALSE) {
         f <- .dir(path=dname
                  ,pattern=paste0("^",lname,"\\.",ext,"$")
                  ,full.names=TRUE)
         file.remove(f)
      }
      if (driver %in% c("GeoJSON","KML","GPX")) {
         obj <- sf::st_transform(obj,4326)
      }
      opW <- options(warn=1)
      if ((interim)&&(interimExt=="shp")) {
        # ind <- head(seq_along(obj),-1)
         ind <- which(is.na(match(colnames(obj),attr(obj,"sf_column"))))
         names(obj)[ind] <- iname <- sprintf("fld%03d",ind)
      }
      if (FALSE) {
         print(c(dsn=fname,layer=lname,appendlayer=as.character(appendlayer))
              ,quote=FALSE)
         print(c(delete_layer=file.exists(fname) & !appendlayer
                ,delete_dsn=file.exists(fname) & !appendlayer
                ,update=appendlayer))
      }
      sf::st_write(obj,dsn=fname,layer=lname,driver=driver
                  ,dataset_options=dopt,layer_options=lopt
                  ,delete_layer=file.exists(fname) & !appendlayer
                  ,delete_dsn=file.exists(fname) & !appendlayer
                  ,update=appendlayer
                  ,quiet=!verbose)
      if ((TRUE)&&(driver %in% "ESRI Shapefile")) {
         prjname <- gsub("\\.shp$",".prj",fname)
         wkt1 <- readLines(prjname,warn=FALSE)
         wkt2 <- sf::st_as_text(sf::st_crs(spatial_crs(obj)))
         if (!identical(wkt1,wkt2))
            writeLines(wkt2,prjname)
      }
      options(opW)
   }
   if (interim) {
      dopt <- character()
      lopt <- character()
      if (driver0=="MapInfo File")
         dopt <- c(dopt,"FORMAT=MIF")
      if (driver0=="SQLite") {
        # dopt <- c(dopt,"SPATIALITE=yes")
         lopt <- c(lopt,"LAUNDER=NO")#,"SPATIAL_INDEX=YES")
      }
      b <- character()
      if (length(dopt))
         b <- c(b,paste("-dco",.dQuote(dopt)))
      if (length(lopt))
         b <- c(b,paste("-lco",.dQuote(lopt)))
      lnameF <- ifelse(interimExt=="shp",.gsub("\\.shp$","",basename(fname)),lname)
      if (length(aname)) {
         s <- paste("select"
                   ,paste(iname,"as",paste0("\\\"",aname,"\\\""),collapse=",")
                   ,"from",paste0("\\\"",lnameF,"\\\""))
         s <- paste("-sql",.dQuote(s))
      }
      else
         s <- ""
      cmd <- paste("ogr2ogr"
                  ,ifelse(verbose,"-progress",""),s
                  ,"-f",.dQuote(driver0)
                  ,ifelse(interimExt=="shp","",paste("-t_srs",.dQuote(p4s)))
                  ,b
                  ,.dQuote(fname0),.dQuote(fname),"-nln",lname)
      if (verbose)
         message(cmd)
      keepHDR <- length(envi_list(lname))
      if (keepHDR) {
         fhdr2 <- tempfile()
         fhdr1 <-paste0(lname,".hdr")
         file.rename(fhdr1,fhdr2)
      }
      system(cmd) ## this ov
      if (keepHDR) {
         file.rename(fhdr2,fhdr1)
      }
      if (file.exists(fname0)) {
         if (interimExt=="shp")
            .shp.remove(fname)
         else
            file.remove(fname)
      }
   }
   else if (driver=="ESRI Shapefile") {
      cpgname <- file.path(dname,paste0(lname,".cpg"))
      if (FALSE)
         writeLines("1251",cpgname)
      else {
         if (file.exists(cpgname))
            file.remove(cpgname)
      }
   }
   if (!nchar(compress))
      return(invisible(NULL))
   if ((.lgrep("gz",compress))&&(nchar(Sys.which("gzip"))))
      system2("gzip",c("-f",fname))
   else if (.lgrep("bz(ip)*2",compress)&&(nchar(Sys.which("bzip2"))))
      system2("bzip2",c("-f",fname))
   else if (.lgrep("xz",compress)&&(nchar(Sys.which("xz"))))
      system2("xz",c("-f",fname))
   else if (compress=="zip") {
      f <- .dir(path=dname
               ,pattern=paste0("^",lname,"\\.",ext,"$")
               ,full.names=TRUE)
      z <- paste0(fname,".zip")
      opW <- options(warn=-1)
      first <- TRUE
      for (i in seq(wait)) {
         if (!file.exists(z))
            break
         if (file.remove(z))
            break
         if (first) {
            cat(paste("Waiting for deleting",.sQuote(z)))
            first <- FALSE
         }
         cat(".")
         Sys.sleep(1)
      }
      if (!first) {
         if (i==wait)
            cat(" FAILURE!\n")
         else
            cat(" ok!\n")
      }
      options(opW)
      utils::zip(z,f,flags="-qm9j") ## verbose output ## 'myzip(z,f,keys="-m -9 -j")'
   }
   invisible(NULL)
}
