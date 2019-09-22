'spatialize' <- function(dsn,engine=c("native","sp","sf")
                         ,layer=".*",field=".+",coords=c("x","y"),crs=character()
                         ,geocode="",place="",area=c("bounding","point")
                         ,grid=NULL,size=NA,cell=NA,expand=1,border=NA
                         ,lat0=NA,lon0=NA,resetProj=FALSE,resetGrid=FALSE
                         ,style="auto" ## auto none internal keep
                        # ,zoom=NA
                         ,subset="",verbose=FALSE,...) {
   engine <- match.arg(engine)
   if (resetGrid)
      session_grid(NULL)
   toResetGrid <- 0L
  # print(c(expand=expand,border=border))
  # geocode <- match.arg(geocode)
   geocodeStatus <- FALSE
   hasOpened <- FALSE
   toUnloadMethods <- !("methods" %in% .loaded())
   if (is.na(resetProj))
      resetProj <- TRUE
   cpg <- NULL
   if (engine=="sp") {
      isSF <- FALSE
      isSP <- TRUE
   }
   else if (engine=="sf") {
      isSF <- requireNamespace("sf",quietly=.isPackageInUse())
      isSP <- !isSF
   }
   else {
      loaded <- loadedNamespaces() #.loaded()
      if ("sf" %in% loaded)
         isSF <- TRUE
      else if (("sp" %in% loaded)||("rgdal" %in% loaded))
         isSF <- FALSE
      else
         isSF <- requireNamespace("sf",quietly=.isPackageInUse())
      isSP <- !isSF
   }
   if ((length(crs))&&(style=="auto"))
      style <- .epsg2proj4(crs)
   else {
      crs2 <- .epsg2proj4(style)
      if (.lgrep("\\+(init|proj)",crs2))
         style <- crs2
   }
   isEPSG <- FALSE
  # isPROJ4 <- nchar(style)>36 ## need more accurate detecton of proj4
   isPROJ4 <- .lgrep("(\\+init=epsg\\:|\\+proj=\\w+|\\+(ellps|datum=))",style)>0
   if (length(style)) {
      if (is.numeric(style))
         isEPSG <- TRUE
      else if ((is.character(style))&&(!nchar(.gsub("\\d","",style))))
         isEPSG <- TRUE
   }
   if ((isEPSG)&&(is.numeric(style)))
      style <- as.character(style)
   if (isPROJ4 | isEPSG)
      session_grid(NULL)
   isNative <- engine=="native"
   if (is.character(dsn)) {
      if (length(dsn)>1) {
         pattern <- "\\.(gpkg|tab|kml|json|geojson|mif|sqlite|shp|osm)(\\.(zip|rar|gz|bz2))*$"
         dsn <- dsn[.grep(pattern,basename(dsn))]
         if (length(dsn)!=1)
            stop("Either filename is not recognized or multiple files")
      }
   }
   if (!((is.character(dsn))&&(length(dsn)==1))) {
      nextCheck <- TRUE
      if ((.isSF(dsn))||(.isSP(dsn))) {
         if ((resetProj)||(length(as.list(match.call())[-1])==1))
            session_grid(NULL)
      }
      if (FALSE) { ## 20180125--
         spcl <- paste0("Spatial",c("Points","Lines","Polygons"))
         spcl <- c(spcl,paste0(spcl,"DataFrame"))
      }
      else
         spcl <- "Spatial"
     # if ((nextCheck)&&(inherits(dsn,spcl))) {
      if ((nextCheck)&&(.isSP(dsn))) {
         if ((!toUnloadMethods)&&(!("methods" %in% .loaded()))) {
            if (FALSE) {
              # .require("methods")
               opW <- options(warn=1)
               warning("Package 'methods' is required for S4 object coercion.")
               options(opW)
            }
            toUnloadMethods <- TRUE
         }
         if ((!isNative)&&(isSF)) {
            obj <- sf::st_as_sf(dsn)
         }
         else {
            isSP <- TRUE
            isSF <- !isSP
            obj <- dsn
         }
         rm(dsn)
         nextCheck <- FALSE
      }
      if ((nextCheck)&&(inherits(dsn,"sf"))) {
         if (isNative) {
            isSF <- TRUE
            isSP <- !isSF
         }
         obj <- dsn
         rm(dsn)
         nextCheck <- FALSE
      }
      if ((nextCheck)&&(is.array(dsn))) {
         return(display(dsn,...))
      }
      if ((nextCheck)&&(is.ursa(dsn))) {
         if ((isSF)||(isSP)) {
            obj <- as.data.frame(dsn)
            if (style!="auto") {
               crsNow <- style
               session_grid(NULL)
            }
            else
               crsNow <- ursa_proj(dsn)
            isCRS <- ((!is.na(crsNow))&&(nchar(crsNow)))
            if (!((is.character(coords))&&(length(coords)==2))) {
               coords <- basename(.maketmp(2))
               colnames(obj)[1:2] <- coords
            }
            if (isSF) {
               if (isCRS)
                  obj <- sf::st_as_sf(obj,coords=coords,crs=crsNow)
               else
                  obj <- sf::st_as_sf(obj,coords=coords)
            }
            else if (isSP) {
               sp::coordinates(obj) <- coords
               if (isCRS)
                  sp::proj4string(obj) <- crsNow
            }
            rm(dsn) ## requierd?
            nextCheck <- FALSE
         }
         else
            return(display(dsn,...))
      }
      if ((nextCheck)&&(is.data.frame(dsn))&&
          (is.character(coords))&&(length(coords)==2)) {
         if (!all(coords %in% colnames(dsn))) {
            mname <- colnames(dsn)
            indX <- .grep("^coords.x1$",mname)
            if (!length(indX))
               indX <- .grep("^x$",mname)
            if (!length(indX))
               indX <- .grep("^lon",mname)
            if (!length(indX))
               indX <- .grep("^east",mname)
            if (!length(indX))
               indX <- .grep("^x1",mname)
            indY <- .grep("^coords.x2$",mname)
            if (!length(indY))
               indY <- .grep("^y$",mname)
            if (!length(indY))
               indY <- .grep("^lat",mname)
            if (!length(indY))
               indY <- .grep("^north",mname)
            if (!length(indY))
               indY <- .grep("^x2",mname)
            if ((!length(indX))&&(!length(indY))) {
               indX <- .grep("^000x1$",mname)
               indY <- .grep("^000x2$",mname)
            }
            if ((!length(indX))&&(!length(indY))) {
               indX <- .grep(paste0("^",coords[1],"$"),mname)
               indY <- .grep(paste0("^",coords[2],"$"),mname)
            }
            ind <- c(indX[1],indY[1])
            if ((any(is.na(ind)))||(length(ind)!=2))
               stop("unable to detect 'x' and 'y' coordinates")
            coords <- mname[ind]
         }
         #isCRS <- ((!is.na(crsNow))&&(nchar(crsNow)))
         if (style!="auto")
            crsNow <- style
         else
            crsNow <- NA
         isCRS <- ((!is.na(crsNow))&&(nchar(crsNow)))
         if (isSF) {
            if (isCRS)
               obj <- sf::st_as_sf(dsn,coords=coords,crs=crsNow)
            else
               obj <- sf::st_as_sf(dsn,coords=coords)
         }
         else if (isSP) {
            obj <- dsn
            sp::coordinates(obj) <- coords
            if (isCRS)
               sp::proj4string(obj) <- crsNow
         }
         rm(dsn)
         nextCheck <- FALSE
      }
      if ((FALSE)&&(nextCheck)&&((isSF)||(isNative))) { ## has checked early
         if (isNative) {
            loaded <- loadedNamespaces() #.loaded() ## changed 20180226
            if ("sf" %in% loaded)
               isSF <- TRUE
            else if (("sp" %in% loaded)||("rgdal" %in% loaded))
               isSF <- FALSE
            else
               isSF <- requireNamespace("sf",quietly=.isPackageInUse())
            isSP <- !isSF
         }
        # nextCheck <- isSP
      }
      if ((nextCheck)&&(isSF)) {
         if (is.array(dsn)) {
            message("process 'array' by 'sf' -- TODO (dilemma: raster is array)")
         }
         else if (is.numeric(dsn)) {
            proj4 <- attr(dsn,"proj4")
            limLonLat <- all(dsn>=-360 & dsn<=+360)
            if (length(dsn)==2) { ## point
              # obj <- sf::st_sfc(sf::st_point(dsn))
               obj <- sf::st_as_sf(data.frame(lon=dsn[1],lat=dsn[2])
                                  ,coords=c("lon","lat"))#,crs=4326)
            }
            else if (length(dsn)==4) { ## boundary
               if (dsn[1]>dsn[3])
                  dsn[1] <- dsn[1]-360
               obj <- matrix(dsn[c(1,2,1,4,3,4,3,2,1,2)],ncol=2,byrow=TRUE)
               if (TRUE) {
                  x <- obj[,1]
                  y <- obj[,2]
                  n <- 256
                  x <- c(seq(x[1],x[2],len=n),seq(x[2],x[3],len=n)
                        ,seq(x[3],x[4],len=n),seq(x[4],x[5],len=n))
                  y <- c(seq(y[1],y[2],len=n),seq(y[2],y[3],len=n)
                        ,seq(y[3],y[4],len=n),seq(y[4],y[5],len=n))
                  obj <- cbind(x,y)
               }
              # colnames(obj) <- c("lon","lat")
               ##~ obj <- sf::st_sf(sf::st_sfc(sf::st_polygon(list(obj)))
                               ##~ ,coords=c("lon","lat"),crs=4326)
               obj <- sf::st_sfc(sf::st_multilinestring(list(obj)))#,crs=4326)
              # obj <- sf::st_sf(obj)
            }
            else
               obj <- NULL
            if (!is.null(obj)) {
               if (limLonLat)
                  sf::st_crs(obj) <- 4326
               else if (!is.null(proj4)) {
                  sf::st_crs(obj) <- proj4
                  style <- proj4
                  session_grid(NULL)
               }
               else
                  sf::st_crs(obj) <- session_proj4()
            }
            else
               rm(obj)
         }
         else if (inherits(dsn,c("sfc","sf"))) {
            obj <- dsn
            dsn <- class(obj)
         }
         else {
            if ((TRUE)&&(inherits(dsn,"list"))&&(inherits(dsn,"data.frame"))) {
               if (length(unique(sapply(dsn,length)))==1)
                  dsn <- as.data.frame(dsn)
               cl <- sapply(dsn,function(x) {
                  if (!inherits(x,c("integer","numeric","character","factor"
                                   ,"Date","POSIXt")))
                     stop("TODO #38")
               })
            }
            obj <- try(sf::st_as_sf(dsn,coords=coords))
            if (inherits(obj,"try-error")) {
               cat(geterrmessage())
               message(paste("(#32) unable to process object of class"
                            ,.sQuote(paste(class(dsn),collapse=" | "))))
               return(NULL) ## 32L
            }
            else {
               limLonLat <- all(dsn[,coords]>=-360 & dsn[,coords]<=+360)
               if (limLonLat)
                  sf::st_crs(obj) <- 4326
            }
            dsn <- class(dsn)
         }
      }
      else if ((nextCheck)&&(isSP)) { # if (isSP)
         proj4 <- attr(dsn,"proj4")
         if (is.array(dsn))
            message("process 'array' by 'sp' -- TODO (dilemma: raster is array)")
         else if (is.numeric(dsn)) {
            limLonLat <- all(dsn>=-360 & dsn<=+360)
            if (length(dsn)==2) { ## point
               obj <- data.frame(lon=dsn[1],lat=dsn[2])
               sp::coordinates(obj) <- ~lon+lat
              # sp::proj4string(obj) <- sp::CRS("+init=epsg:4326")
            }
            else if (length(dsn)==4) { ## boundary
               if (dsn[1]>dsn[3])
                  dsn[1] <- dsn[1]-360
               obj <- matrix(dsn[c(1,2,1,4,3,4,3,2,1,2)],ncol=2,byrow=TRUE)
               if (TRUE) {
                  x <- obj[,1]
                  y <- obj[,2]
                  n <- 256
                  x <- c(seq(x[1],x[2],len=n),seq(x[2],x[3],len=n)
                        ,seq(x[3],x[4],len=n),seq(x[4],x[5],len=n))
                  y <- c(seq(y[1],y[2],len=n),seq(y[2],y[3],len=n)
                        ,seq(y[3],y[4],len=n),seq(y[4],y[5],len=n))
                  obj <- cbind(x,y)
               }
              # obj <- sp::SpatialLines(list(sp::Lines(sp::Line(obj),1L))
              #                        ,proj4string=sp::CRS("+init=epsg:4326"))
               obj <- sp::SpatialLines(list(sp::Lines(sp::Line(obj),1L)))
            }
           # else
           #    obj <- NULL
            if (limLonLat)
               sp::proj4string(obj) <- sp::CRS("+init=epsg:4326")
            else if (!is.null(proj4)) {
               sp::proj4string(obj) <-  sp::CRS(proj4)
               style <- proj4
               session_grid(NULL)
            }
            else
               sp::proj4string(obj) <- sp::CRS(session_proj4())
         }
         else if (inherits(dsn,"data.frame")) {
            obj <- dsn
            sp::coordinates(obj) <- coords
            limLonLat <- all(dsn[,coords]>=-360 & dsn[,coords]<=+360)
            if (limLonLat)
               sp::proj4string(obj) <- sp::CRS("+init=epsg:4326")
            dsn <- class(dsn)
         }
         else {
            obj <- try(methods::as(dsn,"Spatial"))
         }
         if (inherits(obj,"try-error")) {
            message(paste("(#33) unable to process object of class"
                   ,.sQuote(class(dsn))))
            return(NULL) ## 33L
         }
         else {
           # isSP <- TRUE
           # isSF <- !isSP
         }
      }
   }
   else {
      if ((FALSE)&&(isNative)) { ## this check has been done early
         loaded <- .loaded()
         if ("sf" %in% loaded)
            isSF <- TRUE
         else if (("sp" %in% loaded)||("rgdal" %in% loaded))
            isSF <- FALSE
         else
            isSF <- requireNamespace("sf",quietly=.isPackageInUse())
         isSP <- !isSF
      }
      if (file.exists(zname <- paste0(dsn,".gz"))) {
         dsn <- zname
      }
      else if (file.exists(zname <- paste0(dsn,".bz2"))) {
         dsn <- zname
      }
      if (!file.exists(dsn)) {
         list1 <- spatial_dir(path=dirname(dsn),pattern=basename(dsn))
         if (length(list1)==1)
            dsn <- list1
      }
      if (!file.exists(dsn)) {
         aname <- paste0(dsn,".zip")
         if (isZip <- file.exists(aname)) {
            ziplist <- unzip(aname,exdir=tempdir());on.exit(file.remove(ziplist))
            dsn <- .grep("\\.shp$",ziplist,value=TRUE)
         }
         else if (.lgrep("^(http|https|ftp)://",dsn)) {
            mode <- ifelse(.lgrep("(txt|json)$",dsn),"wt","wb")
            dsn <- .ursaCacheDownload(dsn,mode=mode)
         }
         else if (.lgrep("\\.(gpkg|tab|kml|json|geojson|mif|sqlite|shp|osm)(\\.(zip|gz|bz2))*$"
                 ,basename(dsn))) {
            message(paste("#40. It seems that specified non-existent file name"
                         ,sQuote(dsn),"rather than geocode request."))
            return(NULL)
         }
         else {
            geocodeArgs <- as.list(args(.geocode))
            geocodeList <- eval(geocodeArgs$service)
            if ((length(geocode)==1)&&(!nchar(geocode)))
               geocode <- geocodeList
            if (FALSE) {
               arglist <- list(...)
               if (length(ind <- .grep("area",names(arglist)))) {
                  area <- arglist[[ind]]
               }
               else
                  area <- eval(geocodeArgs$area)
            }
            da <- try(.geocode(dsn,service=geocode[1],place=place
                          ,area=area,select="top",verbose=verbose))
            if ((inherits(da,"try-error"))||((is.null(da))&&(length(geocode)==2))) {
               geocode <- geocode[2]
               da <- .geocode(dsn,service=geocode,area=area,select="top"
                             ,verbose=verbose)
            }
            else if (length(geocode)==2)
               geocode <- geocode[1]
            if (is.null(da)) {
               cat(paste("unable to geocode request",dQuote(dsn),"\n"))
               return(NULL)
            }
            if (is.null(dim(da))) {
               if (length(da)==4)
                  da <- da[c("minx","miny","maxx","maxy")]
               else if (length(da)==2) {
                  arglist <- as.list(match.call())
                  arglist$dsn <- da
                  return(do.call(as.character(arglist[[1]]),arglist[-1])) ## RECURSIVE!!!
               }
            }
            else
               da <- da[,c("minx","miny","maxx","maxy")]
            if (da[1]>da[3])
               da[3] <- da[3]+360
            da <- matrix(da[c(1,2,1,4,3,4,3,2,1,2)],ncol=2,byrow=TRUE)
            if (TRUE) {
               x <- da[,1]
               y <- da[,2]
               n <- 256
               x <- c(seq(x[1],x[2],len=n),seq(x[2],x[3],len=n)
                     ,seq(x[3],x[4],len=n),seq(x[4],x[5],len=n))
               y <- c(seq(y[1],y[2],len=n),seq(y[2],y[3],len=n)
                     ,seq(y[3],y[4],len=n),seq(y[4],y[5],len=n))
               da <- cbind(x,y)
            }
            if (isSF) {
               obj <- sf::st_sfc(sf::st_multilinestring(list(da)),crs=4326)
              # sf::st_write(obj,"rect.geojson");q()
            }
            if (isSP) {
               obj <- sp::SpatialLines(list(sp::Lines(sp::Line(da),1L))
                                      ,proj4string=sp::CRS("+init=epsg:4326"))
            }
            geocodeStatus <- TRUE
            hasOpened <- TRUE
         }
      }
      else {
         if (isZip <- .lgrep("\\.zip$",dsn)>0) {
            ziplist <- unzip(dsn,exdir=tempdir());on.exit(file.remove(ziplist))
            dsn <- .grep("\\.(shp|sqlite|gpkg|geojson)$",ziplist,value=TRUE)
         }
         else if ((nchar(Sys.which("gzip")))&&(isZip <- .lgrep("\\.gz$",dsn)>0)) {
            dsn0 <- dsn
            dsn <- tempfile();on.exit(file.remove(dsn))
            system2("gzip",c("-f -d -c",.dQuote(dsn0)),stdout=dsn,stderr=FALSE)
         }
         else if ((nchar(Sys.which("bzip2")))&&(isZip <- .lgrep("\\.bz2$",dsn)>0)) {
            dsn0 <- dsn
            dsn <- tempfile();on.exit(file.remove(dsn))
            system2("bzip2",c("-f -d -c",.dQuote(dsn0)),stdout=dsn,stderr=FALSE)
         }
         else if ((T)&&(nchar(Sys.which("7z")))&&(isZip <- .lgrep("\\.rar$",dsn)>0)) {
            stop(dsn,": this archive type is not supported")
            ziplist <- system(paste("7z","l","-scsUTF-8",dsn),intern=TRUE)
            ind <- .grep("-{19}\\s",ziplist)
            ziplist <- ziplist[(ind[1]+1):(ind[2]-1)]
            ziplist <- substr(ziplist,54L,nchar(ziplist))
            ziplist <- .gsub("\\\\","/",ziplist)
            dir1 <- dir(path=tempdir())
           # print(dir1)
            system(paste("7z","e","-aos",.dQuote(dsn),paste0("-o",tempdir())))
            dir2 <- dir(path=tempdir())
           # print(dir2)
            print(ziplist)
            q()
            print(dir(path=tempdir()))
            file.remove(ziplist)
            q()
            rarlist <- system(paste("rar","lb",.dQuote(dsn)),intern=TRUE)
            print(rarlist)
            system2("rar",c("e -o+",.dQuote(dsn),tempdir()),stdout=NULL)
           # print(rarlist)
           # file.remove(rarlist)
            stop("RAR")
         }
         if (isCDF <- .lgrep("\\.(nc|hdf)$",dsn)>0) {
            obj <- .read_nc(dsn,".+")
            if (!inherits(obj,"data.frame"))
               obj <- as.data.frame(as.ursa(obj[sapply(obj,is.ursa)]))
            else {
               indX <- .grep("^(lon|x$|west|east)",colnames(obj))
               indY <- .grep("^(lat|y$|south|north)",colnames(obj))
               if (!length(indX))
                  indX <- 1L
               if (!length(indY))
                  indY <- 2L
               colnames(obj)[c(indX,indY)] <- c("x","y")
            }
            p4s <- attr(obj,"proj4")
            if (isSF) {
               if (!is.null(p4s))
                  obj <- sf::st_as_sf(obj,coords=c("x","y"),crs=attr(obj,"proj4"))
               else
                  obj <- sf::st_as_sf(obj,coords=c("x","y"))
            }
            if (isSP) {
               sp::coordinates(obj) <- ~x+y
               if (!is.null(p4s))
                  sp::proj4string(obj) <- sp::CRS(p4s)
            }
            hasOpened <- TRUE
           # display(a)
         }
      }
      if ((!hasOpened)&&((!geocodeStatus)||(file.exists(dsn)))) {
         opW <- options(warn=ifelse(isSP,-1,0))
         if (isSF)
            lname <- try(sf::st_layers(dsn)$name)
         else {
            lname <- try(rgdal::ogrListLayers(dsn))
         }
         if (inherits(lname,"try-error")) {
            cat("Cannot get layers\n")
            return(NULL)
         }
         if (!is.character(layer))
            layer <- lname[layer[1]]
         else
            layer <- .grep(layer,lname,value=TRUE)
         if (length(layer)>1) {
            print(paste("Select only one layer:",paste(paste0(seq(layer),")")
                                       ,.sQuote(layer),collapse=", ")),quote=FALSE)
            return(NULL)
         }
         if (isSF) {
           # opW2 <- options(warn=0)
            obj <- sf::st_read(dsn,layer=layer,quiet=TRUE)
           # options(opW2)
            if (!spatial_count(obj))
               return(obj)
            if (TRUE)
               obj <- sf::st_zm(obj,drop=TRUE)
         }
         else {
            if (isSHP <- .lgrep("\\.shp$",dsn)>0) {
               cpgname <- .gsub("\\.shp$",".cpg",dsn)
               if (file.exists(cpgname)) {
                  cpg <- readLines(cpgname,warn=FALSE)
               }
               else
                  cpg <- "UTF-8"
            }
            else {
               cpg <- "UTF-8"
            }
            ##~ obj <- rgdal::readOGR(dsn,layer,pointDropZ=TRUE,encoding=enc
                                 ##~ ,use_iconv=!is.null(enc),verbose=FALSE)
            obj <- rgdal::readOGR(dsn,layer,pointDropZ=TRUE,encoding=cpg
                                 ,use_iconv=!isSHP,verbose=FALSE)
            if ((length(names(obj))==1)&&(names(obj)=="FID")) {
               info <- rgdal::ogrInfo(dsn,layer)
               if (info$nitems==0)
                  methods::slot(obj,"data")$FID <- NULL
            }
         }
         options(opW)
      }
   }
   if (style=="keep") {
      crs <- spatial_crs(obj)
     # style <- crs
     # print(data.frame(style=style,crs=crs,isEPSG=isEPSG,isPROJ4=isPROJ4))
     # print(resetProj)
   }
  # resetProj <- FALSE
   if (verbose)
      print(c('engine.sf'=isSF,'engine.sp'=isSP))
   if (nchar(subset)) {
      obj <- do.call("subset",list(obj,parse(text=subset)))
   }
   if ((geocodeStatus)&&(style=="auto")) {
      style <- switch(geocode,nominatim=c("mapnik","openstreetmap color")[1]
                             ,google="google terrain color")
   }
   if ((toUnloadMethods)&&("package:methods" %in% search())) {
     # print(search())
      detach("package:methods",unload=FALSE) 
     # but namespace "methods" is not unloaded, because namespace "sp" is loaded
     # 'as' is not found now
   }
   if (FALSE) { ## deprecated
      if (isSF) {
         dname <- try(names(sf::st_agr(obj)),silent=TRUE)
         if (inherits(dname,"try-error"))
            dname <- character()
      }
      else if (isSP) {
         dname <- try(colnames(methods::slot(obj,"data")),silent=TRUE)
         if (inherits(dname,"try-error"))
            dname <- character()
      }
   }
   else
      dname <- spatial_fields(obj)
   dname0 <- dname
   hasTable <- length(dname)>0
   if (is.na(field)[1])
      field <- ".+"
   else if (length(field)>1)
      field <- paste0("^(",paste(field,collapse="|"),")$")
   dname <- .grep(field,dname,value=TRUE)
  # str(dname);q()
   if ((hasTable)&&(!length(dname))) {
      message("unable to get fields by name")
     # str(asf)
     # return(invisible(20L))
   }
   if ((!identical(dname0,dname))&&(length(dname))) {
      if (isSF)
         obj <- obj[,dname]
      if (isSP) {
         obj <- obj[,dname]
      }
   }
   if (hasTable) {
      if (isSF)
         cl <- lapply(obj,class)[dname]
      else if (isSP)
         cl <- lapply(methods::slot(obj,"data"),class)[dname]
      for (i in seq_along(dname)) {
         cl2 <- cl[i]
         if (isSF) {
            da <- obj[,dname[i],drop=TRUE] ## sf>=0.5
           # da <- obj[,dname[i],drop=TRUE][,,drop=TRUE] ## sf>=0.4
           # str(da)
         }
         if (isSP) {
            da <- methods::slot(obj,"data")[,dname[i],drop=TRUE]
         }
         if (is.character(da)) {
           # str(dname[i])
            isDateTime <- FALSE
            skipParse <- TRUE
           # if (dname[i]=="time")
           #    str(da)
            nc <- try(nchar(na.omit(da)))
            if (inherits(nc,"try-error")) {
               message("Data field ",.sQuote(dname[i])," has problem:")
               str(da)
               warning("Check specified encoding for input data table")
            }
            if ((dev <- TRUE)&&(length(unique(nc))==1)&&
                  (.lgrep("\\d{4}.*\\d{2}.*\\d{2}",da))) {
               nNA <- length(which(is.na(da)))
               s <- sapply(gregexpr("(-|\\.|/)",da),function(x) length(x[x>=0]))
               if (all(s>=2)) {
                  s <- sapply(gregexpr("(-|\\.|/)",da),function(x) diff(x)[1])
                  if (all(s==3))
                     da <- .gsub(".*(\\d{4})(.?)(\\d{2})(.?)(\\d{2})(.*)"
                                ,"\\1-\\3-\\5\\6",da)
               }
               else if (FALSE & length(grep("(\\d{8})($|\\D.*$)",da))==length(da)) {
                  da <- .gsub(".*(\\d{4})(.?)(\\d{2})(.?)(\\d{2})(.+)*"
                             ,"\\1-\\3-\\5\\6 6='\\6' 7='\\7'",da)
               }
               else
                  skipParse <- TRUE
               if (!skipParse) {
                 # da <- .gsub(".*(\\d{4})(.?)(\\d{2})(.?)(\\d{2})(.*)","\\1-\\3-\\5\\6",da)
                  a <- as.POSIXct(as.POSIXlt(da,format="%Y-%m-%dT%H:%M:%SZ",tz="UTC"))
                  if (all(is.na(a))) {
                     a <- as.POSIXct(da,tz="",format="%Y-%m-%d %H:%M:%S")
                  }
                  if (all(is.na(a))) {
                     a <- as.POSIXct(da,tz="",format="%Y-%m-%d %H:%M")
                  }
                  if (all(is.na(a))) {
                     a <- as.POSIXct(da,tz="",format="%Y-%m-%dT%H:%M")
                  }
                  if (all(is.na(a))) {
                     a <- as.Date(da,format="%Y-%m-%d")
                  }
                  if (length(which(is.na(a)))==length(which(is.na(da)))) {
                     da <- a
                     rm(a)
                     if (nchar(tz <- Sys.getenv("TZ"))) {
                        da <- as.POSIXct(as.POSIXlt(da,tz=tz))
                     }
                     isDateTime <- TRUE
                  }
               }
            }
           # if (dname[i]=="time")
           #    str(da)
            if (!isDateTime) {
              # da <- iconv(da,to="UTF-8")
              # Encoding(da) <- "UTF-8"
              # if (is.null(cpg)) ## ++ 20180527
              #    Encoding(da) <- "UTF-8"
            }
           ## if inherits(da,"POSIXlt") then 'da' is a list with 9 items
            if (isSF)
               obj[,dname[i]] <- da
            if (isSP) {
               if (!inherits(da,c("Date","POSIXct"))) {
                  opW <- options(warn=-1)
                  da2 <- as.numeric(da)
                  options(opW)
                  if ((!anyNA(da2))&&(length(grep("^0.+",da))==0)) {
                     da <- if (.is.integer(da2)) as.integer(round(da2)) else da2
                  }
               }
               methods::slot(obj,"data")[,dname[i]] <- da
            }
         }
         else if (TRUE) {
            isInt <- !is.factor(da) && .is.integer(na.omit(da))
           # isInt <- .is.integer(da)
            if (isInt) { # &&(!is.integer(da))
               da <- as.integer(round(da))
               if (isSF)
                  obj[,dname[i]] <- da
               if (isSP)
                  methods::slot(obj,"data")[,dname[i]] <- da
            }
         }
         else if ((FALSE)&&(isSP)) {
            if (.is.integer(na.omit(da))) {
               methods::slot(obj,"data")[,dname[i]] <- as.integer(da)
            }
         }
      }
     # Sys.setlocale("LC_CTYPE",lc)
     # str(asf)
   }
   if (!exists("obj")) {
      stop("Object cannot be recognized as spatial")
   }
   if (isSF) {
      if (TRUE) { ## not tested for multiple geometries POLYGON/MULTIPOLYGON
         if (inherits(obj,"sfc"))
            geoType <- .grep("^sfc_.+$",class(obj),value=TRUE)
         else
            geoType <- .grep("^sfc_.+$",class(obj[[attr(obj,"sf_column")]]),value=TRUE)
         geoType <- .gsub("^sfc_","",geoType)
         if (geoType=="GEOMETRY")
            geoType <- unique(as.character(sf::st_geometry_type(obj)))
      }
      else { ## low perfomance for long geometry
         geoType <- unique(as.character(sf::st_geometry_type(obj)))
      }
   }
   if (isSP)
      geoType <- switch(class(sp::geometry(obj))
                       ,SpatialPolygons="POLYGON"
                       ,SpatialPoints="POINT"
                       ,SpatialLines="LINE")
   if (("POLYGON" %in% geoType)&&("MULTIPOLYGON" %in% geoType)) {
      if (isSF) {
         ret <- .try(obj <- sf::st_cast(obj,"MULTIPOLYGON"))
      }
      if (isSP) {
         stop("POLYGON to MULTIPOLYGON for 'Spatial' is not implemented")
      }
   }
   projClass <- c("longlat","stere","laea","merc")
   projPatt <- paste0("(",paste(projClass,collapse="|"),")")
   staticMap <- c("openstreetmap","google","sputnikmap")
   tilePatt <- paste0("(",paste0(unique(c(staticMap,.tileService())),collapse="|"),")")
   len <- 640L
   if (is.na(size[1]))
      size <- c(len,len)
   else if (is.character(size)) {
      size <- as.integer(unlist(strsplit(
                   .gsub("(\\d+)\\D+(\\d+)","\\1 \\2",size),split="\\s")))
   }
   else if (is.numeric(size))
      size <- rep(size,length=2)
   if (is.numeric(size))
      len <- as.integer(round(max(size)))
   g2 <- getOption("ursaSessionGrid")
   if (any(is.na(border))) {
      if ((!is.null(g2))||(!is.null(grid)))
         border <- 0L
      else
         border <- 27L
   }
   if (!.lgrep("(none|auto|keep)",style))
      resetProj <- TRUE
   if (isEPSG)
      resetProj <- FALSE
  # q()
  # if ((proj=="internal")&&(!is.na(keepProj))) {
  #    g2 <- NULL
  # }
   if (resetProj)
      g0 <- NULL
   else if ((is.null(grid))&&(!is.null(g2)))
      g0 <- g2
   else if (is.null(grid))
      g0 <- NULL
   else {
      g0 <- ursa_grid(grid)
   }
  # style <- "merc"
   if (!.lgrep(projPatt,style))
      proj <- "auto"
   else
      proj <- .gsub2(projPatt,"\\1",style)
   if (!.lgrep(tilePatt,style))
      art <- "none"
   else {
      art <- .gsub2(tilePatt,"\\1",style)
      proj <- "merc" #ifelse(art=="polarmap",art,"merc")
   }
   isStatic <- .lgrep("static",style)>0
   mlen <- switch(art,google=640,openstreetmap=960,sputnikmap=640)
   if (isStatic) {
      len[len>mlen] <- mlen
   }
  # canTile <- .lgrep(art,eval(as.list(args(".tileService"))$server))>0
   canTile <- .lgrep(art,.tileService())>0
   isTile <- .lgrep("(tile|polarmap)",style)>0 & canTile
   if ((!isStatic)&&(!isTile)) {
      if (art %in% staticMap)
         isStatic <- TRUE
      else if (canTile)
         isTile <- TRUE
      else
         art <- "none"
   }
   tpat <- unlist(gregexpr("\\{[xyz]\\}",style))
   tpat <- length(tpat[tpat>0])
   toZoom <- (isTile)||(isStatic)||(style=="web")||.lgrep("^tile",style)||
             (style=="polarmap")||
             (tpat==3)
  # isColor <- .lgrep("colo(u)*r",style)>0
   isWeb <- .lgrep(tilePatt,art)
   if (verbose)
      print(data.frame(proj=proj,art=art,static=isStatic
                      ,canTile=canTile,tile=isTile,web=isWeb))
  # isOSM <- proj %in% "osm"
  # isGoogle <- proj %in% "google"
  # http://static-api.maps.sputnik.ru/v1/?width=400&height=400&z=6&clng=179&clat=70
  #                                &apikey=5032f91e8da6431d8605-f9c0c9a00357
  # isWeb <- isOSM | isGoogle | tryTile
   if ((is.null(g0))||(is.numeric(lon0))||(is.numeric(lat0))) {
  # if ((resetProj)||(is.ursa(g0,"grid"))||(is.numeric(lon0))||(is.numeric(lat0))) {
      if (isSF)
         proj4 <- sf::st_crs(obj)$proj4string
      if (isSP)
         proj4 <- sp::proj4string(obj)
      if ((is.na(proj4))&&(nchar(style))&&(.lgrep("\\+proj=.+",style))) { ## ++ 20180530
         proj4 <- style
        # isPROJ4 <- FALSE
      }
      if ((proj4=="")&&(!(proj %in% c("auto","internal","keep")))) {
         resetProj <- TRUE
         proj4 <- "auto"
      }
      isLonLat <- .lgrep("(\\+proj=longlat|epsg:4326)",proj4)>0
      if ((proj %in% c("auto"))&&(isLonLat)&&(!isEPSG)&&(style!="keep")) { ## added 2016-08-09
         resetProj <- TRUE
         proj4 <- "auto"
      }
      isMerc <- .lgrep("\\+proj=merc",proj4)>0
      if (isMerc) {
         major <- .gsub(".+\\+a=(\\S+)\\s.+","\\1",proj4) ## 20037508
         if (identical(major,proj4)) {
            if (.lgrep("\\+(datum|ellps)=WGS84",proj4))
               B <- 20037508
            else ## yandex?
               B <- 20037508
           # print(B)
         }
         else
            B <- as.numeric(major)*pi
      }
      if (isPROJ4)
         resetProj <- FALSE
      if (is.numeric(lon0) | is.numeric(lat0) | resetProj) {
         if (isSF) {
            asf2 <- sf::st_transform(obj,4326)
            asf_geom2 <- sf::st_geometry(asf2)
            xy <- lapply(asf_geom2,function(z) {
               if (!is.list(z)) {
                  if (is.null(dim(z))) {
                     d <- ifelse(inherits(z,"XYZ"),3,2)
                     dim(z) <- c(length(z)/d,d)
                  }
                  z <- list(z)
               }
               xy2 <- lapply(z,function(z2) {
                  if (!is.list(z2))
                     z2 <- list(z2)
                  unlist(t(z2[[1]])[1:2,])
               })
            })
            rm(asf2,asf_geom2)
         }
         if (isSP) {
            asp2 <- sp::spTransform(obj,"+init=epsg:4326")
            if (geoType=="POINT") {
               xy <- sp::coordinates(asp2)
            }
            else {
               asp2_geom <- switch(geoType,POLYGON=methods::slot(sp::geometry(asp2),"polygons")
                                            ,LINE=methods::slot(sp::geometry(asp2),"lines")
                                           ,POINT=sp::geometry(asp2))
               xy <- lapply(asp2_geom,function(z) {
                  gz <- switch(geoType
                              ,POLYGON=methods::slot(z,"Polygons")
                              ,LINE=methods::slot(z,"Lines")
                              ,POINT=methods::slot(z,"Points"))
                  lapply(gz,function(z3) t(sp::coordinates(z3)))
               })
               rm(asp2,asp2_geom)
            }
         }
         if (is.list(xy)) {
            xy <- unlist(xy)
            if (is.null(xy)) {
               if (verbose)
                  cat("Spatial object is NULL")
               return(NULL)
            }
            xy <- matrix(c(xy),ncol=2,byrow=TRUE)
         }
         if (verbose)
            print(summary(xy))
         lon2 <- xy[,1]
         lat2 <- xy[,2]
         if (nrow(xy)>1) {
           # x <- cos(lon2*pi/180)
           # y <- sin(lon2*pi/180)
           # x <- mean(x)
           # y <- mean(y)
           # print(c(x=mean(x),y=mean(y)))
           # theta <- rep(0,length(x))
           # ind <- which(x!=0)
           # theta[-ind] <- pi/2*sign(y[-ind])
           # theta[ind] <- atan(y[ind]/x[ind])
           # ind <- which(x<0)
           # theta[ind] <- pi+theta[ind]
           # theta <- theta-pi
           # theta <- theta*180/pi
           # theta <- mean(theta)
           # lon2 <- range(lon2)
           # lon2 <- theta*pi/180
           # if (theta>180)
           #    theta <- theta-360
           # else if (theta<=(-180))
           #    theta <- theta+360
            lon3 <- lon2
            lon4 <- lon2
            ind3 <- which(lon3<0)
            ind4 <- which(lon4>180)
            lon3[ind3] <- lon3[ind3]+360
            lon4[ind4] <- lon4[ind4]-360
            lon5 <- lon2+360
            sd2 <- sd(lon2)
            sd3 <- sd(lon3)
            sd4 <- sd(lon4)
            sd5 <- sd(lon5)
            dr2 <- diff(range(lon2))
            dr3 <- diff(range(lon3))
            dr4 <- diff(range(lon4))
            dr5 <- diff(range(lon5))
            dr0 <- min(c(dr2,dr3,dr4,dr5))
            if (verbose)
               print(data.frame(r2=diff(range(lon2))
                               ,r3=diff(range(lon3))
                               ,r4=diff(range(lon4))
                               ,r5=diff(range(lon5))))
            if (verbose)
               print(data.frame(sd2=sd2,'sd3R'=sd3,'sd4L'=sd4,'sd5C'=sd5
                               ,n3=length(ind3),n4=length(ind4)))
            if ((sd3<=sd2)&&(sd3<=sd4)&&(dr3==dr0)) {
              # if (length(ind3))
              #    selection <- 3L
               lon2 <- lon3
            }
            else if ((sd4<=sd2)&&(sd4<=sd3)&&(dr4==dr0)) {
              # if (length(ind4))
              #    selection <- 4L
               lon2 <- lon4
            }
         }
        # if ((any(lon2<180))&&(any(lon2>180)))
        #    selection <- 3L
         if (verbose)
            print(summary(lon2))
        # selection <- 0L
         if ((FALSE)&&(mean(lon2)>0))
            lon2[lon2<0] <- lon2[lon2<0]+360
         bbox <- c(range(lon2),range(lat2))[c(1,3,2,4)]
        # options(ursaRasterizeSelection=selection)
        # options(ursaRasterizeBbox=bbox)
         theta2 <- mean(range(lon2))
        # print(c(old=theta2,new=theta))
        # lon_0 <- if (is.numeric(lon0)) lon0 else mean(range(lon2))
         lon_0 <- if (is.numeric(lon0)) lon0 else round(theta2,4)
         lat_ts <- if (is.numeric(lat0)) lat0 else round(mean(lat2),4)
         lat_0 <- if (lat_ts>=0) 90 else -90
         if (verbose)
            print(c(lon0=lon_0,lat0=lat_0,lat_ts=lat_ts))
         if ((!FALSE)&&(proj=="merc")&&(.lgrep("(polarmap|tile357[123456])",style)))
            proj <- "laea"
         else if (proj=="auto") {
            if (("web" %in% style)||(tpat==3))
           # if (style=="web")
               proj <- "merc"
            else if (.lgrep("tile3857",style))
               proj <- "merc"
            else if (.lgrep("(polarmap|tile357[123456])",style))
               proj <- "laea"
            else if ((any(lat2<0))&&(any(lat2>0)))
               proj <- "merc"
            else if (isEPSG)
               proj <- "epsg"
            else
               proj <- "stere"
         }
         if (verbose)
            print(c(lon_0=lon_0,lat_ts=lat_ts))
         if (proj=="stere") {
            t_srs <- paste("","+proj=stere"
                          ,paste0("+lat_0=",lat_0)
                          ,paste0("+lat_ts=",lat_ts)
                          ,paste0("+lon_0=",lon_0)
                          ,"+k=1","+x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
         }
         else if (proj=="laea") {
            if (style=="polarmap") {
               lon_0[lon_0<(-165) || lon_0>=(+135)] <- -180
               lon_0[lon_0>=(-165) && lon_0<(-125)] <- -150
               lon_0[lon_0>=(-125) && lon_0<(-70)] <- -100
               lon_0[lon_0>=(-70) && lon_0<(-25)] <- -40
               lon_0[lon_0>=(-25) && lon_0<(+50)] <- 10
               lon_0[lon_0>=(50) && lon_0<(+135)] <- 90
            }
            t_srs <- paste("","+proj=laea"
                          ,paste0("+lat_0=",lat_0)
                          ,paste0("+lon_0=",lon_0)
                          ,"+k=1","+x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
         }
         else if (proj=="merc")
            t_srs <- paste("","+proj=merc +a=6378137 +b=6378137"
                          ,"+lat_ts=0.0",paste0("+lon_0=",lon_0)
                          ,"+x_0=0.0 +y_0=0 +k=1.0"
                          ,"+units=m +nadgrids=@null +wktext +no_defs")
         else if ((proj %in% c("longlat"))||(isLonLat)) {
            t_srs <- "+proj=longlat +datum=WGS84 +no_defs"
         }
         else if (proj %in% c("zzzgoogle")) {
            if (FALSE)#(selection %in% c(1000L,3L))
               t_srs <- paste("","+proj=merc +a=6378137 +b=6378137"
                             ,"+lat_ts=0.0 +lon_0=180.0 +x_0=0.0 +y_0=0 +k=1.0"
                             ,"+units=m +nadgrids=@null +wktext +no_defs")
            else
               t_srs <- paste("","+proj=merc +a=6378137 +b=6378137"
                             ,"+lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0"
                             ,"+units=m +nadgrids=@null +wktext +no_defs")
         }
         else
            t_srs <- NULL
         if (is.character(t_srs)) {
           # bbox <- ursa:::.project(matrix(spatial_bbox(a),ncol=2,byrow=TRUE)
           #                        ,proj=spatial_crs(a),inv=TRUE)
            if (isSF) {
               obj <- sf::st_transform(obj,t_srs)
               if ((TRUE)&&(.lgrep("\\+proj=longlat",t_srs))&&(max(lon2)>180)) {
                  if (verbose)
                     .elapsedTime("lon+360 -- start")
                  objG0 <- spatial_geometry(obj)
                  objG1 <- lapply(objG0,function(g1) {
                     if (!is.list(g1))
                        return(.lonPlus360(g1))
                     ret1 <- lapply(g1,function(g2) {
                        if (!is.list(g2))
                           return(.lonPlus360(g2))
                        lapply(g2,.lonPlus360)
                     })
                     attributes(ret1) <- attributes(g1)
                     ret1
                  })
                  attributes(objG1) <- attributes(objG0)
                  spatial_geometry(obj) <- objG1
                  if (verbose)
                     .elapsedTime("lon+360 -- finish")
               }
            }
            if (isSP) {
               obj <- sp::spTransform(obj,t_srs)
               if ((TRUE)&&(.lgrep("\\+proj=longlat",t_srs))&&(max(lon2)>180)) {
                  if (verbose)
                     .elapsedTime("lon+360 -- start")
                  geoType <- spatial_geotype(obj)
                  if (geoType=="POINT") {
                     methods::slot(obj,"coords") <- .lonPlus360(methods::slot(obj,"coords"))
                  }
                  else {
                     objG0 <- spatial_geometry(obj)
                     objG1 <- switch(geoType
                                    ,POLYGON=methods::slot(objG0,"polygons")
                                    ,LINE=methods::slot(objG0,"lines")
                                    ,POINT=g0)
                     xy <- lapply(objG1,function(z) {
                        gz <- switch(geoType
                                    ,POLYGON=methods::slot(z,"Polygons")
                                    ,LINE=methods::slot(z,"Lines")
                                    ,POINT=methods::slot(z,"Points"))
                        gz <- lapply(gz,function(z3) {
                           methods::slot(z3,"coords") <- .lonPlus360(methods::slot(z3,"coords"))
                           z3
                        })
                        if (geoType=="POLYGON")
                           methods::slot(z,"Polygons") <- gz
                        else if (geoType=="LINE")
                           methods::slot(z,"Lines") <- gz
                        else if (geoType=="POINT")
                           methods::slot(z,"Points") <- gz
                        z
                     })
                     if (geoType=="POLYGON")
                        methods::slot(objG0,"polygons") <- xy
                     else if (geoType=="LINE")
                        methods::slot(objG0,"lines") <- xy ## gz? (20190921)
                     else if (geoType=="POINT")
                        objG0 <- xy ## gz?  (20190921)
                     spatial_geometry(obj) <- objG0
                  }
                  if (verbose)
                     .elapsedTime("lon+360 -- finish")
               }
            }
         }
        # xy <- .project(xy,t_srs)
        # print(summary(xy))
      }
      else if (isEPSG | isPROJ4) {
         a <- .try({
            t_srs <- ifelse(isPROJ4,style,.epsg2proj4(style))
           # str(style)
           # str(t_srs)
            if (isSF) {
              # str(sf::st_crs(obj))
               obj <- sf::st_transform(obj,t_srs)
            }
            if (isSP) {
              # str(sp::proj4string(obj))
               obj <- sp::spTransform(obj,t_srs)
            }
         })
         if (!a) {
            t_srs <- ifelse(isPROJ4,style,.epsg2proj4(style,force=TRUE))
            if (isSF) {
               obj <- sf::st_transform(obj,t_srs)
            }
            if (isSP) {
               obj <- sp::spTransform(obj,t_srs)
            }
         }
      }
   }
  # else if (((isSF)&&(is.ursa(a,"grid")))||((isSP)&&(is.ursa(asp,"grid")))) {
   else if (is.ursa(g0,"grid")) {
      t_srs <- g0$proj4
      if (isSF) {
        # opE <- options(show.error.messages=TRUE)
        # print(sf::st_bbox(obj))
         src0 <- sf::st_crs(obj)$proj4string
         if (!is.na(src0))
            obj <- sf::st_transform(obj,t_srs)
        # print(sf::st_crs(obj)$proj4string)
        # print(sf::st_bbox(obj))
        # options(opE)
      }
      if (isSP) {
         src0 <- sp::proj4string(obj)
        # print(c(sp::bbox(obj)))
         if (!is.na(src0))
            obj <- sp::spTransform(obj,t_srs) ## not tested
        # print(sp::proj4string(obj))
        # print(c(sp::bbox(obj)))
      }
   }
   if (FALSE) { ## deprecated
      if (isSF) {
         obj_geom <- sf::st_geometry(obj)
        # bbox <- c(sf::st_bbox(obj_geom)) ## low performance sp<=0.5-2
         if (inherits(obj,"sfc"))
            bbox <- c(sf::st_bbox(obj_geom))
         else
            bbox <- attr(obj[[attr(obj,"sf_column")]],"bbox") ## ~ sf::st_bbox
         proj4 <- sf::st_crs(obj)$proj4string
      }
      if (isSP) {
         obj_geom <- switch(geoType,POLYGON=methods::slot(sp::geometry(obj),"polygons")
                                      ,LINE=methods::slot(sp::geometry(obj),"lines")
                                     ,POINT=sp::geometry(obj))
         bbox <- c(sp::bbox(obj))
         if (length(bbox)==6) {
            bbox <- bbox[c(1,2,4,5)]
           # names(bbox) <- c("xmin","ymin","zmin","xmax","ymax","zmax")
         }
         names(bbox) <- c("xmin","ymin","xmax","ymax")
         proj4 <- sp::proj4string(obj)
      }
   }
   else {
      bbox <- spatial_bbox(obj)
      proj4 <- spatial_crs(obj)
      obj_geom <- spatial_geometry(obj)
      if (isSP)
         obj_geom <- switch(geoType,POLYGON=methods::slot(obj_geom,"polygons")
                                      ,LINE=methods::slot(obj_geom,"lines")
                                     ,POINT=obj_geom)
   }
   if ((bbox["xmin"]==bbox["xmax"])||(bbox["ymin"]==bbox["ymax"]))
      bbox <- bbox+100*c(-1,-1,1,1)
   if (FALSE) {
      .sc <- ifelse(.lgrep("\\+proj=(zzzlonglat|zzzmerc)",proj4)>0,0,expand-1)
      indx <- c("minx","maxx")
      bbox[indx] <- mean(bbox[c(1,3)])+c(-1,1)*expand*diff(bbox[c(1,3)])/2
      bbox[c(2,4)] <- mean(bbox[c(2,4)])+c(-1,1)*expand*diff(bbox[c(2,4)])/2
   }
   else {
      .sc <- (expand-1)*sqrt(diff(bbox[c(1,3)])*diff(bbox[c(2,4)]))/2
      bbox <- bbox+c(-1,-1,1,1)*.sc
   }
   if (is.null(g0)) {
      if (!is.na(cell)) {
         res <- rep(cell,length=2)
         g0 <- regrid(bbox=unname(bbox[c("xmin","ymin","xmax","ymax")]),res=res
                     ,proj4=proj4,border=0)
      }
      else {
         res <- max(c(bbox["xmax"]-bbox["xmin"]),(bbox["ymax"]-bbox["ymin"]))/len
         p <- pretty(res)
         res <- p[which.min(abs(res-p))]
         g1 <- ursa_grid()
         g1$resx <- g1$resy <- as.numeric(res)
         g1$proj4 <- proj4
         g0 <- regrid(g1,bbox=unname(bbox[c("xmin","ymin","xmax","ymax")])
                     ,border=0) ## border=border
      }
   }
   if (toZoom) {
      res <- with(g0,sqrt(resx*resy))
      s <- 2*6378137*pi/(2^(1:21+8))
      zoom <- which.min(abs(s-res))
      g0 <- regrid(g0,res=s[zoom])
   }
   if (any(border!=0)) {
      g0 <- regrid(g0,border=border)
   }
   if ((FALSE)&&(isWeb)) {
      bbox <- with(g0,.project(cbind(c(minx,maxx),c(miny,maxy))
                              ,proj4,inv=TRUE))[c(1,3,2,4)]
      basemap <- .geomap(bbox,border=0,style=style,verbose=verbose)
      g0 <- ursa(basemap,"grid")
      attr(obj,"basemap") <- basemap
   }
   if (is.null(g2))
      session_grid(g0)
   if (how_to_cancel_it <- !FALSE) {
      attr(obj,"grid") <- g0
      attr(obj,"toUnloadMethods") <- toUnloadMethods
      attr(obj,"colnames") <- dname
      attr(obj,"style") <- style
      attr(obj,"geocodeStatus") <- geocodeStatus
     # attr(obj,"engine") <- ifelse(isSF,"sf","sp")
      if (exists("dsn"))
         attr(obj,"dsn") <- dsn
   }
  # class(obj) <- c(class(obj),"ursaVectorExternal")
   obj
}
