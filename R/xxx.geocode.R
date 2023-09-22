# http://geo2tag.org/?page_id=671&lang=en_US
# https://geocode-maps.yandex.ru/1.x/?geocode=metro Tretyakovskaya&results=100&format=json
# geocodeList <- eval(as.list(args(.geocode))$service)
'.geocode' <- function(loc=NULL,area=c("bounding","point","shape"),place=""
                      ,select=c("top","expand","all")
                      ,service=c("nominatim","pickpoint","google")
                      ,cache=TRUE,verbose=FALSE) {
   if (is.null(loc)) {
      if (!TRUE) {
         dst <- tempfile()
         download.file("http://ip-api.com/csv",dst,mode="wt")
         a <- readLines(dst,warn=FALSE)
         file.remove(dst)
         a <- unlist(strsplit(a,split=","))
         pt <- c(lon=as.numeric(a[9]),lat=as.numeric(a[10]))
      }
      else
         a <- readLines("http://ip-api.com/line")
      pt <- cbind(lon=as.numeric(a[9]),lat=as.numeric(a[8]))
      rownames(pt) <- "ip-api.com"
      return(pt)
   }
   area <- match.arg(area)
  # print(c(service=service,loc=loc))
   service <- match.arg(service)
   pSea <- "(Sea|\u043C\u043e\u0440\u0435)" ## 'MOPE' in Russian
   isSea <- .lgrep(paste0("(",pSea,"\\s|\\s",pSea,")"),loc)>0
   if ((area=="bounding")&&(isSea)&&(service=="nominatim")) {
      cat("More faults with 'nominatim' for 'sea' geocoding\n")
     # cat("Redirecting to 'google' for 'sea' geocoding\n")
     # service <- "google"
   }
   select <- match.arg(select)
   shape <- NULL
   if (service %in% c("nominatim","pickpoint")) {
     ## curl -H Accept-Language:de 'https://nominatim.openstreetmap.org......."
      if (service %in% "nominatim")
         endpoint <- "https://nominatim.openstreetmap.org/search.php?"
      else if (service %in% "pickpoint") {
         key <- getOption("pickpointKey")
         if (!is.character(key)) {
            message(paste("Unable to get 'pickpoint' API key using"
                         ,"`getOption(\"pickpointKey\")`."
                         ,"Switched to 'nominatim' geocoding."))
            service <- "nominatim"
            endpoint <- "https://nominatim.openstreetmap.org/search.php?"
         }
         else
            endpoint <- paste0("https://api.pickpoint.io/v1/forward?&key=",key,"&")
      }
      src <- paste0(endpoint,"q=",loc
                  # ,"&polygon_text=1"
                   ,"&format=xml","&bounded=0","&accept-language=en-US,ru")
     # dst <- tempfile() # "nominatim.xml" # tempfile()
     # print(src)
      dst <- .ursaCacheDownload(src,quiet=!verbose)
      xmlstring <- scan(dst,character(),quiet=!verbose)
     # Encoding(xmlstring) <- "UTF-8"
     # str(as.list(xmlstring))
     # if (dirname(dst)==tempdir())
     #    file.remove(dst)
      ind <- grep("geotext",xmlstring)
      if (length(ind)) {
         geotext <- xmlstring[ind]
         print(geotext)
      }
      ptype <- .grep("^type=",xmlstring,value=TRUE)
      ptype <- .gsub(".*(\'|\")(.+)(\'|\").*","\\2",ptype)
      pclass <- .grep("^class=",xmlstring,value=TRUE)
      pclass <- .gsub(".*(\'|\")(.+)(\'|\").*","\\2",pclass)
      prank <- .grep("^place_rank=",xmlstring,value=TRUE)
      prank <- .gsub(".*(\'|\")(.+)(\'|\").*","\\2",prank)
      if (FALSE) {
         prank[prank=="8"] <- "state"
         prank[prank=="10"] <- "region"
         prank[prank=="12"] <- "county"
         prank[prank=="16"] <- "city"
        # prank[prank=="17"] <- "town|island"
        # prank[prank=="18"] <- "village|"
        # prank[prank=="19"] <- ""
      }
      ptype <- paste(pclass,ptype,prank,sep=":")
      lon <- .grep("lon=",xmlstring,value=TRUE)
      lon <- as.numeric(.gsub(".*(\'|\")(.+)(\'|\").*","\\2",lon))
      lat <- .grep("lat=",xmlstring,value=TRUE)
      lat <- as.numeric(.gsub(".*(\'|\")(.+)(\'|\").*","\\2",lat))
      pt <- cbind(lon=lon,lat=lat)
      if (!nrow(pt))
         return(NULL)
      rownames(pt) <- ptype
      ind <- grep("boundingbox",xmlstring)
      bounding <- xmlstring[ind]#[1]
      bounding <- .gsub(".*\"(.+)\".*","\\1",bounding)
      bounding <- lapply(bounding,function(p){
         as.numeric(unlist(strsplit(p,split=",")))
      })
      bounding <- do.call(rbind,bounding)
      rownames(bounding) <- ptype
      colnames(bounding) <- c("miny","maxy","minx","maxx")
      ann <- .grep("display_name=",xmlstring,value=TRUE)
      ann <- .gsub(".*\"(.+)\".*","\\1",ann)
      importance <- .grep("importance",xmlstring,value=TRUE)
      importance <- as.numeric(.gsub(".*(\'|\")(.+)(\'|\").*","\\2",importance))
     # type <- NULL ## debug
      typeInd <- integer()
      if ((is.character(place))&&(nchar(place))) {
         typeInd <- which(!is.na(match(ptype,place)))
         typeInd <- .grep(place,ptype)
         if (length(typeInd)) {
            bounding <- bounding[typeInd,,drop=FALSE]
            pt <- pt[typeInd,,drop=FALSE]
            importance <- importance[typeInd]
         }
      }
      else if (is.numeric(place)) {
         print("numeric place -- 'place_rank' -- ignored (TODO)")
      }
      important <- which(importance==max(importance))[1]
      if (select=="top") {
         lat <- lat[important]
         lon <- lon[important]
         pt <- pt[important,,drop=FALSE]
         bounding <- bounding[important,,drop=FALSE]
      }
      if (select %in% c("expand","top")) {
         dg <- 1
         is180 <- (bounding[,"minx"]<=(-180+dg))&&(bounding[,"maxx"]>=(180-dg))
         isShape <- "shape" %in% area
         if ((isShape)||(is180)) {
            if (verbose)
               .elapsedTime("shape|180 -- download")
            if (T & !isShape) { ## to deprecate
               src <- paste0(endpoint,"q=",loc
                            ,"&polygon_text=1"
                            ,"&format=xml"
                            ,"&bounded=0","&accept-language=en-US,ru")
               dst <- .ursaCacheDownload(src,cache=cache,quiet=!verbose)
               if (verbose)
                  .elapsedTime("shape|180 -- parsing")
               b <- readLines(dst,encoding="UTF-8",warn=FALSE)
              # print(file.size(dst))
               b <- unlist(strsplit(b,split="'"))
               b <- .grep("(MULTI)*(POLYGON|LINESTRING)",b,value=TRUE,ignore.case=FALSE)
               b <- .gsub("(^.+(MULTI)*(POLYGON|LINESTRING)|\\(|\\))"," ",b)
               b <- .gsub("(^\\s+|\\s+(\".+)*$)","",b)
               b <- lapply(b,function(b2) {
                  b3 <- unlist(strsplit(b2,split=","))
                  b3 <- unlist(strsplit(b3,split="\\s+"))
                  b3 <- b3[nchar(b3)>0]
                  b3 <- matrix(as.numeric(b3),ncol=2,byrow=TRUE)[,1]
                  b4 <- b3
                  ind <- b4<0
                  if (length(ind))
                     b4[ind] <- b4[ind]+360
                  sd3 <- sd(b3)
                  sd4 <- sd(b4)
                  if (sd4<sd3)
                     ret <- c(minx=min(b3[b3>0]),maxx=max(b3[b3<0]))
                  else
                     ret <- c(minx=min(b3),maxx=max(b3))
                  ret
               })
               b <- do.call("rbind",b)
               ##~ b <- unlist(strsplit(b,split=","))
               ##~ b <- unlist(strsplit(b,split="\\s+"))
               ##~ b <- b[nchar(b)>0]
               ##~ b <- matrix(as.numeric(b),ncol=2,byrow=TRUE)[,1]
              # bounding[,"minx"] <- min(b[b>0])
              # bounding[,"maxx"] <- max(b[b<0])
               if (select=="top")
                  ind <- important
               else if (length(typeInd))
                  ind <- typeInd
               else
                  ind <- seq(nrow(bounding))
               bounding[,c("minx","maxx")] <- b[ind,c("minx","maxx")]
            }
            else {
               loaded <- loadedNamespaces()
               isSF <- "sf" %in% loaded ||
                  requireNamespace("sf",quietly=.isPackageInUse())
               if (!isSF) {
                  isSP <- "rgeos" %in% loaded || 
                     requireNamespace("rgeos",quietly=.isPackageInUse())
               }
               else
                  isSP <- FALSE
               isWKT <- isSF || isSP
               if (verbose)
                  print(c('sf'=isSF,'sp+rgeos'=isSP,'wkt'=isWKT))
               src <- paste0(endpoint,"q=",loc
                            ,"&polygon_",ifelse(isWKT,"text=1","geojson=1")
                            ,"&format=xml"
                            ,"&bounded=0","&accept-language=en-US,ru")
               dst <- .ursaCacheDownload(src,cache=cache,quiet=!verbose)
               if (verbose)
                  .elapsedTime("shape|180 -- parsing")
              # str(dst)
               b <- readLines(dst,encoding="UTF-8",warn=FALSE)[3]
              # ind1 <- unlist(gregexpr("geojson)=\\'\\{",b))
              # ind2 <- unlist(gregexpr("\\}\\'",b))
               ##~ ind1 <- gregexpr("geo(text|json)=\\'",b)
               ##~ ind2 <- gregexpr("(\\)|\\})\\'",b)
               ind1 <- unlist(gregexpr("geo(text|json)=\\'",b))
               ind2 <- unlist(gregexpr("(\\)|\\})\\'",b))
               ind3 <- which(ind1>0)
               ind4 <- which(ind2>0)
               if ((identical(ind3,ind4))&&(length(ind3)>0)) { ## ind3[1]==ind4[1]
                  ind1 <- ind1[ind3]
                  ind2 <- ind2[ind4]
                  shape <- lapply(seq_along(ind3),function(i) {
                     n1 <- nchar(.gsub2("(geo(text|json)=\\')","\\1",b))
                     b2 <- substr(b,ind1[i]+n1,ind2[i])
                     if (F)
                        return(b2)
                     if (!isWKT) {
                        if (T) ## or overwrite cached?
                           dst <- tempfile(fileext=".geojson")
                        Fout <- file(dst,encoding="UTF-8")
                        writeLines(b2,Fout)
                        close(Fout)
                        d2 <- spatial_read(dst,engine="sf")
                       # d2 <- sf::st_read(b2,quiet=TRUE)
                     }
                     else if (isSF) {
                        d2 <- data.frame(id=seq_along(b2))
                        d2$geom <- b2
                        d2 <- spatial_geometry(sf::st_as_sf(d2,wkt="geom"))
                     }
                     else if (isSP) {
                        d2 <- rgeos::readWKT(b2)
                     }
                     spatial_crs(d2) <- 4326
                     if (("top" %in% select)&&(is_spatial_points(d2)))
                        return(NULL)
                     if ("shape" %in% area)
                        return(d2)
                     if (is180) {
                        d2 <- spatialize(d2,style="merc")
                        bb2 <- matrix(spatial_bbox(d2),ncol=2,byrow=TRUE)
                        bb2 <- .project(bb2,spatial_crs(d2),inv=TRUE)
                        bb2 <- c(t(bb2))
                        names(bb2) <- c("xmin","ymin","xmax","ymax")
                     }
                     else
                        bb2 <- spatial_bbox(d2)
                    # print(bb2)
                    # d2 <- sf::st_cast(d2,"POINT")
                     bb2
                  })
                  if ("bounding" %in% area) {
                    # shape <- t(list2DF(shape))
                     if (T) {
                       # bname <- rownames(bounding)
                        bounding <- do.call("rbind",shape)
                       # rownames(bounding) <- bname
                     }
                     else {
                        bounding[,"minx"] <- shape[,"xmin"]
                        bounding[,"maxx"] <- shape[,"xmax"]
                     }
                  }
               }
            }
            if (verbose)
               .elapsedTime("shape|180 -- finish")
         }
         if (select=="top")
            ptype <- ptype[important]
      }
      if ((area=="shape")&&(!is.null(shape))) {
         if ("top" %in% select) {
            ind <- which(sapply(shape,function(x) !is.null(x)))
            if (length(ind)) {
               if ((length(typeInd))&&(!is.na(ind2 <- ind[typeInd])))
                  ind <- ind2
               shape <- shape[[ind[1]]]
              # session_grid(shape)
            }
         }
         return(shape)
      }
      if (area=="bounding") {
         bounding <- bounding[,c(3,1,4,2),drop=FALSE]
         if (FALSE) {
            print(bounding)
            da <- data.frame(lon=range(bounding[,c(3,4)])
                            ,lat=range(bounding[,c(1,2)]))#,z=1:2)
         }
         if (select=="expand")
            bounding <- c(minx=min(bounding[,1]),miny=min(bounding[,2])
                         ,maxx=max(bounding[,3]),maxy=max(bounding[,4]))
         if (select=="top") {
        #    attr(bounding,"type") <- ptype
            bounding <- bounding[1,,drop=TRUE]
         }
         return(bounding)
      }
      if (area=="point") {
         if (nrow(pt)==1) {
            ptype <- rownames(pt)
            ptname <- colnames(pt)
            pt <- c(pt)
            names(pt) <- ptname
           # attr(pt,"type") <- ptype
         }
         return(pt)
      }
   }
   else if (service=="google") {
      src <- paste0("https://maps.googleapis.com/maps/api/geocode/xml?"
                   ,"address=",loc)
      if (proposed <- TRUE) {
         apiKey <- getOption("googleMaps")
         if (is.character(apiKey))
            src <- paste0(src,"&key=",apiKey)
      }
      dst <- .ursaCacheDownload(src,cache=cache,quiet=!verbose)
      xmlstring <- scan(dst,character(),quiet=!verbose)
      istatus <- .grep("<status>",xmlstring)
      status <- .gsub("<status>(.+)</status>","\\1",xmlstring[istatus])
      if (status=="OVER_QUERY_LIMIT") {
         message(paste("Over query limit for Google geocoding."
                      ,"Try to specify `options(googleMaps=<YOUR_API>)`"))
         return(NULL)
      }
      ilat <- .grep("<lat>",xmlstring)
      ilon <- .grep("<lng>",xmlstring)
      glat <- as.numeric(.gsub("<lat>(.+)</lat>","\\1",xmlstring[ilat]))
      glon <- as.numeric(.gsub("<lng>(.+)</lng>","\\1",xmlstring[ilon]))
      aname <- .grep("(location|southwest|northeast)",xmlstring[c(ilat,ilon)-1]
                    ,value=TRUE)
      aname <- .gsub("(<|>)","",aname)
      bname <- .grep("<(viewport|bounds)>",xmlstring[c(ilat,ilon)-2]
                    ,value=TRUE)
      bname <- .gsub("(<|>)","",bname)
      iname <- .grep("<(viewport|bounds)>",xmlstring)
      names(glon) <- names(glat) <- aname
      if ((!length(glon))||(!length(glat))) {
        # loc <- RJSONIO::fromJSON("http://ip-api.com/json")
         return(NULL)
      }
      ptype <- .grep("<location_type>",xmlstring,value=TRUE)
      ptype <- .gsub("<.+>(.+)</.+>","\\1",ptype)
      pt <- cbind(lon=unname(glon[.grep("location",names(glon))])
                 ,lat=unname(glat[.grep("location",names(glat))]))
      attr(pt,"type") <- ptype
      glon <- glon[.grep("location",aname,invert=TRUE)]
      glat <- glat[.grep("location",aname,invert=TRUE)]
      lname <- paste0(rep(bname,each=2),".",names(glon))
      names(glon) <- names(glat) <- lname
      n <- length(bname)
      bbox <- cbind(minx=rep(NA,n),miny=rep(NA,n),maxx=rep(NA,n),maxy=rep(NA,n))
      rownames(bbox) <- bname
      ##~ for (i in seq_along(bname)) {
         ##~ bbox[i,"minx"] <- glon[.grep(paste0(bname[i],"\\.southwest"),lname)]
         ##~ bbox[i,"miny"] <- glat[.grep(paste0(bname[i],"\\.southwest"),lname)]
         ##~ bbox[i,"maxx"] <- glon[.grep(paste0(bname[i],"\\.northeast"),lname)]
         ##~ bbox[i,"maxy"] <- glat[.grep(paste0(bname[i],"\\.northeast"),lname)]
      ##~ }
      bbox[,"minx"] <- glon[.grep("\\.southwest",lname)]
      bbox[,"miny"] <- glat[.grep("\\.southwest",lname)]
      bbox[,"maxx"] <- glon[.grep("\\.northeast",lname)]
      bbox[,"maxy"] <- glat[.grep("\\.northeast",lname)]
      if (verbose) {
         print(pt)
         print(bbox)
      }
      if (area=="point") {
         if (nrow(pt)==1) {
            ptype <- rownames(pt)
            ptname <- colnames(pt)
            pt <- c(pt)
            names(pt) <- ptname
           # attr(pt,"type") <- ptype
         }
         return(pt)
      }
      else if (area=="bounding") {
         if (select %in% "top")
            return(bbox["viewport",])
         if (select %in% "expand")
            return(bbox["bounds",])
         return(bbox)
         if (FALSE) {
            glon <- range(glon)
            glat <- range(glat)
            dlon <- abs(diff(glon))
            dlat <- abs(diff(glat))
            if ((dlon<0.01)&&(dlat<0.01)) {
               sc <- abs(1/cos(mean(glat)))
               mul <- 1 # 3
               glon <- mean(glon)+mul*abs(diff(glon))*sc*c(-1,1)/2
               glat <- mean(glat)+mul*abs(diff(glat))*c(-1,1)/2
            }
            da <- data.frame(lon=range(glon),lat=range(glat))
         }
         return(bbox)
      }
   }
   return(NULL)
}
