'print.ursaCRS' <- function(x,...) {
  # if (!.crsForceProj4())
  #    cat("'ursaCRS' ")
  # x <- unclass(.crsBeauty(x))
   class(x) <- "character"
   if (.crsForceProj4()) {
      x <- .proj4string(x)
   }
   if (.isWKT(x)) {
      return(cat(x,...,"\n"))
   }
   print(x,...)
}
'str.ursaCRS' <- function(object,...) {
   if (isP4 <- .isProj4(object))
      comment(object) <- NULL
   if ((!.crsForceProj4())&&(!isP4)&&(!.isPackageInUse()))
      cat(" 'ursaCRS'")
   str(unclass(.crsBeauty(object))) #.crsBeauty(object,...)
}
'.ursaCRS' <- function(crs) {
   if (T & .crsForceProj4()) {
      crs <- .proj4string(crs)
   }
   if (F & .crsForceWKT())
      crs <- .WKT(crs)
   class(crs) <- c("ursaCRS","character")
   crs
}
'.isUrsaCRS' <- function(crs) inherits(crs,"ursaCRS")
'.crsSemiMajor' <- function(crs) {
   if (missing(crs))
      crs <- session_crs()
   if (.isWKT(crs)) {
     # .elapsedTime("B1")
      B <- NA
      a <- strsplit(crs,split="(\\]|\\[|,)")[[1]]
      if (length(ind <- grep("^SPHEROID",a))==1) {
         opW <- options(warn=-1)
         B <- as.numeric(a[ind+2])
         options(opW)
        # if (is.na(B))
        #    B <- 6378137
      }
      if ((is.na(B))&&(isNamespaceLoaded("sf"))) {
         B <- as.numeric(sf::st_crs(crs)$SemiMajor,units="m")
      }
      if (is.na(B)) {
         opW <- options(warn=-1)
         warning("SemiMajor was not determed. Used from `WGS84`")
         options(opW)
         B <- 6378137
      }
     # .elapsedTime("B2")
      return(B)
   }
   ell <- .gsub(".*\\+ellps=(\\S+)\\s.*","\\1",crs)
   if (ell=="WGS84")
      B <- 6378137
   else if (ell==crs) {
      B <- .gsub(".*\\+a=(\\S+)\\s.*","\\1",crs)
      if (B!=crs)
         B <- as.numeric(B)
      else {
         opW <- options(warn=-1)
         warning("Supposed that this projection is not supported yet")
         options(opW)
         B <- 6378137
      }
   }
   else {
      opW <- options(warn=-1)
      warning("Supposed that this projection is not supported yet")
      options(opW)
      B <- 6378137
   }
   B
}
'.isCRS' <- function(crs) {
   if (is.na(crs))
      return(FALSE)
   if (!nchar(crs))
      return(FALSE)
   if (grepl("^(ENGCRS)\\[",crs))
      return(FALSE)
   if (grepl("^(EPSG:\\d+|ESRI:\\d+|WGS84)$",crs))
      return(TRUE)
   TRUE # .isProj4(crs) | .isWKT(crs)
}
'.isWKT' <- function(crs) {
   if (missing(crs))
      crs <- session_crs()
   isTRUE(grepl("(^|\\[\n\\s+)(BOUNDCRS|PROJCS|PROJCRS|GEOGCS|GEOGCRS|ENGCRS)\\[",crs))
}
'.isWKT2' <- function(crs) {
   if (missing(crs))
      crs <- session_crs()
   isTRUE(grepl("(^|\\[\n\\s+)(PROJCRS|GEOGCRS)\\[",crs))
}
'.isLongLat' <- function(crs) {
   if (missing(crs))
      crs <- session_crs()
   if (.isWKT(crs)) {
      if (ret <- grepl("^(PROJCS|PROJCRS)\\[",crs,))
         return(!ret)
      if (ret <- grepl("^(GEOGCS|GEOGCRS)\\[",crs))
         return(ret)
      a1 <- strsplit(crs,split="(\\n\\s+|,|\\[|\\])")[[1]]
      ret <- grepl("^(GEOGCS|GEOGCRS)$",head(a1,7))
      return(any(ret))
   }
   .lgrep("(\\+proj=longlat|epsg:4326)",crs)>0
}
'.isMerc' <- function(crs) {
  # if (missing(crs))
  #    crs <- session_crs()
   "merc" %in% .crsProj(crs)
}
'.isProj4' <- function(crs) {
   if (missing(crs))
      crs <- session_crs()
   (!.isWKT(crs))&&(grepl("\\+proj=",crs))
}
'.isEPSG' <- function(crs) {
   if (missing(crs))
      crs <- session_crs()
   grepl("EPSG:\\d+",crs)
}
'.crsProj' <- function(crs) {
   unknown <- ""
   if (missing(crs))
      crs <- session_crs()
   if (inherits(crs,"crs")) {
      crs <- .WKT(crs)
   }
   else if (.isProj4(crs)) {
      return(.gsub(".+proj=(\\S+)\\s.+","\\1",crs))
   }
   else if (!.isWKT(crs)) {
      return(unknown)
   }
   if (.isLongLat(crs))
      return("longlat")
   a1 <- strsplit(crs,split="(\\n\\s+|,|\\[|\\])")[[1]]
   if (length(ind <- grep("(PROJECTION|METHOD)",a1))>0) {
      a2 <- a1[ind[1]+1L]
      if (.lgrep("stereographic",a2))
         res <- "stere"
      else if (.lgrep("transverse.*mercator",a2))
         res <- c("tmerc","utm")[1]
      else if (.lgrep("mercator",a2)) ## after tmerc
         res <- "merc"
      else if (.lgrep("Cylindrical.*Equal.*Area",a2))
         res <- "cea"
      else if (.lgrep("Equirectangular",a2))
         res <- "eqc"
      else if (.lgrep("Equidistant.*Cylindric",a2))
         res <- "eqc"
      else if (.lgrep("Equidistant.*Conic",a2))
         res <- "eqdc"
      else if (.lgrep("Lambert.*(Conformal.*Conic|Conic.*Conformal)",a2))
         res <- "lcc"
      else if (.lgrep("Lambert.*Azimuthal.*Equal.*Area",a2))
         res <- "laea"
      else {
         if (!.isPackageInUse()) {
            opW <- options(warn=1)
            warning(paste("Need to recognize Projection class from:",a2))
            options(opW)
         }
         res <- "undefined"
      }
   }
   else
      res <- unknown
   res
}
'.crsLon0' <- function(crs) {
   if (missing(crs))
      crs <- session_crs()
   if (!.isWKT(crs))
      return(as.numeric(.gsub2("\\+lon_0=(\\S+)\\s","\\1",crs)))
   if (.isLongLat(crs))
      return(NA)
   a1 <- strsplit(crs,split="(\\n\\s+|,|\\[|\\])")[[1]]
   patt <- c("Longitude\\sof\\s(natural\\s|false\\s)*origin"
            ,"[Cc]entral_[Mm]eridian"
            ,"longitude_of_center")
   if (length(ind <- grep(paste0("(",paste(patt,collapse="|"),")"),a1))>0) {
      opW <- options(warn=1)
      a2 <- as.numeric(a1[ind+1L])
      options(opW)
   }
   else {
      a2 <- NA
   }
  # if ((is.na(a2))&&(isNamespaceLoaded("sf")))
  #    return(.crsLon0(sf::st_crs(crs)$proj4string)) ## RECURSIVE
   a2
}
'.crsLat0' <- function(crs) {
   if (missing(crs))
      crs <- session_crs()
   if (!.isWKT(crs)) {
      patt <- "\\+lat_0=(\\S+)\\s"
      if (!grepl(patt,crs))
         return(NA)
      return(as.numeric(.gsub2(patt,"\\1",crs)))
   }
   if (.isLongLat(crs))
      return(NA)
   a1 <- strsplit(crs,split="(,|\\[|\\])")[[1]]
   projClass <- .crsProj(crs)
   patt <- c("Latitude\\sof\\s(natural|false)\\sorigin"
            ,"latitude_of_center"
            ,"latitude_of_origin"
            ,"Latitude_Of_Origin"
            )
   if (projClass %in% "stere")
      patt <- c(patt,c("Latitude\\sof\\sstandard\\sparallel"
                      ,"[Ss]tandard_[Pp]arallel"))
   if (length(ind <- grep(paste0("(",paste(patt,collapse="|"),")"),a1))==1) {
      opW <- options(warn=1)
      a2 <- as.numeric(a1[ind+1L])
      options(opW)
      if (projClass %in% c("stere"))
         a2 <- 90*sign(a2)
   }
   else {
      if (projClass %in% "tmerc") {
         a2 <- NA
      }
      else if (projClass %in% "merc") {
         a2 <- 0
      }
      else {
         a2 <- NA
      }
   }
  # if ((is.na(a2))&&(isNamespaceLoaded("sf")))
  #    return(.crsLat0(sf::st_crs(crs)$proj4string)) ## RECURSIVE
   a2
}
'.crsLatTS' <- function(crs) {
   if (missing(crs))
      crs <- session_crs()
   if (!.isWKT(crs)) {
      a2 <- .gsub2("\\+lat_(ts|1|2)=(\\S+)\\s","\\2",crs)
      a2 <- ifelse(a2==crs,0,as.numeric(a2))
      return(a2)
   }
   if (.isLongLat(crs))
      return(NA)
   a1 <- strsplit(crs,split="(\\n\\s+|,|\\[|\\])")[[1]]
   projClass <- .crsProj(crs)
   patt <- c("Latitude\\sof\\s(1st\\s|2nd\\s)*standard\\sparallel"
            ,"[Ss]tandard_[Pp]arallel_[12]")
   if (projClass %in% c("stere"))
      patt <- c(patt,"latitude_of_origin")
   if (length(ind <- grep(paste0("(",paste(patt,collapse="|"),")"),a1))>0) {
      opW <- options(warn=1)
      a2 <- as.numeric(a1[ind+1L])
      options(opW)
   }
   else {
      lat0 <- .crsLat0(crs)
      if (isTRUE(abs(lat0)==90))
         a2 <- lat0
      else if (.crsProj(crs) %in% c("tmerc")) # "laea" "merc"
         a2 <- lat0
      else
         a2 <- NA
   }
   if ((is.na(a2))&&(projClass %in% c("merc")))
      a2 <- .crsLat0(crs)
  # if ((is.na(a2))&&(isNamespaceLoaded("sf")))
  #    return(.crsLatTS(sf::st_crs(crs)$proj4string)) ## RECURSIVE
   a2
}
'.crsName' <- function(crs) {
   if (missing(crs))
      crs <- session_crs()
   else if (!.isWKT(crs)) {
      if ((!.isProj4(crs))&&(nchar(crs)>64))
         return("unknown")
      crs <- .WKT(crs)
   }
   a1 <- strsplit(crs,split="(\\n\\s+|,|\\[|\\])")[[1]]
  # print(head(a1,288))
   if (length(ind <- grep("^(PROJCS|PROJCRS|GEOGCS|GEOGCRS)$",a1))>0) {
      a2 <- a1[ind[1]+1L]
      return(gsub("(^\"|\"$)","",a2))
   }
   "notfound"
}
'.crsBeauty' <- function(crs,extended=FALSE) {
   digits <- ifelse(extended,6,2)
   if (missing(crs))
      crs <- session_crs()
   else if (is_spatial(crs))
      crs <- spatial_crs(crs)
   else if (is_ursa(crs))
      crs <- ursa_crs(crs)
   if (!.isCRS(crs))
      return("")
   if (!inherits(crs,"ursaCRS"))
      crs <- .ursaCRS(crs)
   if (!nchar(crs))
      return(crs)
   if (.isProj4(crs)) {
      if (!extended)
         return(crs)
   }
   if (.crsForceProj4())
      return(.proj4string(crs))
   if (!.isWKT(crs)) {
      crs <- .WKT(crs)
   }
  # if (.isLongLat(crs))
  #    return("WGS 84")
   pname <- .crsName(crs)
   if ((!extended)&&(pname!="unknown"))
      return(pname)
   a1 <- strsplit(crs,split="(\\n\\s+|,|\\[|\\])")[[1]]
  # print(a1,quote=FALSE)
   if (length(ind <- grep("(ELLIPSOID|SPHEROID)",a1))>0) {
      ellps <- gsub("^\"|\"$","",a1[ind[1]+1L])
      rf <- sprintf(paste0("%.",digits,"f"),as.numeric(gsub("^\"|\"$","",a1[ind[1]+3L])))
   }
   else {
      ellps <- ""
      rf <- paste0("0.",paste0(rep("0",digits),collapse=""))
   }
   if (length(ind <- grep("DATUM",a1))>0) {
      datum <- gsub("^\"|\"$","",a1[ind[1]+1L])
   }
   else
      datum <- ""
   ellps[ellps=="WGS_1984"] <- "WGS 84"
   if (F)
      res <- data.frame(foo="bar"
                       ,proj=.crsProj(crs)
                       ,datum=datum
                       ,ellps=ellps
                      # ,name=.pname
                       ,a=.crsSemiMajor(crs)
                       ,rf=rf
                       ,lon_0=.crsLon0(crs)
                       ,lat_0=.crsLat0(crs)
                       ,lat_ts=.crsLatTS(crs)
                      )
   ret <- projClass <- .crsProj(crs)
   if (projClass=="longlat") {
      if (extended)
         return(ret)
      return(.ursaCRS(ret))
   }
   ret <- paste(ret,paste0("lon_0=",round(.crsLon0(crs),digits))
                   ,paste0("lat_0=",round(.crsLat0(crs),digits)))
   if (all(!is.na(lat_ts <- .crsLatTS(crs)))) {
      ret <- paste(ret,paste0("lat_ts=",paste(round(lat_ts,digits),collapse=",")))
   }
   if (ellps=="unknown")
      ret <- paste(ret,paste0("a=",.crsSemiMajor(crs),"+",rf))
   else
      ret <- paste(ret,sQuote(ellps))
   if (extended)
      return(ret)
   .ursaCRS(ret)
}
'.proj4string' <- function(crs) {
   if (missing(crs))
      crs <- session_crs()
   if (inherits(crs,"crs")) {
      if (.isProj4(crs$input))
         return(crs$input)
      crs <- crs$wkt
   }
   if (isTRUE(crs==""))
      return("")
   if (.isProj4(crs))
      return(crs)
   ret <- sf::st_crs(unclass(crs))$proj4string
   if (is.na(ret))
      return("")
   ret
}
'.WKT' <- function(crs,WKT2=TRUE) {
  # isWKT2 <- !FALSE ## failed for writting
   if (missing(crs))
      crs <- session_crs()
   if (inherits(crs,"crs")) {## sf::
      if (WKT2)
         return(crs$wkt)
      return(crs$Wkt)
   }
   if (WKT2)
      return(sf::st_crs(crs)$wkt)
   return(sf::st_crs(crs)$Wkt)
}
'.crsWGS84' <- function() {
   if ((!.crsForceWKT())||(.crsForceProj4()))
      return("+proj=longlat +datum=WGS84 +no_defs")
   ret <- paste0('GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563]],'
      ,'PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],'
      ,'AXIS["Latitude",NORTH],AXIS["Longitude",EAST],AUTHORITY["EPSG","4326"]]')
   .ursaCRS(ret)
}
'.crsWGS84simple' <- function() "+proj=longlat +datum=WGS84 +no_defs"
# '.crsForceProj4' <- function() isTRUE(getOption("ursaProj4Legacy"))
'.crsForceProj4' <- function(value) {
   if (missing(value))
      return(isTRUE(getOption("ursaProj4Legacy")))
   options(ursaProj4Legacy=value)
   invisible(value)
}
# '.crsWeakWKT' <- function() isFALSE(getOption("ursaForceWKT"))
'.crsForceWKT' <- function(value) {
   if (missing(value)) {
      return(!isFALSE(getOption("ursaForceWKT")))
   }
   options(ursaForceWKT=value)
   invisible(value)
}
'.identicalCRS' <- function(src,dst) {
   identical(.crsBeauty(src,extended=TRUE),.crsBeauty(dst,extended=TRUE))   
}
