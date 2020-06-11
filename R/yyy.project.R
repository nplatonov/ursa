'.project' <- function(xy,proj,inv=FALSE,verbose=!FALSE) {
   ## because of quicker load of 'proj4' package
  # show.error.messages=verbose
   if (isSF <- .isSF(xy)) {
      return(sf::st_transform(xy,proj))
   }
   else if (isSP <- .isSP(xy)) {
      return(sp::spTransform(xy,proj))
   }
  # print("---")
  # print(class(xy))
  # print("===")
   opW <- options(warn=-11,show.error.messages=verbose);on.exit(options(opW))
  # if (("package:rgdal" %in% search())||
  #     (!requireNamespace("proj4",quietly=.isPackageInUse())))
  #    res <- rgdal::project(xy=xy,proj=proj,inv=inv)
  # else
  #    res <- proj4::project(xy=t(xy),proj=proj,inverse=inv)
   a <- FALSE
  # print(summary(xy))
  # proj4 <- requireNamespace("proj4",quietly=.isPackageInUse())
  # print(loadedNamespaces())
   if (F) {
      if (.lgrep("^\\s*\\+init=epsg:\\d+\\s*$",proj)) {
        # proj <- .epsg2proj4(proj,force=TRUE,verbose=TRUE)
        # requireNamespace("rgdal",quietly=.isPackageInUse())
      }
   }
  # if ((!FALSE)&&(!("package:rgdal" %in% search()))&&
   if ((!FALSE)&&(!("rgdal" %in% loadedNamespaces()))&&
       (requireNamespace("proj4",quietly=.isPackageInUse()))) {
      if (F) {
         dimx <- dim(xy)
         if ((dimx==2)&&(dimx[1]==2)) {
            str(dimx)
         }
      }
      a <- .try({
        # str(xy)
        # str(list(xy[,1],xy[,2]))
        ## suppressMessages(require(proj4)) ## uncomment?
         if (utils::packageVersion("proj4")>="1.0.10")
            res <- proj4::project(xy=xy,proj=proj,inverse=inv)
         else
            res <- proj4::project(xy=t(xy),proj=proj,inverse=inv)
        # res <- proj4::project(xy=list(xy[,1],xy[,2]),proj=proj,inverse=inv)
      },silent=TRUE)
      if ((!FALSE)&&(!a)&&(nrow(xy)==2)) {
         if (verbose) {
            cat(geterrmessage())
            message("         trying to not transpose matrix")
         }
         a <- .try({
           ## suppressMessages(require(proj4)) ## uncomment?
            res <- proj4::project(xy=xy,proj=proj,inverse=inv)
         },silent=TRUE)
      }
   }
   if (!a) {
      if (!("rgdal" %in% loadedNamespaces()))
         requireNamespace("rgdal",quietly=.isPackageInUse())
      if (is.list(xy))
         xy <- cbind(xy[[1]],xy[[2]])
      else if (!is.matrix(xy))
         xy <- matrix(xy,ncol=2)
     # ind <- which((is.na(xy[,1]))|(is.na(xy[,2])))
      ind <- which(is.na(xy[,1])) ## less conditions
      if (length(ind)) {
         res <- matrix(NA,ncol=2,nrow=nrow(xy))
         a <- .try(res[-ind,] <- rgdal::project(xy=xy[-ind,],proj=proj,inv=inv))
      }
      else {
         a <- .try(res <- rgdal::project(xy=xy,proj=proj,inv=inv))
      }
      if (!a) {
         .epsg3411 <- paste("","+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1"
                           ,"+x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")
         if (requireNamespace("proj4",quietly=.isPackageInUse())) {
            a <- .try(res <- proj4::project(xy=xy,proj=.epsg3411))
            if (a) {
               a <- .try(res <- proj4::ptransform(res,.epsg3411,proj))
               if (a)
                  return(res)
            }
         }
      }
      if ((FALSE)&&(!a)) {
         str(xy)
         str(proj)
         if (inv) {
            xy <- as.data.frame(xy)
            sp::coordinates(xy) <- ~x+y
            sp::proj4string(xy) <- sp::CRS(proj,doCheckCRSArgs=FALSE)
            a <- .try(res <- sp::spTransform(xy,sp::CRS("+init=epsg:4326")))
            str(a)
            q()
         }
         q()
      }
   }
   if ((FALSE)&&(!inv)&&(.lgrep("\\+proj=merc",g1$proj))) {
      g1 <- session_grid()
      ext <- 20037508
      if (g1$maxx>ext) {
         ind <- which(res[,1]<0)
        # print("before:")
        # print(res)
         if (length(ind))
            res[ind,1] <- 2*ext+res[ind,1]
        # print("after:")
        # print(res)
      }
   }
   if (!exists("res"))
      return(NULL)
   return(res)
}
'.epsg2proj4' <- function(code,force=FALSE,verbose=FALSE) {
  ## 'proj4::project' doesnot understand pure EPSG
  # a <- try(as.integer(code),silent=TRUE)
   if (!.lgrep("\\D",code))
      p4epsg <- paste0("+init=epsg:",code)
   else if (.lgrep("^epsg:\\d+",code))
      p4epsg <- paste0("+init=",code)
   else if (.lgrep("^(\\s+)*\\+init=epsg:\\d+",code))
      p4epsg <- .gsub("^\\s+","",code)
   else if (is.character(code))
      return(code)
   else
      stop(code)
   loaded <- loadedNamespaces()
   if (!force) {
      if ("sf" %in% loaded) {
         if (verbose)
            message("force to use 'sf'")
         p4s <- sf::st_crs(code)$proj4string
      }
      else {
         if (verbose)
            message("force to use 'sp/rgdal'")
        # requireNamespace(c("sp","rgdal")[2],quietly=.isPackageInUse())
         p4s <- p4epsg
      }
   }
   else { ## force
      fail <- TRUE
      if (any(c("sp","rgdal") %in% loaded)) {
         if (verbose)
            message("'sp' loaded + ('rgdal' for reprojection - SKIPPED)")
         if (TRUE) {
            opW <- options(warn=ifelse(verbose,0,-1))
            p4s <- try(methods::slot(sp::CRS(p4epsg,doCheckCRSArgs=TRUE),"projargs"))
            options(opW)
         }
         else {
            ##~ p4s <- sp::CRS(p4epsg,doCheckCRSArgs=FALSE)
            ##~ print(comment(p4s))
            ##~ p4s <- meslot(p4s,"projargs")
            ##~ str(p4s)
           ##~ # sp::CRS(SRS_string=p4epsg)
            ##~ q()
            p4s <- try(methods::slot(sp::CRS(p4epsg,doCheckCRSArgs=FALSE),"projargs"))
         }
         if (!inherits(p4s,"try-error")) {
            fail <- FALSE
           # if (!("rgdal" %in% loaded))
           #    requireNamespace("rgdal",quietly=.isPackageInUse())
         }
      }
      if ((fail)&&("sf" %in% loaded)) {
         if (verbose)
            message("'sf' loaded")
        # p4s <- try(sf::st_crs(as.integer(code))$proj4string) ## 'code', not 'p4epsg'
         p4s <- try(sf::st_crs(.p4s2epsg(p4epsg))$proj4string)
         if (!inherits(p4s,"try-error"))
            fail <- FALSE
      }
      if (fail) {
         if (verbose)
            message("Otherwise, use 'sp' + ('rgdal' for reprojection - SKIPPED)")
         p4s <- try(methods::slot(sp::CRS(p4epsg,doCheckCRSArgs=FALSE),"projargs"))
         if (inherits(p4s,"try-error")) {
            fail <- TRUE
           # if (!("rgdal" %in% loaded))
           #    requireNamespace("rgdal",quietly=.isPackageInUse())
         }
         else
            fail <- FALSE
      }
      if ((fail)&&(!FALSE)&&(requireNamespace("sf",quietly=.isPackageInUse()))) {
         if (verbose)
            message("force to load 'sf'")
         p4s <- try(sf::st_crs(as.integer(code))$proj4string)
         if (!inherits(p4s,"try-error"))
            fail <- FALSE
      }
      if (fail) {
         p4s <- ""
         if (verbose)
            message("Unable to get PROJ.4 string from EPSG code")
      }
   }
   p4s
}
'.lonPlus360' <- function(xy) {
   if (scalar <- is.null(dim(xy))) {
      if (xy[1]<0)
         xy[1] <- xy[1]+360
   }
   else if (length(ind <- which(xy[,1]<0)))
      xy[ind,1] <- xy[ind,1]+360
   xy
}
'.p4s2epsg' <- function(p4s) {
   patt <- "\\+init=epsg\\:(\\d+)$"
   if (!length(grep(patt,p4s)))
      return(p4s)
   code <- gsub(patt,"\\1",p4s)
   if (code==p4s)
      return(p4s)
   as.integer(code)
}
