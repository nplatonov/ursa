'polygonize' <- function(obj,fname,engine=c("native","sp","sf")
                        ,verbose=NA,...) {
   engine <- match.arg(engine)
   if (engine=="sp") {
      isSF <- FALSE
      isSP <- TRUE
   }
   else if (engine=="sf") {
      isSF <- requireNamespace("sf",quietly=.isPackageInUse())
      isSP <- !isSF
   }
   else {
      loaded <- loadedNamespaces() # .loaded()
      if ("sf" %in% loaded)
         isSF <- TRUE
      else if (("sp" %in% loaded)||("rgdal" %in% loaded))
         isSF <- FALSE
      else
         isSF <- requireNamespace("sf",quietly=.isPackageInUse())
      isSP <- !isSF
   }
   if ((!is.na(verbose))&&(verbose))
      print(c(isSP=isSP,isSF=isSF))
   if (is.na(verbose))
      verbose <- isSP
   g0 <- session_grid()
   if ((!missing(obj))&&(is.numeric(obj))&&(length(obj)==4)&&
       (!anyNA(match(names(obj),c("minx","maxx","miny","maxy"))))) {
      obj <- regrid(bbox=unname(obj),dim=c(1,1),proj4=session_crs())
   }
   onlyGeometry <- missing(obj) || .is.grid(obj)
   isList <- !onlyGeometry && .is.ursa_stack(obj)
   if ((!isList)&&(!((onlyGeometry)||(is.ursa(obj))||(is.data.frame(obj))))) ## propose?
  # if ((!isList)&&(!(is.ursa(obj))))
      return(NULL)
  # requireNamespace("sp",quietly=.isPackageInUse())
  # requireNamespace("methods",quietly=.isPackageInUse())
   g1 <- if (onlyGeometry) {
      if (missing(obj))
         session_grid()
      else 
         ursa(obj,"grid")
   }
   else if (isList)
      ursa_grid(obj[[1]])
   else
      ursa_grid(obj)
   if (is.null(g1))
      g1 <- g0
   prj <- ursa_proj(g1)
   if (onlyGeometry)
      b <- as.data.frame(g1)
   else if (isList) {
      for (i in seq_along(obj)) {
         b0 <- as.data.frame(obj[[i]],na.rm=FALSE)
         b <- if (i==1) b0 else cbind(b,b0[,3:ncol(b0),drop=FALSE])
      }
      ind <- which(apply(b[,3:ncol(b),drop=FALSE],1,function(x) all(is.na(x))))
      if (length(ind))
         b <- b[-ind,]
   }
   else if (is.ursa(obj))
      b <- as.data.frame(obj)
   else
      b <- obj
  # b <- b[sample(seq(nrow(b)),1e3),]
   n <- nrow(b)
   sa <- vector("list",n)
   dx <- g1$resx/2
   dy <- g1$resy/2
   if (isSF) {
      if (verbose)
         cat("create polygons from points...")
      xy <- cbind(b$x-dx,b$y-dy,b$x-dx,b$y+dy,b$x+dx,b$y+dy,b$x+dx,b$y-dy
                 ,b$x-dx,b$y-dy)
      if (!onlyGeometry) {
         b <- b[,3:ncol(b),drop=FALSE]
         for (i in seq(ncol(b))) {
            if ((!is.factor(b[,i]))&&(.is.integer(na.omit(b[,i]))))
               b[,i] <- as.integer(b[,i])
         }
      }
      sa <- apply(xy,1,function(x) {
         res <- list(matrix(x,ncol=2,byrow=TRUE))
         class(res) <- c("XY","POLYGON","sfg")
        ## check for new versions of 'sf':
        # res2 <- sf::st_polygon(res);print(identical(res,res2));q()
         res
      })
      if (verbose)
         cat(" done!\ncreate geometry...")
      sa <- sf::st_sfc(sa)
      if (verbose)
         cat(" done!\n")
      if (!onlyGeometry) {
         if (verbose)
            cat("assign data to geometry...")
         sa <- sf::st_sf(b,coords=sa,crs=g1$proj4)
         if (verbose)
            cat(" done!\n")
      }
      else
         sf::st_crs(sa) <- g1$proj4
      if ((TRUE)&&(!onlyGeometry)) { ## ++ 20171128
         colnames(sa)[head(seq(ncol(sa)),-1)] <- colnames(b)
      }
   }
   else if (isSP) {
      if (verbose) {
         pb <- ursaProgressBar(min=0,max=n)
         i <- -1
         repeat({
            n1 <- n*(10^i)
            if (n1<100)
               break
            i <- i-1
         })
         n1 <- 10^(-i-1)
      }
      for (i in seq(n))
      {
         x <- b$x[i]+c(-dx,-dx,+dx,+dx,-dx)
         y <- b$y[i]+c(-dy,+dy,+dy,-dy,-dy)
         sa[[i]] <- sp::Polygons(list(sp::Polygon(cbind(x,y))),i)
         if ((verbose)&&(i%%n1==0))
            setUrsaProgressBar(pb,i)
      }
      if (verbose)
         close(pb)
      sa <- sp::SpatialPolygons(sa,proj4string=sp::CRS(prj))
      if (!onlyGeometry) {
         b <- b[,3:ncol(b),drop=FALSE]
         for (i in seq(ncol(b)))
            if ((!is.factor(b[,i]))&&(.is.integer(na.omit(b[,i]))))
               b[,i] <- as.integer(b[,i])
         sa <- sp::SpatialPolygonsDataFrame(sa,data=b,match.ID=FALSE)
      }
      if (verbose)
         close(pb)
   }
   if (!missing(fname)) {
     # return(.shp.write(sa,fname,...))
      return(spatial_write(sa,fname,verbose=verbose,...))
   }
   session_grid(g0)
   sa
}
'.vectorize' <- function(obj,fname,opt="") {
   Fout <- .maketmp()
   write_envi(obj,paste0(Fout,"."))
   cmd <- paste("python",Sys.which("gdal_polygonize.py")
               ,opt," -f \"ESRI Shapefile\"",Fout,".",fname)
   system(cmd)
   envi_remove(Fout)
   0L
}
