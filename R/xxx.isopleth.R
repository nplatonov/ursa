'.isopleth' <- function(obj,engine=c("native","sf","sp")
                       ,target=c("polygon","contour","both"),...) {
   
   if (!is_ursa(obj))
      return(invisible(NULL))
   if (length(obj)>1) {
      ret <- do.call(spatial_bind,lapply(seq(obj),function(i)
                   .isopleth(obj=obj[i],engine=engine,target=target,...)))
      return(ret)
   }
   verbose <- T
   isColored <- T
  # volume <- T
   engine <- match.arg(engine)
   target <- match.arg(target)
   isContour <- TRUE
   isPolygon <- TRUE
  # engine <- c("sf","sp")[2]
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
   res <- .panel_contour(obj,category=isColored,...)
   cl <- with(res,contourLines(x,y,z,levels=lev))
   da <- data.frame(level=sapply(cl,function(z) {z$level}))
   lev <- sort(unique(sapply(cl,function(z) {z$level})))
   print(lev)
   .elapsedTime("A")
   if (isSF) {
      if (devel <- TRUE) {
         sa <- lapply(lev,function(l) {
            ch <- lapply(cl[da$level %in% l],function(z) {
               try(sf::st_polygon(list(cbind(z$x,z$y))))
            })
            ind <- which(!sapply(ch,inherits,"try-error"))
            sa2 <- try(sf::st_multipolygon(lapply(cl[da$level %in% l][ind],function(z)
                  list(cbind(z$x,z$y)))))
            str(sa2)
            q()
         })
         str(cl)
         q()
      }
      if (isPolygon) {
         sa <- lapply(lev,function(l)
            sf::st_multipolygon(lapply(cl[da$level %in% l],function(z)
               list(cbind(z$x,z$y)))))
         sa <- sf::st_sfc(sa,crs=ursa_crs(obj))
      }
      if (isContour) {
         sl <- lapply(lev,function(l)
            sf::st_multilinestring(lapply(cl[da$level %in% l],function(z)
               cbind(z$x,z$y))))
         sl <- sf::st_sfc(sl,crs=ursa_crs(obj))
      }
   }
   else if (isSP) {
      if (isPolygon) {
         sa <- lapply(lev,function(l)
            sp::Polygons(lapply(cl[which(da$level %in% l)],function(z)
               sp::Polygon(cbind(z$x,z$y))),l))
         sa <- sp::SpatialPolygons(sa,proj4string=sp::CRS(ursa_crs(obj)))
      }
      if (isContour) {
         sl <- lapply(lev,function(l)
            sp::Lines(lapply(cl[which(da$level %in% l)],function(z)
               sp::Line(cbind(z$x,z$y))),l))
         sl <- sp::SpatialLines(sl,proj4string=sp::CRS(ursa_crs(obj)))
      }
   }
   da <- data.frame(name=names(obj),level=lev)
   if (isPolygon)
      spatial_data(sa) <- da
   if (isContour)
      spatial_data(sl) <- da
   ##~ print(spatial_area(sa)*1e-6)
   ##~ print(spatial_length(sl)*1e-3)
   ##~ print(spatial_geotype(sa))
   ##~ print(spatial_geotype(sl))
   ##~ .elapsedTime("B")
  # spatial_write(sl,"res2_isopleth.geojson")
  # spatial_write(sa,"res2_polygon.geojson")
  # glance(sl)
   if (F) {
      rm(sl)
      sl <- spatial_read("res2")
      print(spatial_area(sl)*1e-6)
      print(spatial_length(sl)*1e-3)
      q()
      str(da)
   }
   if ((isPolygon)&&(target %in% "polygon"))
      return(sa)
   if ((isContour)&&(target %in% "contour"))
      return(sl)
   list(polygon=sa,contour=sl)
}
