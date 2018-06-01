# wrappers to spatial (not raster) objects

'spatial_crs' <- 'spatial_proj4' <- function(obj,verbose=FALSE) {
   isUrsa <- is.ursa(obj) | is.ursa(obj,"grid")
   isPrm <- is.numeric(obj) | is.character(obj)
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(ursa=isUrsa,sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      return(sf::st_crs(obj)$proj4string)
   }
   if (isSP) {
      return(sp::proj4string(obj))
   }
   if (isUrsa)
      return(ursa_proj4(obj))
   if (isPrm) {
      return(.epsg2proj4(obj,force=TRUE))
   }
   return(NULL)
}
'spatial_crs<-' <- 'spatial_proj4<-' <- function(obj,verbose=FALSE,value) {
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      sf::st_crs(obj) <- sf::NA_crs_
      sf::st_crs(obj) <- value
   }
   if (isSP) {
      if (is.numeric(value))
         value <- .epsg2proj4(value,force=FALSE)
      sp::proj4string(obj) <- NA_character_
      sp::proj4string(obj) <- value
   }
   obj
}
'spatial_geometry' <- function(obj,verbose=FALSE) {
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      return(sf::st_geometry(obj))
   }
   if (isSP) {
      return(sp::geometry(obj))
   }
   return(NULL)
}
'spatial_geometry<-' <- function(obj,verbose=FALSE,value) {
   isSF <- .isSF(value)
   isSP <- .isSP(value)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      if (!.isSF(obj)) {# lost geometry colname
         cname <- colnames(obj)
         obj <- sf::st_sf(obj,geom=spatial_geometry(value))
         spatial_fields(obj) <- cname
      }
      else {
         obj[,attr(obj,"sf_column")][[1]] <- value
      }
   }
   if (isSP) {
      geotype <- spatial_geotype(value)
      obj <- switch(geotype
                   ,POLYGON=sp::SpatialPolygonsDataFrame(value,obj,match.ID=FALSE)
                   ,LINESTRING=sp::SpatialLinesDataFrame(value,obj,match.ID=FALSE)
                   ,POINT=sp::SpatialPointsDataFrame(value,obj,match.ID=FALSE)
                   ,stop(paste("unimplemented selection:",geotype)))
   }
   if (!isSP & !isSF) {
      isSF <- .isSF(obj)
      isSP <- .isSP(obj)
      if (is.null(value)) {
         if (isSF)
            sf::st_geometry(obj) <- NULL
         if (isSP)
            obj <- methods::slot(obj,"data")
      }
   }
   obj
}
'spatial_bbox' <- function(obj,verbose=FALSE) {
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      res <- sf::st_bbox(obj)
      rname <- names(res)
      res <- as.numeric(res)
      names(res) <- rname
      return(res)
   }
   if (isSP) {
      res <- c(sp::bbox(obj))
      if (length(res)==6)
         res <- res[c(1,2,4,5)]
      names(res) <- c("xmin","ymin","xmax","ymax")
      return(res)
   }
   NULL
}
'spatial_bbox<-' <- function(obj,verbose=FALSE,value) {
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      NULL ## 'sf' has no replacement function for bbox
   }
   if (isSP) {
      val <- matrix(value,ncol=2)
      colnames(val) <- c("min","max")
      rownames(val) <- paste0("coords.x",seq(nrow(val)))
      methods::slot(obj,"bbox") <- val
   }
   obj
}
'spatial_engine' <- function(obj,verbose=FALSE) {
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      return("sf")
   }
   if (isSP) {
      return("sp")
   }
   return(NULL)
}
'spatial_fields' <- 'spatial_colnames' <- function(obj,verbose=FALSE) {
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      if (FALSE) { ## 'st_agr' FAILURE for 'obj$newfield' <- assignment
         dname <- try(names(sf::st_agr(obj)),silent=TRUE)
         if (inherits(dname,"try-error"))
            dname <- character()
      }
      else {
         dname <- colnames(obj)
         if (is.null(dname))
            dname <- character()
         else if (length(ind <- match(attr(obj,"sf_column"),dname)))
            dname <- dname[-ind]
      }
      return(dname)
   }
   if (isSP) {
      dname <- try(colnames(methods::slot(obj,"data")),silent=TRUE)
      if (inherits(dname,"try-error"))
         dname <- character()
      return(dname)
   }
   return(NULL)
}
'spatial_fields<-' <- 'spatial_colnames<-' <- function(obj,verbose=FALSE,value) {
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      oldvalue <- names(attr(obj,"agr"))
      colnames(obj)[match(oldvalue,colnames(obj))] <- value
     # colnames(obj)[-match(attr(obj,"sf_column"),colnames(obj))] <- value
      names(attr(obj,"agr")) <- value
   }
   if (isSP) {
      colnames(spatial_data(obj)) <- value 
   }
   obj
}
'spatial_data' <- function(obj,subset=".+",drop=NA,verbose=FALSE) {
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      if (inherits(obj,"sfc"))
         return(NULL)
      res <- obj
      sf::st_geometry(res) <- NULL
      attributes(res) <- attributes(res)[c("names","row.names","class")]
   }
   else if (isSP) {
      if (!methods::.hasSlot(obj,"data"))
         return(NULL)
      res <- methods::slot(obj,"data")
   }
   else if (is.data.frame(obj))
      return(obj)
   else
      return(NULL)
   ind <- .grep(subset,colnames(res))
   if (!length(ind))
      return(res)
   if (is.na(drop))
      drop <- if (TRUE) FALSE else length(ind)==1
   return(res[,ind,drop=drop])
}
'spatial_data<-' <- function(obj,verbose=FALSE,value) {
   operation <- "unknown"
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (isSF | isSP)
      operation <- "geometry"
   else {
      isSF <- .isSF(value)
      isSP <- .isSP(value)
      if (isSF | isSP)
         operation <- "data"
   }
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,operation=operation,row.names="engine"))
   if (operation=="geometry") {
      n <- spatial_count(obj)
      if (nrow(value)!=n) {
         value <- value[rep(seq(nrow(value)),len=n),,drop=FALSE]
      }
      spatial_geometry(value) <- obj
      return(value)
   }
   if (operation=="data")
      stop(paste("Not imlemented for operation",sQuote(operation)))
   NULL
}
'spatial_transform' <- function(obj,crs,verbose=FALSE,...) {
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (missing(crs))
      crs <- session_proj4()
   else if ((is.ursa(crs))||(is.ursa(crs,"grid")))
      crs <- ursa(crs,"crs")
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      return(sf::st_transform(obj,sf::st_crs(crs),...))
   }
   if (isSP) {
      if (is.numeric(crs))
         crs <- .epsg2proj4(crs,force=FALSE)
      else if (.isSP(crs))
         crs <- sp::proj4string(crs)
      return(sp::spTransform(obj,crs,...)) ## sp::CRS(crs) ?
   }
   return(NULL)
}
'spatial_geotype' <- function(obj,verbose=FALSE) {
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      if (inherits(obj,"sfc"))
         geoType <- .grep("^sfc_.+$",class(obj),value=TRUE)
      else
         geoType <- .grep("^sfc_.+$",class(obj[[attr(obj,"sf_column")]]),value=TRUE)
      geoType <- .gsub("^sfc_","",geoType)
      if (geoType=="GEOMETRY")
         geoType <- unique(as.character(sf::st_geometry_type(obj)))
      return(geoType)
   }
   if (isSP) {
      geoType <- switch(class(sp::geometry(obj))
                       ,SpatialPolygons="POLYGON"
                       ,SpatialPoints="POINT"
                       ,SpatialLines="LINESTRING")
      return(geoType)
   }
   return(NULL)
}
'spatial_coordinates' <- function(obj,verbose=FALSE) {
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   geoType <- spatial_geotype(obj)
   if (isSF) {
      if (length(geoType)>1) {
         if (("POLYGON" %in% geoType)&&("MULTIPOLYGON" %in% geoType)) {
            obj <- sf::st_cast(obj,"MULTIPOLYGON")
            geoType <- "MULTIPOLYGON"
         }
         else
            stop(paste("Unimplemented for multiple geometries (sf): "
                      ,paste(geoType,collapse=", ")))
      }
      if (geoType=="POINT") {
        # ret <- do.call("rbind",lapply(sf::st_geometry(obj),unclass))
         ret <- t(sapply(sf::st_geometry(obj),unclass))
         rownames(ret) <- seq(nrow(ret))
         colnames(ret) <- c("x","y")
         return(ret)
      }
      if (geoType=="LINESTRING") {
         multi <- any(sapply(sf::st_geometry(obj),is.list))
         if (!multi) {
            ret <- lapply(sf::st_geometry(obj),unclass)
            names(ret) <- seq_along(ret)
            return(ret)
         }
         else
            stop(paste("Unimplemented MULTILINESTRING (sf)"))
      }
      if (geoType=="MULTIPOLYGON") {
         ret <- lapply(sf::st_geometry(obj),unclass) ## Holes are not ignored
        # ret <- lapply(sf::st_geometry(obj),unlist,recursive=FALSE) ## ignore holes
         names(ret) <- seq_along(ret)
         return(ret)
      }
      if (geoType=="POLYGON") {
         ret <- lapply(sf::st_geometry(obj),unclass)
         names(ret) <- seq_along(ret)
         return(ret)
      }
     # ret <- lapply(sf::st_geometry(obj),unclass) ## dummy
      stop(paste("Unimplemented for geometry (sf): "
                ,paste(geoType,collapse=", ")))
     ## back to sf 
     # g1 <- lapply(geom,function(x1) {
     #    y1 <- sf::st_multilinestring(x1)
     #   # y1 <- sf::st_multipolygon(list(x1))
     # })
   }
   if (isSP) {
      if (geoType=="POINT") {
         ret <- unname(sp::coordinates(obj))
         rownames(ret) <- seq(nrow(ret))
         colnames(ret) <- c("x","y")
         return(ret)
      }
      if (geoType=="LINESTRING") {
         ulen <- sort(unique(sapply(methods::slot(obj,"lines")
                                ,function(x) length(methods::slot(x,"Lines")))))
         if ((length(ulen)==1)&&(ulen==1)) {
            ret <- lapply(methods::slot(obj,"lines"),function(x) {
               x2 <- lapply(methods::slot(x,"Lines"),sp::coordinates)[[1]]
            })
            names(ret) <- seq_along(ret)
            return(ret)
         }
         else 
            stop(paste("Unimplemented MULTILINESTRING (sp)"))
      }
      if (geoType=="POLYGON") {
         ulen <- sort(unique(sapply(methods::slot(obj,"polygons"),function(x) length(methods::slot(x,"Polygons")))))
        # ret <- vector("list",length(ulen))
         ret <- lapply(methods::slot(obj,"polygons"),function(x1) {
            x2 <- methods::slot(x1,"Polygons")
            hole <- sapply(x2,function(x3) methods::slot(x3,"hole"))
            if (TRUE) {# if (any(hole)) {
               indF <- which(!hole)
               ret2 <- vector("list",length(indF))
               jF <- 1L
               prevHole <- TRUE
               for (i in seq_along(hole)) {
                  if (!hole[i]) {
                     if (i==length(hole))
                        h2 <- 0
                     else {
                        h1 <- i+1
                        hole2 <- hole[h1:length(hole)]
                        if (hole2[1]) {
                           indE <- which(diff(hole2)!=0)
                           if (!length(indE))
                              h2 <- length(hole2)
                           else
                              h2 <- indE[1]
                           print(h2)
                        }
                        else
                           h2 <- 0
                     }
                     if (!h2) {
                        ret2[[jF]] <- list(sp::coordinates(x2[[i]]))
                        jF <- jF+1L
                        next
                     }
                     h2 <- h2+1
                     ret3 <- vector("list",h2)
                     ret3[[1]] <- sp::coordinates(x2[[i]])
                     jH <- 2L
                  }
                  else {
                     ret3[[jH]] <- sp::coordinates(x2[[i]])
                     jH <- jH+1L
                     if (jH>h2) {
                        ret2[[jF]] <-ret3
                        jF <- jF+1L
                     }
                  }
               }
              # ret2 <- unlist(ret2,recursive=FALSE) ## TRUE ignores holes
               ret2
            }
            else 
               stop(paste("Unimplemented POLYGON (sp) without holes"))
         })
         names(ret) <- seq_along(ret)
         return(ret)
      }
      stop(paste("Unimplemented for geometry (sp): "
                ,paste(geoType,collapse=", ")))
   }
   return(NULL)
}
'spatial_area' <- function(obj,verbose=FALSE) {
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      res <- sf::st_area(obj)
      if (TRUE) {
         u <- attr(res,"units")$numerator
         m <- rep(1,length(u))
         for (i in seq_along(u))
            m[i] <- switch(u[i],"m"=1,"km"=1e3,"mm"=1e-3,stop(u[i]))
         res <- as.numeric(res)*prod(m)
      }
      return(res)
   }
   if (isSP) {
      if (!("POLYGON" %in% spatial_geotype(obj)))
         return(NULL)
     # res <- sapply(obj@polygons,function(x) sapply(x@Polygons,methods::slot,"area"))
      res <- sapply(methods::slot(obj,"polygons"),function(x1)
         sum(sapply(methods::slot(x1,"Polygons"),function(x2)
            y2 <- ifelse(methods::slot(x2,"hole"),-1,1)*methods::slot(x2,"area"))))
      return(res)
   }
   NULL
}
'spatial_length' <- function(obj,verbose=FALSE) {
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      res <- sf::st_length(obj)
      if (TRUE) {
         u <- attr(res,"units")$numerator
         m <- rep(1,length(u))
         for (i in seq_along(u))
            m[i] <- switch(u[i],"m"=1,"km"=1e3,"mm"=1e-3,stop(u[i]))
         res <- as.numeric(res)*prod(m)
      }
      return(res)
   }
   if (isSP) {
      if (!("LINESTRING" %in% spatial_geotype(obj)))
         return(NA)
      if (FALSE) { ## thesame
         res <- sapply(methods::slot(obj,"lines"),function(x1)
            sum(sapply(methods::slot(x1,"Lines"),function(x2) {
               xy <- diff(methods::slot(x2,"coords"))
               sum(sqrt(xy[,1]*xy[,1]+xy[,2]*xy[,2]))
            }))
         )
      }
      else {
         res <- sp::SpatialLinesLengths(obj
                         ,longlat=.lgrep("\\+proj=longlat",spatial_proj4(obj))>0)
      }
      return(res)
   }
   NULL
}
'spatial_dim' <- function(obj,verbose=FALSE) {
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSP)
      return(2L)
   if (isSF) {
      return(max(sapply(obj[[attr(obj,"sf_column")]],function(x) {
         if (is.list(x)) max(sapply(x,function(y) {
            if (is.list(y)) {
               max(sapply(y,function(z) {
                  if (!is.null(dim(z))) ncol(z) else length(z)
               }))
            }
            else if (!is.null(dim(y))) ncol(y) else length(y)
         }))
         else if (!is.null(dim(x))) ncol(x)
         else length(x)
      })))
   }
   NULL
}
'is_spatial_points' <- function(obj,verbose=FALSE) {
   res <- .lgrep("POINT",spatial_geotype(obj,verbose=verbose))>0
}
'is_spatial_lines' <- function(obj,verbose=FALSE) {
   .lgrep("LINES",spatial_geotype(obj,verbose=verbose))>0 ## LINEString
}
'is_spatial_polygons' <- function(obj,verbose=FALSE) {
   .lgrep("POLYGON",spatial_geotype(obj,verbose=verbose))>0
}
'spatial_nrow' <- 'spatial_count' <- function(obj,verbose=FALSE) {
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   length(spatial_geometry(obj))
}
'is_spatial' <- function(obj,verbose=FALSE) {
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   isSF | isSP
}
'spatial_filelist' <- function(path=".",pattern=NA,full.names=TRUE
                              ,recursive=FALSE) {
   if (!is.character(pattern))
      pattern <- "\\.(gpkg|tab|kml|json|geojson|mif|sqlite|shp|osm)(\\.(zip|gz|bz2))*$"
   res <- dir(path=path,pattern=pattern,full.names=full.names,recursive=recursive)
  # res <- gsub("(\\.zip|gz|bz2)*$","",res) ## lack for 'file.info'
   res
}
'.spatial_shape' <- function(data,geometry,verbose=FALSE) { ## not useful
   isSF <- .isSF(geometry)
   isSP <- .isSP(geometry)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   spatial_geometry(data) <- geometry
   data
}
'spatial_ncol' <- function(obj,verbose=FALSE) length(spatial_fields(obj,verbose=verbose))
'spatial_centroid' <- function(obj,verbose=FALSE) {
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      return(sf::st_centroid(obj))
   }
   if (isSP) {
      geoType <- spatial_geotype(obj)
      if (geoType=="POLYGON") {
         res <- lapply(methods::slot(spatial_geometry(obj),"polygons"),function(x) {
            a1 <- methods::slot(x,"labpt")
            if (FALSE) {
               a2 <- lapply(methods::slot(x,"Polygons"),function(y) {
                  methods::slot(y,"labpt")
               })
               str(a2)
               q()
            }
            a1
         })
         res <- do.call("rbind",res)
         res <- data.frame(x=res[,1],y=res[,2])
         sp::coordinates(res) <- c("x","y")
        # sp::proj4string(res) <- spatial_crs(obj)
         spatial_crs(res) <- spatial_crs(obj)
         da <- spatial_fields(obj)
         if (length(da))
            spatial_data(res) <- spatial_data(obj)
      }
      else
         stop(paste("Unimplemented for geometry:",geoType))
      return(res)
   }
   NULL
}
'spatial_clip' <- function(obj,verbose=FALSE) {
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      ind <- sf::st_intersects(polygonize(session_bbox(),engine="sf"),obj)[[1]]
      return(obj[ind,])
   }
   if (isSP) {
      ind <- unname(sp::over(polygonize(session_bbox(),engine="sp")
                            ,spatial_geometry(obj),returnList=TRUE)[[1]])
      return(obj[ind,])
   }
   obj
}
'.spatial_intersection' <- function(x,y,verbose=FALSE) {
   isSF <- .isSF(x) & .isSF(y)
   isSP <- .isSP(x) & .isSP(y)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      sf::st_agr(x) <- "constant"
      sf::st_agr(y) <- "constant"
      res <- sf::st_intersection(x,y)
      return(res)
   }
   if (isSP) {
      stop("unimplemented for 'sp' objects")
     # requireNamespace("rgeos",quietly=.isPackageInUse())
     # res <- rgeos::gIntersection(x,y,byid=TRUE,drop_lower_td=TRUE
     #                            ,unaryUnion_if_byid_false=FALSE)
     # res2 <- names(sp::over(spatial_geometry(x),spatial_geometry(y),returnList=TRUE))
     # res2 <- grep("NA",res2,invert=TRUE,value=TRUE)
     # str(res2)
     # q()
     # spatial_data(res) <- data.frame(I=rep(1L,spatial_nrow(res)))
     # spatial_write(res,"res1.shp")
     # q()
     # return(res)
   }
   NULL
}
