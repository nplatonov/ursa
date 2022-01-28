# wrappers to spatial (not raster) objects
#.syn('spatial_crs',0,...)
'spatial_proj4' <- 'spatial_proj' <- 'spatial_crs' <- function(obj,verbose=FALSE) {
   if (!is.null(attr(obj,"crs"))) {
      res <- attr(obj,"crs")
      if (inherits(res,"crs"))
         return(res$proj4string)
      return(res)
   }
   if ((TRUE)&&(is.character(obj))&&(nchar(obj)>0)&&(length(spatial_dir(obj))==1)) {
      obj <- spatial_read(obj)
   }
   isUrsa <- is.ursa(obj) | is.ursa(obj,"grid")
   isPrm <- is.numeric(obj) | is.character(obj)
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(ursa=isUrsa,sf=isSF,sp=isSP,prm=isPrm,row.names="engine"))
   if ((isSF)||(inherits(obj,"crs"))) {
      return(sf::st_crs(obj)$proj4string)
   }
   if (isSP) {
      if (FALSE) ## `sp`<1.4-2
         return(sp::proj4string(obj))
      spCRS <- methods::slot(obj,"proj4string")
      if (methods::is(spCRS,"CRS")) {
         wkt <- comment(spCRS)
         ret <- methods::slot(spCRS,"projargs")
         if (FALSE) ## possible for future use
            comment(ret) <- wkt
         return(ret)
      }
      return(NA_character_)
   }
   if (isUrsa)
      return(ursa_crs(obj))
   if (isPrm) {
      return(.epsg2proj4(obj,verbose=verbose,force=TRUE))
   }
   if (isTRUE(all(sapply(obj,.isSF))))
      return(lapply(obj,spatial_crs,verbose=verbose))
   if (isTRUE(all(sapply(obj,.isSP))))
      return(lapply(obj,spatial_crs,verbose=verbose))
   return(NULL)
}
#.syn('spatial_crs<-',0,...)
'spatial_proj4<-' <- 'spatial_proj<-' <- 'spatial_crs<-' <- function(obj,verbose=FALSE,value) { 
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      sf::st_crs(obj) <- sf::NA_crs_ ## ??? comment it?
      sf::st_crs(obj) <- .p4s2epsg(value)
   }
   if (isSP) {
      if (is.numeric(value))
         value <- .epsg2proj4(value,force=!TRUE)
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
   if (isTRUE(all(sapply(obj,.isSF))))
      return(lapply(obj,spatial_geometry,verbose=verbose))
   if (isTRUE(all(sapply(obj,.isSP))))
      return(lapply(obj,spatial_geometry,verbose=verbose))
   return(NULL)
}
'spatial_geometry<-' <- function(obj,verbose=FALSE,value) {
   isSF <- .isSF(value)
   isSP <- .isSP(value)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="value"))
   if (isSF) {
      if (!.isSF(obj)) {# lost geometry colname
         cname <- colnames(obj)
         obj <- sf::st_sf(obj,geometry=spatial_geometry(value))
         spatial_fields(obj) <- cname
      }
      else {
         if (inherits(obj,"sfc"))
            return(value)
         obj[,attr(obj,"sf_column")][[1]] <- value
      }
   }
   if (isSP) {
      if (inherits(obj,c("SpatialLinesDataFrame","SpatialPointsDataFrame"
                         ,"SpatialPolygonsDataFrame"))) {
         obj <- methods::slot(obj,"data")
      }
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
      if (verbose)
         print(data.frame(sf=isSF,sp=isSP,row.names="obj"))
      if (is.null(value)) {
         if (isSF)
            sf::st_geometry(obj) <- NULL
         if (isSP)
            obj <- methods::slot(obj,"data")
         return(obj)
      }
      print(class(value))
      stop("no sp, no sf")
   }
   obj
}
'spatial_bbox' <- function(obj,verbose=FALSE) {
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if ((!isSF)&&(!isSP)&&(is.list(obj))) {
      if (isTRUE(all(sapply(obj,function(o) isTRUE(.isSF(o)) | isTRUE(.isSP(o)))))) {
         return(lapply(obj,spatial_bbox,verbose=verbose))
      }
   }
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   res <- NULL
   isLongLat <- .lgrep("\\+proj=longlat",spatial_crs(obj))>0
   if (F & isLongLat) {
      xy <- spatial_coordinates(obj)
      if (F & isSF) {
         xy <- spatial_coordinates(sf::st_cast(spatial_geometry(obj),"POINT"))
         res <- c(xmin=min(xy[,1]),ymin=min(xy[,2]),xmax=max(xy[,1]),ymax=max(xy[,2]))
        # return(res)
      }
      if (T | isSP) {
         xy <- spatial_coordinates(obj)
         while(all(sapply(xy,is.list)))
            xy <- unlist(xy,recursive=FALSE)
         if (is.list(xy)) ## deprecated and resored 20190930
            xy <- do.call(rbind,xy)
         res <- c(xmin=min(xy[,1]),ymin=min(xy[,2]),xmax=max(xy[,1]),ymax=max(xy[,2]))
        # return(res)
      }
   }
   if ((is.null(res))&&(isSF)) {
      res <- sf::st_bbox(obj)
      rname <- names(res)
      res <- as.numeric(res)
      names(res) <- rname
      attr(res,"crs") <- spatial_crs(obj)
     # return(res)
   }
   if ((is.null(res))&&(isSP)) {
      res <- c(sp::bbox(obj))
      if (length(res)==6)
         res <- res[c(1,2,4,5)]
      names(res) <- c("xmin","ymin","xmax","ymax")
      attr(res,"crs") <- spatial_crs(obj)
     # return(res)
   }
   ##~ cat("----------------\n")
   ##~ if ((isLongLat)&&(res["xmin"]<(-180+1e-6))&&(res["xmax"]>(180-1e-6))) {
      ##~ print("HERE")
   ##~ }
   ##~ print(res)
   ##~ cat("----------------\n")
   ##~ q()
  # return(res)
   if (!is.null(res)) {
      if (!isLongLat)
         return(res)
      dg180 <- 0.1
      is180 <- (res["xmin"]<=(-180+dg180))&&(res["xmax"]>=(180-dg180))
      if (!is180)
         return(res)
      dg90 <- -1e-6
      is90 <- (res["ymin"]<=(-90+dg90))||(res["ymax"]>=(90-dg90))
      if (is90)
         return(res)
      if (skip_dev <- !TRUE)
         return(res)
     # if ((TRUE)&&(res["xmin"]>0)&&(res["xmax"]<0))
     #    res["xmax"] <- res["xmax"]+360
      if (T) {
         xy <- unname(spatial_coordinates(obj))
         if (T) {
            for (i in seq(4)) {
               if (isFALSE(any(sapply(xy,is.list))))
                  break
               if (length(ind <- which(sapply(xy,is.list)))) {
                  xy <- c(xy[-ind],unlist(xy[ind],recursive=FALSE))
               }
            }
         }
         else {
            repeat({
               if (isFALSE(any(sapply(xy,is.list))))
                  break
               xy <- unlist(xy,recursive=FALSE)
            })
         }
         if (is.list(xy))
            xy <- do.call("rbind",xy)
         x1 <- xy[,1]
         x2 <- x1
         ind <- which(x2<(0))
         if (length(x2))
            x2[ind] <- x2[ind]+360
         sd1 <- sd(x1)
         sd2 <- sd(x2)
         if (verbose) {
            print(summary(x1))
            print(summary(x2))
            print(c(sd1=sd1,sd2=sd2))
         }
         if (sd1<sd2)
            return(res)
         res[c(1,3)] <- range(x2)
      }
      else {
         d2 <- spatialize(obj,style="merc",verbose=verbose) ## sp: failed for 90.0N 
         res <- matrix(spatial_bbox(d2),ncol=2,byrow=FALSE)
         res <- .project(res,spatial_crs(d2),inv=TRUE)
         res <- c(t(res))
         names(res) <- c("xmin","ymin","xmax","ymax")
         attr(res,"crs") <- spatial_crs(obj)
      }
   }
   res
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
   if ((!isSF)&&(!isSP)&&(is.list(obj))) {
      if (isTRUE(all(sapply(obj,function(o) isTRUE(.isSF(o)) | isTRUE(.isSP(o)))))) {
         return(lapply(obj,spatial_fields,verbose=verbose))
      }
   }
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
      if (!is.null(oldvalue)) {
         colnames(obj)[match(oldvalue,colnames(obj))] <- value
        # colnames(obj)[-match(attr(obj,"sf_column"),colnames(obj))] <- value
         names(attr(obj,"agr")) <- value
      }
      else {
         ind <- grep(attr(obj,"sf_column"),names(obj),invert=TRUE,value=FALSE)
         colnames(obj)[ind] <- value
      }
   }
   if (isSP) {
      colnames(spatial_data(obj)) <- value 
   }
   obj
}
'spatial_data' <- function(obj,subset=".+",drop=NA,verbose=FALSE) {
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if ((!isSF)&&(!isSP)&&(is.list(obj))) {
      if (isTRUE(all(sapply(obj,function(o) isTRUE(.isSF(o)) | isTRUE(.isSP(o)))))) {
         return(lapply(obj,spatial_data,subset=subset,drop=drop,verbose=verbose))
      }
   }
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      if (inherits(obj,"sfc"))
         return(NULL)
     # res <- obj
      ##~1 sf::st_geometry(res) <- NULL
      ##~1 attributes(res) <- attributes(res)[c("names","row.names","class")]
      ##~2 res[[attr(res,"sf_column")]] <- NULL
      ##~2 attr(res,"sf_column") <- NULL
      ##~2 class(res) <- grep("^(sf|sfc)$",class(res),value=TRUE,invert=TRUE)
      res <- sf::st_set_geometry(obj,NULL)
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
   if ((!isSF)&&(!isSP)&&(is.list(obj))) {
      if (isTRUE(all(sapply(obj,function(o) isTRUE(.isSF(o)) | isTRUE(.isSP(o)))))) {
         return(lapply(obj,spatial_transform,crs=crs,verbose=verbose))
      }
   }
   if (missing(crs))
      crs <- session_crs()
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
      opW <- options(warn=ifelse(verbose,0,-1))
      crs <- methods::slot(sp::CRS(crs,doCheckCRSArgs=TRUE),"projargs")
      crs0 <- spatial_crs(obj)
      if (.lgrep("\\+init=epsg\\:\\d+",crs0)) {
         spatial_crs(obj) <- methods::slot(sp::CRS(crs0,doCheckCRSArgs=TRUE),"projargs")
      }
      ret <- sp::spTransform(obj,crs,...)
      options(opW)
      return(ret)
     # return(sp::spTransform(obj,crs,...)) ## sp::CRS(crs) ?
   }
   return(NULL)
}
'spatial_geotype' <- 'spatial_shape' <- function(obj,each=FALSE,verbose=FALSE) {
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      if (each)
         return(sf::st_geometry_type(obj))
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
                       ,SpatialLines="LINESTRING"
                       ,SpatialPixels="GRID"
                       ,stop(class(sp::geometry(obj)))
                       )
      if (!each)
         return(geoType)
      return(rep(geoType,spatial_count(obj)))
   }
  # print(class(obj))
   return(NULL)
}
'spatial_coordinates' <- function(obj,verbose=FALSE) {
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   geoType <- spatial_geotype(obj)
   if (isSF) {
      if ((FALSE)&&(length(geoType)>1)) {
         if (("POLYGON" %in% geoType)&&("MULTIPOLYGON" %in% geoType)) {
            obj <- sf::st_cast(obj,"MULTIPOLYGON")
            geoType <- "MULTIPOLYGON"
         }
         else
            stop(paste("Unimplemented for multiple geometries (sf): "
                      ,paste(geoType,collapse=", ")))
      }
      if (all(geoType=="POINT")) {
        # ret <- do.call("rbind",lapply(sf::st_geometry(obj),unclass))
         ret <- t(sapply(sf::st_geometry(obj),unclass))
         rownames(ret) <- seq(nrow(ret))
         colnames(ret) <- c("x","y")
         return(ret)
      }
      if (all(geoType=="LINESTRING")) {
         multi <- any(sapply(sf::st_geometry(obj),is.list))
         if (!multi) {
            ret <- lapply(sf::st_geometry(obj),unclass)
            names(ret) <- seq_along(ret)
            return(ret)
         }
         else
            stop(paste("Unimplemented MULTILINESTRING (sf)"))
      }
      if (all(geoType=="MULTIPOLYGON")) {
         ret <- lapply(sf::st_geometry(obj),unclass) ## Holes are not ignored
        # ret <- lapply(sf::st_geometry(obj),unlist,recursive=FALSE) ## ignore holes
         names(ret) <- seq_along(ret)
         return(ret)
      }
      else if (all(geoType=="POLYGON")) {
         ret <- lapply(sf::st_geometry(obj),unclass)
         names(ret) <- seq_along(ret)
         return(ret)
      }
      else if (("POLYGON" %in% geoType)&&("MULTIPOLYGON" %in% geoType)) {
         if (FALSE) { ## make all multu
            k <- 0
            ret <- lapply(sf::st_geometry(obj),function(xy) {
               str(xy)
               k <<- k+1
               ##~ if (k<=1442)
                  ##~ return(NULL)
               ##~ ret1 <- lapply(xy,function(xy1) {
                  ##~ str(xy1)
               ##~ })
               NULL
            })
            rm(k)
            q()
         }
         else
            ret <- lapply(sf::st_geometry(obj),unclass)
         names(ret) <- seq_along(ret)
         return(ret)
      }
      else if (all(geoType=="MULTILINESTRING")) {
         ret <- lapply(sf::st_geometry(obj),unclass)
         nseg <- unique(sapply(ret,length))
         if ((FALSE)&&(length(nseg)==1)&&(nseg==1)) ## consistence with 'sp'
            ret <- lapply(ret,function(x) x[[1]])
         names(ret) <- seq_along(ret)
         return(ret)
      }
      else if (all(geoType=="MULTIPOINT")) {
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
         ulen <- sort(unique(sapply(methods::slot(obj,"polygons")
                             ,function(x) length(methods::slot(x,"Polygons")))))
        # ret <- vector("list",length(ulen))
         ret <- lapply(methods::slot(obj,"polygons"),function(x1) {
            x2 <- methods::slot(x1,"Polygons")
            hole <- sapply(x2,function(x3) methods::slot(x3,"hole"))
            if (TRUE) {# if (any(hole)) {
               if ((any(hole))&&(hole[1])) {
                  ind <- order(hole)
                  x2 <- x2[ind]
                  hole <- hole[ind]
               }
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
                          # print(h2)
                        }
                        else
                           h2 <- 0
                     }
                     if (!h2) {
                        ret2[[jF]] <- list(unname(sp::coordinates(x2[[i]])))
                        jF <- jF+1L
                        next
                     }
                     h2 <- h2+1
                     ret3 <- vector("list",h2)
                     ret3[[1]] <- unname(sp::coordinates(x2[[i]]))
                     jH <- 2L
                  }
                  else {
                     if (!exists("ret3")) {
                        str(x2)
                        str(hole)
                        q()
                     }
                     ret3[[jH]] <- unname(sp::coordinates(x2[[i]]))
                     jH <- jH+1L
                     if (jH>h2) {
                        ret2[[jF]] <- ret3
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
      if (geoType=="GRID") {
         ret <- sp::coordinates(obj)
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
      if (is_spatial_polygons(obj))
         res <- sf::st_length(sf::st_cast(obj,"MULTILINESTRING"))
      else
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
      if (!("LINESTRING" %in% spatial_geotype(obj))) {
         if (!requireNamespace("rgeos",quietly=.isPackageInUse()))
            stop("suggested package is required for this operation")
         res <- try(rgeos::gLength(obj,byid=TRUE))
         if (inherits(res,"try-error"))
            res <- try(rgeos::gLength(spatial_buffer(obj),byid=TRUE))
         return(unname(res))
      }
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
                         ,longlat=.lgrep("\\+proj=longlat",spatial_crs(obj))>0)
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
'spatial_dim<-' <- function(obj,verbose=FALSE,value) {
   'redim' <- function(x) {
      d <- dim(x)[2]
      if (d==value)
         return(x)
      if (d<value) {
         res <- cbind(x,0)
         class(res) <- c("XYZ",grep("^XY$",class(x),value=TRUE))
         return(res)
      }
      res <- x[,seq_len(value)]
      class(res) <- c("XY",grep("^XY[MTZ]$",class(x),value=TRUE))
      res
    }
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSP)
      return(obj)
   if (isSF) {
      res0 <- spatial_geometry(obj)
      res <- lapply(res0,function(g1) {
         if (is.list(g1)) {
            res1 <- lapply(g1,function(g2) {
               if (is.list(g2)) {
                  res2 <- lapply(g2,redim)
                  class(res2) <- class(g2)
               }
               else
                  res2 <- redim(g2)
               res2
            })
         }
         else
            res1 <- redim(g1)
         class(res1) <- class(g1)
         res1
      })
      attributes(res) <- attributes(res0)
      ##~ cat("----------------\n")
      ##~ str(unclass(res))
      ##~ cat("----------------\n")
      ##~ str(unclass(res0))
      ##~ cat("----------------\n")
      spatial_geometry(obj) <- res0
      return(obj)
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
'spatial_filelist' <- 'spatial_dir' <- function(path=".",pattern=NA,full.names=TRUE
                                               ,recursive=FALSE,ignore.case=TRUE) {
   patt0 <- "\\.(gpkg|tab|kml|json|geojson|mif|sqlite|shp|osm)(\\.(zip|gz|bz2))*$"
   if (devel <- TRUE & all(!dir.exists(path))) {
      dpath <- list.dirs(dirname(path),full.names=FALSE)
      ind <- grep(basename(path),dpath,ignore.case=ignore.case)
      if (length(ind)==1)
         path <- file.path(dirname(path),dpath[ind])
   }
   res <- dir(path=path,pattern=patt0,full.names=full.names
             ,recursive=recursive,ignore.case=ignore.case)
   if ((!length(res))&&(is.na(pattern))) {
      if ((path==basename(path))&&(!dir.exists(path))) {
        # print("A")
         pattern <- path
         path <- "."
         res <- dir(path=path,pattern=patt0,full.names=full.names
                   ,recursive=recursive,ignore.case=ignore.case)
      }
      else {
         pattern <- basename(path)
         path2 <- dirname(path)
         res <- dir(path=path2,pattern=patt0,full.names=full.names
                   ,recursive=recursive,ignore.case=ignore.case)
         if (!length(res)) {
            pattern <- path
            path3 <- "."
            res <- dir(path=path3,pattern=patt0,full.names=full.names
                      ,recursive=recursive,ignore.case=ignore.case)
         }
      }
   }
   if (is.character(pattern)) {
      ind <- grep(pattern,basename(res),ignore.case=ignore.case)
      if (!length(ind)) {
         ind <- na.omit(match(pattern,basename(res)))
         if (!length(ind)) {
            ind <- na.omit(match(pattern,spatial_basename(res)))
           # return(res[ind])
         }
      }
      res <- res[ind]
   }
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
      if (inherits(obj,"sf"))
         sf::st_agr(obj) <- "constant"
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
'spatial_intersection' <- function(x,y,geometry=c("default","polygons","lines"
                                                 ,"points","all"),verbose=FALSE) {
   geometry <- match.arg(geometry)
   if (is.ursa(x)) {
      if (.isSF(y))
         x <- polygonize(x,engine="sf",verbose=verbose)
      else if (.isSP(y))
         x <- polygonize(x,engine="sp",verbose=verbose)
      else
         x <- polygonize(x,verbose=verbose)
   }
   if (is.ursa(y)) {
      if (.isSF(x))
         y <- polygonize(y,engine="sf",verbose=verbose)
      else if (.isSP(x))
         y <- polygonize(y,engine="sp",verbose=verbose)
      else
         y <- polygonize(y,verbose=verbose)
   }
   isSF <- .isSF(x) & .isSF(y)
   isSP <- .isSP(x) & .isSP(y)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      if (inherits(x,"sf"))
         sf::st_agr(x) <- "constant"
      if (inherits(y,"sf"))
         sf::st_agr(y) <- "constant"
     # x_
      res <- try(sf::st_intersection(x,y))
      if (inherits(res,"try-error")) {
         if (length(grep("st_crs\\(x\\) == st_crs\\(y\\) is not TRUE"
                        ,as.character(res))))
         res <- sf::st_intersection(x,spatial_transform(y,x))
      }
      if (FALSE) {
        # spatial_geometry(res) <- sf:::st_cast_sfc_default(spatial_geometry(res))
      }
      else {
         if (geometry=="all")
            return(res)
         xGeotype <- as.character(spatial_geotype(x,each=TRUE))
         yGeotype <- as.character(spatial_geotype(y,each=TRUE))
         geotype <- as.character(spatial_geotype(res,each=TRUE))
         uGeotype <- unique(geotype)
         xPolygon <- .lgrep("polygon",xGeotype)>0
         xLine <- .lgrep("line",xGeotype)>0
         xPoint <- .lgrep("point",xGeotype)>0
         yPolygon <- .lgrep("polygon",yGeotype)>0
         yLine <- .lgrep("line",yGeotype)>0
         yPoint <- .lgrep("point",yGeotype)>0
         if (geometry=="default") {
            if ((xPolygon)&&(yPolygon))
               geometry <- "polygons"
            else if (((xLine)&&(yLine))||((xLine)&&(yPolygon))||((xPolygon)&&(yLine)))
               geometry <- "lines"
            else if ((xPoint)||(yPoint))
               geometry <- "points"
            else
               return(res)
         }
         if (geometry=="polygons") {
            ind <- .grep("(polygon|collection)",geotype)
            if (!length(ind))
               return(res)
            res <- res[ind,]
            geotype <- geotype[ind]
            if (length(na.omit(match(unique(geotype)
                                    ,c("POLYGON","MULTIPOLYGON"))))==2) {
               if (length(ind2 <- .grep("collection",geotype))) {
                  opWC <- options(warn=-1)
                  res2 <- sf::st_collection_extract(res[ind2,],"POLYGON")
                  options(opWC)
                  if (spatial_count(res2)) {
                     res <- spatial_bind(res[-ind2,],res2)
                     geotype <- as.character(spatial_geotype(res,each=TRUE))
                  }
               }
               res <- sf::st_cast(res[grep("POLYGON",geotype),],"MULTIPOLYGON")
            }
            return(res)
         }
         else if (geometry=="lines") {
            ind <- .grep("(line|collection)",geotype)
            if (!length(ind))
               return(res)
            res <- res[ind,]
            geotype <- geotype[ind]
            if (length(na.omit(match(unique(geotype)
                                    ,c("MULTILINESTRING","LINESTRING"))))==2) {
               if (length(ind2 <- .grep("collection",geotype))) {
                  res <- spatial_bind(res[-ind2,]
                            ,sf::st_collection_extract(res[ind2,],"LINESTRING"))
                  geotype <- as.character(spatial_geotype(res,each=TRUE))
               }
               res <- sf::st_cast(res[grep("LINESTRING",geotype),],"MULTILINESTRING")
            }
            return(res)
         }
         else if (geometry=="points") {
            ind <- .grep("point",geotype)
            if (!length(ind))
               return(res)
            res <- res[ind,]
            return(res)
         }
         if ("GEOMETRYCOLLECTION" %in% uGeotype) {
            if (length(grep("POLYGON",uGeotype)))
               res <- sf::st_collection_extract(res,"POLYGON")
         }
         else if (length(na.omit(match(uGeotype,c("POLYGON","MULTIPOLYGON"))))==2) {
            res <- sf::st_cast(res[grep("POLYGON",geotype),],"MULTIPOLYGON")
         }
         else if (length(na.omit(match(uGeotype,c("MULTILINESTRING","LINESTRING"))))==2) {
            res <- sf::st_cast(res[grep("LINESTRING",geotype),],"MULTILINESTRING")
         }
      }
      return(res)
   }
   else if (isSP) {
      if (!requireNamespace("raster",quietly=.isPackageInUse()))
         stop("suggested package is required for this operation")
     # opW <- options(warn=-1)
      res <- try(raster::intersect(x,y))
     # options(opW)
      if (inherits(res,"try-error")) {
         res <- raster::intersect(spatial_buffer(x),spatial_buffer(y))
      }
      if (is.null(res))
         return(res)
      if (spatial_geotype(res) %in% "POLYGON") {
         k <- 0
         res@polygons <- lapply(res@polygons,function(x) {
            k <<- k+1
            x@ID <- as.character(k)
            x
         })
         rm(k)
      }
      return(res)
   }
   else if (implement_by_rgeos <- FALSE) {
     # stop("unimplemented for 'sp' objects")
      requireNamespace("rgeos",quietly=.isPackageInUse())
      res <- rgeos::gIntersection(x,y,byid=TRUE,drop_lower_td=TRUE
                                 ,unaryUnion_if_byid_false=FALSE)
      res2 <- names(sp::over(spatial_geometry(x),spatial_geometry(y),returnList=TRUE))
      res2 <- grep("NA",res2,invert=TRUE,value=TRUE)
      str(res2)
     # q()
     # spatial_data(res) <- data.frame(I=rep(1L,spatial_nrow(res)))
     # spatial_write(res,"res1.shp")
     # q()
     # return(res)
   }
   NULL
}
'spatial_difference' <- function(x,y,verbose=FALSE) {
   isSF <- .isSF(x) & .isSF(y)
   isSP <- .isSP(x) & .isSP(y)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      if (inherits(x,"sf"))
         sf::st_agr(x) <- "constant"
      if (inherits(y,"sf"))
         sf::st_agr(y) <- "constant"
      res <- try(sf::st_difference(x,y))
      if (inherits(res,"try-error")) {
         if (length(grep("st_crs\\(x\\) == st_crs\\(y\\) is not TRUE"
                        ,as.character(res))))
         res <- sf::st_difference(x,spatial_transform(y,x))
      }
      return(res)
   }
   else if (isSP) {
      if (!requireNamespace("rgeos",quietly=.isPackageInUse()))
         stop("suggested package is required for this operation")
     # opW <- options(warn=-1)
      res <- try(rgeos::gDifference(x,y,byid=TRUE))
     # options(opW)
      if (inherits(res,"try-error"))
         res <- try(rgeos::gDifference(spatial_buffer(x),spatial_buffer(y),byid=TRUE))
      return(res)
   }
   else if (dev <- FALSE) {
   }
   NULL
}
'spatial_symdifference' <- function(x,y,verbose=FALSE) {
   isSF <- .isSF(x) & .isSF(y)
   isSP <- .isSP(x) & .isSP(y)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      if (inherits(x,"sf"))
         sf::st_agr(x) <- "constant"
      if (inherits(y,"sf"))
         sf::st_agr(y) <- "constant"
      res <- try(sf::st_sym_difference(x,y))
      if (inherits(res,"try-error")) {
         if (length(grep("st_crs\\(x\\) == st_crs\\(y\\) is not TRUE"
                        ,as.character(res))))
         res <- sf::st_sym_difference(x,spatial_transform(y,x))
      }
      return(res)
   }
   else if (isSP) {
      if (!requireNamespace("rgeos",quietly=.isPackageInUse()))
         stop("suggested package is required for this operation")
     # opW <- options(warn=-1)
      res <- try(rgeos::gSymdifference(x,y,byid=TRUE))
     # options(opW)
      if (inherits(res,"try-error"))
         res <- try(rgeos::gSymdifference(spatial_buffer(x),spatial_buffer(y),byid=TRUE))
      return(res)
   }
   else if (dev <- FALSE) {
   }
   NULL
}
'spatial_buffer' <- function(obj,dist=0,quadsegs=30L,verbose=FALSE) {
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (is.character(dist)) {
      if (is.null(sc <- getOption("ursaPngScale")))
         sc <- 1
      if (is.null(retina <- getOption("ursaPngRetina")))
         retina <- 1
      if (.lgrep("px$",dist)) {
        # print(c(dist=dist))
        # print(c(scale=sc))
         dist <- session_cellsize()*as.numeric(gsub("px","",dist))/sc*retina
        # print(c(dist=dist))
      }
      else if (.lgrep("lwd$",dist)) {
         dist <- as.numeric(gsub("lwd","",dist))
         cell <- session_cellsize()
         sc <- getOption("ursaPngScale")
         dpi <- getOption("ursaPngDpi")/96
         px <- cell/sc
         res <- dist*(px*dpi)
        # print(data.frame(dist=dist,cell=cell,dpi=dpi,sc=sc,px=px,res=res))
         dist <- res
      }
   }
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      res <- sf::st_buffer(obj,dist=dist,nQuadSegs=quadsegs)
      return(res)
   }
   else if (isSP) {
      if (!requireNamespace("rgeos",quietly=.isPackageInUse()))
         stop("suggested package is required for this operation")
     # opW <- options(warn=-1)
      res <- rgeos::gBuffer(obj,byid=TRUE,width=dist,quadsegs=quadsegs)
     # options(opW)
      return(res)
   }
   else if (dev <- FALSE) {
   }
   NULL
}
'spatial_union' <- function(x,y,byid=NA,verbose=FALSE) {
   if (missing(y)) {
      isSF <- .isSF(x)
      isSP <- .isSP(x)
      if (is.na(byid))
         byid <- FALSE
   }
   else {
      isSF <- .isSF(x) & .isSF(y)
      isSP <- .isSP(x) & .isSP(y)
      if (is.na(byid))
         byid <- TRUE
   }
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
     # message("first run: 'spatial_union' for 'sf' object(s)")
     # sf::st_agr(x) <- "constant"
     # sf::st_agr(y) <- "constant"
      res <- sf::st_union(x,y,by_feature=byid)
      return(res)
   }
   else if (isSP) {
      if (!requireNamespace("rgeos",quietly=.isPackageInUse()))
         stop("suggested package is required for this operation")
      if (missing(y)) {
         res <- try(rgeos::gUnaryUnion(x,id=NULL))
         if (inherits(res,"try-error")) {
            res <- rgeos::gUnaryUnion(.spatial_repair(x),id=NULL)
         }
         return(res)
      }
      res <- rgeos::gUnion(x,y,byid=byid)
      return(res)
   }
   else if (dev <- FALSE) {
   }
   NULL
}
'spatial_simplify' <- function(obj,tol=0,topologyPreserve=TRUE,verbose=FALSE) {
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      res <- sf::st_simplify(obj,preserveTopology=topologyPreserve,dTolerance=tol)
      return(res)
   }
   else if (isSP) {
      if (!requireNamespace("rgeos",quietly=.isPackageInUse()))
         stop("suggested package is required for this operation")
     # opW <- options(warn=-1)
      res <- rgeos::gSimplify(obj,tol=tol,topologyPreserve=topologyPreserve)
     # options(opW)
      return(res)
   }
   else if (dev <- FALSE) {
   }
   NULL
}
'.spatial_repair' <- function(obj,verbose=FALSE) {
 # https://www.r-spatial.org/r/2017/03/19/invalid.html
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      valid <- try(sf::st_is_valid(obj,NA_on_exception=TRUE,reason=verbose))
   }
   else if (isSP) {
      if (!requireNamespace("rgeos",quietly=.isPackageInUse()))
         stop("suggested package is required for this operation")
      valid <- try(rgeos::gIsValid(obj,reason=verbose))
   }
   else
      return(NULL)
   if (verbose) {
      print(valid)
      valid <- FALSE
   }
   if ((inherits(valid,"try-error"))||(!valid)) {
      if (verbose)
         print("repaired by buffering")
      obj <- spatial_buffer(obj)
   }
   obj
}
'spatial_bind' <- function(...) {
   arglist <- list(...)
   if (length(ind <- which(sapply(arglist,is.null))))
      arglist <- arglist[-ind]
   res <- arglist[[1]]
   isSF <- .isSF(res)
   isSP <- .isSP(res)
   if (!is.null(spatial_data(res))) {
      if (isSP)
         return(do.call("rbind",arglist))
      if (isSF) {
         geom <- unique(sapply(arglist,function(x) attr(x,"sf_column")))
         if (length(unique(geom))==1)
            return(do.call("rbind",arglist))
         if (TRUE) { ## fixed st_column name 'geometry'
            for (i in seq_along(arglist))
               arglist[[i]] <- sf::st_sf(spatial_data(arglist[[i]])
                                        ,geometry=spatial_geometry(arglist[[i]]))
         }
         else { ## repeeat st_column name for first argument
            for (i in tail(seq_along(arglist),-1)) {
               res <- arglist[[i]]
               ind <- match(attr(res,"sf_column"),colnames(res))
               colnames(res)[ind] <- geom[1]
               attr(res,"sf_column") <- geom[1]
               arglist[[i]] <- res
            }
         }
         return(do.call("rbind",arglist))
      }
   }
   if (isSF)
      return(do.call("c",arglist))
   geoType <- spatial_geotype(res)
   coerce <- switch(geoType
                   ,POLYGON="SpatialPolygonsDataFrame"
                   ,LINESTRING="SpatialLinesDataFrame"
                   ,POINT="SpatialPointsDataFrame"
                   ,stop("geoType"))
   for (i in seq_along(arglist))
      arglist[[i]] <- methods::as(arglist[[i]],coerce)
   res <- spatial_geometry(do.call("rbind",arglist))
   res
}
'spatial_basename' <- function(fname) {
   gsub(paste0("\\."
              ,"(shp|geojson|json|sqlite|gpkg|mif|kml|osm|tif|envi|bin|envigz|img|bingz)"
              ,"(\\.(gz|bz2|zip|rar))*$")
       ,"",basename(fname),ignore.case=TRUE)
}
'spatial_pattern' <- function(fname) {
   s <- "|()[]{}^$*+?."
   p <- paste0("(",paste(paste0("\\",unlist(strsplit(s,split="+"))),collapse="|"),")")
   gsub(p,"\\\\\\1",spatial_basename(fname))
}
'spatial_fileext' <- function(fname) {
   a <- gsub("^(.+)(\\.(gz|bz2|zip|rar))$","\\1",basename(fname),ignore.case=TRUE)
   gsub(".*\\.(.+)$","\\1",a)
}
'spatial_revise' <- function(obj,engine=c("auto","sf","sp"),verbose=FALSE) {
   engine <- match.arg(engine)
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (engine=="auto")
      engine <- ifelse(isSF,"sp",ifelse(isSP,"sf",stop('unknown spatial class')))
   toSP <- engine=="sp"
   toSF <- engine=="sf" & requireNamespace("sf",quietly=.isPackageInUse())
   if (isSP & toSP | isSF & toSF)
      return(obj)
   if (toSP)
      return(sf::as_Spatial(obj))
   if (toSF) {
     ## 'methods' have already loaded because Spatial* is S4
      return(methods::as(obj,"sf"))
   }
   NULL
}
'spatial_valid' <- function(obj,each=FALSE,reason=FALSE,verbose=FALSE) {
   if (is.character(obj))
      return(all(obj %in% c("Valid Geometry")))
   isSF <- .isSF(obj)
   isSP <- .isSP(obj)
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSP) {
      if (!requireNamespace("rgeos",quietly=.isPackageInUse()))
         stop("suggested package is required for this operation")
      res <- rgeos::gIsValid(obj,byid=TRUE,reason=TRUE)
      if (reason)
         return(unname(res))
      if (each)
         return(res %in% c("Valid Geometry"))
      return(all(res %in% c("Valid Geometry")))
   }
   if (isSF) {
      res <- sf::st_is_valid(obj,reason=TRUE)
      if (reason)
         return(res)
      if (each)
         return(res %in% c("Valid Geometry"))
      return(all(res %in% c("Valid Geometry")))
   }
   NULL
}
'spatial_trim' <- function(obj) {
   attr(obj,"grid") <- NULL
   attr(obj,"toUnloadMethods") <- NULL
   attr(obj,"colnames") <- NULL
   attr(obj,"style") <- NULL
   attr(obj,"geocodeStatus") <- NULL
   attr(obj,"dsn") <- NULL
   attr(obj,basename(tempfile())) <- NULL ## dummy
   obj
}
'spatial_grid' <- function(obj) {
   if (!is_spatial(obj))
      return(ursa_grid(obj))
   if ((is.numeric(obj))&&(length(obj)==4)) {
      bbox <- obj
      if (!is.null(attr(obj,"crs")))
         crs <- attr(obj,"crs")
      else
         crs <- session_crs()
   }
   else {
      bbox <- spatial_bbox(obj)
      if ((bbox["xmin"]==bbox["xmax"])||(bbox["ymin"]==bbox["ymax"]))
         bbox <- bbox+100*c(-1,-1,1,1)
      crs <- spatial_crs(obj)
   }
   if ((.lgrep("\\+proj=longlat",crs))&&(bbox["xmax"]<0)&&(bbox["xmin"]>0))
      bbox["xmax"] <- bbox["xmax"]+360
   nc <- (bbox["xmax"]-bbox["xmin"])
   nr <- (bbox["ymax"]-bbox["ymin"])
   res <- max(nc,nr)/640
   p <- as.numeric(pretty(res))
   res <- p[which.min(abs(res-p))]
   regrid(setbound=unname(bbox),crs=crs,res=res)
}
