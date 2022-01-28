## http://leaflet-extras.github.io/leaflet-providers/preview/index.html
'glance' <- function(...) {
   arglist <- list(...)
   execute <- .getPrm(arglist,name="(execute|view|open|render)",default=!.isShiny())
   plotKnitr <- .isKnitr() & execute
   if (!length(arglist)) {
      viewer <- session_pngviewer(TRUE)
      on.exit(session_pngviewer(viewer))
      arglist <- .args2list()
   }
   if (!length(arglist)) {
      return(display())
   }
   if (inherits(arglist[[1]],c("list","character"))) {
      isWMS <- FALSE
      if ((is.list(arglist[[1]]))&&(all(sapply(arglist[[1]],.is.wms)))) {
         s1 <- unlist(arglist[[1]])
         isWMS <- TRUE
         sname <- names(s1)
      }
      else if (.is.wms(arglist[[1]])) {
         s1 <- arglist[[1]]
         isWMS <- TRUE
         sname <- ""
      }
      if (isWMS) {
        # session_grid(NULL)
         s2 <- unlist(sapply(s1,.compose_wms,extend=FALSE))
         compose_open(length(s2),legend=NULL,...)#,fileout="res1.png")
         pb <- ursaProgressBar(min=0,max=length(s2),tail=TRUE)
        # print(getOption("ursaPngScale"))
         for (i in seq_along(s2)) {
            arglist[[1]] <- s2[i]
            do.call("panel_new",arglist[-1])
            arglist$extend <- FALSE
            arglist$verbose <- !.isPackageInUse()
           # .panel_wms(s2[i],tile=1e5,legend=TRUE,verbose=!TRUE)
            a <- try(do.call(".panel_wms",arglist))
            if (inherits(a,"try-error"))
               cat(as.character(a))
            arglist$verbose <- FALSE
            if (length(sname))
               panel_annotation(sname[i],pos="bottomright")
            do.call("panel_decor",arglist[-1])
            setUrsaProgressBar(pb,i)
         }
         close(pb)
         compose_close(...)
         return(invisible(0L))
      }
   }
   if (!is.character(arglist[[1]])) {
      a <- do.call(".glance",arglist)
      if (plotKnitr)
         return(a)
      return(invisible(a))
   }
   if (!nchar(arglist[[1]])) {
      return(invisible(10L))
   }
  # if (.lgrep("\\.rds$",basename(arglist[[1]]))) {
  #    arglist[[1]] <- readRDS(arglist[[1]])
  #   # return(do.call(".glance",arglist))
  # }
  # str(arglist)
   if (is.character(arglist[[1]])) {
      if (.lgrep("\\.(gpkg|tab|kml|json|geojson|mif|sqlite|shp|osm)(\\.(zip|gz|bz2))*$"
                     ,arglist[[1]])) {
         ret <- do.call(".glance",arglist)
         if (plotKnitr)
            return(ret)
         return(invisible(ret))
      }
      else if (envi_exists(arglist[[1]],exact=TRUE)) {
         return(do.call("display",arglist))
      }
      else if (.lgrep("\\.(tif|tiff|img|png|bmp|dat|grb)$",arglist[[1]])) {
         return(do.call("display",arglist))
      }
      else {
         if ((TRUE)&&  #(!.isPackageInUse())&&
              (.lgrep("\\.(nc|hdf)$",basename(arglist[[1]])))) { ## dev
           # obj <- .open_nc(arglist[[1]])
            obj <- .read_nc(arglist[[1]],".+")
            if (!inherits(obj,"data.frame")) {
               obj <- obj[sapply(obj,is.ursa)]
               if (length(arglist)==1)
                  return(display(obj))
               return(do.call("display",list(obj,arglist[-1])))
            }
            else {
               indX <- .grep("^(lon|x$|west|east)",colnames(obj))
               indY <- .grep("^(lat|y$|south|north)",colnames(obj))
               if (!length(indX))
                  indX <- 1L
               if (!length(indY))
                  indY <- 2L
               colnames(obj)[c(indX,indY)] <- c("x","y")
            }
            stop("Development has been stopped")
            ##~ p4s <- attr(obj,"crs")
            ##~ if (isSF) {
               ##~ if (!is.null(p4s))
                  ##~ obj <- sf::st_as_sf(obj,coords=c("x","y"),crs=attr(obj,"crs"))
               ##~ else
                  ##~ obj <- sf::st_as_sf(obj,coords=c("x","y"))
            ##~ }
            ##~ if (isSP) {
               ##~ sp::coordinates(obj) <- ~x+y
               ##~ if (!is.null(p4s))
                  ##~ sp::proj4string(obj) <- sp::CRS(p4s)
            ##~ }
         }
         if (.lgrep("\\.rds$",basename(arglist[[1]]))) {
            obj <- readRDS(arglist[[1]])
           # print(class(obj))
           # print(inherits(obj,"Spatial"))
            if (.isSP(obj)) {
              # print(isS4(obj))
              # print(loadedNamespaces())
              # requireNamespace("methods")
               arglist[[1]] <- quote(obj) ## 'GADM' distributes 'rds'
               arglist$engine <- "sp"
               return(do.call(".glance",arglist))
            }
            if (inherits(obj,"may be GDAL???")) { ## not good idea
               arglist[[1]] <- quote(obj)
               return(do.call("display",arglist))
            }
         }
         
         b <- if (file.exists(arglist[[1]])) open_gdal(arglist[[1]]) else NULL
         if (!is.null(b)) {
            close(b)
            do.call("display",arglist)
            return(invisible(2L))
         }
        # else if (requireNamespace("sf",quietly=.isPackageInUse())) {
        #    do.call(".glance",arglist)
        # }
         else {
           # message("Cannot complete without suggested package 'sf'.")
            ret <- do.call(".glance",arglist)
            if (plotKnitr)
               return(ret)
            return(invisible(ret))
         }
      }
   }
   invisible(0L)
}
'.glance' <- function(dsn,layer=".*",grid=NULL,field=".+",size=NA,expand=1
                        ,border=27,lat0=NA,lon0=NA,resetProj=FALSE,resetGrid=FALSE
                        ,style="auto"
                        ,feature=c("auto","field","geometry"),alpha=NA
                        ,basemap.order=c("after","before"),basemap.alpha=NA
                        ,engine=c("native","sp","sf")
                        ,geocode="",place="",area=c("bounding","point","shape")
                        ,zoom=NA,gdal_rasterize=FALSE
                        ,silent=FALSE,verbose=FALSE,...) {
   arglist <- list(...)
   a <- as.list(match.call())
  # feature <- "geometry"
   if (TRUE) {
      geocodeList <- eval(as.list(args(.geocode))$service)
      if (!nchar(geocode))
         geocode <- geocodeList
      else
         for (i in seq_along(geocode))
            geocode[i] <- match.arg(geocode[i],geocodeList)
   }
   else
      geocode <- match.arg(geocode)
   projClass <- c("longlat","stere","laea","merc")
   projPatt <- paste0("(",paste(projClass,collapse="|"),")")
   staticMap <- c("openstreetmap","sputnikmap","google")
   tileClass <- c(staticMap,.tileService())
   tilePatt <- paste0("(",paste0(unique(c(staticMap,tileClass)),collapse="|"),")")
   basemap.order <- match.arg(basemap.order)
   after <- basemap.order %in% "after"
   before <- basemap.order %in% "before"
   feature <- match.arg(feature)
   engine <- match.arg(engine)
  # area <- match.arg(area)
  # print(c(dsn=class(dsn)))
  # obj <- spatialize(dsn)
   if (missing(dsn)) {
      dsn <- if (style!="auto") .geomap(style=style) else .geomap()
      return(display(dsn,...)) ## ++20180617
   }
   toUnloadMethods <- !("methods" %in% .loaded())
   S4 <- isS4(dsn)
   if (S4) {
      .require("methods",quietly=.isPackageInUse())
     # requireNamespace("methods",quietly=.isPackageInUse())
   }
   obj <- spatialize(dsn=dsn,engine=engine,layer=layer,field=field,geocode=geocode
                   ,place=place,area=area,grid=grid,size=size
                  # ,expand=expand,border=border
                   ,expand=expand,border=0
                   ,lat0=lat0,lon0=lon0
                   ,resetProj=resetProj,resetGrid=resetGrid
                   ,style=style#,zoom=NA
                   ,verbose=verbose)
   if (inherits(obj,"NULL"))
      return(invisible(NULL))
   isSF <- inherits(obj,c("sfc","sf"))
   isSP <- !isSF
   g0 <- attr(obj,"grid")
   if (is.null(g0))
      g0 <- session_grid()
  # g1 <- getOption("ursaSessionGrid")
  # if (identical(g0,g1))
  #    border <- 0
   if ((FALSE)&&(expand!=1)) {
      bbox <- with(g0,c(minx,miny,maxx,maxy))
      .sc <- (expand-1)*sqrt(diff(bbox[c(1,3)])*diff(bbox[c(2,4)]))
      bbox <- bbox+c(-1,-1,1,1)*.sc
      g0 <- regrid(g0,bbox=bbox)
      print(g0)
   }
   toUnloadMethods <- if (S4) toUnloadMethods else attr(obj,"toUnloadMethods")
  # dname <- attr(obj,"colnames")
   dname <- spatial_fields(obj)
   style <- attr(obj,"style")
   geocodeStatus <- attr(obj,"geocodeStatus")
  # obj <- sp::SpatialLinesDataFrame(obj,data=data.frame(uid="test it")) ## for debug
   noAttr <- geocodeStatus |
       ((isSF)&&(inherits(obj,"sfc"))) |
       ((isSP)&&(!inherits(obj,paste0("Spatial",c("Points","Lines","Polygons")
                                     ,"DataFrame"))))
   toColor <- ((is.numeric(dsn))||(noAttr))||(TRUE)
   if ((toColor)&&(!.lgrep("(colo(u)*r|gr[ae]y(scale)*)",style))&&
          (length(style)==1))
      style <- paste(style,"color")
   else if ((!toColor)&&(!.lgrep("(colo(u)*r|gr[ae]y(scale)*)",style)&&
          (length(style)==1)))
      style <- paste(style,"greyscale")
   if (!.lgrep(projPatt,style))
      proj <- "auto"
   else
      proj <- .gsub2(projPatt,"\\1",style)
   if (!FALSE) { ## dev
      s1 <- .tileService()
      s2 <- strsplit(style,split="\\s+")
      s <- expand.grid(s1,s2)
   }
   isUrl <- .lgrep("http(s)*://",style)
   if (isUrl) {
      ind <- .grep("http(s)*://",style)
      style[ind] <- unlist(strsplit(style[ind],split="\\s+"))[1]
   }
   if (!.lgrep(tilePatt,style)) {
      if (isUrl) {
         art <- style
         proj <- "merc"
      }
      else
         art <- "none"
   }
   else {
      art <- .gsub2(tilePatt,"\\1",style)
      proj <- "merc"
   }
   isWeb <- .lgrep(tilePatt,art) | isUrl
  # proj <- match.arg(proj)
  # attr(obj,"grid") <- NULL
  # attr(obj,"toUnloadMethods") <- NULL
  # attr(obj,"dname") <- NULL
   #attr(obj,"geocodeStatus") <- NULL
   basemapRetina <- FALSE
   if (isWeb) {
      bbox <- with(g0,c(minx,miny,maxx,maxy))
      lim <- c(.project(matrix(bbox,ncol=2,byrow=TRUE)
                                               ,g0$crs,inv=TRUE))[c(1,3,2,4)]
      ostyle <- unlist(strsplit(style,split="\\s+"))
      isStatic <- ostyle[1] %in% staticMap
      ustyle <- ""#ostyle[1]
      if (isStatic)
         nextStyle <- .grep(ostyle[1],staticMap
                           ,invert=TRUE,value=TRUE)
      else
         nextStyle <- .grep(ostyle[1],c("mapnik","mapsurfer","cartoDB","opentopomap")
                           ,invert=TRUE,value=TRUE)[seq(3)]
      nsize <- length(nextStyle)+1
      cache <- .getPrm(arglist,"cache",class=c("logical","character"),default=TRUE)
      for (i in seq(nsize)) {
         opE <- options(show.error.messages=TRUE)
         basemap <- try(.geomap(lim,style=style,size=size,zoom=zoom
                       ,border=border,cache=cache,verbose=verbose))
         options(opE)
         if (!inherits(basemap,"try-error"))
            break
        # message(geterrmessage())
         if (i==nsize)
            break
         .style <- style
         style <- paste(nextStyle[1],ostyle[-1],collapse=" ")
         nextStyle <- nextStyle[-1]
         message(paste("failed to get map; change service:"
                      ,.sQuote(.style),"->",.sQuote(style)))
      }
     # print(basemap)
     # str(basemap)
      if (inherits(basemap,"try-error")) {
         message(paste("failed to get map; cancel"))
         print(lim)
         basemap <- NULL
      }
      else {
         basemapRetina <- isTRUE(attr(basemap,"retina"))
         g0 <- ursa(basemap,"grid") ## 0605 TODO
      }
     # print(g0)
   }
   else {
      basemap <- NULL
      zrel <- zabs <- NA
      if (isTRUE(is.numeric(zoom))) { ## absolute
         zabs <- zoom
      }
      else if (isTRUE(is.character(zoom))) { ## relative
         if (nchar(gsub("\\d+","",zoom))==0)
            zabs <- as.integer(gsub("\\D","",zoom))
         else {
            zrel <- gregexpr("^(\\+)+$",zoom)[[1]]
            if ((length(zrel)==1)&&(zrel==1)) {
              # str(zrel)
               zrel <- 2^(-attr(zrel,"match.length"))
            }
            else {
               zrel <- gregexpr("^(\\-)+$",zoom)[[1]]
               if ((length(zrel)==1)&&(zrel==1)) {
                 # str(zrel)
                  zrel <- 2^(+attr(zrel,"match.length"))
               }
               else if (.lgrep("^(\\+|\\-)\\d$",zoom)) {
                  zrel <- 2^(-eval(parse(text=paste0("",zoom))))
               }
               else
                  zrel <- NA
            }
         }
      }
     # print(c(zabs=zabs,zrel=zrel))
      if ((!is.na(zabs))||(!is.na(zrel))) {
         ps <- ursa(g0,"cellsize")
         if (!is.na(zabs)) {
           # print(g0)
            z1 <- 2*6378137/(2^(seq(21)+8))
            z2 <- 2^c(24,25)[1]/(2^(seq(21)+8))
            ##~ print(format(z1,sci=FALSE),quote=FALSE)
            ##~ print(format(z2,sci=FALSE),quote=FALSE)
            res <- if (!is.na(.is.near(ps,z1))) z1[round(zabs)] else z2[zabs]
         }
         else if (!is.na(zrel))
            res <- ps*zrel # ps*2^zrel
         if (FALSE) {
           # g0 <- regrid(g0,mul=1/m,expand=m)
            g0 <- regrid(g0,res=res,expand=res/ps)
         }
         else {
            m <- res/ps
            bbox <- with(g0,c(minx,miny,maxx,maxy))
            m2 <- if (m<1) 1 else with(g0,sqrt((maxx-minx)*(maxy-miny)))/2
           # print(c(m=m,m2=m2,ps=ps,res=res,expand=m*m2))
           # g0 <- regrid(g0,mul=1/m,bbox=bbox+0*c(-1,-1,1,1)*m*m2)
            g0 <- regrid(g0,res=res,bbox=bbox+c(-1,-1,1,1)*m*m2)
         }
      }
   }
   if ((is.null(basemap))&&(border>0)) {
      g0 <- regrid(g0,border=border)
   }
   attr(obj,"grid") <- g0
   session_grid(g0)
  # str(g0)
  # xy <- with(g0,.project(rbind(c(minx,miny),c(maxx,maxy)),crs,inv=TRUE))
  # display(blank="white",col="black");q()
   if (verbose)
      print(c(sf=isSF,sp=isSP))
   if (isSF) {
      geoType <- unique(as.character(sf::st_geometry_type(obj)))
      obj_geom <- sf::st_geometry(obj)
     # bbox <- c(sf::st_bbox(obj))
     # crs <- sf::st_crs(obj)$proj4string
   }
   if (isSP) {
      geoType <- switch(class(sp::geometry(obj))
                       ,SpatialPolygons="POLYGON"
                       ,SpatialPoints="POINT"
                       ,SpatialLines="LINE")
      obj_geom <- switch(geoType,POLYGON=methods::slot(sp::geometry(obj),"polygons")
                                   ,LINE=methods::slot(sp::geometry(obj),"lines")
                                  ,POINT=sp::geometry(obj))
     # bbox <- c(sp::bbox(obj))
     # names(bbox) <- c("xmin","ymin","xmax","ymax")
     # crs <- sp::proj4string(obj)
   }
   toCoast <- !isWeb | isWeb & .getPrm(list(...),name="coast",default=FALSE)
   if ((FALSE)&&(.lgrep("POLYGON",geoType))&&(isSF)) {
      valid <- .try(ov <- sf::st_covers(obj_geom,sparse=!FALSE))
      if (!valid)
         isOverlap <- FALSE
      else 
         isOverlap <- length(which(sapply(ov,function(x) length(x)>1)))>0
   }
   else
      isOverlap <- FALSE
   if (feature=="auto")
      feature <- ifelse(isOverlap,"geometry","field")
   if ((!toUnloadMethods)&&(isSP)&&(!"package:methods" %in% search())) {
     # I've read "R-exts 1.1.3.1",
     # but sp::plot() is failed for 'requireNamespace("methods")
     # .require("methods")
      opW <- options(warn=-1)
      warning("Package 'methods' is required for S4 object coercion.")
      options(opW)
      toUnloadMethods <- TRUE
   }
   if ((gdal_rasterize)&&(nchar(Sys.which("gdal_rasterize")))) {
      res <- .rasterize(obj,feature=feature,verbose=verbose)
   }
   ##~ print(session_grid())
   if (isSF)
      geoType <- unique(as.character(sf::st_geometry_type(obj)))
   if (isSP) {
     # geoType <- geoType ## no change
   }
  # dname <- names(attr(obj,"relation_to_geometry"))
  # ct <- colorize(obj[,dname[4],drop=TRUE][,1],alpha=1)
  # print(ct)
  # print(unname(ct$colortable[ct$ind]))
  # require(methods)
   if (geocodeStatus) { ## move to 'visualization' block
      if ("auto" %in% style) {
         if (geocode=="google")
            style <- "google static"
         else if (geocode=="nominatim")
            style <- "openstreetmap static"
         else
            style <- "openstreetmap static"
      }
      if ((is_spatial_lines(obj))&&("bounding" %in% area)) {
         basemap.alpha <- 1
         alpha <- 0
      }
   }
   if ((isWeb)&&(is.na(basemap.alpha)))
      basemap.alpha <- ifelse(before,0.5,0.35)
   if (is.na(alpha))
      alpha <- ifelse(isWeb,ifelse(before,0.75,1),1)
   if (feature=="field") {
      noData <- length(dname)==0
      ct <- vector("list",length(dname))
      cpg <- NULL#"1251"
      for (i in seq_along(dname)) {
        # print(i)
        # print(dname[i])
        # str(obj[,dname[i]][,1])
         if (isSF) {
           # val <- obj[,dname[i],drop=TRUE][,1] ## sf>=0.4
            val <- obj[,dname[i],drop=TRUE]#[,1] ## sf>=0.5
         }
         if (isSP)
            val <- methods::slot(obj,"data")[,dname[i],drop=TRUE]
         if ((is.character(cpg))&&(is.character(val)))
            val <- iconv(val,"UTF-8","1251")
        # print(all(is.na(val)))
        # print(obj[,dname[i],drop=TRUE][,1])
         if (all(is.na(val)))
            ct[[i]] <- colorize("(all values are NA)")
         else 
            ct[[i]] <- colorize(val,alpha=alpha,...)
        # print(names(ct[[i]]$colortable),quote=FALSE)
        # print(ct[[i]])
        # print("-----------------------------------------")
      }
      if (noData)
         ct <- list(colorize(seq(spatial_count(obj)),alpha=0.5))
      hasField <- any(sapply(ct,function(x) any(!is.na(x$index))))
      if ((!hasField)&&(alpha==1))
         alpha <- 0.5
      if (!gdal_rasterize) {
         if (length(dname))
            res <- lapply(rep(NA,length(ct)),ursa_new)
         else
            res <- list(geometry=ursa_new())
      }
      if (isWeb) {
        # retina1 <- getOption("ursaRetina")
         retina2 <- .getPrm(arglist,"retina",default=NA_integer_)
         retina3 <- 1L+as.integer(basemapRetina)
        # print(c(retina1=retina1,retina2=retina2,retina3=retina2))
         retina <- ifelse(is.na(retina2),retina3,retina2)
         compose_open(res,scale=1,retina=retina,...)
      }
      else
         compose_open(res,...)
      gline <- compose_graticule(...)
      if (toCoast)
         cline <- compose_coastline(...)
      pb <- ursaProgressBar(min=0,max=length(res),silent=silent,tail=TRUE)
      for (i in seq_along(res)) {
         if (isWeb)
            panel_new(fill="transparent",...)
         else
            panel_new(...) #fill=ifelse(isWeb,"transparent","chessboard"))
         if (before) {
            panel_plot(basemap,alpha=basemap.alpha)
         }
        # if ((!length(ct))||(all(is.na(ct[[i]]$index)))) {
         if ((!length(ct))||(!hasField)) {
            if (isSF) {
               panel_plot(obj_geom,col=rgb(0,0,0,alpha))
            }
            if (isSP) {
               panel_plot(obj,col=rgb(0,0,0,alpha))#,add=TRUE)
            }
         }
         else if (gdal_rasterize){
            ct[[i]] <- panel_raster(colorize(res[[i]]),alpha=alpha)
           # ct[[i]] <- panel_raster(res[[i]],alpha=alpha,verbose=TRUE)
         }
         else {
           # col <- unname(ct[[i]]$colortable[ct[[i]]$ind])
            col <- ct[[i]]$colortable[ct[[i]]$ind]
            ind <- which(is.na(col))
            bg.line <- rep("#0000007F",length(col))
            bg.point <- rep("#000000FF",length(col))
            bg.polygon <- rep("#000000FF",length(col))
            if (length(ind)) {
               col[ind] <- "#FFFFFF00"
               bg.line[ind] <- "#0000000F"
               bg.point[ind] <- "#0000002F"
               bg.polygon[ind] <- "#0000002F"
            }
            if (.lgrep("polygon",geoType)) {
               if ((T)&&(spatial_count(obj)>1000)&&(sd(spatial_area(obj))<0.05))
                  lwd <- 1e-6
               else
                  lwd <- 0.1
               if (length(ind <- .grep("plot\\.lwd",names(arglist))))
                  lwd <- arglist[[ind]]
               if (lwd==0)
                  lwd <- 1e-6
              # .elapsedTime("Z")
              # panel_plot(obj,col=col,border=bg.polygon,lwd=lwd)
              # str(col)
               if (FALSE) {
                  .elapsedTime("P1")
                  panel_plot(obj,col=col,border=bg.polygon,lwd=lwd,lty="blank")
                  .elapsedTime("P2")
                  panel_plot(obj,col="transparent",border=bg.polygon,lwd=lwd)
                  .elapsedTime("P3")
               }
               else {
                  panel_plot(obj,col=col,border=bg.polygon,lwd=lwd)
               }
            }
            if (.lgrep("point",geoType)) {
               lwd <- 0.25
               if (length(ind <- .grep("plot\\.lwd",names(arglist))))
                  lwd <- arglist[[ind]]
               cex <- 1.2
               if (length(ind <- .grep("plot\\.cex",names(arglist))))
                  cex <- arglist[[ind]]
               pch <- 21
               if (length(ind <- .grep("plot\\.pch",names(arglist))))
                  cex <- arglist[[ind]]
               panel_plot(obj,col=bg.point,bg=col,pch=pch,lwd=lwd,cex=cex)
            }
            if (.lgrep("line",geoType)) {
               lwd <- c(3,2)
               if (length(ind <- .grep("plot\\.lwd",names(arglist)))) {
                  lwd <- rep(arglist[[ind]],length=2)
               }
               panel_plot(obj,lwd=lwd[1],col=bg.line)
               panel_plot(obj,lwd=lwd[2],col=col)
            }
         }
         if (after) {
            panel_plot(basemap,alpha=basemap.alpha)
         }
         if (toCoast)
            panel_coastline(cline)
         if ((geocodeStatus)||(!length(ct)))
            panel_graticule(gline,margin=c(T,T,F,F))
         else
            panel_graticule(gline)
         if (proj %in% c("merc")) {
            if ((g0$miny<0)&&(g0$maxy>0)) {
               if ((-g0$miny)>(g0$maxy))
                  y <- c(g0$miny,0)
               else
                  y <- c(0,g0$maxy)
            }
            else
               y <- c(g0$miny,g0$maxy)
            sc <- 1/cos(.project(cbind((g0$minx+g0$maxx)/2,y)
                                ,g0$crs,inv=TRUE)[,2]*pi/180)
           # x <- 0#ifelse(((isWeb)&&(isStatic)&&(isGoogle)),0.5,0)
           # print(art)
            x <- ifelse(any(art %in% "google"),0.5,0)
            if (max(sc)/min(sc)>1.2) {
               y <- (y-g0$miny)/(g0$maxy-g0$miny)
               panel_scalebar(pos=c(x,min(y)),...)
               panel_scalebar(pos=c(x,max(y)),...)
            }
            else { # if (isWeb)
               panel_scalebar(pos=c(x[1],0),...)
            }
           # else
           #    panel_scalebar(pos="bottomleft",...)
         }
         else
            panel_scalebar(pos="bottomleft",...)
         setUrsaProgressBar(pb)
      }
      close(pb)
      if ((length(ct))&&(!noData)) {
         if (!gdal_rasterize) {
            ct <- lapply(ct,function(x) {
               y <- ursa(x,"colortable") ## y <- x$colortable
               if ((TRUE)&&(isWeb)&&(after)) {
                  alpha2 <- 0.65
                  y[] <- paste0(substr(y,1,7),toupper(as.hexmode(round(alpha2*255))))
               }
               y
            })
         }
         names(ct) <- dname
         if (TRUE) {
            if (!.lgrep("las",names(arglist)))
               arglist$las <- 2
            if (!.lgrep("trim",names(arglist)))
               arglist$trim <- 2L
            arglist <- c(ct,arglist)
            do.call("compose_legend",arglist)
         }
         else
            compose_legend(ct,las=2,trim=2L)
      }
      ret <- compose_close(...) #res)
   }
   else if (feature=="geometry") {
     # print(geoType)
      if (isSF)
         n <- length(obj_geom)
      if (isSP)
         n <- length(obj_geom)
      if (FALSE) {
         if (isSF) {
           # da <- obj[,dname,drop=TRUE][,1] ## wrong
            da <- obj[,dname,drop=TRUE]#[,dname,drop=TRUE]
            names(da) <- dname
         }
         if (isSP)
            da <- methods::slot(obj,"data")[,dname,drop=FALSE]
      }
      else
         da <- spatial_data(obj)
      if ((is.data.frame(da))&&(!ncol(da)))
         da <- NULL
      if (!is.null(da)) {
         da <- rbind(format(da),paste0(names(da),":"))
        # print(da)
        # print(format(da))
        # e <- format(t(da),justify="right")
         e <- .gsub("^\\s+","",t(da))
        # e1 <- paste(apply(e[,c(n+1,1)],1,paste,collapse=" "),collapse="\n")
        # print(e)
        # message(e1)
        # q()
     }
      if (!gdal_rasterize)
         res <- ursa_new(nband=n)
      ct <- lapply(seq(n),function(i) colorize(0L))
      if (isWeb)
         compose_open(res,scale=1,legend=NULL,...)
      else
         compose_open(res,legend=NULL,...)
      gline <- compose_graticule(...)
      if (toCoast)
         cline <- compose_coastline(...)
      pb <- ursaProgressBar(min=0,max=length(res))
     # geoType <- "skip"
      for (i in seq_along(res)) {
         if (isWeb)
            panel_new(fill="transparent",...)
         else
            panel_new(...)
         if (before) {
            panel_plot(basemap,alpha=basemap.alpha)
         }
         if (!gdal_rasterize) {
           # panel_plot(obj_geom[[i]])
            if (!isSP) {
               if (.lgrep("polygon",geoType)) {
                  panel_plot(obj_geom[[i]]
                            ,col=unname(ct[[i]]$colortable[ct[[i]]$ind]),lwd=0.1)
               }
               if (.lgrep("point",geoType)) {
                  panel_plot(obj_geom[[i]]
                            ,col="black",bg=unname(ct[[i]]$colortable[ct[[i]]$ind])
                            ,pch=21,lwd=0.25,cex=1)
               }
               if (.lgrep("line",geoType)) {
                  panel_plot(obj_geom[[i]],lwd=3,col="#0000007F")
                  panel_plot(obj_geom[[i]],lwd=2
                            ,col=unname(ct[[i]]$colortable[ct[[i]]$ind]))
               }
            }
            else {
               if (.lgrep("polygon",geoType)) {
                  panel_plot(obj[i,],col=unname(ct[[i]]$colortable[ct[[i]]$ind])
                            ,lwd=0.1)#,add=TRUE)
               }
               if (.lgrep("point",geoType)) {
                  panel_plot(obj[i,]
                            ,col="black",bg=unname(ct[[i]]$colortable[ct[[i]]$ind])
                            ,pch=21,lwd=0.25,cex=1)#,add=TRUE)
               }
               if (.lgrep("line",geoType)) {
                  panel_plot(obj[i,],lwd=3,col="#0000007F",add=TRUE)
                  panel_plot(obj[i,],lwd=2
                            ,col=unname(ct[[i]]$colortable[ct[[i]]$ind]))#,add=TRUE)
               }
            }
         }
         else
            panel_raster(res[i])
         if (after) {
            panel_plot(basemap,alpha=basemap.alpha)
         }
         if (toCoast)
            panel_coastline(cline)
         panel_graticule(gline)
         panel_scalebar(pos=ifelse(isWeb,"bottomleft","bottomleft"),...)
         if (!is.null(da)) {
            e1 <- paste(apply(e[,c(n+1,i),drop=FALSE],1,paste,collapse=" ")
                       ,collapse="\n")
            panel_annotation(text=e1,adj=0,...) # pos="topleft")
         }
         setUrsaProgressBar(pb)
      }
      close(pb)
      ret <- compose_close(...)
     # str(n)
   }
   if ((toUnloadMethods)&&("package:methods" %in% search())) {
      if (!(any(c("package:sp","zzzzzzzzzz") %in% search())))
      detach("package:methods",unload=FALSE) 
     # but namespace "methods" is not unloaded, because namespace "sp" is loaded
     # 'as' is not found now
   }
   if (.isKnitr()) {
      return(invisible(ret))
      if (proposed <- FALSE) {
         render <- .getPrm(arglist,name="(open|render|execute|view)",default=TRUE)
         print(c(render=render))
         if (render)
            return(ret)
         else
            return(invisible(ret))
      }
   }
   invisible(ret)
}
'.cmd.glance' <- function() {
   a <- .args2list()
   do.call(".glance",a)
   NULL
}
