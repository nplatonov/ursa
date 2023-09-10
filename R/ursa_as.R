'as_ursa' <- function(obj,...) as.ursa(obj=obj,...)
'as.ursa' <- function(obj,...) {
   if (missing(obj))
      return(ursa_new(...))
   if (is.ursa(obj))
      return(obj)
   if (.is.ursa_stack(obj))
      return(ursa_brick(obj)) ## 20170122 removed 'return(obj)'
   if (is.data.frame(obj)) {
      return(allocate(obj,...))
   }
   if (inherits(obj,c("SpatialGridDataFrame"))) {
      requireNamespace("sp",quietly=.isPackageInUse())
      cs <- methods::slot(sp::getGridTopology(obj),"cellsize")
      bb <- c(sp::bbox(obj))
      opW6 <- options(warn=ifelse(.isPackageInUse(),0,1))
      pr <- sp::proj4string(obj)
      options(opW6)
     # g1 <- regrid(bbox=bb,res=cd,proj=p)
      g1 <- regrid(ursa_grid(),bbox=bb,res=cs,proj=pr)
      session_grid(g1)
      isList <- F
     # dt <- all(sapply(obj@data,class) %in% c("numeric"))
      dt <- sapply(obj@data,class)
      isList <- !(all(dt %in% "integer") | all(dt %in% "numeric"))
      if (isList) {
         res <- vector("list",ncol(methods::slot(obj,"data")))
         names(res) <- names(dt) # colnames(methods::slot(obj,"data"))
      }
      else {
         res <- ursa(bandname=names(dt))
      }
      for (i in seq_len(ncol(methods::slot(obj,"data")))) {
         value <- methods::slot(obj,"data")[,i]
         if (!is.numeric(value)) {
            val <- factor(value)
            value <- as.integer(val)-1L
            ct <- ursa_colortable(colorize(value,name=levels(val)))
            if (TRUE)
               ct[] <- NA
            res[[i]] <- ursa_new(value,colortable=ct,flip=FALSE
                                ,permute=FALSE)#,bandname=names(res)[i])
            class(res[[i]]$value) <- "ursaCategory"
         }
         else {
            if (isList)
               res[[i]] <- ursa_new(value,flip=FALSE
                                   ,permute=FALSE)#,bandname=names(res)[i])
            else {
               res$value[,i] <- value
            }
         }
      }
      if (isList) {
         if (length(ind <- which(sapply(res,is.null)))) {
            res[ind] <- as.list(ursa_new(NA,nband=length(ind)))
         }
         for (i in seq_along(res))
            names(res[[i]]) <- names(res)[i]
      }
      return(res)
   }
   if (inherits(obj,c("SpatialPointsDataFrame"#,"SpatialPoints"
                     ,"SpatialPixelsDataFrame"))) {
      return(allocate(obj,...))
   }
   if (inherits(obj,c("RasterBrick","RasterStack","RasterLayer"))) {
      g1 <- .grid.skeleton()
      e <- raster::extent(obj)
      ##~ g1$minx <- round(e@xmin,5)
      ##~ g1$maxx <- round(e@xmax,5)
      ##~ g1$miny <- round(e@ymin,5)
      ##~ g1$maxy <- round(e@ymax,5)
      g1$minx <- e@xmin
      g1$maxx <- e@xmax
      g1$miny <- e@ymin
      g1$maxy <- e@ymax
      g1$columns <- raster::ncol(obj)
      g1$rows <- raster::nrow(obj)
      g1$crs <- raster::projection(obj)
      if (is.na(g1$crs))
         g1$crs <- ""
      g1$resx <- with(g1,(maxx-minx)/columns)
      g1$resy <- with(g1,(maxy-miny)/rows)
      session_grid(g1)
      if (all(is.factor(obj))) {
         cname <- raster::levels(obj)[[1]]$code
         if (is.null(cname))
            cname <- raster::levels(obj)[[1]]$category
         ct <- raster::colortable(obj)
         if (!length(ct))
            ct <- rep(NA,length(cname))
         names(ct) <- cname
         class(ct) <- "ursaColorTable"
      }
      else {
         if (!inherits(obj,"RasterStack")) {
           # at <- obj@data@attributes[[1]]#[,2,drop=FALSE]
            pal <- raster::colortable(obj)
            if (length(pal)) {
               ct <- colorize(pal=pal,name=obj@data@attributes[[1]]$code)
               ct[pal=="NA"] <- NA_character_
            }
            else {
               ct <- character() #colorize(name=obj@data@attributes[[1]]$code)
            }
           # ct <- ursa_colortable(as.character(raster::colortable(obj)))
         }
         else {
            ct <- lapply(methods::slot(obj,"layers"),function(x) {
               ursa_colortable(as.character(raster::colortable(x)))
            })
         }
      }
      if (inherits(obj,"RasterStack")) {
         res <- vector("list",dim(obj)[3])
         for (i in seq_along(res)) {
            res[[i]] <- ursa_new(value=raster::values(obj[[i]])
                                ,bandname=names(obj[[i]])
                                ,flip=FALSE,permute=FALSE)
            ursa_colortable(res[[i]]) <- ct[[i]]
         }
      }
      else if (inherits(obj,"RasterBrick")) {
         res <- ursa_new(value=raster::values(obj),flip=FALSE,permute=FALSE)
         ursa_colortable(res) <- ct
      }
      else if (inherits(obj,"RasterLayer")) {
         if (length(ct))
            res <- ursa_new(raster::values(obj),colortable=ct
                           ,flip=FALSE,permute=FALSE)
         else {
            at <- obj@data@attributes#[[1]]#[,2,drop=FALSE]
            if ((!length(at))||((length(at)==1)&&(!length(at[[1]]))))
               res <- ursa_new(raster::values(obj),flip=FALSE,permute=FALSE)
            else {
               at <- at[[1]]
               v <- raster::values(obj)
               u <- ursa_new(v-1L,flip=FALSE,permute=FALSE)
               res <- vector("list",ncol(at))
               aname <- colnames(at)
               for (i in seq_along(res)) {
                  tname <- names(table(at[,i]))
                  ind <- match(at[,i],tname)
                  obj <- ursa_new(ind[v]-1L,flip=FALSE,permute=FALSE
                                 ,bandname=aname[i])
                  res[[i]] <- reclass(obj,src=seq_along(tname)-1L,dst=tname)
               }
              # stop("R: as.ursa from raster with attributes")
            }
         }
      }
      return(res)
   }
   if ((is.list(obj))&&(!anyNA(match(c("filename","cols","rows","bands","crs"
                                      ,"geotransform","datatype","meta")
                                    ,names(obj))))) { ## from 'sf::gdal_read'
     # .elapsedTime("A")
      columns <- obj$cols[2]
      rows <- obj$rows[2]
      bands <- obj$bands[2]
     # patt <- "^Band_(\\d+)=\\t*(.+)$"
     # bname <- grep(patt,obj$meta,value=TRUE)
     # b1 <- .grep(patt,obj$meta,value=TRUE)
     # bname <- .gsub(patt,"\\2",b1)
     # bname[as.integer(.gsub(patt,"\\1",b1))] <- bname
      resx <- obj$geotransform[2]
      resy <- -obj$geotransform[6]
      minx <- obj$geotransform[1]
      maxy <- obj$geotransform[4]
      maxx <- minx+columns*resx
      miny <- maxy-rows*resy
      if (miny>maxy) {
         interim <- maxy
         maxy <- miny
         miny <- interim
      }
      ##~ prm <- list(minx=minx,maxx=maxx,miny=miny,maxy=maxy,columns=columns,rows=rows
                  ##~ ,crs=obj$crs$proj4string)
      ##~ g1 <- regrid(minx=minx,maxx=maxx,miny=miny,maxy=maxy,columns=columns,rows=rows
                  ##~ ,crs=obj$crs$proj4string)
     # .elapsedTime("I")
      if (F) 
         g1 <- regrid(setbound=c(minx,miny,maxx,maxy),dim=c(rows,columns)
                     ,crs=obj$crs$proj4string)
      else {
         g1 <- .grid.skeleton()
         g1$columns <- as.integer(columns)
         g1$rows <- as.integer(rows)
         g1$minx <- minx
         g1$maxx <- maxx
         g1$miny <- miny
         g1$maxy <- maxy
         g1$resx <- with(g1,(maxx-minx)/columns)
         g1$resy <- with(g1,(maxy-miny)/rows)
         g1$crs <- obj$crs$proj4string
      }
      if (is.na(g1$crs))
         g1$crs <- ""
     # .elapsedTime("J")
      session_grid(g1)
     # hasData <- inherits("NULL",class(attr(obj,"data")))
      hasData <- !inherits(attr(obj,"data"),"NULL")
     # .elapsedTime("sf::gdal_read -- start")
      if (!hasData) {
         if (!requireNamespace("sf",quietly=.isPackageInUse()))
            stop("Package 'sf' is required for this operation")
         res <- as.ursa(attr(sf::gdal_read(obj$filename,read_data=TRUE),"data")
                       ,flip=TRUE) ## RECURSIVE
      }
      else {
         v <-  attr(obj,"data")
         attr(v,"units") <- NULL
         dimv <- dim(v)
         if (R.Version()$arch %in% c("i386","x86_64","dummy")[1:2]) {
           # print("U")
            if (devel2 <- TRUE) {
               if (length(dimv)==2)
                  dimv <- c(dimv,band=1L)
               dimv <- unname(c(prod(dimv[1:2]),dimv[3]))
               dim(v) <- dimv
               isClass <- length(obj$attribute_tables[[1]])>0
               isColor <- length(obj$color_tables[[1]])>0
               isCat <- (isClass)||(isColor)
               if (isCat) {
                  if (isClass) {
                     ctName <- obj$attribute_tables[[1]][["category"]]
                     if (is.null(ctName))
                        isCat <- FALSE
                  }
               }
               if (isCat) {
                  if (isColor) {
                     ctCol <- obj$color_tables[[1]]
                     ct <- rgb(ctCol[,1],ctCol[,2],ctCol[,3],ctCol[,4],maxColorValue=255)
                     if (isClass)
                        if (length(ct)>length(ctName))
                           ct <- ct[seq_len(length(ctName))]
                  }
                  else
                     ct <- rep(NA,length(ctName))
                  if (isClass)
                     names(ct) <- ctName
                 # else
                 #    names(ct) <- 
                  class(ct) <- "ursaColorTable"
               }
               if ((isCat)||((T & !.lgrep("float",obj$datatype)))) {
                 # .elapsedTime("F")
                 # v <- as.integer(v)
                 # dim(v) <- dimv
                 # storage.mode(v) <- "integer"
                  mode(v) <- "integer"
                 # .elapsedTime("G")
               }
              # .elapsedTime("as.ursa -- before")
              # res <- as.ursa(v) ## RECURSIVE
               res <- ursa_new(v)
              # .elapsedTime("as.ursa -- after")
               if (isCat) {
                  ursa_colortable(res) <- ct
                  class(res$value) <- "ursaCategory"
               }
               bname <- obj$description
               if (any(nchar(bname)>0)) {
                  names(res) <- gsub("\\t","",bname) ## patch for ENVI 'band name'
               }
            }
            else {
               if (length(dimv)==2)
                  res <- ursa_new() ## or RECURSIVE as_ursa()
               else
                  res <- ursa_new(nband=dimv[3])
               res$value <- v
               dima <- dim(res)
               dim(res$value) <- c(prod(dima[1:2]),dima[3])
               class(res$value) <- "ursaNumeric"
               rm(v)
            }
         }
         else {## quicker for 'x86_64'
           # print("V")
            res <- as.ursa(v,flip=TRUE) ## RECURSIVE!!!
         }
         rm(v)
      }
     # .elapsedTime("C")
     # .elapsedTime("sf::gdal_read -- finish")
      return(res)
   }
   if (inherits(obj,"stars")) {
      d <- sapply(obj,function(x) length(dim(x)))
      if (any(d>3))
         stop("import 'stars' object: unhandled 4-dimensional (>3D) arrays")
      md <- attr(obj,"dimensions")
      cond1 <- identical(md$x$geotransform,md$y$geotransform)
      cond2 <- identical(md$x$refsys,md$y$refsys)
      if ((!cond1)||(!cond2))
         stop("import 'stars' object: unhandled difference in 'x' and 'y' dimensions")
      geot <- md$x$geotransform
      columns <- md$x$to
      rows <- md$y$to
      if (!is.null(geot)) {
         resx <- geot[2]
         resy <- -geot[6]
         minx <- geot[1]
         maxy <- geot[4]
      }
      else {
         resx <- md$x$delta
         resy <- -md$y$delta
         minx <- md$x$offset
         maxy <- md$y$offset
      }
      maxx <- minx+columns*resx
      miny <- maxy-rows*resy
      ##~ prm <- list(minx=minx,maxx=maxx,miny=miny,maxy=maxy,columns=columns,rows=rows
                  ##~ ,crs=md$x$refsys)
      ##~ g1 <- regrid(minx=minx,maxx=maxx,miny=miny,maxy=maxy,columns=columns,rows=rows
                  ##~ ,crs=md$x$refsys)
      if (F) ## quick development
         g1 <- regrid(setbound=c(minx,miny,maxx,maxy),dim=c(rows,columns)
                     ,crs=md$x$refsys$proj4string)
      else { ## quick implementation
         g1 <- .grid.skeleton()
         g1$columns <- as.integer(columns)
         g1$rows <- as.integer(rows)
         g1$minx <- minx
         g1$maxx <- maxx
         g1$miny <- miny
         g1$maxy <- maxy
         g1$resx <- with(g1,(maxx-minx)/columns)
         g1$resy <- with(g1,(maxy-miny)/rows)
         g1$crs <- if (is.character(md$x$refsys)) md$x$refsys else md$x$refsys$proj4string
      }
      session_grid(g1)
      isHomo <- length(obj)==0 ##
     # print(c(isHomo=isHomo))
      if (isHomo) {
         res <- as.ursa(obj[[1]],flip=TRUE)
      }
      else {
         res <- vector("list",length(obj))
         names(res) <- names(obj)
         for (i in seq_along(res)) {
            if (R.Version()$arch %in% c("i386","x86_64","zzz")[-3]) {
               o <- obj[[i]]
              # print("A")
               dima <- dim(o)
               if (length(dima)==2)
                  dima <- c(dima,band=1L)
               if (devel2 <- TRUE) {
                  dim(o) <- c(prod(dima[1:2]),dima[3])
                  if (isCat <- is.factor(o)) {
                     ct <- attr(o,"colors")
                     names(ct) <- levels(o)
                     class(ct) <- "ursaColorTable"
                     o <- as.integer(o)-1L
                  }
                  res[[i]] <- as.ursa(o)
                  if (isCat) {
                     class(res[[i]]$value) <- "ursaCategory"
                     ursa_colortable(res[[i]]) <- ct
                  }
                  bname <- attr(obj[i],"dimensions")$band$values
                  if (!is.null(bname))
                     names(res[[i]]) <- bname
                  rm(o)
               }
               else {
                  a <- ursa(nband=dima[3])
                  a$value <- o
                  dim(a$value) <- c(prod(dima[1:2]),dima[3])
                  class(a$value) <- "ursaNumeric"
                  res[[i]] <- a
                  rm(a)
               }
            }
            else { ## faster for 'x86_64' // 20210203 -- slower for any
              # print("B")
               res[[i]] <- as.ursa(obj[[i]],flip=TRUE) ## RECURSIVE!!!
            }
         }
      }
      if (length(obj)==1)
         return(res[[1]])
      return(res)
   }
   if (inherits(obj,"SpatRaster")) { ## package `terra`
      if (devel <- FALSE) {
        # a1 <- obj@ptr$getCategories()
        # a1 <- obj@ptr$getCatIndex()
         ##~ ctname <- terra::levels(obj)
         ##~ str(ctname)
         ##~ ct <- terra::coltab(obj)
         ##~ str(ct)
         ##~ a1 <- obj@ptr$getColors()
         ##~ str(a1)
         ##~ print(a1)
         ##~ str(a1[[1]]$finalize())
         ##~ q()
      }
      bbox <- as.vector(obj)[c(1,3,2,4)]
      res <- terra::res(obj)
      crs <- terra::crs(obj, proj=TRUE)
      aname <- terra::names(obj)
      g1 <- regrid(bbox=bbox,res=res,crs=crs)
      if (identical(bbox,c(0,0,1,1)))
         g1 <- regrid(bbox=c(0,0,rev(dim(g1))),res=1,crs=crs)
     # g0 <- getOption("ursaSessionGrid")
     # session_grid(g1)
      if (approved <- TRUE)
         res <- ursa(obj[]) ## as.matrix()
      else {
         .elapsedTime("rast -- 1")
         o <- obj[]
        # o <- as.matrix(obj)
        # o <- as.array(obj)
         .elapsedTime("rast -- 2")
         res <- ursa(o)
         .elapsedTime("rast -- 3")
      }
      names(res) <- aname
      return(res)
   }
   if (is.list(obj)) {
      if ((length(obj$x)==length(obj$z))&&(length(obj$y)==length(obj$z))) {
         return(allocate(obj,...))
      }
      g <- .grid.skeleton()
      g$resx <- mean(diff(obj$x))
      g$resy <- mean(diff(obj$y))
      g$minx <- min(obj$x)-g$resx/2
      g$miny <- min(obj$y)-g$resy/2
      g$maxx <- max(obj$x)+g$resx/2
      g$maxy <- max(obj$y)+g$resy/2
      g$columns <- with(g,(maxx-minx)/resx)
      g$rows <- with(g,(maxy-miny)/resy)
      tolerance <- 1e-11
      if ((!.is.integer(g$columns,tolerance))||(!.is.integer(g$rows,tolerance)))
         stop(paste("Unable to calculate integer dim size."
                   ,"Try to change 'tolerance'",paste0("(",tolerance,")")))
      g$columns <- as.integer(round(g$columns))
      g$rows <- as.integer(round(g$rows))
      p <- attr(obj,"crs")
      if ((is.character(p))&&(nchar(p)))
         g$crs <- p
      g2 <- getOption("ursaSessionGrid")
      session_grid(g)
      arglist <- list(...)
      ind <- .grep("^reset(Grid)*",names(arglist))
      if ((length(ind)==1)&&(is.logical(arglist[[ind]]))&&(arglist[[ind]]))
         resetGrid <- TRUE
      else
         resetGrid <- FALSE
     # res <- ursa_new(value=obj$z[,rev(seq(ncol(obj$z)))],bandname="z")
      res <- ursa_new(value=obj$z,bandname="z",flip=FALSE,permute=FALSE)
      ursa_grid(res) <- g
      if ((.is.grid(g2))&&(resetGrid))
         session_grid(g2)
      else
         session_grid(g)
      return(res)
   }
   if (inherits(obj,"ggmap")) {
      B <- 6378137*pi
      .epsg3857 <- paste("+proj=merc +a=6378137 +b=6378137"
                        ,"+lat_ts=0.0 +lon_0=0.0"
                        ,"+x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null"
                        ,"+wktext +no_defs")
      ll <- matrix(as.numeric(attr(obj,"bb")),ncol=2,byrow=TRUE)
      bbox <- .project(list(x=ll[,2],y=ll[,1]),.epsg3857)
     # bbox <- c(bbox$x[1],bbox$y[1],bbox$x[2],bbox$y[2])
      if (is.list(bbox))
         bbox <- with(bbox,c(x[1],y[1],x[2],y[2]))
      else
         bbox <- c(bbox)[c(1,3,2,4)]
      g1 <- .grid.skeleton()
      g1$columns <- dim(obj)[2]
      g1$rows <- dim(obj)[1]
     # str(obj)
      if (ll[2,2]>180)
         bbox[3] <-bbox[3]+2*B
      if (ll[1,2]<(-180))
         bbox[1] <-bbox[1]-2*B
     # print(bbox)
      if (bbox[1]>bbox[3]) {
         if ((ll[1,2]<0)&&(bbox[1]>0))
           bbox[1] <- bbox[1]-2*B
         if ((ll[2,2]>0)&&(bbox[3]<0))
           bbox[3] <- bbox[3]+2*B
        # else
        #    print(bbox)
      }
      g1$minx <- bbox[1]
      g1$miny <- bbox[2]
      g1$maxx <- bbox[3]
      g1$maxy <- bbox[4]
     # print(ll)
      if (FALSE) {
         if (g1$maxx<g1$minx) {
            if ((g1$minx>0)&&(g1$maxx<0)) {
               if ((ll[1,2]<(-180))&&(ll[1,2]<(-180)))
                  g1$minx <- g1$minx-2*B
               g1$maxx <- g1$maxx+2*B
            }
            else {
               print(g1)
               stop("AS:1")
               g1$maxx <- 2*B+g1$maxx
            }
         }
      }
      g1$crs <- .epsg3857
      g1$resx <- with(g1,(maxx-minx)/columns)
      g1$resy <- with(g1,(maxy-miny)/rows)
      stopifnot(with(g1,abs((resx-resy)/(resx+resy)))<1e-3)
      session_grid(g1)
      a1 <- factor(obj)
      v1 <- levels(a1)
     # names(v1) <- seq(length(v1))-1L
      class(v1) <- "ursaColorTable"
      a1 <- as.integer(a1)-1
      dim(a1) <- rev(dim(obj))
      res <- ursa_new(value=a1[,rev(seq(dim(a1)[2]))],flip=FALSE,permute=FALSE)
      res$colortable <- v1
      class(res$value) <- "ursaCategory"
      return(res)
   }
   if (inherits(obj,"cimg")) { ## require(imager)
      dima <- dim(obj)
      if (dima[3]==1) {
         res <- ursa_new(value=obj[,rev(seq(dima[2])),1,],flip=FALSE,permute=FALSE)
         return(res)
      }
      if (dima[4]!=1) {
         res <- ursa_new(value=obj[,rev(seq(dima[2])),,1],flip=FALSE,permute=FALSE)
         return(res)
      }
   }
   ind <- inherits(obj,c("bitmap","rgba"),which=TRUE) ## package 'magick'
   ind <- ind[ind>0]
   if (length(ind)>=2) {
      res <- ursa_new(as.integer(obj),flip=TRUE,permute=TRUE)
      return(res)
   }
  # isMatrix <- is.matrix
  # if ((is.matrix(obj))||(is.array(obj)))
   isArray <- !is.null(dim(obj)) ## 20170327 replaced 'isArray <- !is.null(dim)'
   if ((!isArray)&&(is.numeric(obj))) {
     # print("NOT-ARRAY")
      if (!length(obj)) {
         return(session_grid())
      }
      res <- ursa_new(obj,...)
      return(res)
   }
   if (is.array(obj)) {
      arglist <- list(...)
      bname <- NULL
      if (!is.null(b1 <- attr(obj,"grid"))) {
         g1 <- .grid.skeleton()
         aname <- names(b1)
         indx <- .grep("^(x$|lon)",aname)
         indy <- .grep("^(y$|lat)",aname)
         proj4 <- .grep("(crs|proj4)",aname)
         aname <- .grep("(crs|proj4)",aname,value=TRUE,invert=TRUE)
         indz <- which(is.na(match(seq_along(aname),c(indx,indy))))
         if ((length(indx))&&(length(indy))) {
            x <- b1[[indx]]
            y <- b1[[indy]]
            g1$resx <- mean(unique(diff(x)))
            g1$resy <- mean(unique(diff(y)))
            g1$minx <- min(x)-g1$resx/2
            g1$maxx <- max(x)+g1$resx/2
            g1$miny <- min(y)-g1$resy/2
            g1$maxy <- max(y)+g1$resy/2
            g1$columns <- length(x)
            g1$rows <- length(y)
            if ((length(proj4))&&(nchar(b1[[proj4]]))) {
               g1$crs <- p
            }
            else if (.lgrep("(lon|lat)",aname)==2)
               g1$crs <- "+proj=longlat +datum=WGS84 +no_defs"
            session_grid(g1)
         }
         if (length(indz)==1)
            bname <- as.character(b1[[indz]])
      }
      res <- ursa_new(value=obj,bandname=bname,...)
      return(res)
   }
   if (is.character(obj)) {
      if ((FALSE)&&(envi_exists(obj))) ## FALSE to keep alternative to read ENVI using GDAL 
         return(read_envi(obj,...))
      if ((!FALSE)&&(envi_exists(obj))) {
         return(read_envi(obj,...))
      }
      if (file.exists(obj))
         return(read_gdal(obj,...))
      if (ursa_exists(obj))
         return(read_gdal(obj,...))
      if (isURL <- .lgrep("^(http://|https://|ftp://|file:///)",obj)>0) {
         arglist <- list(...)
         fname <- tempfile()
        # download.file(obj,fname,...)
         ind <- .grep("(method|cache|extra|quiet)",names(arglist)) ## -- "|mode|"
         args2 <- c(url=obj,mode="wb",arglist[ind])
        # do.call("download.file",args2)
         fname <- do.call(".ursaCacheDownload",args2)
         return(read_gdal(fname,...))
      }
   }
   ursa_new(obj,...)
  # str(obj)
  # print(class(obj))
  # stop("Unsupportes type of source object")
}
