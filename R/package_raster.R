# TODO: replace 'a@value' --> 'methods::slot(a,"value")'

# 'raster' <- function(obj) UseMethod("as.Raster",obj) 
## The following object is masked from 'package:...'

'as.Raster' <- function(obj=NULL) UseMethod("as.Raster",obj)
'as.Raster.ursaRaster' <- function(obj) .as.Raster(obj)
'as.Raster.list' <- function(obj) .as.Raster(obj)
'as.Raster.ursaStack' <- function(obj) .as.Raster(obj)
# 'as.Raster.ursaBrick' <- function(obj) .as.Raster(obj)
'as.Raster.NULL' <- function(obj) .as.Raster(ursa())
'.as.Raster' <- function(obj) {
  # suppressMessages({
      requireNamespace("methods",quietly=.isPackageInUse())
     # requireNamespace("rgdal",quietly=.isPackageInUse())
     # require("raster") ## FAILED if 'requireNamespace'
      requireNamespace("raster",quietly=.isPackageInUse())
  # })
   '.addColorTable<-' <- function(r,value) {
      if (!length(value))
         return(r)
      r <- raster::as.factor(r)
      x <- raster::levels(r)[[1]]
      x$code <- names(value)
      r@data@attributes[[1]] <- x # levels(r) <- x ## 'levels<-' is not public
      if (!anyNA(value))
         raster::colortable(r) <- as.character(value)
      r@legend@values <- seq_along(value)-1
     # res@legend@color <- as.character(value)
     # res@legend@type <- "a"
      r@legend@names <- names(value)
      r
   }
   isStack <- .is.ursa_stack(obj)
   if ((!isStack)&&(!.is.ursa_brick(obj)))
      return(NULL)
   g1 <- ursa_grid(obj)
   ct <- ursa_colortable(obj)
  # ct2 <- lapply(obj,ursa_colortable)
   isCT <- FALSE # length(ct)>0
   isLayer <- !isStack && length(obj)==1
   isBrick <- !isStack && !isLayer
  # print(c(isLayer=isLayer,isBrick=isBrick,isStack=isStack))
   if (isLayer) {
      res <- raster::raster(as.array(obj,permute=TRUE,flip=TRUE,drop=TRUE))
      .addColorTable(res) <- ursa_colortable(obj)
   }
   else if (isBrick) {
      res <- raster::brick(as.array(discolor(obj),permute=TRUE,flip=TRUE,drop=TRUE))
   }
   else if (isStack) {
      res <- vector("list",length(obj))
      for (i in seq_along(res)) {
         r <- raster::raster(as.array(obj[[i]],permute=TRUE,flip=TRUE,drop=TRUE))
         if (isCT <- FALSE) {
            r <- raster::as.factor(res)
            x <- raster::levels(r)[[1]]
            x$code <- names(ct)
            r@data@attributes[[1]] <- x # levels(r) <- x ## 'levels<-' is not public
            raster::colortable(r) <- as.character(ct)
            r@legend@values <- seq_along(ct)-1
           # res@legend@color <- as.character(ct)
           # res@legend@type <- "a"
            r@legend@names <- names(ct)
         }
         .addColorTable(r) <- ursa_colortable(obj[[i]])
         res[[i]] <- r
      }
      res <- raster::stack(res)
   }
   raster::extent(res) <- with(g1,c(minx,maxx,miny,maxy))
   raster::crs(res) <- ursa_proj4(g1)
   nodata <- sapply(obj,ursa_nodata)
   if (!anyNA(nodata))
      raster::NAvalue(res) <- nodata
   names(res) <- names(obj)
   res
}
