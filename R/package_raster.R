# TODO: replace 'a@value' --> 'methods::slot(a,"value")'

# 'raster' <- function(obj) UseMethod("as.Raster",obj) 
## The following object is masked from 'package:...'

## how to register:
# ?knitr::knit_print
# library(knitr)
# knit_print.data.frame = function(x, ...) {
#    res = paste(c("", "", kable(x, output = FALSE)), collapse = "\n")
#    asis_output(res)
# }
# registerS3method("knit_print", "data.frame", knit_print.data.frame)


'as.Raster' <- function(obj=NULL) UseMethod("as.Raster",obj)
'as.Raster.ursaRaster' <- function(obj) .as.Raster(obj)
'as.Raster.list' <- function(obj) .as.Raster(obj)
'as.Raster.ursaStack' <- function(obj) .as.Raster(obj)
# 'as.Raster.ursaBrick' <- function(obj) .as.Raster(obj)
'as.Raster.NULL' <- function(obj) .as.Raster(session_grid())
#'as.Raster.NULL' <- function(obj) .as.Raster(ursa())
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
   isGrid <- .is.grid(obj)
   isStack <- .is.ursa_stack(obj)
   if ((!isGrid)&&(!isStack)&&(!.is.ursa_brick(obj)))
      return(NULL)
   g1 <- ursa_grid(obj)
   ct <- ursa_colortable(obj)
  # ct2 <- lapply(obj,ursa_colortable)
   isCT <- FALSE # length(ct)>0
   isLayer <- !isGrid && !isStack && length(obj)==1
   isBrick <- !isGrid && !isStack && !isLayer
  # print(c(isGrid=isGrid,isLayer=isLayer,isBrick=isBrick,isStack=isStack))
   if (isLayer) {
      a <- as.array(obj,permute=TRUE,flip=TRUE,drop=TRUE)
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
   else if (isGrid) {
      res <- with(g1,raster::raster(nrows=rows,ncols=columns))
   }
   raster::extent(res) <- with(g1,c(minx,maxx,miny,maxy))
   opC <- options(warn=0)
   raster::crs(res) <- sp::CRS(ursa_crs(g1),doCheckCRSArgs=!FALSE) ## 20220118 doCheck...=TRUE
   options(opC)
   if (isGrid)
      return(res)
   nodata <- sapply(obj,ursa_nodata)
   if (!anyNA(nodata))
      raster::NAvalue(res) <- nodata
   names(res) <- names(obj)
   res
}
