#'ursaGrid' <- function(...) .syn('ursa_grid',0,...)
#'ursaGrid<-' <- function(...) .syn('ursa_grid<-',0,...)
'ursa_grid' <- function(obj=NULL)
{
   if (is.null(obj))
      return(.grid.skeleton())
   if (.is.ursa_stack(obj))
      obj <- obj[[1]]
   if (is.ursa(obj))
      return(obj$grid)
   if (is.ursa(obj,"grid"))
      return(obj)
   if ((is.character(obj))&&(envi_exists(obj,exact=TRUE))) {
      g1 <- getOption("ursaSessionGrid")
      a <- open_envi(obj,resetGrid=TRUE,decompress=FALSE)
      res <- a$grid
      close(a)
      if (is.null(g1))
         session_grid(res)
      else
         session_grid(g1)
      return(res)
   }
   if ((is.character(obj))&&(file.exists(obj))&&
      (.lgrep("\\.(tif|tiff|img|dat|png|jpeg|jpg|bmp)$",basename(obj)))) {
      g1 <- getOption("ursaSessionGrid")
      session_grid(NULL)
      a <- try(open_gdal(obj),silent=TRUE)
      if (inherits(a,"try-error")) {
         session_grid(g1)
         return(NULL)
      }
      res <- a$grid
      close(a)
      if (is.null(g1))
         session_grid(res)
      else
         session_grid(g1)
      return(res)
   }
   NULL
}
'ursa_grid<-' <- function(obj,value)
{
   if (!is.ursa(obj))
      return(obj)
   if (!.is.grid(value))
      return(obj)
   obj$grid <- value
   if ((inherits(obj$con$handle,"connection"))&&(is.null(dim(obj$value))))
      .write.hdr(obj,clear=FALSE)
   obj
}
'ursa_nrow' <- 'ursa_lines' <- 'ursa_rows' <- function(obj) ursa_grid(obj)$rows
'ursa_ncol' <- 'ursa_samples' <- 'ursa_columns' <- function(obj) ursa_grid(obj)$columns
'ursa_extent' <- 'ursa_bbox' <- function(obj) {
   res <- with(ursa_grid(obj),c(xmin=minx,ymin=miny,xmax=maxx,ymax=maxy))
   attr(res,"crs") <- ursa_crs(obj)
   res
}
'consistent_grid' <- function(obj,ref,expand=1,border=rep(0,4)) {
   verbose <- !.isPackageInUse()
   if ((!missing(obj))&&(is_ursa(obj)))
      obj <- ursa_grid(obj)
   if ((!missing(ref))&&(is_ursa(ref)))
      obj <- ursa_grid(ref)
   g0 <- getOption("ursaPngComposeGrid")
   isPlot <- isFALSE(is.null(g0))
   if (!isPlot)
      g0 <- getOption("ursaSessionGrid")
  # if (identical(obj,g0))
   if (missing(ref)) {
      if ((FALSE)&&(!missing(obj))&&(!identical(obj,g0))) {
         ref <- obj
         obj <- g0
      }
      else
         ref <- g0
   }
   if (missing(obj))
      obj <- g0
   if (identical(obj,ref))
      return(obj)
   isWeb <- ((.lgrep("\\+proj=merc",session_crs()))&&
      (!is.na(.is.near(ursa(obj,"cellsize"),2*6378137*pi/(2^(1:21+8))))))
   if (is_spatial(ref))
      ref <- spatial_grid(ref)
   if (is_ursa(ursa_grid(ref),"grid"))
      d2 <- unname(ursa(ref,"dim"))
   else if ((is.numeric(ref))&&(length(ref)==2)) {
      d2 <- unname(ref)
      if (!is.na(g0$retina))
         d2 <- g0$retina*d2
   }
   if (missing(d2))
      stop("Unable to detect reference grid. Check 'ref' argument)")
   d1 <- unname(ursa(obj,"dim"))
   d <- min(d2/d1)
   d.web <- trunc(log(d)/log(2))
   d.less <- 2^(d.web-1)
   d.more <- 2^(d.web+1)
   if (d>1) {
      d <- d*expand
     # cat("---------\n")
     # str(obj)
     # g2 <- regrid(obj,expand=d) ## ifelse(isWeb,2^(trunc(log(d)/log(2))+1),d)
      g2 <- regrid(obj,mul=ifelse(isWeb,1/d.more,d))
     # str(g2)
     # cat("---------\n")
     # g2$retina <- NA
   }
   else {
      d <- d/expand
      g2 <- regrid(obj,mul=ifelse(isWeb,d.less,d))
     # g2$retina <- NA
   }
  # cat("--------------\n")
  # str(g2)
  # cat("--------------\n")
   d3 <- c(ursa(g2,"nrow"),ursa(g2,"ncol"))
   if (verbose) {
      print(c(web=isWeb))
      print(c('d1:'=d1))
      print(c('d2:'=d2))
      print(c('d2/d1:'=d2/d1))
      print(c(d=d,d.less=d.less,d.web=d.web,d.more=d.more))
      print(c('d3:'=d3))
   }
   d4 <- d2-d3
   dx <- rep(floor(d4[1]/2),2)
   dy <- rep(floor(d4[2]/2),2)
   if (d4[1] %% 2 ==1)
      dx <- dx+c(0,1)
   if (d4[2] %% 2 ==1)
      dy <- dy+c(0,1)
   b <- c(dx[1],dy[1],dx[2],dy[2])
   g3 <- regrid(g2,border=b)
   if (any(border!=0)) {
      g3 <- consistent_grid(regrid(g3,border=border),ref=g3) ## RECURSIVE
   }
   ##~ d4 <- c(ursa(g3,"nrow"),ursa(g3,"ncol"))
   ##~ print(d4)
   if (isPlot) {
     # options(ursaPngPanelGrid=g3)
      session_grid(g3)
   }
   g3
}
