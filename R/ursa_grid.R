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
   attr(res,"proj4") <- ursa_proj(obj)
   res
}
