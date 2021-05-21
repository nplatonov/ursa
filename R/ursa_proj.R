'ursa_proj4'   <- function(obj) .syn('ursa_crs',0,obj)
'ursa_proj4<-' <- function(obj,keepGrid=FALSE,value) .syn('ursa_crs<-',0,obj,keepGrid,value)
'ursa_proj'   <- function(obj) .syn('ursa_crs',0,obj)
'ursa_proj<-' <- function(obj,keepGrid=FALSE,value) .syn('ursa_crs<-',0,obj,keepGrid,value)
'ursa_crs' <- function(obj) {
   if (is.ursa(obj,"stack"))
      return(obj[[1]]$grid$crs)
   if (is.ursa(obj)) {
      return(obj$grid$crs)
   }
   if (.is.grid(obj))
      return(obj$crs)
   if ((is.character(obj))&&(nchar(obj))&&(envi_exists(obj,exact=TRUE))) {
      g1 <- session_grid()
      a <- open_envi(obj,resetGrid=TRUE)
      res <- a$grid$crs
      close(a)
      session_grid(g1)
      return(res)
   }
   if (is.character(obj)) {
     # class(obj) <- c("character","ursaProjection")
      return(obj)
   }
   NULL
}
'ursa_crs<-' <- function(obj,keepGrid=FALSE,value) {
   if ((is.numeric(value))&&(.is.integer(value)))
      value <- paste0("+init=epsg:",round(value))
   else if (inherits(value,"CRS"))
      value <- methods::slot(value,"projargs")
   else if (!is.character(value))
   {
      warning("unable to detect projection")
      return(obj)
   }
   if (!is.ursa(obj)) {
      if (!.is.grid(obj))
         return(NULL)
      obj$crs <- ursa_crs(value)
      return(obj)
   }
   obj$grid$crs <- ursa_crs(value)
   if (!keepGrid)
      session_grid(obj)
   obj
}
