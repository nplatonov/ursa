'ursa_proj4'   <- function(obj) .syn('ursa_proj',0,obj)
'ursa_proj4<-' <- function(obj,keepGrid=FALSE,value) .syn('ursa_proj<-',0,obj,keepGrid,value)
'ursa_proj' <- function(obj)
{
   if (is.ursa(obj,"stack"))
      return(obj[[1]]$grid$proj4)
   if (is.ursa(obj)) {
      return(obj$grid$proj4)
   }
   if (.is.grid(obj))
      return(obj$proj4)
   if ((is.character(obj))&&(nchar(obj))&&(envi_exists(obj,exact=TRUE))) {
      g1 <- session_grid()
      a <- open_envi(obj,resetGrid=TRUE)
      res <- a$grid$proj4
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
'ursa_proj<-' <- function(obj,keepGrid=FALSE,value) 
{
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
      obj$proj4 <- ursa_proj(value)
      return(obj)
   }
   obj$grid$proj4 <- ursa_proj(value)
   if (!keepGrid)
      session_grid(obj)
   obj
}
