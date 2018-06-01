'display_rgb' <- function(obj,...)
{
   if (is.character(obj))
      obj <- if (envi_exists(obj)) read_envi(obj) else read_gdal(obj)
   options(ursaPngAuto=TRUE)
   arglist <- list(...)
   if (.getPrm(arglist,name="bpp",default=0L)==0L)
      arglist$bpp <- 24L
   if (.getPrm(arglist,name="crop",default="")=="")
      arglist$crop <- "crop"
   margin <- .getPrm(arglist,name="margin",class=list("logical","integer")
                    ,default=-1L)
   if (is.na(useRaster <- .getPrm(arglist,name="useRaster",default=NA)))
      arglist$useRaster <- FALSE
   if ((length(margin)==1)&&(margin==-1L))
      arglist$margin <- c(T,T,F,F)
   if (.is.ursa_stack(obj))
      obj <- ursa_brick(obj)
   if (!is.integer(obj$value))
      obj <- as.integer(round(obj))
   copyright <- attr(obj,"copyright")
   copyright <- ((is.character(copyright))&&(nchar(copyright)))
   if (copyright) {
      if (.getPrm(arglist,name="scale",default=-1)==-1)
         arglist$scale <- 1
      if (is.na(.getPrm(arglist,name="coast",default=NA))) {
         if (!.lgrep("coast",names(arglist)))
            arglist$coast <- FALSE
      } 
   }
   do.call("compose_open",c("rgb",arglist))
   do.call("compose_plot",c(quote(obj),arglist))
   do.call("compose_close",c(quote(obj),arglist))
  # invisible(NULL)
}
