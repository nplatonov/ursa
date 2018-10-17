## 'caTools::write.ENVI'

'write_envi' <- function(obj,...)
{
   if (.is.ursa_stack(obj))
      obj <- if (length(obj)>1) ursa_brick(obj) else obj[[1]]
   arglist <- list(...)
   isRaster <- is.ursa(obj)
   isGrid <- .is.grid(obj)
   isCT <- .getPrm(arglist,name="colortable",default=TRUE)
   if (!.is.colortable(obj))
      isCT <- FALSE
  # if (((isCT)&&(!obj$category))||((!isCT)&&(obj$category)))
   if (isCT+.is.category(obj)==1)
   {
      obj <- reclass(obj)
   }
   if ((isRaster)&&(!isCT)) {
      ursa_colortable(obj) <- character(0)
   }
   if (!FALSE) {# removed (20161225 restored)
      if (isGrid) {
         g1 <- session_grid()
         session_grid(obj)
         res <- create_envi(obj,nodata=NA,datatype=1L,compress=FALSE,...)
         close(res)
         file.remove(res$con$fname)
         session_grid(g1)
         return(invisible(100L+res$con$datatype))
      }
      res <- create_envi(obj,...)
   }
   else {
      nodata <- .getPrm(arglist,name="(^bg$|nodata|ignore)",default=NA)
      if ((is.na(nodata))&&(anyNA(obj$value)))
         arglist$nodata <- .optimal.nodata(obj$value)
      res <- do.call("create_envi",c(list(obj),arglist))
   }
   if (isRaster)
      res[] <- obj
   close(res)
   return(invisible(res$con$datatype))
}
