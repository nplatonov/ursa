'.metadata' <- function(...) .syn('ursa_info',0,...) ## raster::metadata
# 'ursa_info' <- function(obj,detail=NA,digits=3,...) {
'ursa_info' <- function(obj,detail=NA,...) {
   toClose <- FALSE
   isList <- .is.ursa_stack(obj)
   if (isList) { ## recursive!!!
      rel <- as.list(match.call())
      fun <- as.character(rel[1])
      res <- vector("list",length(obj))
     # oname <- names(obj)
      for (i in seq_along(obj)) {
         rel[["obj"]] <- quote(obj[[i]])
         img <- do.call(fun,rel[-1])
        # if ((!is.null(oname))&&(nband(img)==1))
        #    bandname(img) <- oname[i]
         res[[i]] <- img
         rm(img)
      }
     # names(res) <- oname
      return(res)
   }
   if (is.character(obj)) {
      list1 <- envi_list(obj)
      if (length(list1)==1)
         obj <- open_envi(list1)
      else
         obj <- open_gdal(list1)
      toClose <- TRUE
   }
   if (!is.ursa(obj))
      return(NULL)
   res <- obj$grid
   if (!length(res$seqx))
      res$seqx <- NULL
   if (!length(res$seqy))
      res$seqy <- NULL
   if (!is.na(obj$con$nodata))
      res$nodata <- obj$con$nodata
   if (!is.na(obj$con$datatype))
      res$datatype <- obj$con$datatype
   if (!is.na(obj$con$interleave))
      res$interleave <- obj$con$interleave
   res$mode <- c(obj$con$mode,storage.mode(obj$value))
   res$bandname <- obj$name
   if (!is.na(obj$con$posZ[1]))
      res$bandname <- res$bandname[obj$con$posZ]
   if (.is.colortable(obj$colortable))
      res$colortable <- obj$colortable
   class(res) <- "ursaMetadata"
   if (toClose)
      close(obj)
   str(res,...)
}
