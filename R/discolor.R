'discolor' <- function(obj,nodata=NA) {
   if (!is.ursa(obj))
      return(invisible(obj))
  # if (!is.colortable(obj))
  #    return(invisible(obj))
   obj <- .extract(obj)
   ursa_colortable(obj) <- character()
   if (!is.na(nodata))
      ursa_nodata(obj) <- nodata
   invisible(obj)
}
