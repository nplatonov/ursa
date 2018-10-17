'display_brick' <- 'display_homo' <- function(obj,...) {
   if (is.character(obj))
      obj <- if (envi_exists(obj)) read_envi(obj,...) else read_gdal(obj,...)
   if (.is.ursa_stack(obj))
      obj <- ursa_brick(obj)
   options(ursaPngAuto=TRUE)
   arglist <- list(...)
   ind <- .grep("decor",names(arglist))
   if (!length(ind))
      arglist$decor <- TRUE
   if (.is.sparse(obj))
      obj <- decompress(obj)
   compose_open(obj,...)
  # do.call("compose_plot",c(quote(obj),arglist))
   compose_plot(obj,...)
   compose_close(...)
  # invisible(.Last.value)
}
