'.display_stack' <- function(obj,oneside=FALSE
                                            ,horizontal=NA,vertical=NA
                                            ,ratio=1.5,decor=TRUE
                                            ,crop="crop",...) {
  NULL
}
'display_stack' <- 'display_hetero' <- function(obj,...)
{
   if (is.character(obj))
      obj <- if (envi_exists(obj)) read_envi(obj) else read_gdal(obj)
   isList <-  .is.ursa_stack(obj)
   if (isList) {
     # p <- colorize(obj,...)
     # p <- lapply(obj,colorize,...)
      p <- lapply(obj,function(x) colorize(decompress(x),...))
   }
   else {
     # p <- colorize(ursa_stack(obj),...)
     # p <- lapply(ursa_stack(obj),colorize,...)
      p <- lapply(ursa_stack(decompress(obj)),function(x) do.call("colorize",list(x,...)))
   }
  # cl <- .compose_design.stack(p,oneside=oneside
  #                           ,horizontal=horizontal,vertical=vertical,ratio=ratio)
   cl <- compose_design(p,...)
   s1 <- colSums(cl$layout)
   arglist <- list(...)
   las <- if (all(s1[c(1,length(s1))]==0)) 2L else 1L
   options(ursaPngAuto=TRUE)
   compose_open(cl,...)
   if ("las" %in% names(arglist))
      compose_plot(p,trim=2L*(cl$legend>0),...)
   else
      compose_plot(p,las=las,trim=2L*(cl$legend>0),...) ## ,decor=decor
   compose_close(...)
  # invisible(0L)
}
