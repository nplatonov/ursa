'panel_decor' <- function(...) {
   arglist <- list(...)
   opR <- options(ursaPngAuto=TRUE)
   isWeb <- getOption("ursaPngWebCartography")
   ann <- getOption("ursaPngCopyright")
   ann <- ((is.character(ann))&&(nchar(ann)))
   ann <- ((ann)&&(!.getPrm(arglist,name="coast",default=FALSE)))
   isWeb <- (is.logical(isWeb)&&(isWeb))
   if (isWeb & !ann)
      do.call("panel_coastline",arglist)
   ind <- .grep("decor",names(arglist))
   if (!length(ind))
      arglist$decor <- TRUE
   if (!isWeb)
      do.call("panel_coastline",arglist)
   do.call("panel_graticule",arglist)
   do.call("panel_scalebar",arglist)
   do.call("panel_annotation",arglist)
   options(opR)
   invisible(NULL)
}
