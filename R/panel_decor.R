'panel_decor' <- function(...) {
   arglist <- list(...)
   opR <- options(ursaPngAuto=TRUE)
   isWeb <- getOption("ursaPngWebCartography")
   ann <- getOption("ursaPngCopyright")
   ann <- ((is.character(ann)[1])&&(nchar(ann[1])))
   ann <- ((ann)&&(!.getPrm(arglist,name="coast",default=FALSE)))
   isWeb <- (is.logical(isWeb)&&(isWeb))
   isBefore <- isWeb & !ann
   isAfter <- !isBefore
   if (isBefore)
      do.call("panel_coastline",arglist)
   ind <- .grep("decor",names(arglist))
   if (!length(ind))
      arglist$decor <- TRUE
   if (isAfter)
      do.call("panel_coastline",arglist)
   do.call("panel_graticule",arglist)
   do.call("panel_scalebar",arglist)
   do.call("panel_annotation",arglist)
   options(opR)
   invisible(NULL)
}
