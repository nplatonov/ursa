'.Cursa' <- function(...) {
   return(.C(...))
  ## without package registration:
   arglist <- list(...)
  # message(paste(".C:",arglist[[1]]))
   if (.isPackageInUse())
      arglist$PACKAGE <- "ursa"
   do.call(".C",arglist)
}
