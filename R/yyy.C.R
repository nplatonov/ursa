'.Cursa' <- function(...) {
   if (T) ## if .isPackageInUse()
      return(.C(...))
   print("without package")
  ## without package registration:
   arglist <- list(...)
  # message(paste(".C:",arglist[[1]]))
   if (.isPackageInUse())
      arglist$PACKAGE <- "ursa"
   do.call(".C",arglist)
}
