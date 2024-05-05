'.proj4_requireNamespace' <- function(...) {
  requireNamespace("proj4",quietly=.isPackageInUse())
}
'.proj4_project' <- function(...) {
   if (!.proj4_requireNamespace())
      .Missing("proj4") ## .Retired()
   proj4::project(...)
}
'.proj4_ptransform' <- function(...) {
   if (!.proj4_requireNamespace())
      .Missing("proj4") ## .Retired()
   proj4::ptransform(...) 
}
