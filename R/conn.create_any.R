'create_envi' <- function(x,...) {
   .prepare.con(x,implement="ENVI",...)
}
'create_gdal' <- function(x,...) {
   if (!.isPackageInUse()) {
      .rgdal_requireNamespace()
      return(.prepare.con(x,implement="RGDAL",...))
   }
   .prepare.con(x,implement="EGDAL",...)
}
#'create_ncdf' <- function(x,...) {
#   requireNamespace("ncdf4",quietly=.isPackageInUse())
#   .prepare.con(x,implement="NCDF",...)
#}
