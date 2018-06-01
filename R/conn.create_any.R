'create_envi' <- function(x,...) {
   .prepare.con(x,implement="ENVI",...)
}
'create_gdal' <- function(x,...) {
   requireNamespace("rgdal",quietly=.isPackageInUse())
   .prepare.con(x,implement="GDAL",...)
}
#'create_ncdf' <- function(x,...) {
#   requireNamespace("ncdf4",quietly=.isPackageInUse())
#   .prepare.con(x,implement="NCDF",...)
#}
