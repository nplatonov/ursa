'.as_terra' <- function(a) {
   if (!requireNamespace("terra",quietly=.isPackageInUse()))
      stop("Package 'terra' is required for this operation")
   r <- terra::rast(resolution=ursa(a,"cell")
                   ,extent=ursa_bbox(a)[c(1,3,2,4)]
                   ,nlyrs=length(a)
                   ,names=names(a)
                   ,crs=ursa_crs(a)
                   ,vals=ursa_value(a)
                   )
   r
}
