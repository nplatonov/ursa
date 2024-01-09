'ursa_open' <- function(fname,verbose=FALSE) open_gdal(fname=fname,verbose=verbose)
'open_gdal' <- function(fname,engine=c("native","sf","gdalraster","vapour")
                       ,verbose=FALSE) {
   engList <- as.character(as.list(match.fun("open_gdal"))[["engine"]])[-1]
   if (length(engine)<length(engList)) {
      if (!.isPackageInUse()) {
         engList <- c(engList,"rgdal")
      }
   }
   engine <- match.arg(engine,engList)
  # if (engine=="native")
  #    engine <- "sf" ## replace to 'sf'
  # if (verbose)
  #    print(c(engine=engine))
   fname <- gsub("\\.$","",fname)
   if ((engine=="vapour")&&(requireNamespace("vapour",quite=!.isPackageInUse()))) {
      return(.open_vapour(fname,engine=engine,verbose=verbose))
   }
   if ((engine=="gdalraster")&&(requireNamespace("gdalraster",quite=!.isPackageInUse()))) {
      return(.open_gdalraster(fname,engine=engine,verbose=verbose))
   }
   if (engine!="rgdal") {
      return(.open_sfgdal(fname,engine=engine,verbose=verbose))
   }
   if (engine=="rgdal") {
      return(.open_rgdal(fname,engine=engine,verbose=verbose))
   }
  ## 20170116 removed '...' argument
   if (!is.character(fname))
      return(NULL)
   stop("unknown 'engine'")
}
