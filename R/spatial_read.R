'spatial_read' <- function(dsn,engine=c("native","sp","sf","geojsonsf")) {
   if (.lgrep("\\.(tif|tiff|tif\\.zip|img|png|bmp|dat)$",dsn))
      return(ursa_read(dsn))
   g0 <- getOption("ursaSessionGrid")
   res <- spatialize(dsn=dsn,engine=engine,style="keep")
   if (FALSE)
      res <- spatial_trim(res)
   else if (TRUE) {
      sapply(c("toUnloadMethods","colnames","style","geocodeStatus","dsn","grid")
            ,function(x) attr(res,x) <<- NULL)
   }
   else {
      attr(res,"toUnloadMethods") <- NULL
      attr(res,"colnames") <- NULL
      attr(res,"style") <- NULL
      attr(res,"geocodeStatus") <- NULL
      attr(res,"dsn") <- NULL
      attr(res,"grid") <- NULL
   }
   res
}
