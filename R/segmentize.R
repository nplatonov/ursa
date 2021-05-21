'segmentize' <- function(obj,connect=c("consequent","united")) {
   connect <- match.arg(connect)
   if (!is_spatial_points(obj))
      return(NULL)
   xy <- unname(spatial_coordinates(obj))
   if (connect=="united") {
      if (.isSF(obj)) {
         res <- sf::st_sfc(sf::st_linestring(xy),crs=spatial_crs(obj))
      }
      else if (.isSP(obj)) {
         res <- sp::Lines(sp::Line(xy),1L)
         res <- sp::SpatialLines(list(res))
         sp::proj4string(res) <-  sp::CRS(spatial_crs(obj),doCheckCRSArgs=FALSE)
      }
      else
         res <- NULL
      return(res)
   }
   ind <- tail(seq_len(nrow(xy)),-1)
   n <- length(ind)
   res <- vector("list",n)
   if (.isSF(obj)) {
      for (i in seq_along(ind)) {
         res[[i]] <- sf::st_linestring(xy[ind[i]+c(-1,0),])
      }
      res <- sf::st_sfc(res,crs=spatial_crs(obj))
     # str(res)
   }
   else if (.isSP(obj)) {
      for (i in seq_along(ind)) {
         res[[i]] <- sp::Lines(sp::Line(xy[ind[i]+c(-1,0),]),ind[i])
      }
      res <- sp::SpatialLines(res)
      sp::proj4string(res) <-  sp::CRS(spatial_crs(obj),doCheckCRSArgs=FALSE)
   }
   else
      return(NULL)
   spatial_data(res) <- tail(spatial_data(obj),-1)
   res
}
