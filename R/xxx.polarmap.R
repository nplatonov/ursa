'.webmap' <- function(obj) {
   for (pkg in c("leaflet","leafem","leafpop")) 
      if (!requireNamespace(pkg,quietly=.isPackageInUse()))
         stop(paste("Package",sQuote(pkg),"is required for this operation"))
   if (is.character(obj)) {
      if (file.exists(obj))
         layer <- spatial_basename(obj)
      else
         layer <- obj
      obj <- spatialize(obj,resetGrid=TRUE,style="longlat",engine="sf")
   }
   else {
      layer <- as.character(as.list(match.call())[["obj"]])
      obj <- spatialize(obj,style="longlat")
   }
  # glance(obj);q()
   m <- leaflet::leaflet()
   m <- leaflet::addTiles(m)
   m <- leafem::addMouseCoordinates(m)
   m <- leafem::addFeatures(m,data=obj,popup=leafpop::popupTable(obj),weight=0.5)
   m <- leafem::addHomeButton(m,ext=raster::extent(spatial_bbox(obj)[c(1,3,2,4)])
                             ,layer.name=layer)
   m
}
'polarmap' <- function(obj,epsg=NA) {
   for (pkg in c("leaflet","leafem","leafpop")) 
      if (!requireNamespace(pkg,quietly=.isPackageInUse()))
         stop(paste("Package",sQuote(pkg),"is required for this operation"))
   if (is.character(epsg))
      epsg <- as.integer(epsg)
   style <- if (epsg %in% 3571:3576) epsg else "laea"
   if (is.character(obj)) {
      if (file.exists(obj))
         layer <- spatial_basename(obj)
      else
         layer <- obj
      obj <- spatialize(obj,resetGrid=TRUE,style=style,engine="sf")
   }
   else {
      layer <- as.character(as.list(match.call())[["obj"]])
      if (!length(grep("\\+proj=laea",spatial_crs(obj))))
         obj <- spatialize(obj,resetProj=TRUE,resetGrid=TRUE,style=style)
   }
   if (is.na(epsg)) {
      lon_0 <- as.numeric(gsub(".*\\+lon_0=(\\S+)\\s*.*$","\\1",spatial_crs(obj)))
      epsg[lon_0<(-165) || lon_0>=(+135)] <- 3571 ## -180
      epsg[lon_0>=(-165) && lon_0<(-125)] <- 3572 ## -150
      epsg[lon_0>=(-125) && lon_0<(-70)] <- 3573 ## -100
      epsg[lon_0>=(-70) && lon_0<(-25)] <- 3574 ## -40
      epsg[lon_0>=(-25) && lon_0<(+50)] <- 3575 ## 10
      epsg[lon_0>=(50) && lon_0<(+135)] <- 3576 ## 90
   }
   obj <- spatial_transform(obj,4326)
   if ((.isSP(obj))&&(is.null(spatial_data(obj))))
      spatial_data(obj) <- data.frame(fid=seq(spatial_count(obj)))
   extent <- 11000000 + 9036842.762 + 667
   origin <- c(-extent, extent)
   maxResolution <- 2*extent/256
   bounds <- list(c(-extent,extent),c(extent,-extent))
   resolutions <- sapply(0:18,function(x) maxResolution/(2^x))
   crsArctic <- leaflet::leafletCRS(crsClass="L.Proj.CRS",code=paste0("EPSG:",epsg)
                          ,proj4def=ursa::spatial_crs(epsg)
                          ,resolutions=resolutions,origin=origin,bounds=bounds)
   m <- leaflet::leaflet(options=leaflet::leafletOptions(crs=crsArctic,minZoom=3,maxZoom=9))
   m <- leaflet::addTiles(m
      ,urlTemplate=paste0("https://{s}.tiles.arcticconnect.ca/osm_"
                                 ,epsg,"/{z}/{x}/{y}.png")
      ,attribution="Map: \uA9 ArcticConnect. Data: \uA9 OpenStreetMap contributors"
      ,options=leaflet::tileOptions(subdomains="abc",noWrap=TRUE,continuousWorld=FALSE)) 
   m <- leafem::addMouseCoordinates(m)
   m <- leafem::addFeatures(m,data=obj,popup=leafpop::popupTable(obj),weight=0.5)
  # m <- leafem::addHomeButton(m,ext=matrix(spatial_bbox(obj),ncol=2),layer.name=layer)
   m <- leafem::addHomeButton(m,ext=raster::extent(spatial_bbox(obj)[c(1,3,2,4)])
                             ,layer.name=layer)
   m
}
