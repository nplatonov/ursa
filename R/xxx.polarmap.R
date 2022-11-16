# https://geoportal.arctic-sdi.org/
# https://geoportal.arctic-sdi.org/action?action_route=GetLayerTile&id=1&layer=arctic_cascading&style=default&tilematrixset=3575&Service=WMTS&Request=GetTile&Version=1.0.0&Format=image%2Fpng&TileMatrix=5&TileCol=20&TileRow=11
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
      layer <- as.character(as.list(match.call())[["obj"]]) ## try mget(names(match.call())[-1])
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
'polarmap' <- function(obj,epsg=NA,group=NULL,opacity=1
                      ,addFeature=TRUE,addHomeButton=TRUE
                      ,addMouseCoordinates=TRUE,addScaleBar=TRUE
                      ,addMeasure=FALSE
                      ,style=c("Arctic Connect","Arctic SDI")) {
   isSDI <- isTRUE(.lgrep("sdi",style[1])>0)
   isConnect <- !isSDI
   if (missing(obj))
      obj <- spatialize(c(0,50,360,50))
   for (pkg in c("leaflet","leafem","leafpop")) 
      if (!requireNamespace(pkg,quietly=.isPackageInUse()))
         stop(paste("Package",sQuote(pkg),"is required for this operation"))
   if (is.character(epsg))
      epsg <- as.integer(epsg)
   style <- if (epsg %in% 3571:3576) epsg else "laea"
   if (is_ursa(obj)) {
      if (!addFeature)
         obj <- polygonize(ursa_bbox(obj))
      else if (F) {
         obj <- colorize(obj,ramp=FALSE)
         str(obj)
         obj <- polygonize(obj)
         str(obj)
         obj <- aggregate(obj)
         q()
      }
     # addFeature <- FALSE
   }
   if (is.character(obj)) {
      if (file.exists(obj))
         layer <- spatial_basename(obj)
      else
         layer <- obj
      obj <- spatialize(obj,resetProj=TRUE,resetGrid=TRUE,style=style,engine="sf")
   }
   else {
      layer <- as.character(as.list(match.call())[["obj"]]) ## try mget(names(match.call())[-1])
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
  # g0 <- session_grid()
   if (!addFeature)
      spatialize(obj,epsg=epsg,style="polarmap",resetGrid=TRUE)
   g1 <- session_grid()
   bbox <- unname(spatial_bbox(spatial_transform(spatialize(spatial_bbox(obj)),4326)))
   obj <- spatial_transform(obj,4326)
   if ((.isSP(obj))&&(is.null(spatial_data(obj))))
      spatial_data(obj) <- data.frame(fid=seq(spatial_count(obj)))
   if ((!is.character(group))||(!nchar(group)))
      group <- "AOI" # "\u2302" # as.character(as.list(match.call())$obj)
   if (isConnect) {
      extent <- 11000000 + 9036842.762 + 667
      origin <- c(-extent, extent)
      maxResolution <- 2*extent/256
      minZoom <- 3
      maxZoom <- 9
      bounds <- list(c(-extent,extent),c(extent,-extent))
      resolutions <- sapply(0:18,function(x) maxResolution/(2^x))
      crsArctic <- leaflet::leafletCRS(crsClass="L.Proj.CRS",code=paste0("EPSG:",epsg)
                             ,proj4def=ursa::spatial_crs(epsg)
                             ,resolutions=resolutions,origin=origin,bounds=bounds)
      m <- leaflet::leaflet(options=leaflet::leafletOptions(crs=crsArctic,minZoom=3,maxZoom=12))
      m <- leaflet::addTiles(m
                            ,urlTemplate=paste0("https://{s}.tiles.arcticconnect.ca/osm_"
                                               ,epsg,"/{z}/{x}/{y}.png")
                            ,attribution=paste("Map: \uA9 ArcticConnect."
                                              ,"Data: \uA9 OpenStreetMap contributors")
                            ,options=leaflet::tileOptions(subdomains="abc"
                                                         ,noWrap=TRUE
                                                         ,continuousWorld=FALSE
                                                         ,opacity=opacity
                                                         ,minZoom=minZoom
                                                         ,maxZoom=maxZoom
                                                         )
                            ,group="Arctic Connect"
                            )
   }
   else {
      extentASDI <- 4889334.802955
      scale0 <- 136421171.96428573131561279297
      resolutions <- 0.28*1e-3*scale0/(2^seq(0,18))
      minZoom <- 1
      maxZoom <- 10
      crsASDI <- leaflet::leafletCRS(crsClass="L.Proj.CRS"
                                    ,code=paste0("EPSG:",epsg)
                                    ,proj4def=spatial_crs(epsg)
                                    ,resolutions=resolutions
                                    ,origin=c(-extentASDI,extentASDI)
                                    ,bounds=list(c(-extentASDI,extentASDI)
                                                ,c(extentASDI,-extentASDI))
                                    )
      m <- leaflet::leaflet(options=leaflet::leafletOptions(worldCopyJump=F
                                                           ,minZoom=minZoom
                                                           ,maxZoom=maxZoom
                                                           ,crs=crsASDI
                                                           ,center=c(90,0)
                                                           )
                  )
      m <- leaflet::addTiles(m
                            ,urlTemplate=paste0("https://geoportal.arctic-sdi.org/action?"
                                               ,"&action_route=GetLayerTile"
                                               ,"&id=1"
                                               ,"&layer=arctic_cascading"
                                               ,"&style=default"
                                               ,"&Service=WMTS"
                                               ,"&Request=GetTile"
                                               ,"&Version=1.0.0"
                                               ,"&Format=image/png"
                                               ,"&tilematrixset={tileMatrix}"
                                               ,"&TileMatrix={z}"
                                               ,"&TileCol={x}"
                                               ,"&TileRow={y}"
                                               )
                            ,options=leaflet::tileOptions(tileMatrix=epsg
                                                         ,opacity=opacity
                                                         )
                           # ,opacity=0.3 ## unused argument
                            ,attribution=paste0("<a href=https://arctic-sdi.org"
                                               ,"/services/topografic-basemap/>"
                                               ,"Arctic SDI Topographic Basemap"
                                               ,"</a>")
                            ,group="Arctic SDI"
                            )
   }
  # m <- leaflet::setView(m,lat=89.999,lng=-110,zoom=2)
  # sink("C:/tmp/res5-SDI.Rout")
  # str(m)
  # sink()
   if (addMouseCoordinates)
      m <- leafem::addMouseCoordinates(m)
   if (T) {
     # z <- which.min(abs(g1$resx-resolutions))
     # z[z>maxZoom] <- maxZoom
     # print(bbox)
      if (T)
         bbox <- unname(spatial_bbox(obj))
     # print(bbox)
     # ll <- unname(c((bbox[1]+bbox[3])/2,(bbox[2]+bbox[4])/2))
     # m <- leaflet::setView(m,lng=ll[1],lat=ll[2],zoom=z)
      m <- leaflet::fitBounds(m,lng1=bbox[1],lat1=bbox[2],lng2=bbox[3],lat2=bbox[4]
                             ,options=list(maxZoom=maxZoom))
   }
   if (addFeature) {
      m <- leafem::addFeatures(m,data=obj
                              ,popup=leafpop::popupTable(obj)
                              ,weight=0.5
                              ,group=group
                              )
   }
   else if (F) {
      z <- which.min(abs(g1$resx-resolutions))
      z[z>maxZoom] <- maxZoom
      if (F) {
         ll <- c(with(g1,.project(cbind((maxx+minx)/2,(maxy+miny)/2),crs,inv=TRUE)))
      }
      else {
         bbox <- spatial_bbox(obj)
         ll <- unname(c((bbox[1]+bbox[3])/2,(bbox[2]+bbox[4])/2))
      }
     # print(ll)
      m <- leaflet::setView(m,lng=ll[1],lat=ll[2],zoom=z)
     # m <- leaflet::fitBounds(m,bbox[1],bbox[2],bbox[3],bbox[4])
      #q()
   }
   if (addScaleBar)
      m <- leaflet::addScaleBar(m,options=leaflet::scaleBarOptions(imperial=FALSE))
   if (addHomeButton) {
      m <- leafem::addHomeButton(m
                               # ,ext=raster::extent(spatial_bbox(obj)[c(1,3,2,4)])
                                ,ext=matrix(spatial_bbox(obj),ncol=2)
                                ,group=group
                                )
   }
   if (addMeasure) {
      m <- leaflet::addMeasure(m,primaryLengthUnit="meters",primaryAreaUnit="sqmeters")
   }
   m
}
