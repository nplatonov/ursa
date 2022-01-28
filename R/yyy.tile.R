## "http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames", license?
# https://gist.github.com/Yago/05d479de169a21ba9fff
# http://b.sm.mapstack.stamen.com/(toner-background,$fff[difference],$fff[@23],$fff[hsl-saturation@20],toner-lines[destination-in])/9/273/172.png
# https://pogoda1.ru/map/precipitation/7/77/40.png

'.deg2numYa' <- function(lat,lon,zoom,verbose=FALSE) {
   lat_rad <- lat*pi/180
   lon_rad <- lon*pi/180
  # n <- 2^zoom
   a <- 6378137; ## a*pi==20037508.342789
   k <- 0.0818191908426;
   b <- 53.5865938
   z1 <- tan(pi/4+lat_rad/2)/'^'(tan(pi/4 + asin(k*sin(lat_rad))/2),k)
   ytile = floor((a*pi-a*log(z1))*b/'^'(2,23-zoom)/256)
   xtile = floor((a*pi+a*lon_rad)*b/'^'(2,23-zoom)/256)
  # xtile <- floor((lon+180)/360*2^zoom)
   if (verbose)
      print(data.frame(lon=lon,lat=lat,zoom=zoom,x=xtile,y=ytile))
   c(xtile,ytile)
}
'.deg2num' <- function(lat,lon,zoom,verbose=FALSE) {
   lat_rad <- lat*pi/180
   n <- 2^zoom
   xtile <- floor((lon+180)/360*n)
   ytile <- floor((1-log(tan(lat_rad)+(1/cos(lat_rad)))/pi)/2*n)
   if (verbose)
      print(data.frame(lon=lon,lat=lat,zoom=zoom,x=xtile,y=ytile))
   if (TRUE)
      return(c(xtile,ytile))
   osm <- paste0("https://",letters[sample(seq(3),1)],".tile.openstreetmap.org")
   tile <- paste0(paste(osm,zoom,xtile,ytile,sep="/"),".png")
   message(tile)
  # fname <- "tile.png"
  # download.file(tile,fname,mode="wb",quiet=!verbose)
   fname <- .ursaCacheDownload(tile,mode="wb",quiet=!verbose)
   return(tile)
}
# https://leaflet-extras.github.io/leaflet-providers/preview/
# https://leaflet-extras.github.io/leaflet-providers/leaflet-providers.js
'.tileService' <- function(server="") {
   language <- if (.lgrep("Russian",ctype <- Sys.getlocale("LC_TIME"))) "ru"
               else Sys.getenv("LANGUAGE")
   osmCr <- "\uA9 OpenStreetMap contributors"
   optHERE <- getOption("HEREapp")
   TFkey <- getOption("ThunderforestApiKey")
   BingKey <- getOption("BingMapsKey")
   StadiaKey <- getOption("stadiamaps_api_key")
   mapsurferKey <- getOption("openrouteserviceToken")
   googleCr <- "Google: TERMS OF USE ARE VIOLATED"
   yandexCr <- "Yandex: TERMS OF USE ARE VIOLATED"
   StadiaCr <- paste0("\uA9 Stadia Maps, \uA9 OpenMapTiles ",osmCr)
   StamenCr <- paste0("Map tiles by Stamen Design, CC BY 3.0 - Map data ",osmCr)
   s <- list()
   s$mapnik <- c("https://{abc}.tile.openstreetmap.org/{z}/{x}/{y}.png"
                ,osmCr) ## # http://{abc}.tile.osm.org/{z}/{x}/{y}.png
   s$osmbw <- c("http://{abc}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png"
               ,osmCr)
   s$cycle <- c("http://{abc}.tile.opencyclemap.org/cycle/{z}/{x}/{y}.png"
               ,paste(osmCr,"(Cycle)"))
   s$osmfr <- c("http://{abc}.tile.openstreetmap.fr/osmfr/{z}/{x}/{y}.png"
               ,paste("\uA9 Openstreetmap France",osmCr))
   s$transport <- c("http://{abc}.tile2.opencyclemap.org/transport/{z}/{x}/{y}.png"
                   ,osmCr)
  # copyright["transport"] <- paste0("Maps \uA9 Thunderforest, Data ",osmCr)
  # s$mapsurfer <- c("http://korona.geog.uni-heidelberg.de/tiles/roads/x={x}&y={y}&z={z}"
  #                 ,paste0(osmCr,", GIScience Research Group @ Heidelberg University")
  #                 ,"png")
   s$mapsurfer <- c(paste0("https://api.openrouteservice.org/mapsurfer/{z}/{x}/{y}.png?api_key="
                          ,mapsurferKey)
                   ,paste0(osmCr,", powered by MapSurfer.NET")
                   ,"png")
   s$mapsurfer.grayscale <- c("http://korona.geog.uni-heidelberg.de/tiles/roadsg/x={x}&y={y}&z={z}"
                             ,paste0(osmCr,", GIScience Research Group @ Heidelberg University")
                             ,"png")
  # s$sputnik <- "http://tiles.maps.sputnik.ru/tiles/kmt2/{z}/{x}/{y}.png"
   s$sputnik <- c("https://tilessputnik.ru/{z}/{x}/{y}.png"
                 ,paste0(osmCr,", \u0421\u043F\u0443\u0442\u043D\u0438\u043A \uA9 \u0420\u043E\u0441\u0442\u0435\u043B\u0435\u043A\u043E\u043C"))
  # http://cartodb-basemaps-c.global.ssl.fastly.net/light_all/6/37/21.png   
  # http://a.basemaps.cartocdn.com/light_only_labels/6/39/18.png
   s$CartoDB <- c("https://{abcd}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png"
                 ,paste0(osmCr,", \uA9 CartoDB"))
   s$'Positron' <- c("https://{abcd}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png"
                    ,paste0(osmCr,", \uA9 CartoDB"))
   s$'Dark Matter' <- c("https://{abcd}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png"
                       ,paste0(osmCr,", \uA9 CartoDB"))
   s$'Voyager' <- c("https://{abcd}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}{r}.png"
                       ,paste0(osmCr,", \uA9 CartoDB"))
   s$kosmosnimki <- c("http://{abcd}.tile.osm.kosmosnimki.ru/kosmo/{z}/{x}/{y}.png"
                     ,paste0(osmCr,", \uA9 ScanEx"))
   s$Esri.Ocean <- c("https://services.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}.jpg"
                    ,"\uA9 Esri: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri")
   s$Esri.Topo <- c("http://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}.jpg"
                   ,"\uA9 Esri - contributors to Esri World Topo Map")
   s$Esri.Street <- c("http://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}.jpg"
                     ,"\uA9 Esri - contributors to Esri Street Topo Map")
   s$Esri.Terrain <- c("http://server.arcgisonline.com/ArcGIS/rest/services/World_Terrain_Base/MapServer/tile/{z}/{y}/{x}.jpg"
                      ,"\uA9 Esri: USGS, Esri, TANA, DeLorme, and NPS")
   s$Esri.Light <- c("https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}.jpg"
                      ,"\uA9 Esri: Esri, HERE, Garmin, NGA, USGS")
   s$Esri.Dark <- c("https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Dark_Gray_Base/MapServer/tile/{z}/{y}/{x}.jpg"
                      ,"\uA9 Esri: Esri, HERE, Garmin, NGA, USGS")
   s$Esri.Hillshade <- c("https://server.arcgisonline.com/ArcGIS/rest/services/Elevation/World_Hillshade/MapServer/tile/{z}/{y}/{x}.jpg"
                      ,"\uA9 ESRI World Hillshade")
   s$Esri.Satellite <- c("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}.jpg"
                      ,"\uA9 ESRI Satellite")
   s$Esri.WorldImagery <- c("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}.jpg"
                      ,"Tiles \uA9 Esri - Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community")
   s$Esri.Clarity <- c("https://clarity.maptiles.arcgis.com/arcgis/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
                      ,"ESRI.Clarity")
   s$HERE.Aerial <- c(url=paste0("https://{1234}.aerial.maps.cit.api.here.com/maptile"
                                ,"/2.1/maptile/newest/satellite.day/{z}/{x}/{y}/256/png8?"
                                ,"app_id=",optHERE$id,"&app_code=",optHERE$code,"&lg=eng")
                     ,cite="Map \uA9 1987-2014 HERE"
                     ,ext="png")
   s$'2gis' <- c("https://tile{0123}.maps.2gis.com/tiles?x={x}&y={y}&z={z}" #&v=1.2"
                ,paste0(osmCr,", API 2GIS")
                ,"png")
   s$TF.Outdoors <- c(paste0("https://{abc}.tile.thunderforest.com/outdoors/{z}/{x}/{y}.png?apikey=",TFkey)
                     ,paste0("Maps \uA9 Thunderforest, Data ",osmCr))
   s$TF.Landscape <- c(paste0("https://{abc}.tile.thunderforest.com/landscape/{z}/{x}/{y}.png?apikey=",TFkey)
                      ,paste0("Maps \uA9 Thunderforest, Data ",osmCr))
   s$Bing.Map <- c(url=paste0("https://t{0123}.ssl.ak.dynamic.tiles.virtualearth.net/comp/ch/{q}"
                         ,"?mkt=en-us&it=G,L&shading=hill&og=80&n=z&key=",BingKey)
                  ,cite=paste0("Bing \uA9 Microsoft and its suppliers")
                  ,ext="jpg")
   s$Bing.Satellite <- c(paste0("http://ecn.t{0123}.tiles.virtualearth.net/tiles/a{q}.jpeg?g=0&dir=dir_n'&n=z&key=",BingKey)
                        ,paste0("Bing \uA9 Microsoft and its suppliers")
                        ,"jpg")
   s$opentopomap <- c("http://{abc}.tile.opentopomap.org/{z}/{x}/{y}.png"
                     ,paste0(osmCr,", \uA9 OpenTopoMap"))
   s$wiki <- c("https://maps.wikimedia.org/osm-intl/{z}/{x}/{y}{r}.png"
              ,paste0("Wikimedia | ",osmCr))
   s$polarmap <- c("https://{abc}.tiles.arcticconnect.ca/osm_{l}/{z}/{x}/{y}.png"
                  ,paste0("Map \uA9 ArcticConnect. Data ",osmCr))
   s$ArcticConnect <- c("https://{abc}.tiles.arcticconnect.ca/osm_{l}/{z}/{x}/{y}.png"
                  ,paste0("Map \uA9 ArcticConnect. Data ",osmCr))
   s$ArcticSDI <- c(paste0("http://basemap.arctic-sdi.org/mapcache?"
                   ,"&service=WMS"
                   ,"&request=GetMap"
                   ,"&layers=arctic_cascading"
                   ,"&styles="
                   ,"&format=image/jpeg"
                   ,"&transparent=true"
                   ,"&version=1.1.1"
                   ,"&width=256&height=256"
                   ,"&srs=EPSG:{l}"
                   ,"&bbox={minx},{miny},{maxx},{maxy}"
                   )
                  ,paste0("ArcticSDI")
                  ,"jpg")
   s$google.h <- c(paste0("https://mt{0123}.google.com/vt/lyrs=h" ## roads only
                               ,"&x={x}&y={y}&z={z}&hl=",language),googleCr)
   s$google <- c(paste0("https://mt{0123}.google.com/vt/lyrs=m" ## standard roadmap
                               ,"&x={x}&y={y}&z={z}&hl=",language),googleCr)
   s$google.ru <- c(paste0("https://mt{0123}.google.com/vt/lyrs=m" ## standard roadmap
                               ,"&x={x}&y={y}&z={z}&hl=","ru"),googleCr)
   s$google.m <- c(paste0("https://mt{0123}.google.com/vt/lyrs=m" ## standard roadmap
                               ,"&x={x}&y={y}&z={z}&hl=",language),googleCr)
   s$google.r <- c(paste0("https://mt{0123}.google.com/vt/lyrs=r" ## somehow altered roadmap
                               ,"&x={x}&y={y}&z={z}&hl=",language),googleCr)
   s$google.s <- c(paste0("https://mt{0123}.google.com/vt/lyrs=s" ## satellite only
                                 ,"&x={x}&y={y}&z={z}&hl=",language),googleCr)
   s$google.y <- c(paste0("https://mt{0123}.google.com/vt/lyrs=y" ## hybrid
                                 ,"&x={x}&y={y}&z={z}&hl=",language),googleCr)
   s$google.t <- c(paste0("https://mt{0123}.google.com/vt/lyrs=t" ## terrain only
                                 ,"&x={x}&y={y}&z={z}&hl=",language),googleCr)
   s$google.p <- c(paste0("https://mt{0123}.google.com/vt/lyrs=p" ## terrain
                                 ,"&x={x}&y={y}&z={z}&hl=",language),googleCr)
   s$'Yandex' <- c(paste0("https://vec0{1234}.maps.yandex.net/tiles?l=map" 
                                 ,"&x={x}&y={y}&z={z}&scale={r}&lang="
                                 ,switch(language,ru="ru_RU","en_US")),yandexCr)
   s$'Yandex.Map' <- c(paste0("https://vec0{1234}.maps.yandex.net/tiles?l=map" 
                                 ,"&x={x}&y={y}&z={z}&scale={r}&lang="
                                 ,switch(language,ru="ru_RU","en_US")),yandexCr)
   s$'Yandex.Satellite' <- c(paste0("https://vec0{1234}.maps.yandex.net/tiles?l=sat" 
                                 ,"&x={x}&y={y}&z={z}&scale={r}&lang="
                                 ,switch(language,ru="ru_RU","en_US")),yandexCr)
  # '\u044f\u043d\u0434\u0435\u043a\u0441' 
   s$'Yandex.ru' <- c(paste0("https://vec0{1234}.maps.yandex.net/"
                              ,"tiles?l=map&x={x}&y={y}&z={z}&scale={r}&lang=ru_RU"),yandexCr)
   s$mapy <- c("https://mapserver.mapy.cz/base-m/{r}/{z}-{x}-{y}","mapy.cz")
   s$'Stadia.AlidateSmooth' <- c(paste0("https://tiles.stadiamaps.com/tiles/alidade_smooth"
                                       ,"/{z}/{x}/{y}{r}.png","?api_key=",StadiaKey)
                                ,StadiaCr)
   s$'Stadia.AlidateSmoothDark' <- c(paste0("https://tiles.stadiamaps.com/tiles/alidade_smooth_dark"
                                           ,"/{z}/{x}/{y}{r}.png","?api_key=",StadiaKey)
                                ,StadiaCr)
   s$'Stadia.OSMBright' <- c(paste0("https://tiles.stadiamaps.com/tiles/osm_bright"
                                   ,"/{z}/{x}/{y}{r}.png","?api_key=",StadiaKey)
                            ,StadiaCr)
   s$'Stadia.Outdoors' <- c(paste0("https://tiles.stadiamaps.com/tiles/outdoors"
                                  ,"/{z}/{x}/{y}{r}.png","?api_key=",StadiaKey)
                           ,StadiaCr)
   s$'Stamen.Terrain' <- c(paste0("https://stamen-tiles-{abcd}.a.ssl.fastly.net/terrain"
                                  ,"/{z}/{x}/{y}{r}.png")
                          ,StamenCr)
   s$'rumap' <- c("https://{abcd}tilecart.kosmosnimki.ru/rw/{z}/{x}/{y}.png"
                 ,"\u0420\u435\u043b\u044c\u0435\u0444 \u0420\u0443\u043c\u0430\u043f Scanex")
  # http://a.maps.owm.io/map/precipitation_new/6/37/19?appid=b1b15e88fa797225412429c1c50c122a1   
   if (!sum(nchar(server))) {
     # print(.grep(".*zzz(google|yandex).*",names(s),value=TRUE,invert=TRUE))
      return(.grep(".*zzz(google|yandex).*",names(s),value=TRUE,invert=TRUE))
   }
   if (!(server[1] %in% names(s))) {
      for (i in seq_along(s)) {
         if (.lgrep("http",server))
            ind <- 0L
         else
            ind <- .lgrep(server[1],s[[i]])
         if (ind>0)
            break
      }
      if (!ind) {
         if (TRUE)
            style <- server
         else { 
            ret <- names(s)
           # attr(ret,"copyright") <- copyright
            return(ret)
         }
      }
      else
         style <- s[[ind[1]]]
   }
   else
      style <- s[[server]]
   if ((.lgrep("HERE",server))&&(is.null(optHERE)))
      message("'options(HEREapp=list(id=<app_id>,code=<app_code>))' is required")
   if ((.lgrep("^TF\\.",server))&&(is.null(TFkey)))
      message("'options(ThunderforestApiKey=<api_key>)' is required")
   if ((.lgrep("^Bing\\.",server))&&(is.null(BingKey)))
      message("'options(BingMapsKey=<api_key>)' is required")
   if ((.lgrep("mapsurfer",server))&&(is.null(mapsurferKey)))
      message("'options(openrouteserviceToken=<api_key>)' is required")
   if ((.lgrep("^Stadia\\.",server))&&(is.null(StadiaKey)))
      message("'options(Stadiamaps_api_key=<api_key>)' is required")
  # if (length(server)==1)
  #    style <- unlist(strsplit(server,split="\\s+"))
   tile <- list(name="custom",url="",copyright="   ",fileext="___")
   if (server[1] %in% names(s))
      tile$name <- server
   indUrl <- .grep("^http(s)*://",style)
   if (!length(indUrl))
      return(names(s))
   indUrl <- indUrl[1]
   indExt <- .grep("(png|jpg|jpeg)",style)
   indCite <- seq(style)[-unique(c(indUrl,indExt))]
   if (!length(indCite)) {
      opW <- options(warn=1)
      warning("Cannot identify citation/copyright/attribution for tile service")
      options(opW)
   }
   tile$url <- style[indUrl]
   if (length(indExt)>1)
      pattExt <- paste0(".",style[indExt[indExt!=indUrl]])
   else if ((length(indExt)==1)&&(indExt==indUrl))
      pattExt <- style[indExt]
   else
      pattExt <- paste0(".",style[indExt])
   if (.lgrep("(\\.|image/)(jpg|jpeg)",pattExt))
      tile$fileext <- "jpg"
   else if (.lgrep("(\\.|image/)png",pattExt))
      tile$fileext <- "png"
   else {
     # cat(paste("Unable to detect either 'png' or 'jpg' format in url:"
     #          ,tile$url,"\n"))
     # stop()
   }
   if (length(indCite))
      tile$copyright <- style[indCite]
   if (tile$name=="custom") {
      if (.is.wms(tile$url)) {
         wurl <- .grep("^(request=|service=WMS)",tile$url,value=TRUE,invert=TRUE)
         tile$url <- paste0(paste(wurl,collapse="&")
                           ,"&width=256&height=256"
                           ,"&service=WMS&request=GetMap")
      }
   }
  # print(tile);q()
   tile
}
'.tileGet' <- function(z=4,x=10,y=3,minx=-2e7,miny=-2e7,maxx=2e7,maxy=2e7
                      ,w=256,h=256,retina=NA,url,fileext,ursa=FALSE,cache=TRUE
                      ,verbose=FALSE) {
   if (is.na(retina))
      retina <- getOption("ursaRetina")
   if (isFALSE(is.numeric(retina)))
      retina <- 1
   isRetina <- retina>1
   tile <- .gsub("{z}",z,.gsub("{y}",y,.gsub("{x}",x,url)))
   tile <- .gsub("{h}",h,.gsub("{w}",w,tile))
   tile <- .gsub("{maxy}",maxy,.gsub("{maxx}",maxx
          ,.gsub("{miny}",miny,.gsub("{minx}",minx,tile))))
   if (.lgrep("maps.+yandex",tile)>0) 
      tile <- .gsub("{r}",ifelse(isRetina,"2","1"),tile)
   else if (.lgrep("mapy\\.cz",tile)>0)
      tile <- .gsub("{r}",ifelse(isRetina,"retina",""),tile)
   else 
      tile <- .gsub("{r}",ifelse(isRetina,"@2x",""),tile)
   if (.lgrep("{q}",tile)) {
      b1 <- b2 <- rep(0,z)
      for (i in seq(z)) {
         b1[i] <- x%%2
         b2[i] <- y%%2
         x <- x%/%2
         y <- y%/%2
      }
      b5 <- apply(matrix(c(matrix(rbind(rev(b2),rev(b1)),ncol=2)),nrow=2)
                 ,2,function(x) strtoi(paste(x,collapse=""),base=2L))
      tile <- .gsub("{q}",paste(b5,collapse=""),tile)
   }
   if ((FALSE)&&(.lgrep("\\{..+}",tile))) {
      dom <- unlist(strsplit(.gsub2("\\{(.+)\\}","\\1",gsub("\\{.\\}","",tile)),""))
      ##~ print(tile)
      ##~ print(dom)
      tile <- .gsub("{.+}",sample(dom,1),tile)
   }
  # fname <- tempfile(fileext=".tile")
   if (!.isPackageInUse())
      print(tile)
  # q()
   fname <- .ursaCacheDownload(tile,mode="wb",cache=cache,quiet=!verbose)
   if (inherits(fname,"try-error")) {
      return(fname)
     # message(a)
     # stop()
   }
  # message(tile)
  # download.file(tile,fname,method="curl",mode="wb",quiet=FALSE
  #              ,extra="-H Accept-Language:de")
   isPNG <- FALSE
   isJPEG <- FALSE
   isGIF <- FALSE
   if (isPNG <- fileext %in% c("png"))
      a <- try(255*png::readPNG(fname),silent=!verbose)
   else if (isJPEG <- fileext %in% c("jpg","jpeg"))
      a <- try(255*jpeg::readJPEG(fname),silent=!verbose)
   else {
      a <- try(255*png::readPNG(fname),silent=!verbose)
      if (inherits(a,"try-error")) {
         a <- try(255*jpeg::readJPEG(fname),silent=!verbose)
         isJPEG <- !inherits(a,"try-error")
         if (inherits(a,"try-error")) {
            print("HERE")
         }
      }
      else
         isPNG <- !inherits(a,"try-error")
   }
   if (inherits(a,"try-error")) {
      if (!FALSE) { ## erroneous file extension
         isPNG <- FALSE
         isJPEG <- FALSE
         a <- try(255*png::readPNG(fname),silent=!verbose)
         if (inherits(a,"try-error"))
            a <- try(255*jpeg::readJPEG(fname),silent=!verbose)
         if (inherits(a,"try-error")) {
           # if (requireNamespace("miss_caTools",quietly=.isPackageInUse())) {
           #    stop("caTools")
           # }
            g0 <- session_grid()
            a <- read_gdal(fname)
            session_grid(g0)
            if (inherits(a,"try-error"))
               cat(geterrmessage())
            if (ursa_blank(a,NA))
               ursa_value(a) <- 0
            a <- as.array(a)
         }
      }
      else
         cat(geterrmessage())
      return(a)
   }
  # file.remove(fname)
   dima <- dim(a)
   dimb <- c(h,w)*ifelse(isRetina,2,1)
   reduce <- (TRUE)&&((dima[1]!=dimb[1])||(dima[2]!=dimb[2]))
  # print(dima)
  # print(dimb)
   if (reduce) {
      mul <- mean(dima[1:2]/dimb[1:2])
     # print(mul)
     # .elapsedTime("firstrun 0205a")
      a <- as.array(regrid(as.ursa(a)
            ,res=c(dima[1]/dimb[1],dima[2]/dimb[2])
            ,resample=ifelse(mul>1,1,0.75)
            ,cover=1e-6,verbose=0L))
      dima <- dim(a)
      if (isPNG)
         png::writePNG(a/256,fname)
      else if (isJPEG)
         jpeg::writeJPEG(a/256,fname)
      else
         stop("unable to update file")
      a <- .round(a)
     # .elapsedTime("firstrun 0205b")
   }
   a <- as.integer(c(a))
   dim(a) <- dima
   if (!ursa)
      return(a)
   epsg3857 <- paste("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0"
                    ,"+lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m"
                    ,"+nadgrids=@null +wktext +no_defs")
   n <- 2^z
   lon <- (x+c(0,1))/n*360-180
   lat <- atan(sinh(pi*(1-2*(y+c(0,1))/n)))*180/pi
   xy <- .project(cbind(lon,rev(lat)),epsg3857)
   dima <- dim(a)
   g1 <- regrid(ursa_grid(),setbound=c(xy)[c(1,3,2,4)]
               ,columns=dima[2],rows=dima[1],crs=epsg3857)
   b <- as.integer(255/255*as.ursa(a,aperm=TRUE,flip=TRUE))
   ursa(b,"grid") <- g1
   attr(b,"copyright") <- "For personal use only"
  # session_grid(b)
  # display(b,scale=1,coast=FALSE)
   b
}
