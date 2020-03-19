'.geomap' <- function(loc=NULL,style="",geocode="",place="",size=NA,zoom="0"
                     ,border=27,cache=TRUE,verbose=FALSE) {
  # if (!nchar(style))
  #    style <- "google static"
   geocodeList <- eval(as.list(args(.geocode))$service)
   tileList <- .tileService()
   if (!nchar(geocode))
      geocode <- if (.lgrep("google",style)) "google" else "nominatim"
   geocode <- match.arg(geocode,geocodeList)
   if (!sum(nchar(style)))
      style <- paste(switch(geocode,nominatim="openstreetmap",google="google"
                           ,"mapnik"),"color")
   if (is.na(zoom))
      zoom <- "0"
   staticMap <- c("openstreetmap","google","sputnikmap")
   tilePatt <- paste0("(",paste0(unique(c(staticMap,tileList))
                                ,collapse="|"),")")
   tilePatt <- .gsub("\\.","\\\\.",tilePatt)
   if (!.lgrep(tilePatt,style))
      art <- "none"
   else {
      if (length(style)>1)
         art <- "none"
      else if (style %in% tileList)
         art <- style
      else
         art <- .gsub2(tilePatt,"\\1",style)
     # print(art);q()
     # proj <- "merc"
   }
   isStatic <- .lgrep("static",style)>0
  # if ((!isStatic)&&("ursa" %in% loadedNamespaces())) {
  #    stop("Operation is prohibited: unable to display attribution.")
  # }
   len <- 640L
   if (is.na(size[1]))
      size <- c(len,len)
   else if (is.character(size)) {
      size <- as.integer(unlist(strsplit(
                   .gsub("(\\d+)\\D+(\\d+)","\\1 \\2",size),split="\\s")))
   }
   else if (is.numeric(size))
      size <- rep(size,length=2)
   if (is.numeric(size))
      len <- as.integer(round(max(size)))
   mlen <- switch(art,google=640,openstreetmap=960,sputnikmap=640)
   if (isStatic) {
      len[len>mlen] <- mlen
   }
   isUrl <- .lgrep("^http(s)*://",style)>0
  # canTile <- .lgrep(art,eval(as.list(args(".tileService()"))$server))>0
   canTile <- isUrl | .lgrep(art,.tileService())>0
   isTile <- .lgrep("tile",style)>0 & canTile
   if ((!isStatic)&&(!isTile)) {
      if (art %in% staticMap)
         isStatic <- TRUE
      else if (canTile)
         isTile <- TRUE
      else
         art <- "none"
   }
  # else if (isUrl)
  #    style <- "custom"
   isColor <- if (isUrl) TRUE else .lgrep("colo(u)*r",style)>0
   isGrey <- ifelse(isColor,FALSE,.lgrep("gr[ae]y(scale)*",style)>0)
   if (isGrey)
      isColor <- FALSE
   isWeb <- .lgrep(tilePatt,art)>0 | isUrl
   if (verbose)
      print(data.frame(art=art,color=isColor,grey=isGrey,static=isStatic
                      ,canTile=canTile,tile=isTile,web=isWeb))
   geocodeStatus <- FALSE
   if (.isSP(loc)) {
      proj4 <- sp::proj4string(loc)
      if (!.lgrep("\\+proj=longlat",proj4)) {
         loc <- sp::bbox(loc)
         if (length(loc)==6)
            loc <- loc[c(1,2,4,5)]
         loc <- c(.project(matrix(c(loc),ncol=2,byrow=TRUE),proj4
                          ,inv=TRUE))[c(1,3,2,4)]
      }
   }
   else if (inherits(loc,c("sf","bbox"))) {
      if (inherits(loc,"sf"))
         loc <- sf::st_bbox(loc)
      proj4 <- attr(loc,"crs")$proj4string
     # if (proj4!="+proj=longlat +datum=WGS84 +no_defs")
      if (!.lgrep("\\+proj=longlat",proj4))
         loc <- c(.project(matrix(loc,ncol=2,byrow=TRUE),proj4
                          ,inv=TRUE))[c(1,3,2,4)]
   }
   
   isWMS <- isUrl & .is.wms(style)
   notYetGrid <- TRUE
   g3 <- NULL
   if ((TRUE)||(isWMS)) {
      if (is.null(loc)) {
         border <- 0
         g3 <- g0 <- getOption("ursaSessionGrid")#session_grid()
         notYetGrid <- is.null(g0)
         if (notYetGrid)
            loc <- c(-179,-82,179,82)
         else {
            loc <- with(g0,.project(rbind(c(minx,miny),c(maxx,maxy)),proj4,inv=TRUE))
            loc <- c(loc)[c(1,3,2,4)]
         }
      }
      if (!((is.numeric(loc))&&(length(loc) %in% c(4,2)))) {
         loc <- try(.geocode(loc,service=geocode,place=place,area="bounding"
                              ,select="top",verbose=verbose))
         if (inherits(loc,"try-error")) {
            geocode <- switch(geocode,google="nominatim",nominatim="google")
            loc <- try(.geocode(loc,service=geocode,place=place,area="bounding"
                                 ,select="top",verbose=verbose))
         }
         if (!inherits(loc,"try-error"))
            geocodeStatus <- TRUE
         else {
            cat(geterrmessage())
            return(NULL)
         }
      }
      if ((is.numeric(loc))&&(length(loc) %in% c(2)))
         geocodeStatus <- TRUE
     # copyright <- attr(.tileService()(),"copyright")[art]
     # str(unname(loc),digits=8)
      if (length(loc)==2)
         bbox <- c(loc,loc)
      else
         bbox <- loc
     # size <- c(640,640)
      B0 <- 6378137
      B <- B0*pi
      x <- B*bbox[c(1,3)]/180
      cross180 <- x[1]>x[2]
      if (cross180) {
         x[1] <- x[1]-2*B
         lon_0 <- round(180*mean(x)/B,6)
      }
      else if ((TRUE)&&(!is.null(g3))&&(.lgrep("\\+proj=(merc|laea)",g0$proj4))) ## ++20180325
         lon_0 <- as.numeric(.gsub(".*\\+lon_0=(\\S+)\\s.*","\\1",g0$proj4))
      else
         lon_0 <- round(180*mean(x)/B,6)
      if (art=="polarmap") {
        # 180\{deg}W, 150\{deg}W, 100\{deg}W, 40\{deg}W, 10\{deg}E, and 90\{deg}E.
         lon_0[lon_0<(-165) || lon_0>=(+135)] <- -180
         lon_0[lon_0>=(-165) && lon_0<(-125)] <- -150
         lon_0[lon_0>=(-125) && lon_0<(-70)] <- -100
         lon_0[lon_0>=(-70) && lon_0<(-25)] <- -40
         lon_0[lon_0>=(-25) && lon_0<(+50)] <- 10
         lon_0[lon_0>=(50) && lon_0<(+135)] <- 90
         proj4 <- paste("","+proj=laea +lat_0=90",paste0("+lon_0=",lon_0)
                       ,"+x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
         if (bbox[3]<bbox[1])
            bbox[3] <- bbox[3]+360
         obj <- matrix(bbox[c(1,2,1,4,3,4,3,2,1,2)],ncol=2,byrow=TRUE)
         if (TRUE) {
            x <- obj[,1]
            y <- obj[,2]
            n <- 3# 256
            x <- c(seq(x[1],x[2],len=n),seq(x[2],x[3],len=n)
                  ,seq(x[3],x[4],len=n),seq(x[4],x[5],len=n))
            y <- c(seq(y[1],y[2],len=n),seq(y[2],y[3],len=n)
                  ,seq(y[3],y[4],len=n),seq(y[4],y[5],len=n))
            bbox <- cbind(x,y)
         }
         bbox <- .project(bbox,proj4)
         bbox <- c(xmin=min(bbox[,1]),ymin=min(bbox[,2])
                  ,xmax=max(bbox[,1]),ymax=max(bbox[,2]))
         res <- max(c((bbox["xmax"]-bbox["xmin"])/size[1]
              ,(bbox["ymax"]-bbox["ymin"])/size[2]))
         if (!FALSE) ## PolarMap.js
            B <- 11000000 + 9036842.762 + 667
         else
            B <- 6378137*pi
         s <- 2*B/(2^(1:19+8))
      }
      else {
         proj4 <- paste("","+proj=merc +a=6378137 +b=6378137"
                       ,"+lat_ts=0.0",paste0("+lon_0=",lon_0)
                       ,"+x_0=0.0 +y_0=0 +k=1.0"
                       ,"+units=m +nadgrids=@null +wktext +no_defs")
         bbox <- matrix(bbox,ncol=2,byrow=TRUE)
         bbox <- .project(bbox,proj4)
         bbox <- c(xmin=bbox[1,1],ymin=bbox[1,2],xmax=bbox[2,1],ymax=bbox[2,2])
         if (bbox[1]>bbox[3]) { ## ++ 20180718
            bbox <- bbox[c(3,2,1,4)]
            names(bbox) <- c("xmin","ymin","xmax","ymax")
         }
         res <- max(c((bbox["xmax"]-bbox["xmin"])/size[1]
              ,(bbox["ymax"]-bbox["ymin"])/size[2]))
         s <- 2*6378137*pi/(2^(1:21+8))
      }
      if (!notYetGrid) {
         res0 <- with(g0,sqrt(resx*resy))
         zoom0 <- which.min(abs(s-res0))
      }
      else
         zoom0 <- -99
      zman <- zoom
      zoom <- which.min(abs(s-res))
      fixRes <- FALSE
      for (i in seq(max(zoom0,zoom+1),1,by=-1)) {
         if (i<1)
            break
         res <- s[i]
         if (FALSE) { ## 20170918
            g0 <- regrid(ursa_grid(),res=res,proj4=proj4,border=border
                        ,setbound=unname(bbox[c("xmin","ymin","xmax","ymax")]))
         }
         else {
            g0 <- regrid(ursa_grid(),res=res,proj4=proj4
                        ,setbound=unname(bbox[c("xmin","ymin","xmax","ymax")]))
            g0 <- regrid(g0,border=border)
         }
        # print(data.frame(res0=res0,res=res,i=i,border=border,notYetGrid=notYetGrid))
         if (!notYetGrid) {
            if (identical(res0,res)) {
               fixRes <- TRUE
               break
            }
            if (softmatching <- TRUE) {
               if (abs(res/res0-1)<1*1e-5) {
                  fixRes <- TRUE
                  break
               }
            }
         }
         else if ((g0$columns<=size[1])&&(g0$rows<=size[2]))
            break
      }
      if ((art=="polarmap")&&(!notYetGrid)) { ## more accurate checking is required
         m1 <- gsub(".*\\+proj=laea\\s.+\\+lon_0=(\\S+)\\s.*","\\1",g0$proj4)
         m2 <- gsub(".*\\+proj=laea\\s.+\\+lon_0=(\\S+)\\s.*","\\1",g3$proj4)
         m3 <- !is.na(.is.near(g0$resx,g3$resx))
         m4 <- !is.na(.is.near(g0$resy,g3$resy))
         m <- m1==m2 & m3 & m4
         if (m)
            g0 <- g3
      }
      if ((art=="sputnikmap")&&(!isTile))
         g0 <- regrid(regrid(g0,mul=1/2),mul=2,border=-1) ## even cols/rows
      zoom <- i
      if ((is.numeric(zman))&&(zman<=0))
         zman <- as.character(zman)
      if (is.numeric(zman))
         zman <- round(zman)
      else if (is.character(zman)) { ## "+1" "---"
         zpos <- .grep("\\+",zman,value=TRUE)
         zneg <- .grep("\\-",zman,value=TRUE)
         if (.lgrep("^(\\+|\\-)\\d$",zman)) {
            zman <- eval(parse(text=paste0(zoom,zman)))
         }
         else if ((length(zpos))&&(zman==zpos)) {
            zman <- zoom+nchar(zman)
         }
         else if ((length(zneg))&&(zman==zneg)) {
            zman <- zoom-nchar(zman)
         }
         else {
            zman <- round(as.numeric(zman))
            if (zman==0)
               zman <- zoom
         }
         if (zman>18)
            zman <- 18
      }
      if (FALSE) {
         pattZoom <- "(zoom=(\\d+))"
         if (.lgrep(pattZoom,style))
            zman <- as.integer(.gsub2(pattZoom,"\\2",style))
         else
            zman <- zoom
      }
      if (zman!=zoom) {
         if (verbose)
            print(c(zoomAuto=zoom,zoomManual=zman))
         m <- 2^(zoom-zman)
         if (!fixRes) {
            if (FALSE)
               g0 <- regrid(g0,mul=1/m,expand=m)
            else {
               bbox <- with(g0,c(minx,miny,maxx,maxy))
               m2 <- if (m<1) 1 else with(g0,sqrt((maxx-minx)*(maxy-miny)))/2
              # print(c(m=m,m2=m2,expand=m*m2))
               g0 <- regrid(g0,mul=1/m,bbox=bbox+c(-1,-1,1,1)*m*m2)
            }
         }
         else {
            g0 <- regrid(g0,mul=1/m)
         }
         zoom <- zman
      }
      if ((TRUE)&&(geocodeStatus)) { ## <-- is this good feature to expand to 640x640?
         x0 <- (g0$minx+g0$maxx)/2
         y0 <- (g0$miny+g0$maxy)/2
         minx <- x0-g0$resx*size[1]/2
         maxx <- x0+g0$resx*size[1]/2
         miny <- y0-g0$resy*size[2]/2
         maxy <- y0+g0$resy*size[2]/2
         g0 <- regrid(g0,minx=minx,maxx=maxx,miny=miny,maxy=maxy)
      }
      B <- 6378137*pi*(0.95+1*0.05)
      if (g0$maxy>(+B))
         g0 <- regrid(g0,maxy=+B)
      if (g0$miny<(-B))
         g0 <- regrid(g0,miny=-B)
     # if (border>0)
     #    g0 <- regrid(g0,border=border)
     # print(fixRes)
     # print(g0)
   }
   else {
      g0 <- session_grid()
      proj4 <- g0$proj4
   }
   cxy <- with(g0,c(minx+maxx,miny+maxy)/2)
   center <- c(.project(cxy,proj4,inv=TRUE))
   bound <- .project(with(g0,rbind(c(minx,miny),c(maxx,maxy))),g0$proj4
                     ,inv=TRUE)
   xr <- with(g0,seq(minx,maxx,len=32))
   yr <- rep(with(g0,(miny+maxy)/2),length(xr))
   lr <- .project(cbind(xr,yr),g0$proj4,inv=TRUE)[,1]
   cross180 <- length(which(diff(lr)<0))
  # print(g0)
   if (isTile) {
     # proj <- c("cycle","mapsurfer","sputnik")[2]
      if (art=="polarmap") {
        # B <- 6378137*pi
         B <- 11000000 + 9036842.762 + 667
         dz <- 2^(zoom)
         res <- 2*B/dz ## '2*' - patch
         g1 <- regrid(ursa_grid(),setbound=c(-B,-B,B,B),res=res,proj=g0$proj4)
         session_grid(g1)
         a <- ursa_new()
         cr <- coord_xy(a,x=c(g0$minx,g0$maxx),y=c(g0$maxy,g0$miny))
        # xy <- coord_cr(a,c=cr[1,],r=cr[2,])
         cr <- cr-1
        # g3 <- regrid(g1,bbox=c(xy[1,1]-1*g2$resx/2,xy[2,2]-1*g2$resy/2
        #                       ,xy[1,2]+1*g2$resx/2,xy[2,1]+1*g2$resy/2))
        # if (is3413)
        #    zoom <- zoom-1
         seqc <- cr[1,1]:cr[1,2]
         seqr <- cr[2,1]:cr[2,2]
         tgr <- expand.grid(z=zoom,y=seqr,x=seqc,minx=NA,miny=NA,maxx=NA,maxy=NA)
         xy <- coord_cr(a,c=tgr$x+1,r=tgr$y+1)
         tgr$minx <- xy["x",]-res/2
         tgr$maxx <- xy["x",]+res/2
         tgr$miny <- xy["y",]-res/2
         tgr$maxy <- xy["y",]+res/2
         h <- sort(unique(tgr[,"x"]))
         v <- sort(unique(tgr[,"y"]))
         g1 <- with(g0,regrid(g1,bbox=c(minx,miny,maxx,maxy),proj=proj4))
         g1 <- regrid(g1,res=ursa(g0,"cell"))
      }
      else {
         B0 <- 6378137
         B <- B0*pi
         dz <- 2^zoom
         res <- 2*pi*B0/dz
         dx0 <- lon_0*pi/180*B0
         minx <- g0$minx+dx0
         maxx <- g0$maxx+dx0
         epsg3857 <- paste("","+proj=merc +a=6378137 +b=6378137"
                          ,"+lat_ts=0.0 +lon_0=0.0"
                          ,"+x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null"
                          ,"+wktext  +no_defs")
         if (FALSE) {
            g2 <- regrid(g0,res=2*pi*B0/dz)
            xr <- with(g2,seq(minx,maxx,by=resx)[-1]-resx/2)+dx0
            yr <- rev(with(g2,seq(miny,maxy,by=resy)[-1]-resy/2))
            g2 <- regrid(g2,res=c(g0$resx,g0$resy),proj4=g0$proj4)
           # g2 <- regrid(g2,minx=g2$minx-dx0,maxx=g2$maxx-dx0)
         }
         else {
            g1 <- regrid(g0,setbound=c(minx,g0$miny,maxx,g0$maxy),proj=epsg3857)
            g1 <- regrid(g1,res=2*pi*B0/dz)
            g1 <- regrid(g1,res=c(g0$resx,g0$resy),proj4=g0$proj4)
            g1$minx <- g1$minx-dx0
            g1$maxx <- g1$maxx-dx0
         }
         sx <- sort(c(c(minx,maxx)
                     ,seq(-B*3,+B*3,by=2*B)))
         sx <- sx[sx>=minx & sx<=maxx]
         dx <- diff(sx)
         dr <- 3+2*zoom
         yr <- with(g0,seq(maxy,miny,len=dr))
         t0 <- NULL
         h <- NULL
         for (j in seq_along(dx)) {
            tX <- NULL
            xr <- seq(sx[j]+1e-6,sx[j+1]-1e-6,len=dr)
            gr <- .project(as.matrix(expand.grid(x=xr,y=yr)),g0$proj4,inv=TRUE)
            gr[,1] <- gr[,1]-lon_0
           # print(unique(gr[,1]))
           # print(unique(gr[,2]))
            for (i in seq(nrow(gr))) {
               tX <- rbind(tX,.deg2num(lon=gr[i,1],lat=gr[i,2],zoom=zoom))
            }
            ind <- which(tX[,1]<0)
            if (length(ind))
               tX[ind,1] <- dz+tX[ind,1]
            ind <- which(tX[,1]>=dz)
            if (length(ind))
               tX[ind,1] <- tX[ind,1]-dz
            tX <- unique(tX)
            hX <- unique(tX[,1])
           # str(tX)
            lon <- (c(head(hX,1),tail(hX,1))+c(0,1))/dz*360-180
           # print(lon)
            t0 <- rbind(t0,tX)
            h <- c(h,hX)
            if (j==1)
               v <- unique(tX[,2])
         }
         dim1 <- unname(dim(g1)/256L)
         dim2 <- c(length(v),length(h))
         changeH <- dim1[2]!=dim2[2]
         changeV <- dim1[1]!=dim2[1]
        # changeDim <- !all(dim1==dim2)
         tgr <- expand.grid(z=zoom,y=v,x=h)
         n <- 2^zoom
         lon1 = (tgr[,"x"]+0)/n*360-180
         lon2 = (tgr[,"x"]+1)/n*360-180
         lat1 = atan(sinh(pi*(1-2*(tgr[,"y"]+1)/n)))*180/pi
         lat2 = atan(sinh(pi*(1-2*(tgr[,"y"]+0)/n)))*180/pi
         xy1 <- .project(cbind(lon1,lat1),epsg3857)
         xy2 <- .project(cbind(lon2,lat2),epsg3857)
         tgr <- cbind(tgr
                     #,lon1=lon1,lat1=lat1,lon2=lon2,lat2=lat2
                     ,minx=xy1[,1],miny=xy1[,2],maxx=xy2[,1],maxy=xy2[,2]
                     )
         if (changeV) {
            y <- sort(unique(c(xy1[,2],xy2[,2])))
            nV <- dim1[1]+1
            y0 <- seq(g1$miny,g1$maxy,len=nV)
            ind <- na.omit(.is.near(y,y0))
            if (length(ind)==nV) {
               indY <- setdiff(seq_along(y),ind)
               if (!anyNA(.is.near(sort(unique(xy1[,2])),y0)))
                  y <- xy2[,2]
               else if (!anyNA(.is.near(sort(unique(xy2[,2])),y0)))
                  y <- xy1[,2]
               else if (indY==4)
                  y <- xy2[,2]
               else if (indY==1)
                  y <- xy1[,2]
               else if (indY==3) { ## ++ 20180331 use case
                  y <- xy1[,2]
               }
               else if (indY==5) { #.geomap("Svalbard",style="mapnik")
                  y <- xy2[,2]
               }
               else
                  stop("extra vertical tile: no handler (#1)")
               ind <- which(!is.na(.is.near(y,y0)))
               tgr <- tgr[ind,]
               v <- sort(unique(tgr[,"y"]))
            }
            else
               stop("extra vertical tile: no handler (#2)")
         }
         if (changeH) {
           ## not appeared during tests 
            x <- sort(unique(c(xy1[,1],xy2[,1])))
            nH <- dim1[1]+1
            x0 <- seq(g1$minx,g1$maxx,len=nH)
            ind <- na.omit(.is.near(x,x0))
            if (length(ind)==nH) {
               indX <- setdiff(seq_along(x),ind)
               if (indX==4)
                  x <- xy2[,1]
               else if (indX==1)
                  x <- xy1[,1]
               else
                  stop("extra horizontal tile: no heandler (#1)")
               ind <- which(!is.na(.is.near(x,x0)))
               tgr <- tgr[ind,]
               h <- sort(unique(tgr[,"x"]))
            }
            else
               stop("extra horizontal tile: no handler (#2)")
         }
      }
      igr <- expand.grid(y=seq_along(v)-1,x=seq_along(h)-1)
      if (verbose) {
         print(tgr)
      }
      tile <- if (isUrl) .tileService(style) else .tileService(art)
      if (art=="polarmap") {
         epsg <- switch(as.character(lon_0),'-180'=3571,'180'=3571,'-150'=3572
                               ,'-100'=3573,'-40'=3574,'10'=3575,'90'=3576
                               ,stop("non-standard central longitude"))
         tile$url <- .gsub("{l}",as.character(epsg),tile$url)
      }
      LL <- (isWMS) & (.lgrep("EPSG:(4326|4269)(\\D|$)",tile$url))
      if (LL) { ## very rought 4236 -> 3857
         x1 <- tgr$minx
         y1 <- tgr$miny
         x2 <- tgr$maxx
         y2 <- tgr$maxy
         tgr$minx <- tgr$lon1
         tgr$miny <- tgr$lat1
         tgr$maxx <- tgr$lon2
         tgr$maxy <- tgr$lat2
         tgr$lon1 <- x1
         tgr$lat1 <- y1 
         tgr$lon2 <- x2
         tgr$lat2 <- y2
      }
      img1 <- vector("list",nrow(tgr))
      for (i in sample(seq(nrow(tgr))))
         img1[[i]] <- .tileGet(z=zoom,x=tgr[i,"x"],y=tgr[i,"y"]
                              ,minx=tgr[i,"minx"],miny=tgr[i,"miny"]
                              ,maxx=tgr[i,"maxx"],maxy=tgr[i,"maxy"]
                              ,url=tile$url
                              ,fileext=tile$fileext,cache=cache,verbose=verbose)
      nb <- sapply(img1,function(x) {
         if (!is.array(x))
            return(0)
         dim(x)[3]
      })
      if (all(nb==0))
         stop("all tiles are failed")
      nbmax <- max(nb)
      if (length(unique(nb))>1) {
         img1 <- lapply(img1,function(x) {
            if (!is.array(x))
               return(x)
            dima <- dim(x)
            if (dima[3]==nbmax)
               return(x)
            dim(x) <- c(dima[1]*dima[2],dima[3])
            for (i in (dima[3]+1L):nbmax)
               x <- cbind(x,255L)
            dim(x) <- c(dima[1],dima[2],nbmax)
            x
         })
      }
      img <- array(0L,dim=c(256*length(v),256*length(h),nbmax))
      for (i in sample(seq(nrow(tgr)))) {
        # img[igr[i,"y"]*256L+seq(256),igr[i,"x"]*256+seq(256),] <- img2[,,1:3]
        # img[igr[i,"y"]*256L+seq(256),igr[i,"x"]*256+seq(256),] <- img2[,,seq(nb)]
        # img[igr[i,"y"]*256L+seq(256),igr[i,"x"]*256+seq(256),seq(nb)] <- img2[,,seq(nb)]
         img2 <- img1[[i]]
         if (inherits(img2,"try-error"))
            next
         dima <- dim(img2)
         if (!((dima[1]==256)&&(dima[2]==256))) {
           # .elapsedTime("everytime 0205a")
            img2 <- as.array(regrid(as.ursa(img2),res=c(dima[1]/256,dima[2]/256)))
           # .elapsedTime("everytime 0205b")
         }
         img[igr[i,"y"]*256L+seq(256),igr[i,"x"]*256+seq(256),] <- img2
      }
      basemap <- as.ursa(img,aperm=TRUE,flip=TRUE)
      ursa(basemap,"grid") <- g1
     # basemap <- as.integer(regrid(basemap,g0,resample=FALSE))
      if (art=="zzzpolarmap") {
         cat("------\n")
         g6 <- regrid(g1,bbox=with(g0,c(minx,miny,maxx,maxy)),zero="node",verbose=TRUE)
         print(g0,digits=12)
         print(g6,digits=12)
        # print(g1,digits=12)
         q()
      }
     # cr <- coord_xy(basemap,x=101234,y=-1001234)
     # print(coord_cr(basemap,c=cr[1,],r=cr[2,]),digits=12)
      if (art=="polarmap")
         g0 <- regrid(g1,bbox=with(g0,c(minx,miny,maxx,maxy)),zero="node")
      basemap <- regrid(basemap,g0,resample=0)
     # cr <- coord_xy(basemap,x=101234,y=-1001234)
     # print(coord_cr(basemap,c=cr[1,],r=cr[2,]),digits=12)
      session_grid(basemap)
   }
   else { ## staticmap
      php <- switch(art
         ,sputnikmap=paste0("http://static-api.maps.sputnik.ru/v1/"
                        ,"?width={w}&height={h}&z={z}&clng={lon}&clat={lat}")
         ,google=paste0("https://maps.googleapis.com/maps/api/staticmap"
                       ,"?center={lat},{lon}&zoom={z}&size={w}x{h}")
         ,openstreetmap=paste0("http://staticmap.openstreetmap.de/staticmap.php"
                              ,"?center={lat},{lon}&zoom={z}&size={w}x{h}")
         )
     # php <- switch(art,google="http://maps.googleapis.com/maps/api/staticmap"
     #        ,openstreetmap="http://staticmap.openstreetmap.de/staticmap.php")
      isOSM <- .lgrep("openstreetmap",art)
      if ((isGoogle <- .lgrep("google",art))&&(proposed <- TRUE)) {
         apiKey <- getOption("googleMaps")
         if (is.character(apiKey))
            php <- paste0(php,"&key=",apiKey)
      }
      adv <- paste(.grep("=",unlist(strsplit(style,split="\\s+")),value=TRUE)
                  ,collapse="&")
      if ((isOSM)&&(cross180)) {
         B0 <- 6378137
         B <- B0*pi
         minx <- g0$minx+lon_0*pi/180*B0
         maxx <- g0$maxx+lon_0*pi/180*B0
         sx <- sort(c(c(minx,maxx)
                     ,seq(-B*3,+B*3,by=2*B)))
         sx <- sx[sx>=minx & sx<=maxx]
         dx <- diff(sx)
         mx <- sx[-1]-dx/2
        # print(sx)
        # print(round(mx))
         lon2 <- 180*mx/B
         lon2[lon2<(-180)] <- lon2[lon2<(-180)]+360
         lon2[lon2>(+180)] <- lon2[lon2>(+180)]-360
        # print(g0$columns)
        # print(g0$columns*dx/sum(dx))
         col2 <- ceiling(g0$columns*dx/sum(dx))
         if (sum(col2)!=g0$columns)
            col2[cross180+1] <- g0$columns-sum(col2[seq(cross180)])
        # print(col2)
         img <- array(0,dim=c(g0$rows,g0$columns,3))
         for (i in seq(cross180+1)) {
            src <- php
            src <- .gsub("{w}",col2[i],src)
            src <- .gsub("{h}",g0$rows,src)
            src <- .gsub("{lon}",round(lon2[i],11),src)
            src <- .gsub("{lat}",round(center[2],11),src)
            src <- .gsub("{z}",zoom,src)
            if (nchar(adv)) {
              #    src <- paste0(src,"&",adv)
               s1 <- .args2list(.gsub("&"," ",src))
               s2 <- .args2list(.gsub("&"," ",adv))
               ind <- match(names(s2),names(s1))
               ind1 <- which(!is.na(ind))
               if (length(ind1))
                  s1[na.omit(ind)] <- s2[ind1]
               ind2 <- which(is.na(ind))
               if (length(ind2))
                  s1 <- c(s1,s2[ind2])
               src <- unlist(s1)
               src <- .gsub("^=","",paste(names(src),src,sep="=",collapse="&"))
            }
           # fname <- tempfile()
           # download.file(src,fname,mode="wb",quiet=!verbose)
            fname <- .ursaCacheDownload(src,mode="wb",quiet=!verbose)
            j <- if (i==1) 0 else sum(col2[seq(i-1)])
            img[,j+seq(col2[i]),] <- png::readPNG(fname)
           # file.remove(fname)
         }
         basemap <- as.integer(255*as.ursa(img,aperm=TRUE,flip=TRUE))
      }
      else {
         center <- round(center,11)
         src <- php
         src <- .gsub("{w}",g0$columns,src)
         src <- .gsub("{h}",g0$rows,src)
         src <- .gsub("{lon}",format(center[1],scientific=FALSE),src)
         src <- .gsub("{lat}",format(center[2],scientific=FALSE),src)
         src <- .gsub("{z}",zoom,src)
         if (nchar(adv)) {
           #    src <- paste0(src,"&",adv)
            s1 <- .args2list(.gsub("&"," ",src))
            s2 <- .args2list(.gsub("&"," ",adv))
            ind <- match(names(s2),names(s1))
            ind1 <- which(!is.na(ind))
            if (length(ind1))
               s1[na.omit(ind)] <- s2[ind1]
            ind2 <- which(is.na(ind))
            if (length(ind2))
               s1 <- c(s1,s2[ind2])
            src <- unlist(s1)
            src <- .gsub("^=","",paste(names(src),src,sep="=",collapse="&"))
         }
         if (cache)
            fname <- .ursaCacheDownload(src,mode="wb",quiet=!verbose)
         else {
            fname <- tempfile()
            download.file(src,fname,mode="wb",quiet=!verbose)
         }
         basemap <- as.integer(255L*as.ursa(png::readPNG(fname)
                                           ,aperm=TRUE,flip=TRUE))
         if (!cache)
            file.remove(fname)
      }
      mul <- unique(c(ursa_ncol(basemap)/ursa_ncol(g0)
                     ,ursa_nrow(basemap)/ursa_nrow(g0)))
      if (length(mul)==1)
         g0 <- regrid(g0,mul=mul)
      ursa(basemap,"grid") <- g0
   }
   if (isGrey) {
      basemap <- as.integer(round(sum(basemap*c(0.30,0.59,0.11))))
      basemap <- colorize(basemap,minvalue=0,maxvalue=255,pal=c("black","white"))
   }
   if (isTile)
      attr(basemap,"copyright") <- tile$copyright
   else {
      if (art=="sputnikmap")
         attr(basemap,"copyright") <- paste("\uA9 OpenStreetMap contributors,"
                        ,"\u0421\u043F\u0443\u0442\u043D\u0438\u043A","\uA9"
                        ,"\u0420\u043E\u0441\u0442\u0435\u043B\u0435\u043A\u043E\u043C")
   else
      attr(basemap,"copyright") <- "   "
   }
   session_grid(g0)
   ursa(basemap,"nodata") <- NA
   basemap
}
