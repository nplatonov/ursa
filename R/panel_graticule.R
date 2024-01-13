##~ '.panel_graticule' <- function(gridline=TRUE,col="grey70",lon=NULL,lat=NULL
                              ##~ ,lwd=1,lty=2,margin=rep(FALSE,4),trim=FALSE
                              ##~ ,cex=0.75,...) {
   ##~ NULL
##~ }
'compose_graticule' <- function(...) {
   arglist <- list(...)
  # kwd <- "grid(line)*"
   kwd <- "(graticule|grid(line)*)"
  # gridline <- .getPrm(arglist,name=paste0("^(",kwd,"|decor)$"),default=TRUE)
   gridline <- .getPrm(arglist,name=paste0("^(",.gsub("(^\\(|\\)$)","",kwd),"|decor)$")
                      ,default=TRUE)
   if (!any(gridline)) {
      res <- list(gridline=NULL,margin=NULL)
      class(res) <- "ursaGridLine"
      return(res)
   }
   lon <- .getPrm(arglist,name="lon",kwd=kwd,default=NA_real_)
   lat <- .getPrm(arglist,name="lat",kwd=kwd,default=NA_real_)
   marginalia <- .getPrm(arglist,name="(decor|margin(alia)*)",kwd=kwd
                        ,class=c("integer","logical")
                        ,default=c(!FALSE,!FALSE,!FALSE,!FALSE))
   panel <- .getPrm(arglist,name=paste0("^",kwd,"$"),default=0L)
   trim <- .getPrm(arglist,name="trim",kwd=kwd,default=TRUE)
   cex <- .getPrm(arglist,name="cex",kwd=kwd,default=0.75)
  # defcol <- ifelse(bg2<128,"#FFFFFF4F","#0000002F") # grey70
   bg1 <- sum(c(col2rgb(getOption("ursaPngBackground")))*c(0.30,0.59,0.11))
   bg2 <- getOption("ursaPngPanel")
   bg2 <- if ((is.null(bg2))||(!nchar(bg2))) bg1 else sum(c(col2rgb(bg2))*c(0.30,0.59,0.11))
   col <- .getPrm(arglist,name="col",kwd=kwd,default="defcol")
   border <- .getPrm(arglist,name="border",kwd=kwd,default=col)
   if (col=="defcol")
      col <- ifelse(bg2<128,"#FFFFFF4F","#0000002F") # grey70
   if (border=="defcol")
      border <- ifelse(bg1<128,"#FFFFFF4F","#0000002F") # grey70
   lwd <- .getPrm(arglist,name="lwd",kwd=kwd,default=0.5)
   lty <- .getPrm(arglist,name="lty",kwd=kwd,default=2L)
   language <- .getPrm(arglist,name="language",kwd=kwd,default=NA_character_)
   verbose <- .getPrm(arglist,name="verb(ose)*",kwd=kwd,default=FALSE)
   if (is.integer(marginalia)) {
      if ((length(panel)==1)&&(panel==0))
         panel <- seq(getOption("ursaPngLayout")$image)
      panel <- -panel
      internal <- which(marginalia<0)
      marginalia <- abs(marginalia)
      ind <- match(marginalia,-panel)
      panel[ind] <- -panel[ind]
      if (length(internal))
         panel[ind][internal] <- panel[ind][internal]+10000L
     # panel <- marginalia
      marginalia <- TRUE
   }
  # else
  #    panel <- 0L
   a <- list(col=col,lon=lon,lat=lat,lwd=lwd,lty=lty,panel=panel
            ,marginalia=marginalia,trim=trim,cex=cex)
   if (verbose)
      str(list(col=col,lon=lon,lat=lat,col=col,lwd=lwd,lty=lty,panel=panel
              ,marginalia=marginalia,trim=trim,cex=cex))
   opStrangeWarn <- options(warn=-1) ## strings not representable in native encoding will be translated to UTF-8
   ret <- .compose_graticule(panel=panel,col=col,border=border
                            ,lon=lon,lat=lat,lwd=lwd,lty=lty
                            ,marginalia=marginalia,trim=trim
                            ,language=language,cex=cex,verbose=verbose)
   options(opStrangeWarn)
   ret
}
'.compose_graticule' <- function(panel=0L,col="grey70",border="grey70",lon=NA,lat=NA
                              ,lwd=0.5,lty=2,marginalia=rep(FALSE,4),trim=FALSE
                              ,language=NA_character_,cex=0.75,verbose=FALSE) {
# verbose <- TRUE
   if (is.na(language)) {
      if (TRUE) {
         ctype <- Sys.getlocale("LC_TIME")
         if (.lgrep("Russian",ctype))
            language <- "ru"
      }
      else
         language <- Sys.getenv("LANGUAGE")
   }
   g1 <- session_grid()
   proj4 <- g1$crs
   isProj <- nchar(proj4)>0
   projClass <- if (isProj) .gsub(".*\\+proj=(\\S+)\\s.+","\\1",proj4) else ""
   isLonLat <- .lgrep("(\\+proj=longlat|epsg:4326)",proj4)>0
   isMerc <- .lgrep("\\+proj=merc",proj4)>0
   minx <- g1$minx
   maxx <- g1$maxx
   if (g1$miny<g1$maxy) {
      miny <- g1$miny
      maxy <- g1$maxy
   }
   else {
      maxy <- g1$miny
      miny <- g1$maxy
   }
   if (any(is.na(marginalia)))
      marginalia <- TRUE
   if ((!anyNA(lon))&&(!anyNA(lat)))
   {
     # dlon <- unique(diff(lon))
     # dlat <- unique(diff(lat))
     # if ((length(dlon)==1)&&(length(dlat)==1))
     # dlon <- max(abs(diff(lon)))
     # dlat <- max(abs(diff(lat)))
     # lon <- c(head(lon,1)-dlon,lon,tail(lon,1)+dlon)
     # lat <- c(head(lat,1)-dlat,lat,tail(lat,1)+dlat)
      lon3 <- lon
      lon4 <- lon
      ind3 <- which(lon3<0)
      ind4 <- which(lon3>180)
      lon3[ind3] <- lon3[ind3]+360
      lon4[ind4] <- lon4[ind4]-360
      sd2 <- sd(lon)
      sd3 <- sd(lon3)
      sd4 <- sd(lon4)
      if (verbose)
         print(c(sd2=sd2,sd3=sd3,sd4=sd4))
      if ((sd3<sd2)&&(sd3<sd4))
         lon <- lon3
      else if ((sd4<sd2)&&(sd4<sd3))
         lon <- lon4
      else if ((sd3<sd2)&&(sd3==sd4))
         lon <- lon3
      dlon <- max(abs(diff(lon)))
      dlat <- max(abs(diff(lat)))
      lon <- c(head(lon,1)-dlon,lon,tail(lon,1)+dlon)
      lat <- c(head(lat,1)-dlat,lat,tail(lat,1)+dlat)
      lonList <- list(lon)
      latList <- list(lat)
   }
   else if (!isProj)
   {
      x <- with(g1,seq(minx,maxx,by=resx))
      y <- with(g1,seq(miny,maxy,by=resy))
      dx <- with(g1,maxx-minx)
      dy <- with(g1,maxy-miny)
      x1 <- max(abs(c(minx,maxx)))
      y1 <- max(abs(c(miny,maxy)))
      for (n in 5:9) {
         x <- pretty(x,n=n)
         y <- pretty(y,n=n)
         dx <- mean(diff(x))
         dy <- mean(diff(y))
         d <- max(dx,dy)
         if (FALSE) { ## removed 20170222
            dx <- d
            dy <- d
         }
         lon <- c(rev(seq(0,-x1-dx,by=-dx)[-1]),seq(0,x1+dx,by=dx))
         lat <- c(rev(seq(0,-y1-dy,by=-dy)[-1]),seq(0,y1+dy,by=dy))
         lon <- lon[lon>=minx-dx & lon<=maxx+dx]
         lat <- lat[lat>=miny-dy & lat<=maxy+dy]
         nx <- length(lon[lon>=minx & lon<=maxx])
         ny <- length(lat[lat>=miny & lat<=maxy])
         if ((nx>=2)&&(ny>=2))
            break
      }
      lonList <- list(lon)
      latList <- list(lat)
   }
   else {
      if (!isLonLat) {
         xy0 <- c((maxx+minx)/2,(miny+maxy)/2)
         aside <- atan(xy0[1]/xy0[2])*180/pi
      }
      ##~ if (FALSE) {
         ##~ g2 <- expand.grid(x=seq(minx,maxx,length=2),y=seq(miny,maxy,length=2)
                          ##~ ,KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)
         ##~ g2 <- cbind(g2$x,g2$y)[c(1,4),]
         ##~ g2a <- proj4::project(g2,g1$crs,inv=TRUE)
         ##~ g2b <- project(g2,g1$crs,inv=TRUE) ## project() from 'rgdal'
         ##~ print(g2)
         ##~ print(g2a)
         ##~ print(g2b)
      ##~ }
      g2 <- expand.grid(x=seq(minx,maxx,length=16),y=seq(miny,maxy,length=16)
                       ,KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)
      if (FALSE) {
         xy <- with(g2,cbind(x,y))
         session_grid(NULL)
         xy <- as.data.frame(cbind(xy,z=runif(nrow(xy),min=1,max=2)))
         a <- allocate(xy)
         display(a)
      }
      ##~ if (FALSE) { ## known reprojection issues
         ##~ xy0 <- as.data.frame(xy)
         ##~ sp::coordinates(xy0) <- ~x+y
         ##~ sp::proj4string(xy0) <- g1$crs
         ##~ xy2 <- sp::coordinates(sp::spTransform(xy0,"+init=epsg:4326"))
         ##~ print(apply(xy2,2,range))
      ##~ }
      if (!isLonLat) {
         xy <- .project(with(g2,cbind(x,y)),g1$crs,inv=TRUE)
         if (is.null(xy)) {
            xy <- with(g2,cbind(x,y))
            minx <- min(xy[,1])
            maxx <- max(xy[,1])
            miny <- min(xy[,2])
            maxy <- max(xy[,2])
            xy <- xy[xy[,1]>minx & xy[,1]<maxx & xy[,2]>miny & xy[,2]<maxy,]
            minx <- min(xy[,1])
            maxx <- max(xy[,1])
            miny <- min(xy[,2])
            maxy <- max(xy[,2])
            xy <- .project(xy,g1$crs,inv=TRUE)
            if (is.null(xy)) {
               cat("Likely, reprojection is failed.\n")
               res <- list(gridline=NULL,margin=NULL)
               class(res) <- "ursaGridLine"
               return(invisible(res))
            }
         }
      }
      else
         xy <- with(g2,cbind(x,y))
      xy <- xy[which(xy[,2]>=-90 & xy[,2]<=90),]
      n <- nrow(xy)
      i1 <- round(0.05*n)
      i1 <- c(i1,n-i1)
      if (i1[1]<1)
         i1[1] <- 1
      if (i1[2]>n)
         i1[2] <- n
      lon <- xy[,1]
     # print(summary(lon,digits=7))
      if (!isLonLat) {
         lon3 <- lon
         lon4 <- lon
         ind3 <- which(lon3<0)
         ind4 <- which(lon3>180)
         lon3[ind3] <- lon3[ind3]+360
         lon4[ind4] <- lon4[ind4]-360
         sd2 <- sd(lon)
         sd3 <- sd(lon3)
         sd4 <- sd(lon4)
         if (verbose)
            print(c(sd2=sd2,sd3=sd3,sd4=sd4))
         if ((sd3<sd2)&&(sd3<sd4))
            lon <- lon3
         else if ((sd4<sd2)&&(sd4<sd3))
            lon <- lon4
         else if ((sd3<sd2)&&(sd3==sd4)) { 
            lon <- lon3
         }
      }
     # print(summary(lon,digits=7))
     # q()
      lat <- xy[,2]
      cond1 <- length(which(abs(lon)<=10))>0
      cond2 <- length(which(abs(abs(lon)-180)<=10))>0
      if (cond1 && cond2 && projClass %in% c("laea","stere"))
         pole <- TRUE
      else {
         pole <- FALSE
         if ((!isLonLat)&&(FALSE)) {
            if ((cond2)&&(length(ind <- which(lon<0))))
               lon[ind] <- lon[ind]+360
            else if ((cond1)&&(length(ind <- which(lon>180))))
               lon[ind] <- lon[ind]-180
         }
      }
      ##~ hist(lon,breaks=180)
      lon <- sort(lon)[i1]
      lat <- sort(lat)[i1]
      isSouth <- all(lat<0)
      isNorth <- all(lat>=0)
      isEquator <- any(lat>0) & any(lat<0)
      isLatDistortion <- isEquator & isProj & !isLonLat &
                         projClass %in% c("laea","merc","cea")
     # alat <- sort(abs(lat))
      alat <- sort(lat)
     # print(c(lat=lat,lon=lon))
     # pole <- any(lat>80)
      dx <- (maxx-minx)/1000
      dy <- (maxy-miny)/1000
      nrE <- ifelse(isLatDistortion,5,3) ## if equator then 5?
      if (dx<dy) {
         sc <- dy/dx
         nc0 <- 3
         nc <- ifelse(sc>2,nc0-1,nc0)
         nr <- floor(nc0*sc)
        # print(nr)
         if (nr<nrE)
            nr <- max(round(nc0*sc),nrE)
      }
      else {
         sc <- dx/dy
         nr <- nrE
         nr0 <- ifelse(sc>2.5,nr-1,nr)
        # print(c(scX=sc,nr=nr,nc=nr0*sc))
         nc <- round(nr0*sc) ## only if floor()<3
      }
      if ((FALSE)&&(!isLonLat)&&(!pole)) {
         xy0 <- c((minx+maxx)/2,(miny+maxy)/2)
         if (any(xy0!=0)) {
            aside <- abs(atan(xy0[1]/xy0[2])*180/pi)
            if (aside>=60) {
               .nc <- nc
               nc <- nr
               nr <- .nc
               rm(.nc)
            }
            else if (aside>30) {
               nc <- nr <- min(c(nc,nr))
            }
         }
      }
      if (verbose)
         print(c(nc=nc,nr=nr))
     # if (length(ind <- lon<0))
     #    lon[ind] <- lon[ind]+360
     # if (pole)
     #    lon <- c(0,lon,360)
      dl <- rev(c(60,45,40,30,20,15,10,6,5,4,3,2,1 ## insert 2.5?
               ,c(30,20,15,12,10,6,5,4,3,2,1)/60 ## 3? 0.5?
               ,c(30,20,15,12,10,6,5,4,3,2,1)/3600))
      if (pole) {
         if (min(alat)>=75)
            .by=15
         else if (min(alat)>=60)
            .by=20
         else
            .by=30
         lon <- seq(0,360,by=.by)
         lat0 <- 90-.prettyLabel(90-alat,ncol=6)$at
         for (i in dl) {
            if (i %in% c(20,40,60))
               next
            lat3 <- seq(-90,90,by=i)
            lat3 <- lat3[lat3>alat[1] & lat3<alat[2]]
            if (length(lat3)<=nr) {
               lat <- lat3
               break
            }
         }
         if (!length(lat))
            lat <- lat0
         else {
            dlat <- mean(diff(lat))
            lat <- unique(c(lat-dlat,lat,lat+dlat))
         }
      }
      else if ((FALSE)&&(isLonLat)) {
         for (i in 1:9) {
            lon <- pretty(lon,n=nc+i-1)
            if (length(which(lon>=g1$minx & lon<=g1$maxx))>=2)
               break
         }
         for (i in 1:9) {
            lat <- pretty(lat,n=nc+i-1)
            if (length(which(lat>=g1$miny & lat<=g1$maxy))>=2)
               break
         }
      }
      else {
         lon2a <- pretty(lon,n=nc)
         lon2b <- .prettyLabel(lon,ncol=nc+1)$at
        # lon1 <- if (length(lon2a)<length(lon2b)) lon2a else lon2a
         lon1 <- lon0 <- lon2a
         mm <- (lon0-floor(lon0))*60
         resa <- with(g1,min(c(maxx-minx,maxy-miny)))/1000
        # mm <- 0.5
         if ((TRUE)||(any(round(mm,6)!=0)))
         {
            if (isMerc) {
               lon_0 <- as.numeric(.gsub2("\\+lon_0=(\\S+)\\s","\\1",proj4))
               lat_ts <- .gsub2("\\+lat_ts=(\\S+)\\s","\\1",proj4)
               lat_ts <- ifelse(lat_ts==proj4,0,as.numeric(lat_ts))
              # lat_ts <- 0
               lon <- c(minx,maxx)/6378137/pi*180/cos(lat_ts*pi/180)+lon_0
            }
            v1 <- ifelse(lon[1]>=-180,-180,-360) #floor(min(lon))
            v2 <- 360 #ceiling(max(lon))
            dl2 <- dl[resa/(2*nc*111)<dl]
            if (!length(dl2))
               dl2 <- dl
            for (i in dl2) {
               lon3 <- seq(v1,v2,by=i)
               lon3 <- lon3[lon3>lon[1] & lon3<lon[2]]
               if (length(lon3)>1)
                  lon0 <- lon3
               if (length(lon3)<=nc) {
                  break
               }
            }
         }
        # if (isMerc)
        #    lon <- seq(v1,v2,by=i)
        # else 
        # if (length(lon3)==1)
        #    lon3 <- .prettyLabel(lon,ncol=nc)$at
         if (TRUE) { ## ++ 20170616
            if (length(lon3)>1)
               lon <- lon3
            else if (length(lon0)<=3)
               lon <- lon0
            else
               lon <- lon2b
         }
         else
            lon <- if (length(lon3)>1) lon3 else lon2b ## -- 20170616
         lat1 <- lat0 <- .prettyLabel(lat,ncol=nr+2)$at
         mm <- (lat0-floor(lat0))*60
        # mm <- 0.5
         if ((TRUE)&&(any(round(mm,6)!=0)))
         {
            v1 <- -90 #floor(min(lat))
            v2 <- 90 #ceiling(max(lat))
            dl2 <- dl[resa/(2*nr*111)<dl]
            if (!length(dl2))
               dl2 <- dl
            for (i in dl) {
               if (i %in% c(20,40,60))
                  next
               lat3 <- seq(v1,v2,by=i)
               lat3 <- lat3[lat3>lat[1] & lat3<lat[2]]
               if (length(lat3)<=nr) {
                  lat0 <- lat3
                  break
               }
            }
         }
         lat <- if (length(lat0)>1) lat0 else lat1
         dlon <- mean(diff(lon))
         dlat <- mean(diff(lat))
         if (length(lon)>1) {
            lon <- unique(c(lon-dlon,lon,lon+dlon))
         }
         if (length(lat)>1) {
            lat <- unique(c(lat-dlat,lat,lat+dlat))
         }
        # if (isMerc) {
           # print(v)
           # print(right)
           # print(lon)
           # q()
        # }
        # if ((0 %in% lon) && (360 %in% lon))
        #    lon <- lon[lon!=360]
         rm(lon0,lon1,lat0,lat1,dlon,dlat)
        # print(lon)
        # print(lat)
      }
     # lat[lat>=90] <- 89.9
     # lat <- lat[lat<90]
      dlon <- abs(diff(lon))[1]*c(1)
      dlat <- abs(diff(lat))[1]*c(1)
      if (!pole) {
         if (!(projClass %in% c("cea"))) {
            lon <- c(lon[1]-rev(dlon),lon,lon[length(lon)]+dlon)
            lat <- c(lat[1]-rev(dlat),lat,lat[length(lat)]+dlat)
         }
      }
      else if (!isSouth){
         lat <- sort(lat)
         lat <- unique(c(lat[1]-rev(dlat),lat,90))
      }
      else {
         lat <- sort(-lat)
         lat <- unique(c(lat[1]-rev(dlat),lat,90))
         lat <- -lat
      }
      lonList <- list(unique(lon))
      latList <- list(unique(lat))
     # print(lon)
     # print(lat)
   }
   marginalia <- rep(marginalia,length=4)
  # north <- 89.5
   if (TRUE) {#((isProj)&&(!isLonLat)) {
      if (projClass %in% c("zzzmerc","zzzlonglat"))
         north <- 90-1e-6
      else
         north <- 90-0.25*abs(mean(diff(lat)))
      south <- -north
   }
   outframe <- NULL
   alim <- 15 ## critical anlge (degree) between border line and grid line
   projclass <- .gsub(".+proj=(\\S+)\\s.+","\\1",g1$crs)
   for (j in seq_along(lonList))
   {
      lonSet <- unique(round(lonList[[j]],11))
      latSet <- unique(round(latList[[j]],11))
      if ((isProj)&&(!isLonLat)) {
         latSet[latSet>north] <- north
         latSet[latSet<south] <- south
         latSet <- unique(latSet)
      }
      if ((FALSE)&&(projClass %in% c("merc","longlat"))) {
         if (180 %in% lonSet)
            lonSet <- sort(c(lonSet[lonSet!=180],180-1e-6,180+1e-6))
      }
      gridline <- vector("list",length(lonSet)+length(latSet))
      llkind <- rep(0L,length(gridline))
      llval <- rep(NA,length(gridline))
      i <- 0L
      if (!isProj) {
         lat <- seq(min(latSet),max(latSet),len=10)
      }
      else {
         latSet <- na.omit(latSet)
         if (projclass %in% c("stere","laea")[1])
            lat <- seq(min(latSet),max(latSet),len=2)
         else if (projclass %in% c("merc"))
            lat <- c(-1,1)*(90-1e-6)
         else {
            if (length(latSet)==1)
               lat <- latSet
            else {
               lat <- seq(min(latSet),max(latSet),by=mean(diff(latSet))/10)
            }
         }
      }
      if (isMerc) {
         B <- .getMajorSemiAxis(g1$crs)*pi
         lon_0 <- as.numeric(.gsub(".*\\+lon_0=(\\S+)\\s.*","\\1",g1$crs))
         lat_ts <- .gsub2("\\+lat_ts=(\\S+)\\s","\\1",g1$crs)
         lat_ts <- ifelse(lat_ts==g1$crs,0,as.numeric(lat_ts))
      }
      for (lon in lonSet)
      {
         if (!((isLonLat)||(isMerc))) {
            if ((lon==360)&&(0 %in% lonSet))
               next
            if ((lon==-180)&&(+180 %in% lonSet))
               next
         }
         i <- i+1L
         ll <- cbind(rep(lon,length(lat)),lat)
        # proj4a <- "+proj=merc +lon_0=48 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
        # gridline[[i]] <- if (isProj & !isLonLat) proj4::project(t(ll),g1$crs) else ll
         if (isProj & !isLonLat) {
            gridline[[i]] <- .project(ll,g1$crs)
           # gridline[[i]] <- .project(ll,proj4a)
            if ((FALSE)&&(isMerc)) {
               x <- gridline[[i]][1,1]
               print(data.frame(lon=lon,x=x,y=(lon-lon_0)/180*B))
               ##~ if (((lon<0)&&(x>maxx))||(lon<=(-180)))
                  ##~ x <- x-2*20037508
               ##~ else if ((lon>360)&(x<minx))#(+180))
                  ##~ x <- x+2*20037508
               if ((lon<0)&&(x>0))
                  x <- x-2*B
               else if ((lon>360)&(x<B))
                  x <- x+2*B
              # if (x>maxx)
              #    x <- x-2*20037508
              # else if (x<minx)
              #    x <- x+2*20037508
              # print(data.frame(lon=lon,lonL=lon-360,lonR=lon+360
              #               ,minx=minx,src=xtmp,dst=x,maxx=maxx))
               gridline[[i]][,1] <- x
            }
            if ((!FALSE)&&(isMerc)) ## -- 20180423
               gridline[[i]][,1] <- (lon-lon_0)/180*B*cos(lat_ts*pi/180)
           # print(gridline[[i]])
           # if ((isMerc)&&((lon<0)&&(gridline[[i]][1,1]>0)))
           #    gridline[[i]][,1] <- -2*20037508+gridline[[i]][,1]
         }
         else
            gridline[[i]] <- ll
         llkind[i] <- 1L
         llval[i] <- lon
      }
      if (projclass %in% c("merc"))
         lon <- seq(-180,360,len=10)
      else if (isProj)
         lon <- seq(min(lonSet),max(lonSet),by=mean(abs(diff(lonSet)))/(j*10))
      else
         lon <- seq(min(lonSet),max(lonSet),len=10)
      for (lat in latSet)
      {
         i <- i+1L
         if (abs(lat)==max(abs(latSet)))
            gridline[[i]] <- cbind(NA,NA)
         else {
            ll <- cbind(lon,rep(lat,length(lon)))
           # print(series(ll,3))
           # gridline[[i]] <- if (isProj & !isLonLat) proj4::project(t(ll),g1$crs) else ll
            if (isProj & !isLonLat) {
               ll <- .project(ll,g1$crs)
               if (projclass %in% "merc") {
                  ll <- ll[order(ll[,1]),]
                  ll[1,1] <- ll[1,1]-1e8
                  ll[nrow(ll),1] <- ll[nrow(ll),1]+1e8
               }
               ind <- which(diff(ll[,1])<0)
               if ((length(ind)==2)&&(ind[1]+1!=ind[2])) {
                  ll <- ll[(ind[1]+1):ind[2],,drop=FALSE]
               }
            }
            gridline[[i]] <- ll
         }
         llkind[i] <- 2L
         llval[i] <- lat
      }
      for (i in seq(along=gridline))
      {
         xy <- gridline[[i]]
         if (all(is.na(xy)))
            next
         if (marginalia[2]) {
            e1 <- which(diff((xy[,1]-minx)>0)!=0)
            for (j in seq_along(e1)) {
               l <- xy[c(e1[j],e1[j]+1),]
               p <- l[1,2]+(minx-l[1,1])/(l[2,1]-l[1,1])*(l[2,2]-l[1,2])
               an <- 90-abs(atan(diff(l[,2])/diff(l[,1]))*180/pi)
               if ((is.finite(p))&&(.is.ge(p,miny))&&(.is.le(p,maxy))&&(abs(an)>=alim))
                  outframe <- rbind(outframe,data.frame(side=2,at=p
                                     ,kind=llkind[i],v=llval[i],an=an
                                     ,i=i,j=j,stringsAsFactors=FALSE))
            }
         }
         if (marginalia[4]) {
            e1 <- which(diff((xy[,1]-maxx)>0)!=0)
            for (j in seq_along(e1)) {
               l <- xy[c(e1[j],e1[j]+1),]
               p <- l[1,2]+(maxx-l[1,1])/(l[2,1]-l[1,1])*(l[2,2]-l[1,2])
               an <- 90-abs(atan(diff(l[,2])/diff(l[,1]))*180/pi)
               if ((is.finite(p))&&(.is.ge(p,miny))&&(.is.le(p,maxy))&&(abs(an)>=alim))
                  outframe <- rbind(outframe,data.frame(side=4,at=p
                                     ,kind=llkind[i],v=llval[i],an=an
                                     ,i=i,j=j,stringsAsFactors=FALSE))
            }
         }
         if (marginalia[1]) {
            e1 <- which(diff((xy[,2]-miny)>0)!=0)
            for (j in seq_along(e1)) {
               l <- xy[c(e1[j],e1[j]+1),]
               p <- l[1,1]+(miny-l[1,2])/(l[2,2]-l[1,2])*(l[2,1]-l[1,1])
               an <- abs(atan(diff(l[,2])/diff(l[,1]))*180/pi)
               if ((is.finite(p))&&(.is.ge(p,minx))&&(.is.le(p,maxx))&&(abs(an)>=alim))
                  outframe <- rbind(outframe,data.frame(side=1,at=p
                                     ,kind=llkind[i],v=llval[i],an=an
                                     ,i=i,j=j,stringsAsFactors=FALSE))
            }
         }
         if (marginalia[3]) {
            e1 <- which(diff((xy[,2]-maxy)>0)!=0)
            for (j in seq_along(e1)) {
               l <- xy[c(e1[j],e1[j]+1),]
               p <- l[1,1]+(maxy-l[1,2])/(l[2,2]-l[1,2])*(l[2,1]-l[1,1])
               an <- abs(atan(diff(l[,2])/diff(l[,1]))*180/pi)
               if ((is.finite(p))&&(.is.ge(p,minx))&&(.is.le(p,maxx))&&(abs(an)>=alim))
                  outframe <- rbind(outframe,data.frame(side=3,at=p
                                     ,kind=llkind[i],v=llval[i],an=an
                                     ,i=i,j=j,stringsAsFactors=FALSE))
            }
         }
      }
   }
   if (is.null(outframe)) {
      res <- list(gridline=gridline,grid=list(lon=lonSet,lat=latSet)
                ,panel=panel,margin=NULL,col=col,border=border,lwd=lwd,lty=lty)
      class(res) <- "ursaGridLine"
      return(res)
   }
   outframe <- outframe[with(outframe,order(side,at)),]
   if (!isLonLat) {
      outframe$at <- round(outframe$at,6)
      outframe$an <- round(outframe$an,6)
   }
   outframe$i <- NULL
   outframe$j <- NULL
   outframe <- unique(outframe)
   if (isProj) {
      if (length(ind <- outframe$v>180))
         outframe$v[ind] <- outframe$v[ind]-360
      if (length(ind <- outframe$v<=(-180)))
         outframe$v[ind] <- outframe$v[ind]+360
      outframe$lab <- NA
     # suffNS <- c("N","S")
     # suffEW <- c("E","W")
      suffNS <- switch(language,ru=c("\u0441.\u0448.","\u044E.\u0448."),c("N","S"))
      suffEW <- switch(language,ru=c("\u0432.\u0434.","\u0437.\u0434."),c("E","W"))
     # suffNS <- switch(language,ru=c("\xF1.\xF8.","\xFE.\xF8."),c("N","S"))
     # suffEW <- switch(language,ru=c("\xE2.\xE4.","\xE7.\xE4."),c("E","W"))
      ind <- (outframe$kind==2)
      outframe$lab[ind] <- .degminsec(outframe$v[ind],suffNS)
      ind <- (outframe$kind==1)
      outframe$lab[ind] <- .degminsec(outframe$v[ind],suffEW)
   }
   else {
      if (g1$resx!=g1$resy) {
         for (i in c(1,2)) {
            ind <- (outframe$kind==i)
            outframe$lab[ind] <- format(outframe$v[ind],trim=TRUE)
         }
      }
      else
         outframe$lab <- format(outframe$v,trim=TRUE)
     # outframe$lab <- sprintf(ifelse(outframe$v==round(outframe$v),"%.0f","%f")
     #                        ,outframe$v)
   }
  # outframe$kind <- NULL
   outframe$adj <- 0.5
   outframe$cex <- cex
   da <- unique(outframe)
   daZ <- data.frame(side=0,at=NA,kind=NA,v=NA,an=90,lab="|",adj=0.5,cex=cex
                    ,stringsAsFactors=FALSE)
   outframe <- NULL
   if (trim) {
      sc <- getOption("ursaPngScale")
      if (is.numeric(sc)) {
         sx <- 5/sc*g1$resx ## set 0, if failed
         sy <- 5/sc*g1$resy ## set 0, if failed
      }
      else {
         opW <- options(warn=-1)
         warning(paste("It seems 'compose_open' have not called yet."
                      ,"The labelling is less optimal."))
         options(opW)
         sx <- 0
         sy <- 0
      }
   }
   for (i in 1:4) {
      if (!marginalia[i])
         next
      ind1 <- which(da$side==i)
      da0 <- da[ind1,]
      if (!nrow(da0))
         next
      if (trim) {
         da0 <- rbind(daZ,da0,daZ)
         if (i %in% c(1,3)) {
            da0$at[1] <- g1$minx-sx
            da0$at[nrow(da0)] <- g1$maxx+sx
         }
         else {
            if (g1$miny<g1$maxy) {
               da0$at[1] <- g1$miny-sy
               da0$at[nrow(da0)] <- g1$maxy+sy
            }
            else {
               da0$at[1] <- g1$maxy-sy
               da0$at[nrow(da0)] <- g1$miny+sy
            }
         }
      }
      nr <- nrow(da0)
      daX <- NULL
      da0$ind <- seq(nr)
      k <- 0
      width <- with(g1,if (i %in% c(1,3)) (maxx-minx) else (maxy-miny))
      res <- abs(with(g1,if (i %in% c(1,3)) resx else resy))
      repeat ({
         if (k>200)
            break
         w <- strwidth(paste0(da0$lab,"mmmm"),units="inches",cex=cex)*
              res*getOption("ursaPngDpi")/getOption("ursaPngScale")
                     # ,family=getOption("ursaPngFamily")
         if ((FALSE)&&(sum(w)/width>2)) {
           # daY <- subset(da0,kind==1)
            daY <- da0[which(da0$kind==1),]
            n <- nrow(daY)
            v1 <- seq(1,n,by=2)
            v2 <- seq(n,1,by=-2)
            d <- v1-v2
            ind <- c(v1[d<=0],n-v2[d>=0]+1)
           # da0 <- rbind(subset(da0,kind==2),daY[ind,])
            da0 <- rbind(da0[which(da0$kind==2),],daY[ind,])
            da0 <- da0[with(da0,order(at)),]
            next
         }
         wL <- da0$at-w*da0$adj
         wR <- da0$at+w*(1-da0$adj)
         wD <- wL[-1]-wR[-length(wR)]
         wD1 <- c(1e-6,wD)
         wD2 <- c(wD,1e-6) # length(w)
         ind2 <- which.min(wD2)
         if (wD2[ind2]>=0) {
            break
         }
         if (length(ind2)==length(w)) {
            break
         }
         ind2 <- c(ind2,ind2+1L)
         adj <- da0[ind2,"adj"]
         adj <- adj+c(0.0999,-0.0999)
         if (all(adj>=-0.01 & adj<=1.01)) {
            da0[ind2,"adj"] <- adj
            next
         }
         k <- k+1
         ind2 <- ind2[which.min(da0$an[ind2])]
         daX <- rbind(daX,da0[ind2,])
         da0 <- da0[-ind2,]
         daX <- daX[order(daX$ind),]
         ind3 <- which(!diff(diff(daX$ind)))+1
         if ((FALSE)&&(length(ind3))) {
            if (length(ind3)==3)
               ind3 <- ind3[2]
            else if (length(ind3)==5)
               ind3 <- ind3[3]
            else
               ind3 <- sample(ind3,1)
            ind4 <- daX$ind[ind3]
            ind5 <- sort(c(da0$ind,ind4))
            ind6 <- ind5[which(!diff(diff(ind5)))+1]
            if (!(ind4 %in% ind6))
            {
               da0 <- rbind(da0,daX[ind3,])
               daX <- daX[-ind3,]
            }
         }
         da0[,"adj"] <- 0.5
      })
     # outframe <- rbind(outframe,subset(da0,side!=0))
      outframe <- rbind(outframe,da0[which(da0$side!=0),])
   }
   outframe <- outframe[with(outframe,order(side,at)),]
   res <- list(gridline=gridline,grid=list(lon=lonSet,lat=latSet)
              ,panel=panel,margin=outframe
              ,col=col,border=border,lwd=lwd,lty=lty)
   class(res) <- "ursaGridLine"
   res
}
'panel_graticule' <- function(...) {
   if (.skipPlot())
      return(NULL)
   arglist <- list(...)
   kwd <- "^(graticule|grid(line)*)$"
   figure <- getOption("ursaPngFigure")
   gridline <- .getPrm(arglist,name=kwd,class=list("integer","logical")
                      ,default=TRUE)
   if (is.integer(gridline))
      gridline <- figure %in% gridline
   if ((length(gridline)==1)&&(!gridline))
      return(NULL)
   obj <- .getPrm(arglist,class="ursaGridLine",default=NULL)
   g1 <- session_grid()
  # if ((!is.null(g1$labx))&&(!is.null(g1$laby))) {
   if ((length(g1$seqx))&&(length(g1$seqy))) {
      .repairForScatterPlot()
      return(NULL)
   }
   if (is.null(obj))
      obj <- compose_graticule(...)
   if (is.null(obj$gridline))
      return(NULL)
   if ((!is.null(attr(g1$seqx,"units"))&&(!is.null(attr(g1$seqy,"units"))))) {
      if (is.null(g1$labx))
         g1$labx <- unique(obj$margin[obj$margin$kind==1,"at"])
      if (is.null(g1$laby))
         g1$laby <- unique(obj$margin[obj$margin$kind==2,"at"])
      session_grid(g1)
      .repairForScatterPlot()
      return(NULL)
   }
   if ((!(0 %in% obj$panel))&&(!((figure %in% abs(obj$panel))||
                                 ((figure+10000L) %in% abs(obj$panel)))))
      return(NULL)
   if (FALSE) {
      obj$col <- .getPrm(arglist,name="col",kwd=kwd,default=obj$col)
      obj$lwd <- .getPrm(arglist,name="lwd",kwd=kwd,default=obj$lwd)
      obj$lty <- .getPrm(arglist,name="lty",kwd=kwd,default=obj$lty)
   }
   verbose <- .getPrm(arglist,name="verb(ose)*",kwd=kwd,default=FALSE)
   marginalia <-  !is.na(match(seq(4),sort(unique(obj$margin$side))))
   marginalia <- .getPrm(arglist,name="(decor|margin(alia)*)",kwd=kwd
                        ,class=list("integer","logical"),default=marginalia)
   if (is.integer(marginalia)) {
      marginalia <- rep(figure %in% abs(marginalia),4)
   }
   if ((any(obj$panel))&&(!((figure %in% obj$panel)||((figure+10000L) %in% obj$panel)))) {
      marginalia <- FALSE
   }
   marginalia <- rep(marginalia,length=4)
   if ((figure+10000L) %in% obj$panel)
      comment(marginalia) <- "internal"
   else
      comment(marginalia) <- NULL
   .panel_graticule(obj,marginalia=marginalia,verbose=verbose)
}
'.panel_graticule' <- function(obj,marginalia=rep(TRUE,4),verbose=FALSE) {
   g1 <- getOption("ursaPngComposeGrid")
   g2 <- getOption("ursaPngPanelGrid")
  # print(g1)
  # print(g2)
  # internal <- isTRUE(comment(marginalia)=="internal")
   internal <- !identical(g1,g2)
   if (internal) {
      g1a <- g1
      g2a <- g2
     # g1a$crs <- NULL
     # g2a$crs <- NULL
      if (!identical(g1a,g2a)) {
         res <- sapply(names(g1a),function(x) {
           # if (x %in% c("retina"))
           #    return(TRUE)
            if ((is.null(g1a[[x]]))||(is.null(g2a[[x]])))
               return(TRUE)
            if ((isTRUE(is.na(g1a[[x]])))||(isTRUE(is.na(g2a[[x]]))))
               return(TRUE)
            ret <- identical(g1a[[x]],g2a[[x]])
            if (!ret) {
               if (is.numeric(g1a[[x]])) {
                  ret <- .is.eq(g1a[[x]],g2a[[x]])
               }
               if (is.character(g1a[[x]])) {
                  if (length(grep("\\+proj",g1a[[x]]))) {
                     proj1 <- gsub(".*\\+proj=(\\S+)($|\\s+.*$)","\\1",g1a[[x]])
                     proj2 <- gsub(".*\\+proj=(\\S+)($|\\s+.*$)","\\1",g2a[[x]])
                     cond1 <- proj1==proj2
                  }
                  else
                     cond1 <- TRUE
                  if (length(grep("\\+lon_0",g1a[[x]]))) {
                     lon1 <- gsub(".*\\+lon_0=(\\S+)($|\\s+.*$)","\\1",g1a[[x]])
                     lon2 <- gsub(".*\\+lon_0=(\\S+)($|\\s+.*$)","\\1",g2a[[x]])
                     cond2 <- lon1==lon2
                  }
                  else
                     cond2 <- TRUE
                  if (length(grep("\\+lat_0",g1a[[x]]))) {
                     lat1 <- gsub(".*\\+lat_0=(\\S+)($|\\s+.*$)","\\1",g1a[[x]])
                     lat2 <- gsub(".*\\+lat_0=(\\S+)($|\\s+.*$)","\\1",g2a[[x]])
                     cond3 <- lat1==lat2
                  }
                  else
                     cond3 <- TRUE
                  ret <- cond1 & cond2 & cond3
               }
            }
            ret
         })
         internal <- !all(res)
      }
   }
   with(obj,{
      if (verbose)
         str(list(col=col,lwd=lwd,lty=lty))
      for (i in seq(along=gridline))
      {
         xy <- gridline[[i]]
        # if (all(is.na(xy)))
        #    next
         lines(xy[,1],xy[,2],col=col,lwd=lwd,lty=lty)
      }
      if (is.null(margin))
         return(NULL)
      pngOp <- options()[.grep("^ursaPng.+",names(options()))]
      layout <- pngOp[["ursaPngLayout"]][["layout"]]
      layout0 <- (layout==pngOp[["ursaPngFigure"]])
      indr <- which(rowSums(layout0)==1)
      indc <- which(colSums(layout0)==1)
     # print(c(row=indr,column=indc))
      if (FALSE) {
         isTop <- all(layout[1L:(indr-1L),indc]==0)
         isBottom <- all(layout[(indr+1L):nrow(layout),indc]==0)
         isLeft <- all(layout[indr,1L:(indc-1L)]==0)
         isRight <- all(layout[indr,(indc+1L):ncol(layout)]==0)
      }
      else {
         isTop <- all(layout[(indr-2L):(indr-1L),indc]==0)
         isBottom <- all(layout[(indr+1L):(indr+2L),indc]==0)
         isLeft <- all(layout[indr,(indc-2L):(indc-1L)]==0)
         isRight <- all(layout[indr,(indc+1L):(indc+2L)]==0)
      }
      marginalia0 <- marginalia
      marginalia <- as.integer(marginalia0 & c(isBottom,isLeft,isTop,isRight))
      if (internal) {
         if ((sum(marginalia[c(1,3)])>0)&&(sum(marginalia[c(2,4)])))
            internal <- FALSE
      }
     # print(c(bottom=isBottom,left=isLeft,top=isTop,right=isRight))
      if (internal) {
         panel2 <- pngOp[["ursaPngLayout"]][["image"]]
        # fig2 <- pngOp[["ursaPngFigure"]]
         layout2 <- layout
         layout2[layout2<=panel2] <- 0L
         isTop2 <- all(layout2[(indr-2L):(indr-1L),indc]==0)
         isBottom2 <- all(layout2[(indr+1L):(indr+2L),indc]==0)
         isLeft2 <- all(layout2[indr,(indc-2L):(indc-1L)]==0)
         isRight2 <- all(layout2[indr,(indc+1L):(indc+2L)]==0)
         marginalia2 <- as.integer(marginalia0 & c(isBottom2,isLeft2,isTop2,isRight2))
         marginalia2 <- as.integer(!marginalia)*marginalia2
         if ((marginalia[4])&&(marginalia2[2]))
            marginalia2[2] <- 0L
         if ((marginalia[2])&&(marginalia2[4]))
            marginalia2[4] <- 0L
         if ((marginalia[3])&&(marginalia2[1]))
            marginalia2[1] <- 0L
         if ((marginalia[1])&&(marginalia2[3]))
            marginalia2[3] <- 0L
        # print(marginalia)
        # print(marginalia2)
        # marginalia2 <- marginalia2-marginalia
         marginalia <- marginalia2+marginalia
         if ((marginalia2[4]==1)&&((marginalia2[2]==marginalia2[4])))
            marginalia2[4] <- marginalia[4] <- 0L
         if ((marginalia2[3]==1)&&((marginalia2[1]==marginalia2[3])))
            marginalia2[3] <- marginalia[3] <- 0L
         rm(layout2,isTop2,isBottom2,isLeft2,isRight2,panel2)
        # print(marginalia)
        # print(marginalia2)
      }
      rm(pngOp,layout,layout0,indc,indr,isTop,isBottom,isLeft,isRight)
     # da1 <- if (marginalia[1]) subset(margin,side==1) else NULL
     # da2 <- if (marginalia[2]) subset(margin,side==2) else NULL
     # da3 <- if (marginalia[3]) subset(margin,side==3) else NULL
     # da4 <- if (marginalia[4]) subset(margin,side==4) else NULL
      da1 <- if (marginalia[1]) margin[which(margin$side==1),] else NULL
      da2 <- if (marginalia[2]) margin[which(margin$side==2),] else NULL
      da3 <- if (marginalia[3]) margin[which(margin$side==3),] else NULL
      da4 <- if (marginalia[4]) margin[which(margin$side==4),] else NULL
     # opT <- par(family="Arial Narrow")
      if (F & internal) {
         print(c((!is.null(da1))&&(nrow(da1)&&(marginalia2[1]))
                ,(!is.null(da2))&&(nrow(da2)&&(marginalia2[2]))
                ,(!is.null(da3))&&(nrow(da3)&&(marginalia2[3]))
                ,(!is.null(da4))&&(nrow(da4)&&(marginalia2[4]))
                ))
      }
      if ((!is.null(da1))&&(nrow(da1)))
         with(da1,{
            if ((internal)&&(marginalia2[1])) {
               mtext(side=1,at=at,text=lab,padj=-1.7,adj=adj,cex=cex,col=border
                    ,family=getOption("ursaPngFamily")
                    )
            }
            else {
               axis(side=1,at=at,labels=NA,tcl=-0.2,col=border,lwd=0,lwd.ticks=lwd)
               mtext(side=1,at=at,text=lab,padj=0.5,adj=adj,cex=cex,col=border
                    ,family=getOption("ursaPngFamily")
                    )
            }
         })
      if ((!is.null(da2))&&(nrow(da2)))
         with(da2,{
            if ((internal)&&(marginalia2[2])) {
               mtext(side=2,at=at,text=lab,padj=0.4,adj=adj,line=-1,cex=cex,col=border
                    ,family=getOption("ursaPngFamily")
                    )
            }
            else {
               axis(side=2,at=at,labels=NA,tcl=-0.2,col=border,lwd=0,lwd.ticks=lwd)
               mtext(side=2,at=at,text=lab,padj=0.4,adj=adj,line=0.6,cex=cex,col=border
                    ,family=getOption("ursaPngFamily")
                    )
            }
         })
      if ((!is.null(da3))&&(nrow(da3)))
         with(da3,{
            if ((internal)&&(marginalia2[3])) {
               mtext(side=3,at=at,text=lab,padj=-0.25,adj=adj,line=-1.4,cex=cex,col=border
                    ,family=getOption("ursaPngFamily")
                    )
            }
            else {
               axis(side=3,at=at,labels=NA,tcl=-0.2,col=border,lwd=0,lwd.ticks=lwd)
               mtext(side=3,at=at,text=lab,padj=-0.25,adj=adj,line=0,cex=cex,col=border
                    ,family=getOption("ursaPngFamily")
                    )
            }
         })
      if ((!is.null(da4))&&(nrow(da4)))
         with(da4,{
            if ((internal)&&(marginalia2[4])) {
               mtext(side=4,at=at,text=lab,line=0,adj=adj,padj=-1.6,cex=cex,col=border
                    ,family=getOption("ursaPngFamily")
                    )
            }
            else {
               axis(side=4,at=at,labels=NA,tcl=-0.2,col=border,lwd=0,lwd.ticks=lwd)
               mtext(side=4,at=at,text=lab,line=0,adj=adj,padj=0.4,cex=cex,col=border
                    ,family=getOption("ursaPngFamily")
                    )
            }
         })
     # options(opT)
   })
   invisible(NULL)
}
