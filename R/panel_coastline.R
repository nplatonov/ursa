## http://www.soest.hawaii.edu/pwessel/gshhg/ 2.3.7 2017-06-15
## http://gis-lab.info/forum/viewtopic.php?t=22977
## http://thematicmapping.org/downloads/world_borders.php



##~ '.panel_coastline' <- function(
                               ##~ coastline=TRUE,col="grey60",fill="transparent"
                              ##~ ,detail=NA,density=NA,angle=NA,land=FALSE
                              ##~ ,lwd=1,lty=1L,...) {
   ##~ NULL
##~ }
'compose_coastline' <- function(...) {
   arglist <- list(...)
   kwd <- "coast(line)*"
   coastline <- .getPrm(arglist,name=paste0("(",kwd,"|decor)$")
                       ,class=list("integer","logical"),default=TRUE)
   if (any(!coastline)) {
      res <- list(coast_xy=NULL)
      class(res) <- "ursaCoastLine"
      return(res)
   }
   res <- .getPrm(arglist,name=kwd,class="ursaCoastLine",default=NULL)
   if (!is.null(res)) {
      return(res)
   }
   res <- getOption("ursaPngCoastLine")
   if (!is.null(res))
      return(res)
   bg <- sum(c(col2rgb(getOption("ursaPngBackground")))*c(0.30,0.59,0.11))
   defcol <- ifelse(bg<128,"#FFFFFF3F","#0000003F") # grey60
   col <- .getPrm(arglist,name="(col|line)",kwd=kwd,default=defcol) 
   fill <- .getPrm(arglist,name="fill",kwd=kwd,default="transparent")
   detail <- .getPrm(arglist,name="detail",kwd=kwd,default=NA_character_)
   density <- .getPrm(arglist,name="density",kwd=kwd,default=NA_real_)
   angle <- .getPrm(arglist,name="angle",kwd=kwd,default=NA_real_)
   land <- .getPrm(arglist,name="land",kwd=kwd,default=FALSE)
   lwd <- .getPrm(arglist,name="lwd",kwd=kwd,default=0.5)
   lty <- .getPrm(arglist,name="lty",kwd=kwd,default=1L)
   fail180 <- .getPrm(arglist,name="fail180",kwd=kwd,default=NA)
   obj <- .getPrm(arglist,name=paste0("(^$|",kwd,")") ## name="^$"
                 ,class=list("character","matrix","SpatialPolygonsDataFrame","sf")#[-3]
                 ,default=NULL)
   verbose <- .getPrm(arglist,name="verbose",kwd=kwd,default=FALSE)
   if (is.integer(coastline)) {
      panel <- coastline
     # coastline <- TRUE
   }
   else
      panel <- 0L
   invisible(.compose_coastline(obj=obj,panel=panel,col=col,fill=fill,detail=detail
                     ,density=density,angle=angle
                     ,land=land,lwd=lwd,lty=lty,fail180=fail180,verbose=verbose))
}
'.compose_coastline' <- function(obj=NULL,panel=0,col=NA,fill="transparent"
                                ,detail=NA,density=NA,angle=NA,land=FALSE
                                ,lwd=0.5,lty=1,fail180=NA,verbose=FALSE) {
   if (verbose)
      str(list(obj=obj,panel=panel,col=col,fill=fill,detail=detail
              ,density=density,angle=angle
              ,land=land,lwd=lwd,lty=lty,fail180=fail180))
   if (!is.null(obj)) {
      isPoly <- inherits(obj,c("sf","SpatialPolygonsDataFrame"))
      if ((is.matrix(obj))&&(ncol(obj)==2))
         coast_xy <- obj
      else if ((is.character(obj))||(isPoly)) {
         if ((isPoly)||(.lgrep("\\.shp",obj))) {
            if (isPoly)
               a <- obj
            else {
              # a <- .shp.read(obj)
               a <- spatialize(obj)#,engine="sp")
            }
            if (.isSP(a)) {
               a <- lapply(methods::slot(a,grep("(polygons|lines)"
                                 ,methods::slotNames(a),value=TRUE)),function(x) {
                  y <- lapply(methods::slot(x,grep("(Polygons|Lines)"
                                 ,methods::slotNames(x),value=TRUE)),function(y) {
                     do.call("rbind",lapply(list(sp::coordinates(y),cbind(NA,NA))
                                           ,matrix,ncol=2))
                  })
                  do.call("rbind",lapply(y,matrix,ncol=2))
               })
               coast_xy <- head(do.call("rbind",lapply(a,matrix,ncol=2)),-1)
               rm(a)
            }
            else if (.isSF(a)) {
               if (.lgrep("^multi",spatial_shape(a))) {
                  a <- do.call("rbind",lapply(spatial_coordinates(a),function(x) {
                     do.call("rbind",lapply(unlist(x,recursive=FALSE),rbind,cbind(NA,NA)))
                  }))
               }
               else {
                  a <- do.call("rbind",lapply(spatial_coordinates(a),function(x) {
                     do.call("rbind",lapply(x,rbind,cbind(NA,NA)))
                  }))
               }
               coast_xy <- head(a,-1)
               rm(a)
            }
         }
         else if (.lgrep("\\.rds$",obj)) {
            g1 <- session_grid()
            coast_xy <- readRDS(obj)
            if (nchar(g1$proj)) {
              # b <- attributes(coast_xy)
               coast_xy <- .project(coast_xy,g1$proj4)
              # attributes(coast_xy) <- b
            }
         }
         else
            coast_xy <- NULL
      }
      if (!is.null(coast_xy))
      {
         shadow <- unname(col2rgb(fill,alpha=TRUE)[4,1])
         options(ursaPngShadow=ifelse(shadow %in% c(0,255),"",fill))
         res <- list(coast_xy=coast_xy,panel=panel
                    ,col=col,fill=fill,shadow=shadow,land=land
                    ,density=density,angle=angle,lwd=lwd,lty=lty)
         class(res) <- "ursaCoastLine"
         options(ursaPngCoastLine=res)
         return(res)
      }
   }
   g1 <- session_grid()
   isLongLat <- .lgrep("(\\+proj=longlat|epsg:4326)",g1$proj4)>0
   isMerc <- .lgrep("\\+proj=merc",g1$proj4)>0
   isCea <- .lgrep("\\+proj=cea",g1$proj4)>0
   isUTM <- .lgrep("\\+proj=utm",g1$proj4)>0
   proj <- g1$proj4
   proj <- proj[nchar(proj)==max(nchar(proj))]
   if ((any(is.na(proj)))||(nchar(proj)==0))
      return(NULL)
   isDetail <- !is.na(detail)
   if (is.na(detail))
      detail <- "l"
   if (!(detail %in% c("l","i","h","f",NA)))
      message(paste("coastline detail:",detail))
   if ((FALSE)&&(.lgrep("proj=(merc|longlat)",proj))&&((g1$maxx<=20037508))) {
      if (detail %in% c("f"))
         detail <- "h180"
      else
         detail <- paste0(detail,"180")
   }
   if (FALSE)
   {
      g2 <- with(g1,my.expand.grid(x=seq(minx,maxx,length=16)
                                  ,y=seq(miny,maxy,length=16)))
      ll <- .project(with(g2,cbind(x,y)),g1$proj4,inv=TRUE)
      if (all(ll[,2]<65))
         return(NULL)
   }
   fpath <- getOption("ursaRequisite")
  # if (!nchar(fpath))
  #    fpath <- system.file("requisite",package="ursa")
   if (!is.na(detail)) {
      fname <- file.path(fpath,paste0("coast-",detail,".rds"))
      if (!file.exists(fname)) {
         detail <- "l"
         fname <- file.path(fpath,paste0("coast-",detail,".rds"))
         if (!file.exists(fname))
            fname <- system.file("requisite/coast-l.rds",package="ursa")
      }
      xy <- readRDS(fname)
   }
   else {
      stop("Detalization level is unknown")
     # xy <- readRDS("C:/platt/shapefile/auxiliary/thematicmapping.org/countries.rds")
   }
   coast_xy <- cbind(lon=xy[,1],lat=xy[,2])
   ind <- .grep("^\\d+$",proj)
  # isLoaded <- .lgrep("package:rgdal",search())>0
   isLoaded <- "rgdal" %in% loadedNamespaces()
   if (length(ind))
   {
      proj4 <- "" ## was NA
      try(proj4 <- get(paste0("epsg",proj[ind])))
      if (!nchar(proj4))
      {
         if (!isLoaded)
            requireNamespace("rgdal",quietly=.isPackageInUse())
         proj4 <- rgdal::CRSargs(sp::CRS(sprintf("+init=epsg:%s",proj[ind])))
      }
      else if (!isLoaded) {
         if (!requireNamespace("proj4",quietly=.isPackageInUse()))
            requireNamespace("rgdal")
      }
   }
   else
   {
      if (!isLoaded) {
         if (!requireNamespace("proj4",quietly=.isPackageInUse()))
            requireNamespace("rgdal",quietly=.isPackageInUse())
      }
      proj4 <- paste(proj,collapse=" ")
   }
   if ((!isLongLat)&&(!isMerc)) {
      lat0 <- .gsub("^.*\\+lat_[012]=(\\S+)\\s.*$","\\1",proj4)
      if (lat0==proj4) {
        # lat0 <- .gsub("^.*\\+lat_ts=(\\S+)\\s.*$","\\1",proj4)
         if (lat0==proj4) {
            epsg <- .gsub("^.+init=epsg:(\\d+).*$","\\1",proj4)
            if ((epsg==proj4))
               lat0 <- NA
            else {
               epsg <- as.integer(epsg)
               if (epsg %in% c(3411,3413,3408,3571:3576,6931,6973))
                  lat0 <- 90
               else if (epsg %in% c(3409,6932,6974,3412,3976))
                  lat0 <- -90
               else
                  lat0 <- NA
            }
         }
         else
            lat0 <- as.numeric(lat0)
      }
      else
         lat0 <- as.numeric(lat0)
   }
   else
      lat0 <- NA
   ant_xy <- NULL
   ind <- attr(xy,"antarctic")
   if (!is.null(ind))
      indS <- which(abs(coast_xy[ind,2])<(85.0-1e-3))
   if (!isLongLat) {
     # indNA <- which(is.na(coast_xy[,1]) | is.na(coast_xy[,2]))
      coast_xy <- .project(coast_xy,proj4)
      isInf <- any(is.infinite(coast_xy))
      if (isInf) {
         shadow <- unname(col2rgb(fill,alpha=TRUE)[4,1]) # if (shadow!=0)
        # coast_xy[indNA,] <- NA
         indI <- which(is.infinite(coast_xy[,1]) | is.infinite(coast_xy[,2]))
         coast_xy[indI,] <- NA
         if (dev <- TRUE) {
            ind <- NULL ## drop Antarctic
           # coast_xy <- coast_xy[500:600,]
            ind1 <- as.integer(!is.na(coast_xy[,1]) & !is.na(coast_xy[,2]))
           # print(c(ind=ind))
            ind2 <- cumsum(ind1)
           # print(ind2)
            ind3 <- which(duplicated(ind2))
           # print(ind3)
            ind4 <- diff(ind3)
           # print(ind4)
            ind5 <- which(ind4==1)
           # print(ind5)
            ind6 <- ind3[ind5]
            coast_xy <- coast_xy[-ind6,]
            if (dev2 <- TRUE) {
               if (anyNA(coast_xy[nrow(coast_xy),]))
                  coast_xy <- head(coast_xy[,1:2],-1)
               ind7 <- which(is.na(coast_xy[,1]) | is.na(coast_xy[,2]))
               ind7a <- c(1,ind7+1L)
               ind7b <- c(ind7-1L,nrow(coast_xy))
               ind8 <- NULL
               for (i in sample(seq_along(ind7a))) {
                  ind9 <- ind7a[i]:ind7b[i]
                  xy <- coast_xy[ind9,,drop=FALSE]
                  if (nrow(xy)==1)
                     ind8 <- c(ind8,c(ind9,tail(ind9,1)+1L))
                 # print(all(abs(head(xy,1)-tail(xy,1))<1e-11))
               }
               if (length(ind8))
                 coast_xy <- coast_xy[-ind8,]
            }
           # print(coast_xy)
           ## for detail=="l": [7720,] [8897,]
           # q()
         }
         else {
            res <- list()
            class(res) <- "ursaCoastLine"
            options(ursaPngCoastLine=res)
            return(res)
         }
      }
   }
   if (!is.null(ind)) {
      if ((is.na(lat0))||(lat0<=0)) {
         ant_xy <- coast_xy[ind,]
         if (length(indS))
            ant_xy <- ant_xy[indS,]
      }
      ind <- c(head(ind,1)-1,ind)
      coast_xy <- coast_xy[-ind,]
   }
   isMerc <- isMerc | isCea
   if (is.na(fail180))
      fail180 <- (isMerc || isLongLat)
   if ((fail180)||(isLongLat || isMerc)) {
      if (!isLongLat) {
         lon0 <- as.numeric(.gsub(".*\\+lon_0=(\\S+)\\s*.*","\\1",proj4))
         B <- mean(abs(.project(rbind(cbind(lon0-180+1e-9,-45),cbind(lon0+180-1e-9,+45))
                               ,proj4)[,1]))
      }
      else
         B <- 180
      if (isMerc) {
        # B <- .getMajorSemiAxis(proj4)*pi
        # B <- 7720000
         '.shift' <- function(seg) {
           # if (all(seg[,2]>0)) ## debug Chukotka vs 
           #    return(NULL)
            j <- which(abs(diff(seg[,1]))>B)
            if (!length(j))
               return(NULL)
           # plot(seg[,1],seg[,2],type="l")
           # abline(v=c(-B,B),lty=2)
            center <- sign(mean(seg[,1]))
            j1 <- c(1,j+1)
            j2 <- c(j,nrow(seg))
            if (center<0)
               k <- which(seg[j1,1]>0.9*B)
            else
               k <- which(seg[j1,1]<=(-0.9*B))
           # da <- data.frame(j1=j1,j2=j2,center=center,s=0,seg=seg[j1,1])
           # da2 <- apply(da,1,function(x) range(seg[x["j1"]:x["j2"],1]))
           # da$s <- -sign(da$seg)
           # da$min <- da2[1,]
           # da$max <- da2[2,]
           # print(da)
           # print(da[k,])
            if (TRUE) { ## added 20180207
               nr <- length(j1)
               if ((1 %in% k)&&(!(nr %in% k)))
                  k <- c(k,nr)
               else if ((nr %in% k)&&(!(1 %in% k)))
                  k <- c(1,k)
            }
            j1k <- j1[k]
            j2k <- j2[k]
           # print(data.frame(j1=j1,j2=j2,center=center,seg=seg[j1,1]))
            if (center<0) { 
               for (m in seq_along(j1k))
                  seg[j1k[m]:j2k[m],1] <- seg[j1k[m]:j2k[m],1]-2*B
            }
            else {
               for (m in seq_along(j1k))
                  seg[j1k[m]:j2k[m],1] <- seg[j1k[m]:j2k[m],1]+2*B
            }
           # for (m in c(1))
           #    seg[j1[m]:j2[m],1] <- seg[j1[m]:j2[m],1]-2*B
           # da <- data.frame(j1=j1,j2=j2,center=center,seg=seg[j1,1])
           # da2 <- apply(da,1,function(x) range(seg[x["j1"]:x["j2"],1]))
           # da$min2 <- da2[1,]
           # da$max2 <- da2[2,]
           # print(da)
           # print(summary(seg))
           # plot(seg[,1],seg[,2],type="l")
           # abline(v=c(-B,B),lty=2)
            seg
         }
         if ((TRUE)||(g1$minx<(-B))||(g1$maxx>(+B))) {
            ind <- which(is.na(coast_xy[,1]))
            ind1 <- c(1,ind+1)
            ind2 <- c(ind-1,nrow(coast_xy))
            for (i in seq(length(ind1))) {
               if (ind1[i]>ind2[i])
                  next
              # if (nrow(coast_xy[ind1[i]:ind2[i],])<1e3) ## debug
              #    next
               seg <- .shift(coast_xy[ind1[i]:ind2[i],])
               if (is.null(seg))
                  next
              # str(seg)
              # if (nrow(seg)<1e3)
              #    next
              # message("-----")
              # str(seg)
              # print(summary(coast_xy[ind1[i]:ind2[i],]))
              # print(summary(seg))
              # message("=====")
              # if (nrow(seg)<1e3)
              #    next
              # print(c(i=i,ind1=ind1[i],ind2=ind2[i]))
               coast_xy[ind1[i]:ind2[i],] <- seg
            }
            if (!is.null(ant_xy)) {
               seg <- .shift(ant_xy)
               if (!is.null(seg))
                  ant_xy <- seg
            }
         }
      }
      cond1 <- g1$minx<(-B*(169.2/180)) ## east boarder of Eurasia
      cond2 <- g1$maxx>(+B)
      if (cond1) {
         if (verbose)
            print("expand to the West")
         opp1 <- coast_xy
         opp1[,1] <- opp1[,1]-2*B
         if (!is.null(ant_xy)) {
            if (FALSE) {
               opp1a1 <- opp1a2 <- ant_xy[-nrow(ant_xy),]
               opp1a1[,1] <- opp1a1[,1]-4*B
               opp1a2[,1] <- opp1a2[,1]-2*B
               opp1a <- rbind(opp1a1,c(NA,NA),opp1a2)
               rm(opp1a1,opp1a2)
            }
            else {
               opp1a <- ant_xy[-1,]
               opp1a[,1] <- opp1a[,1]-2*B
            }
         }
      }
      if (cond2) {
         if (verbose)
            print("expand the East")
         opp2 <- coast_xy
         opp2[,1] <- opp2[,1]+2*B
         if (!is.null(ant_xy)) {
            opp2a <- ant_xy[-1,]
            opp2a[,1] <- opp2a[,1]+2*B
         }
      }
      if (cond1) {
         coast_xy <- rbind(coast_xy,c(NA,NA),opp1)
         if (!is.null(ant_xy))
            ant_xy <- rbind(opp1a,ant_xy)
      }
      if (cond2) {
         coast_xy <- rbind(coast_xy,c(NA,NA),opp2)
         if (!is.null(ant_xy))
            ant_xy <- rbind(ant_xy,opp2a)
      }
   }
   if (any(is.na(coast_xy[1,])))
      coast_xy <- coast_xy[-1,]
   n <- nrow(coast_xy)
   if (any(is.na(coast_xy[n,])))
      coast_xy <- coast_xy[-n,]
   if (!is.null(ant_xy)) {
      if ((isLongLat)||(isMerc)) {
         ant1 <- ant_xy[1,]
         ant2 <- ant_xy[nrow(ant_xy),]
         ant1[2] <- g1$miny-g1$resy
         ant2[2] <- ant1[2]
         ant_xy <- rbind(ant1,ant_xy,ant2,ant1)
         rownames(ant_xy) <- NULL
         if (FALSE) { ## non-reproducible code for non-author 
            plot(0,0,xlim=range(c(coast_xy[,1],ant_xy[,1]),na.rm=TRUE),
                    ,ylim=range(c(coast_xy[,2],ant_xy[,2]),na.rm=TRUE),type="n")
            polypath(coast_xy[,1],coast_xy[,2],col="red")
            polypath(ant_xy[,1],ant_xy[,2],col="green")
            stop("")
         }
      }
      coast_xy <- rbind(coast_xy[,1:2],c(NA,NA),ant_xy[,1:2])
   }
   if (!isDetail) {
      inside <- with(g1,coast_xy[,1]>=minx & coast_xy[,1]<=maxx &
                            coast_xy[,2]>=miny & coast_xy[,2]<=maxy)
      inside <- any(na.omit(unique(inside)))
      if (inside) {
         area <- with(g1,max(maxx-minx,maxy-miny))
         if (isLongLat)
            area <- area*111
         else
            area <- area/1000
         if (area<=(-10))
            return(NULL)
         else if (area<=100)
            detail <- "f"
         else if (area<=500)
            detail <- "h"
         else if (area<=3000)
            detail <- "i"
         else
            detail <- "l"
         if (detail!="l") {
            fname <- file.path(fpath,paste0("coast-",detail,".rds"))
            if (file.exists(fname)) {
               if (FALSE) {
                  arglist <- list(...)
                  ind <- .grep("detail",names(arglist))
                  if (length(ind))
                     arglist[[ind]] <- detail
                  else
                     arglist$detail <- detail
                  return(do.call("compose_coastline",arglist))
               }
               else {
                  arglist <- as.list(match.call())
                  arglist$detail <- detail
                  return(do.call(as.character(arglist[[1]]),arglist[-1]))
               }
            }
         }
      }
   }
  # ind <- which(coast_xy[,2]<(-68))
  # print(summary(coast_xy[ind,1]))
   shadow <- unname(col2rgb(fill,alpha=TRUE)[4,1])
   options(ursaPngShadow=ifelse(shadow %in% c(0,255),"",fill))
   res <- list(coast_xy=coast_xy,panel=panel,col=col,fill=fill,shadow=shadow
              ,land=land,density=density,angle=angle,lwd=lwd,lty=lty)
   class(res) <- "ursaCoastLine"
   options(ursaPngCoastLine=res)
   res
}
'panel_coastline' <- function(...) {
   if (.skipPlot(TRUE))
      return(NULL)
   arglist <- list(...)
   kwd <- "coast(line)*"
   isWeb <- getOption("ursaPngWebCartography")
   if (!is.logical(isWeb))
      isWeb <- FALSE
   coastline <- .getPrm(arglist,name=paste0("^(",kwd,"|decor)$")
                       ,class=list("integer","logical","ursaCoastLine")[1:2]
                       ,default=TRUE,verbose=FALSE)
   ##~ decor <- .getPrm(arglist,name="^decor$"
                   ##~ ,class=list("integer","logical","ursaCoastLine")[1:2]
                   ##~ ,default=TRUE,verbose=FALSE)
   ##~ coastline <- .getPrm(arglist,name=paste0("^",kwd,"$")
                       ##~ ,class=list("integer","logical","ursaCoastLine")[1:2]
                       ##~ ,default=,verbose=FALSE)
   ##~ str(arglist)
   ##~ print(c(coast=coastline,web=isWeb))
   ##~ q()
   ##~ if (inherits(coastline,"ursaCoastLine")) {
      ##~ obj <- coastline
      ##~ coastline <- TRUE
      ##~ isFound <- TRUE
   ##~ }
   ##~ else
      ##~ isFound <- FALSE
   if (!coastline)
      return(NULL)
  # if (!isFound)
   obj <- .getPrm(arglist,class="ursaCoastLine",default=NULL)
   figure <- getOption("ursaPngFigure")
   if ((!is.logical(coastline))&&(figure!=coastline))
      return(NULL)
   if (is.null(obj)) {
      obj <- getOption("ursaPngCoastLine")
      if (is.null(obj))
         obj <- compose_coastline(...)
   }
   if ((any(obj$panel))&&(!(figure %in% obj$panel)))
      return(NULL)
   if (is.null(obj$coast_xy))
      return(NULL)
   if (!FALSE) {
      obj$col <- .getPrm(arglist,name="col",kwd=kwd,default=obj$col)
      obj$fill <- .getPrm(arglist,name="fill",kwd=kwd,default=obj$fill)
      obj$density <- .getPrm(arglist,name="density",kwd=kwd,default=obj$density)
      obj$angle <- .getPrm(arglist,name="angle",kwd=kwd,default=obj$angle)
      obj$land <- .getPrm(arglist,name="land",kwd=kwd,default=obj$land)
      obj$lwd <- .getPrm(arglist,name="lwd",kwd=kwd,default=obj$lwd)
      obj$lty <- .getPrm(arglist,name="lty",kwd=kwd,default=obj$lty)
   }
   verbose <- .getPrm(arglist,name="verbose",kwd=kwd,default=FALSE)
   .panel_coastline(obj,verbose=verbose)
}
'.panel_coastline' <- function(obj,verbose=FALSE) {
   with(obj,{
      shadow <- unname(col2rgb(fill,alpha=TRUE)[4,1])
      if (verbose)
         str(list(col=col,fill=fill,shadow=shadow#,detail=detail
                 ,density=density,angle=angle,land=land,lwd=lwd,lty=lty))
      if ((TRUE)&&(shadow==0)|| ## 20171214 changed 'shadow!=255'
          ((!is.na(angle[1]))&&(!is.na(density[1])))) ## more quick
      {
        # op <- par(usr=par()$usr-c(0,125000,0,125000))
        # print(par()$usr)
         if ((is.na(angle[1]))||(is.na(density[1]))) {
            if (inherits(coast_xy,"SpatialPolygonsDataFrame"))
               plot(coast_xy,border=col,col=fill,lwd=lwd,add=TRUE)
            else {
               polygon(coast_xy[,1],coast_xy[,2],border=col,col=fill,lwd=lwd)
            }
         }
         else {
            for (an in angle) {
              # print(str(list(angle=an,border=col,col=fill,density=density,lwd=lwd)))
               polygon(coast_xy[,1],coast_xy[,2],border=col,col=fill
                      ,density=density,angle=an,lwd=lwd)
            }
         }
        # par(op)
      }
      else
      {
         if (inherits(coast_xy,"SpatialPolygonsDataFrame"))
            plot(coast_xy,border=col,col=fill,lwd=lwd
                ,usePolypath=TRUE,rule=c("winding","evenodd")[2],add=TRUE)
         else if (!all(is.na(c(coast_xy)))) {
            if (land)
            {
               g1 <- session_grid()
               x <- with(g1,c(minx,minx,maxx,maxx,minx)+c(-1,-1,1,1,-1)*resx)
               y <- with(g1,c(miny,maxy,maxy,miny,miny)+c(-1,1,1,-1,-1)*resy)
               coast_xy <- rbind(cbind(x,y),c(NA,NA),coast_xy)
            }
           ## ?polypath: Hatched shading (as implemented for polygon()) is not (currently) supported.
           ## if semi-opacity|trasparency them 'polygon' else fill is transparent
            polypath(coast_xy[,1],coast_xy[,2],border=col,col=fill
                    ,rule=c("winding","evenodd")[2],lwd=lwd) ##,density=15??
         }
      }
   })
   invisible(NULL)
}

'update_coastline' <- function(merge=TRUE) {
   if (!requireNamespace("sf",quietly=TRUE))
      stop("Suggested package 'sf' is required for this operation")
   dpath <- getOption("ursaRequisite")
   ftemp <- tempfile(tmpdir=dpath)
   opW <- options(warn=-1)
   res <- .try(writeLines("1",ftemp))
   options(opW)
   if (res)
      file.remove(ftemp)
   else
      stop("Unable to update 'requisite' directory for package")
   verbose <- !.isPackageInUse()
   if (!verbose)
      merge <- TRUE
   if (FALSE) {
      toUnloadMethods <- !("methods" %in% .loaded())
      .require("methods",quietly=!verbose)
   }
   else
      toUnloadMethods <- FALSE
  # src <- "http://data.openstreetmapdata.com/simplified-land-polygons-complete-3857.zip" ## (depredated)
   src <- "https://osmdata.openstreetmap.de/download/simplified-land-polygons-complete-3857.zip"
   dst <- .ursaCacheDownload(src,mode="wb",quiet=FALSE)
   list1 <- unzip(dst,exdir=tempdir())
   a <- sf::st_read(list1[.grep("\\.shp$",basename(list1))],quiet=TRUE)
   file.remove(list1)
   if (verbose)
      n <- nrow(a)
   aG <- sf::st_geometry(a)
   sf::st_geometry(a) <- NULL
   a <- sf::st_sf(data.frame(FID=as.integer(a$FID)),geom=aG) ## set my name for geometry: 'geom'
   .elapsedTime("reading - done")
   FID <- a$FID
   prj1 <- spatial_crs(a)
   lon0 <- 120
   prj1 <- spatial_proj4(a)
   prj2 <- .gsub("(^.+)(\\s\\+lon_0=)(\\d+(\\.\\d+)*)(\\s.+$)"
                ,paste0("\\1\\2",lon0,"\\5"),prj1)
   if (merge) {
      aG <- sf::st_transform(aG,crs=4326)
      cross180 <- NULL
      if (verbose)
         pb <- ursaProgressBar(min=0,max=n)
      for (i in sample(seq_along(FID))) {
         fid <- FID[i]
         bG <- lapply(aG[[i]],function(x) which(abs(x[,1])>180-1e-11))
         if (verbose)
            setUrsaProgressBar(pb)
         if (!sum(sapply(bG,length)))
            next
         cross180 <- c(cross180,fid)
      }
      if (verbose)
         close(pb)
      a180 <- subset(a,FID %in% cross180)
      sf::st_agr(a180) <- "constant"
      c180 <- sf::st_transform(sf::st_centroid(a180),crs=4326)
      g180 <- do.call("rbind",lapply(lapply(sf::st_geometry(c180),unclass)
                                    ,matrix,ncol=2))
      sf::st_geometry(c180) <- NULL
      c180 <- data.frame(c180,lon=g180[,1],lat=g180[,2])
      if (verbose)
         print(c180)
      p1 <- c180$FID[c180$lat>52.0 & c180$lat<67.0] # Chukotka
      p2 <- c180$FID[c180$lat>71.0 & c180$lat<71.5] # Wrangel
      p3 <- c180$FID[c180$lat>(-17.0) & c180$lat<(-16.7)] # Taveuni (south)
      p4 <- c180$FID[c180$lat>(-16.55) & c180$lat<(-16.45)] # Rabi (center)
      p5 <- c180$FID[c180$lat>(-16.65) & c180$lat<(-16.55) |
                     c180$lat>(-16.3) & c180$lat<(-16.0)] # Labasa (top MANUAL)
      pair <- list(Chukotka=p1,Wrangel=p2,Taveuni_S=p3,Rabi_M=p4,Labasa_N=p5)
      if (verbose) {
         print(pair)
         if (FALSE) {
            a180 <- subset(a180,FID %in% c180$FID[abs(c180$lat)<=20])
            if (FALSE)
            spatial_write(sf::st_transform(a180,prj2),"cross180.sqlite")
         }
      }
      a180 <- vector("list",length(pair))
      for (i in seq_along(pair)) {
         fid5 <- pair[[i]]
         a5 <- subset(a,FID %in% fid5)
         c5 <- subset(c180,FID %in% fid5)
         a5 <- sf::st_transform(a5,crs=prj2)
         if (i==5) {
            ind <- c5$lon<0
            closest <- fid5[which(ind)]
            expanded <- fid5[which(!ind)]
            pt <- sf::st_sfc(sf::st_point(c(179.99993,-16.16841)),crs=4326)
            pt <- unclass(unclass(sf::st_transform(pt,crs=prj2))[[1]])
            a5b <- subset(a5,FID==closest)
            g5b <- unclass(unclass(sf::st_geometry(a5b))[[1]])[[1]]
            db <- (abs(g5b[,1])-pt[1])^2+(g5b[,2]-pt[2])^2
            indb <- which.min(db)
            pt <- g5b[indb,]
            a5a <- subset(a5,FID==expanded)
            g5a <- unclass(unclass(sf::st_geometry(a5a))[[1]])[[1]]
            da <- (abs(g5a[,1])-pt[1])^2+(g5a[,2]-pt[2])^2
            inda <- which.min(da)
            if (FALSE)
               g5a <- rbind(g5a[1:inda,],pt,g5a[(inda+1):nrow(g5a),])
            else
               g5a <- rbind(g5a[1:(inda-1),],pt,g5a[inda:nrow(g5a),])
            sf::st_geometry(a5a) <- sf::st_sfc(sf::st_polygon(list(g5a)),crs=prj2)
            a5 <- rbind(a5a,a5b)
         }
         a5 <- sf::st_sf(FID=65000L+i,geom=sf::st_union(a5))
         a180[[i]] <- sf::st_transform(a5,prj1)
      }
      .elapsedTime("spliting - done")
      a <- list(subset(a,!(FID %in% unlist(pair))))
      a <- do.call("rbind",c(a,a180))
      .elapsedTime("merging - done")
   }
   pArea <- sf::st_area(sf::st_transform(a,crs=4326))
  # pArea <- as.numeric(units::set_units(pArea,km^2))
   u <- attr(pArea,"units")$numerator
   m <- rep(1,length(u))
   for (i in seq_along(u)) {
      if (u[i]=="m")
         m[i] <- 1/1000
      else
         stop(u[i])
   }
   pArea <- as.numeric(pArea)*prod(m)
   .elapsedTime("area calculation - done")
   res <- sapply(c("l","i","h","f"),function(x) .update_coastline(a,pArea,merge,x))
   if ((toUnloadMethods)&&("package:methods" %in% search())) {
      detach("package:methods",unload=FALSE) 
   }
   res
}
'.update_coastline' <- function(a,pArea,merge,detail=c("l","i","h","f")) {
   detail <- match.arg(detail)
   verbose <- !.isPackageInUse()
   thL <- switch(detail,l=12,i=3.5,h=1,f=0,stop("unable set length"))
   thA <- (0.5*thL)^2
   if (thA>0) {
      ind <- which(pArea<thA)
      if (length(ind))
         a <- a[-ind,]
   }
   if (thL>0) {
      a <- sf::st_simplify(a,dTolerance=thL*1e3,preserveTopology=TRUE)
   }
   .elapsedTime(paste0(detail,": simplifying - done"))
   a <- sf::st_transform(a,crs=4326)
   g1 <- sf::st_geometry(a)
   k <- 0
   for (i in seq_along(g1)) {
      g2 <- g1[[i]]
      for (j in seq_along(g2)) {
         g3 <- g2[[j]]
         if (!is.list(g3))
            g3 <- list(g3)
         for (j in seq_along(g3)) {
            k <- k+1L
         }
      }
   }
   xy <- vector("list",k)
   k <- 0
   for (i in seq_along(g1)) {
      g2 <- g1[[i]]
      for (j in seq_along(g2)) {
         g3 <- g2[[j]]
         if (!is.list(g3))
            g3 <- list(g3)
         for (m in seq_along(g3)) {
            g4 <- g3[[m]]
            isATA <- try(as.integer(any(g4[,2]<(-84.9))))
            if (isATA) {
               a$FID[i] <- -abs(a$FID[i])
               if (verbose)
                  print(c('Antarctida FID'=a$FID[i]))  # Antarctida 43705
            }
            k <- k+1
            if (isATA) {
               indX <- which(abs(g4[,1])>180-1e-3)
               indY <- which(abs(g4[,2])>85-1e-3)
               ind2 <- na.omit(match(indY,indX))
               if (length(ind2)) {
                  ind2 <- ind2[1L]-1L
                  g4 <- head(g4,-1L)
                  g4 <- unname(rbind(tail(g4,-ind2),head(g4,ind2)))
                  g4 <- rbind(g4,head(g4,1L))
               }
            }
            else if (any(g4[,1]>(+175)) && any(g4[,1]<(-175))) {
               ind3 <- which(g4[,1]>=(-180) & g4[,1]<=(-120))
               g4[ind3,1] <- g4[ind3,1]+360
            }
            xy[[k]] <- rbind(cbind(g4,isATA),c(NA,NA,NA))
         }
      }
   }
   ind <- which(sapply(xy,function(x) is.null(x)))
   if (length(ind))
      xy <- xy[-ind]
   xy <- do.call("rbind",lapply(xy,I))
   xy <- xy[-nrow(xy),]
   indA <- which(xy[,3]>0)
   colnames(xy) <- c("lon","lat","spole")
   if (TRUE)
      xy <- xy[,1:2]
   if (length(indA))
      attr(xy,"antarctic") <- indA
   indP <- which(xy[,2]<(-84.99))
   if (length(indP))
      attr(xy,"south_pole") <- indP
   .elapsedTime(paste0(detail,": coercing - done"))
   if (merge)
      saveRDS(xy,file.path(getOption("ursaRequisite"),paste0("coast-",detail,".rds"))
             ,version=2) ## Such files are only readable in R >= 3.5.0.
   if (!.isPackageInUse())
      spatial_write(a,paste0(paste0("coast-",detail,ifelse(merge,"","180")),".sqlite"))
   0L
}
