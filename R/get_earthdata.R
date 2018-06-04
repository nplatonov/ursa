# https://earthdata.nasa.gov/about/science-system-description/eosdis-components/global-imagery-browse-services-gibs#ed-gibs-citation

'get_earthdata' <- function(bbox=NA #,c(2000000,400000,2300000,700000)
                           ,res=c("2km","1km","500m","250m")
                           ,date=NA,product="",geocode=""
                           ,expand=1.05,border=0,display=FALSE,verbose=FALSE) {
   productList <- c('1'="MODIS_Aqua_CorrectedReflectance_Bands721"
                   ,'2'="MODIS_Terra_CorrectedReflectance_Bands721"
                   ,'3'="MODIS_Aqua_CorrectedReflectance_TrueColor"
                   ,'4'="MODIS_Terra_CorrectedReflectance_TrueColor"
                   ,'5'="VIIRS_SNPP_CorrectedReflectance_TrueColor"
                   ,'6'="Coastlines")
   epsg3413 <- paste("","+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1"
                 ,"+x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
   epsg3857 <- paste("","+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0"
                    ,"+x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null"
                    ,"+wktext +no_defs")
   if ((length(bbox)==1)&&(is.na(bbox))) {
      g0 <- session_grid()
      if (.lgrep("\\+proj=merc",g0$proj4)) {
        # stop("A")
         ll <- with(g0,.project(cbind(c(minx,maxx),c(miny,maxy)),proj4,inv=TRUE))
         bbox <- c(min(ll[,1]),min(ll[,2]),max(ll[,1]),max(ll[,2]))
      }
      else if (.gsub("(^\\s|\\s$)","\\1",g0$proj4)==.gsub("(^\\s|\\s$)","\\1",epsg3413)) {
        # stop("B")
         bbox <- with(g0,c(minx,miny,maxx,maxy))
      }
      else {
        # stop("C")
         bbox <- NULL
      }
   }
   if (is.null(bbox))
      return(productList)
   g4 <- session_grid()
   if (product %in% seq_along(productList))
      product <- productList[product]
   else {
      pr2 <- try(match.arg(product,productList),silent=TRUE)
      if (inherits(pr2,"try-error")) {
         isAqua <- length(grep("aqua",product,ignore.case=TRUE))>0
         isTerra <- length(grep("terra",product,ignore.case=TRUE))>0
         isSuomi <- length(grep("(snpp|suomi)",product,ignore.case=TRUE))>0
         is721 <- length(grep("721",product,ignore.case=TRUE))>0
         isTrueColor <- length(grep("(true|color)",product,ignore.case=TRUE))>0
        # print(c(aqua=isAqua,terra=isTerra,suomi=isSuomi,b721=is721,TT=isTrueColor))
         if ((isAqua)&&(is721))
            product <- productList[1]
         else if ((isTerra)&&(is721))
            product <- productList[2]
         else if (isAqua) #((isAqua)&&(isTrueColor))
            product <- productList[3]
         else if (isTerra) #((isTerra)&&(isTrueColor))
            product <- productList[4]
         else if (isSuomi)
            product <- productList[5]
         else
            product <- productList[6]
      }
      else
         product <- pr2
   }
   if (is.na(date)) {
      t0 <- as.integer(format(as.POSIXlt(Sys.time(),tz="UTC"),"%H"))
      date <- Sys.Date()-ifelse(t0>17.5,0L,1L)
   }
   if (is.character(date)) {
      if (.lgrep("today",date))
         date <- Sys.Date()
      else if (.lgrep("yesterday",date))
         date <- Sys.Date()-1L
      date <- as.Date(date)
   }
   if (is.character(bbox)) {
      g0 <- attr(spatialize(bbox,geocode=geocode,expand=expand,border=border
                          ,verbose=verbose),"grid")
      xy <- with(g0,cbind(c(minx,maxx),c(miny,maxy)))
      ll <- with(g0,.project(xy,proj4,inv=TRUE))
     # bbox <- c(min(ll[,1]),min(ll[,2]),max(ll[,1]),max(ll[,2]))
      bbox <- c(ll)[c(1,3,2,4)]
      if (length(bbox)!=4)
         return(productList)
   }
   epsg <- if ((any(bbox>(+360)))||(any(bbox<(-180)))) epsg3413 else epsg3857
   is3857 <- epsg==epsg3857
   is3413 <- epsg==epsg3413
  # print(c('3413'=is3413,'3857'=is3857))
   tsize <- ifelse(is3413,512,256)
   zoomList <- if (is3413) c(3:6) else c(1:9)-1
   B <- ifelse(is3413,4194304,20037508.34278925)
   cross180 <- FALSE
   if (is3857) {
      bbox <- c(.project(rbind(c(bbox[1],bbox[2]),c(bbox[3],bbox[4]))
                        ,epsg3857))[c(1,3,2,4)]
      if (bbox[1]>bbox[3]) {
         bbox[3] <- bbox[3]+2*B
         cross180 <- TRUE
      }
   }
   g0 <- regrid(ursa_grid(),setbound=c(-B,-B,B,B),dim=c(tsize,tsize),proj=epsg)
   if ((is.character(res))&&((length(res)==1))) {
      res <- match.arg(res)
      zoom <- switch(res,'2km'=3,'1km'=4,'500m'=5,'250m'=6,NA)
      g1 <- regrid(g0,mul=2^(zoom+0),bbox=bbox)
   }
   else if ((length(res)==1)&&(res %in% zoomList)) {
      zoom <- res
      g1 <- regrid(g0,mul=2^(zoom),bbox=bbox)
   }
   else {
      size <- if ((is.numeric(res))&&(res>128)) res else 480
     # print(g0)
      for (z in zoomList) {
         g1 <- regrid(g0,mul=2^(z+1),bbox=bbox)
         if ((g1$columns>size)||(g1$rows>size))
            break
      }
      zoom <- z+1
   }
   g2 <- regrid(ursa_grid(),setbound=c(-B,-B,B,B),dim=c(2^zoom,2^zoom)
               ,proj=ifelse(is3413,epsg3413,epsg3857))
   session_grid(g2)
   a <- ursa_new()
   cr <- coord_xy(a,x=c(g1$minx,g1$maxx-ifelse(cross180,2*B,0)),y=c(g1$maxy,g1$miny))
   xy <- coord_cr(a,c=cr[1,],r=cr[2,])
   if (cross180)
      xy[1,2] <- xy[1,2]+2*B
   cr <- cr-1
   g3 <- regrid(g1,bbox=c(xy[1,1]-1*g2$resx/2,xy[2,2]-1*g2$resy/2
                         ,xy[1,2]+1*g2$resx/2,xy[2,1]+1*g2$resy/2))
   if (is3413)
      zoom <- zoom-1
   if (cr[1,2]<cr[1,1])
      seqc <- c(cr[1,1]:(g2$columns-1),0:cr[1,2])
   else
      seqc <- cr[1,1]:cr[1,2]
   seqr <- cr[2,1]:cr[2,2]
   tile <- expand.grid(z=zoom,y=seqr,x=seqc,time=date)
   tile$time[which(tile$x<tile$x[1])] <- date-1L
   crind <- cr-apply(cr,1,min)
   tind <- expand.grid(z=zoom,r=seq_along(seqr)-1,c=seq_along(seqc)-1)
   img <- array(1,dim=c(tsize*length(seqc),tsize*length(seqr),4))
   a <- .getEarthdataTile(x=tile[,"x"],y=tile[,"y"],z=tile[,"z"]
                         ,epsg=ifelse(is3413,"3413","3857")
                         ,product=product,time=tile[,"time"]
                         ,verbose=verbose)
   for (i in seq(nrow(tile))) {
      if (is.null(a[[i]]))
         next
      img[tind[i,"c"]*tsize+seq(tsize)
         ,tind[i,"r"]*tsize+seq(tsize),seq(dim(a[[i]])[3])] <- a[[i]]
   }
   b <- as.ursa(img,flip=TRUE)
  # session_grid(b);display_rgb(b*255);q()
   ursa(b,"grid") <- g3
   b <- as.integer(regrid(b,g1,resample=0)*255)
   ursa(b,"nodata") <- NA
   if ((nband(b)==4)&&(global_min(b[4])==255)&&(global_max(b[4])==255))
      b <- b[-4]
   attr(b,"copyright") <- "Global Imagery Browse Services, NASA/GSFC/ESDIS"
   cond1 <- .lgrep("\\+proj=merc\\s",g1$proj4) & .lgrep("\\+proj=merc\\s",g4$proj4)
   cond2 <- g1$columns==g4$columns & g1$rows==g4$rows &
            .is.near(g1$miny,g4$miny) & .is.near(g1$maxy,g4$maxy) &
            .is.near(g1$resx,g4$resx) & .is.near(g1$resy,g4$resy) &
            TRUE
   if (cond1 & cond2)
      ursa(b,"grid") <- g4
   session_grid(b)
   if (!display)
      return(b)
   display_brick(b,scale=1,coast=TRUE)
}
'.getEarthdataTile' <- function(x=13,y=23,z=4,epsg=c("3413","3857")
                      ,product="MODIS_Aqua_CorrectedReflectance_Bands721"
                      ,time=Sys.Date()-2L
                      ,verbose=FALSE) {
   if (inherits(time,c("Date","POSIXct","POSIXlt")))
      time <- format(time,"%Y-%m-%d")
  # product <- c("Coastlines","arctic_coastlines")[1]
   isDecor <- product %in% c("Coastlines","arctic_coastline")
   if (isDecor)
      time <- ""
   ext <- ifelse(product %in% c("Coastlines","arctic_coastline"),".png",".jpg")
   epsg <- match.arg(epsg)
   matrixSet <- switch(epsg,'3413'="250m",'3857'="GoogleMapsCompatible_Level9")
   isPNG <- length(grep("png",ext))>0
   isJPG <- length(grep("jpg",ext))>0
   src <- file.path("https://gibs.earthdata.nasa.gov/wmts",paste0("epsg",epsg)
                   ,"best",product,"default",time,matrixSet,paste0(z,"/",y,"/",x,ext))
  # dst <- paste0("tmp-",z,"-",y,"-",x,ext)
  # dst <- sapply(seq_along(src),function(x) tempfile())
   method <- getOption("download.file.method")
   isBundle <-  ((!is.null(method))&&(method=="libcurl")&&(capabilities("libcurl")))
   if (isBundle) {
      download.file(src,dst,mode="wb",method="libcurl",quiet=!verbose)
   }
   a <- vector("list",length(src))
   for (i in seq_along(a)) {
      if (!isBundle) {
        # res <- try(download.file(src[i],dst[i],mode="wb",quiet=!verbose))
         dst <- try(.ursaCacheDownload(src[i],mode="wb",quiet=!verbose))
         if (inherits(dst,"try-error")) {
            print(dst)
            a[[i]] <- NULL
            next
         }
      }
      if (isPNG) {
         a[[i]] <- aperm(png::readPNG(dst),c(2,1,3))
      }
      else if (isJPG)
         a[[i]] <- aperm(jpeg::readJPEG(dst),c(2,1,3))
   }
  # file.remove(dst)
   a
}
