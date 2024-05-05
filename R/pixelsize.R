'pixelsize' <- function(obj,verbose=FALSE) {
   if (missing(obj))
      g1 <- session_grid()
   else if (is.ursa(obj))
      g1 <- ursa_grid(obj)
   else if (.is.grid(obj))
      g1 <- obj
   else if (is.null(obj)) {
      session_grid(NULL)
      g1 <- session_grid()
   }
   projClass <- .crsProj(g1$crs)
   if (projClass=="stere")
      return(.pxlsize.stere(g1,verbose=verbose))
   if (projClass=="merc")
      return(.pxlsize.merc(g1,verbose=verbose))
   mul <- if (with(g1,resx*resy)<1e5) c("sq.m"=1) else c("sq.km"=1e-6)
   ursa_new(value=with(g1,resx*resy*mul)
            ,bandname=paste0("Pixel Size (",names(mul),")"),ignorevalue=-99L)
}
'.pxlsize.merc' <- function(g,verbose=FALSE) {
   mul <- if (with(g,resx*resy)<1e5) c("sq.m"=1) else c("sq.km"=1e-6)
   a <- ursa_new(value=0,bandname=paste0("Pixel Size (",names(mul),")"))
   da <- as.data.frame(a)
   lat <- .project(with(da,cbind(x,y)),g$crs,inv=TRUE)[,2]
  # ursa_value(a) <- with(g,resx*resy*mul/cos(lat*pi/180))
   a$value[] <- with(g,resx*resy*mul/cos(lat*pi/180))
   a
}
'.pxlsize.stere' <- function(g,verbose=FALSE) {
  # https://en.wikibooks.org/wiki/PROJ.4
   '.pow' <- function(x,y) x^y
   proj4 <- .proj4string(g$crs) ## 'sf' is loaded here
   if (FALSE)
      semi <- c("6378137","6356752.3") ## low-precision
   if (.lgrep("\\+datum=WGS84",proj4))
      semi <- c("6378137","6356752.314245179")
   else if (.lgrep("\\+ellps=WGS84",proj4))
      semi <- c("6378137","6356752.314245179")
   else if (.lgrep("\\+ellps=krass",proj4))
      semi <- c("6378245","6356863.018773047")
   else {
      semi <- c(.gsub2("\\s\\+a=(\\S+)\\s","\\1",proj4)
               ,.gsub2("\\s\\+b=(\\S+)\\s","\\1",proj4)
               ,.gsub2("\\s\\+rf=(\\S+)\\s","\\1",proj4)
               )
      semi <- semi[nchar(semi)<nchar(proj4)]
   }
   semi <- as.numeric(semi)
   lat_ts <- as.numeric(.gsub2("\\s\\+lat_ts=(\\S+)\\s","\\1",proj4))
   lat0 <- as.numeric(.gsub2("\\s\\+lat_0=(\\S+)\\s","\\1",proj4))
   lon0 <- as.numeric(.gsub2("\\s\\+lon_0=(\\S+)\\s","\\1",proj4))
   x0 <- as.numeric(.gsub2("\\s\\+x_0=(\\S+)\\s","\\1",proj4))
   y0 <- as.numeric(.gsub2("\\s\\+y_0=(\\S+)\\s","\\1",proj4))
   if (.lgrep("\\s\\+k=(\\S+)\\s",proj4))
      scale <- as.numeric(.gsub2("\\s\\+k=(\\S+)\\s","\\1",proj4))
   else
      scale <- 1
   major <- max(semi)
   minor <- min(semi)
   if (minor<6300000)
      minor <- major*(1-1/minor) ## +rf=298.278...
   ecentr2 <- 1-minor*minor/(major*major)
   ecentr <- sqrt(ecentr2)
   if (verbose)
       print(c(lat0=lat0,lat_ts=lat_ts,lon0=lon0,k=scale,x0=x0,y0=y0
              ,a=major,b=minor),digits=16)
   xlon <- 0
   ylat <- 90
   xlon <- pi*xlon/180.0
   ylat <- pi*ylat/180.0
   lat_ts <- (90-lat_ts)*pi/180
   fi0 <- pi*lat0/180
   sinfi0 <- sin(fi0)
   cosfi0 <- cos(fi0)
   lamba0 <- pi*lon0/180.0
   sinlamba0 <- sin(lamba0)
   coslamba0 <- cos(lamba0)
   if (lat_ts==0.0)
      perimper4 <- 2*scale*major*major/minor*
                             .pow((1-ecentr)/(1+ecentr),ecentr/2)
   else {
      Rk <- major*sin(lat_ts)/sqrt(1-ecentr2*cos(lat_ts)*cos(lat_ts))
      Uk <- ((1+cos(lat_ts))/sin(lat_ts))*
         .pow((1-ecentr*cos(lat_ts))/(1+ecentr*cos(lat_ts)),ecentr/2)
      perimper4=scale*Rk*Uk;
   }
   fi1 <- acos(sin(ylat)*sinfi0+cos(ylat)*cosfi0*cos(xlon-lamba0))
   d1 <- cos(ylat)*sin(xlon-lamba0)
   d2 <- sin(ylat)*cosfi0-sinfi0*cos(ylat)*cos(xlon-lamba0)
   if (d2==0.0)
      lamba1 <- 0.0
   else if (d2>0.0)
      lamba1 <- atan(d1/d2)
   else if (d2<0.0)
      lamba1 <- atan(d1/d2)+pi
   dist <- perimper4*(sin(fi1)/(1+cos(fi1)))*
      .pow((1+ecentr*cos(fi1))/(1-ecentr*cos(fi1)),ecentr/2)
   xshift <- -dist*scale*sin(lamba1)+x0
   yshift <- -dist*scale*cos(lamba1)+y0
   mul <- if (with(g,resx*resy)<1e5) c("sq.m"=1) else c("sq.km"=1e-6)
   ps <- ursa_new(value=0,bandname=paste0("Pixel Size (",names(mul),")")
                 ,ignorevalue=-99L)
   da <- as.data.frame(ps)
   x1 <- da$x+xshift
   y1 <- da$y+yshift
   lamba1 <- rep(0,length(y1))
   if (length(ind <- which(y1>0)))
      lamba1[ind] <- atan(x1[ind]/y1[ind])
   if (length(ind <- which(y1<0)))
      lamba1[ind] <- atan(x1[ind]/y1[ind])+pi
   fi1 <- rep(NA,length(lamba1))
   if (length(ind <- which(sin(lamba1)==0)))
      fi1[ind] <- 2*atan(y1[ind]/(perimper4*cos(lamba1[ind])));
   if (length(ind <- which(sin(lamba1)!=0)))
      fi1[ind] <- 2*atan(x1[ind]/(perimper4*sin(lamba1[ind])));
   val <- (1.0/scale)*(1+cos(fi1))/(1+cos(lat_ts))*
       .pow((1+ecentr*cos(lat_ts))/(1-ecentr*cos(lat_ts)),ecentr/2)*
       .pow((1-ecentr*cos(fi1))/(1+ecentr*cos(fi1)),ecentr/2);
  # ursa_value(ps) <- val*val*with(g,resx*resy)*mul
   ps$value[] <- val*val*with(g,resx*resy)*mul
   ps
}
