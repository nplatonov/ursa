## graphics::image
## graphics::filled.contour
## fields::image.plot
## lattice::levelplot
'plot.ursaRaster' <- function(x,...) {
   ct <- ursa_colortable(x)
   if (!.is.colortable(ct))
      return(filled.contour(as.matrix(x,coords=TRUE),...))
   a1 <- as.numeric(.deintervale(ct))
   da <- max(diff(a1))
   if (length(ct)==length(a1)) { ## categoral
      a2 <- a1[-1]-diff(a1)/2
      lev <- c(min(a2)-da,a2,max(a2)+da)
   }
   else { ## interval
      lev <- c(min(a1)-da,a1,max(a1)+da)
   }
   filled.contour(as.matrix(reclass(x),coords=TRUE),col=ct
                 ,zlim=range(seq(length(ct))-1),levels=lev,...)
}
'image.ursaRaster' <- function(x,...) {
   ct <- ursa_colortable(x)
   if (!.is.colortable(ct))
      return(image(as.matrix(x,coords=TRUE),...))
   image(as.matrix(x,coords=TRUE),col=ct,zlim=range(seq(length(ct))-1),...)
}
'.plot.ursaRaster.example' <- function() {
   a <- colorize(pixelsize(),byvalue=10,interval=TRUE)
   plot(a,asp=TRUE)
}
