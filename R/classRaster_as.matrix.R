'[[.ursaRaster' <- function(x,i) {
   if (missing(i))
      i <- 1L
   as.matrix(x[i],coords=TRUE)
}
'as.matrix.ursaRaster' <- function(x,...) {
   coords <- .getPrm(list(...),name="(^$|crd|coord)",default=FALSE)
   if (!coords)
      return(unclass(ursa_value(x)))
   g1 <- x$grid
   if (FALSE) { ## failed due to round of floating point 
      xo <- with(g1,seq(minx,maxx,by=resx)[-1]-resx/2)
      yo <- with(g1,seq(miny,maxy,by=resy)[-1]-resy/2)
   }
   else {
      xo <- with(g1,seq(minx,maxx,len=columns+1)[-1]-resx/2)
      yo <- with(g1,seq(miny,maxy,len=rows+1)[-1]-resy/2)
   }
   isValue <- !is.null(dim(x$value)[1])
   if (isValue) {
      posR <- x$con$posR
      if (is.na(posR[1])) {
         z <- x$value[,1]
      }
      else {
         z <- rep(NA,with(g1,columns*rows))
         z2 <- x$value[,1]
         nc <- g1$columns
         sc <- seq(nc)
         for (i in seq_along(posR))
            z[(posR[i]-1)*nc+sc] <- z2[(i-1)*nc+sc]
      }
   }
   dim(z) <- with(g1,c(columns,rows))
   isFlip <- g1$miny>g1$maxy
   if (isFlip)
      yo <- rev(yo)
   z <- z[,rev(seq(g1$rows)),drop=FALSE]
   img <- list(x=xo,y=yo,z=z)#,nodata=ignorevalue(x),crs=proj4[length(proj4)])
   attr(img,"crs") <- with(g1,crs[length(crs)])
   if (length(x$colortable))
      attr(img,"colortable") <- x$colortable
   img
}
