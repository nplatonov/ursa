'.read_stars' <- function(fname) {
   if (!requireNamespace("sf",quietly=.isPackageInUse()))
      stop("Package 'sf' is required for this operation")
   a <- sf::gdal_read(fname,read_data=FALSE)
   columns <- a$cols[2]
   rows <- a$rows[2]
   bands <- a$bands[2]
   patt <- "^Band_(\\d+)=(.+)$"
   patt <- "^Band_(\\d+)=\\t*(.+)$"
   bname <- grep(patt,a$meta,value=TRUE)
   b1 <- .grep(patt,a$meta,value=TRUE)
   bname <- .gsub(patt,"\\2",b1)
   bname[as.integer(.gsub(patt,"\\1",b1))] <- bname
   if (all(is.na(a$geotransform))) {
      resx <- 1
      resy <- 1
      minx <- 0
      miny <- 0
      maxx <- columns
      maxy <- rows
   }
   else {
      resx <- a$geotransform[2]
      resy <- -a$geotransform[6]
      minx <- a$geotransform[1]
      maxy <- a$geotransform[4]
      maxx <- minx+columns*resx
      miny <- maxy-rows*resy
   }
   g1 <- regrid(minx=minx,maxx=maxx,miny=miny,maxy=maxy,columns=columns,rows=rows
               ,crs=a$proj4string)
   session_grid(g1)
   res <- ursa(attr(sf::gdal_read(fname,read_data=TRUE),"data"),flip=TRUE)
   if (length(bname)==length(res))
      names(res) <- bname
   res
}
