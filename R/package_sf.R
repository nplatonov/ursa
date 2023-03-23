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
               ,crs=sf::st_crs(a$crs)$proj4string)
   session_grid(g1)
   res <- ursa(attr(sf::gdal_read(fname,read_data=TRUE),"data"),flip=TRUE)
   if (length(bname)==length(res))
      names(res) <- bname
   res
}
'as_stars' <- function(obj) {
   if (!inherits(obj,"ursaRaster"))
      return(NULL)
   g <- ursa_grid(obj)
   crs <- ursa_crs(obj)
   if ((FALSE)&&(requireNamespace("sf",quietly=.isPackageInUse())))
      crs <- sf::st_crs(crs)
   md <- list(x=NULL
             ,y=NULL
             ,band=NULL
             )
   md$x <- list(from=1L
               ,to=g$columns
               ,offset=g$minx
               ,delta=g$resx
               ,refsys=crs
               ,point=NA
               ,values=NULL
               )
   md$y <- list(from=1L
               ,to=g$rows
               ,offset=g$maxy
               ,delta=-g$resy
               ,refsys=crs
               ,point=NA
               ,values=NULL
               )
   md$band <- list(from=1L
                  ,to=unname(length(obj))
                  ,offset=NA_real_
                  ,delta=NA_real_
                  ,refsys=NA_character_
                  ,point=NA,values=names(obj)
                  )
   class(md$x) <- class(md$y) <- class(md$band) <- "dimension"
   band <- list(affine=c(0,0)
               ,dimensions=c("x","y")
               ,curvilinear=FALSE
               ,blocksizes=NULL
               )
   class(band) <- "stars_raster"
   attr(md,"raster") <- band
   class(md) <- "dimensions"
   ret <- list(imported=as.array(obj,flip=TRUE,permute=FALSE))
   attr(ret,"dimensions") <- md
   class(ret) <- "stars"
   ret
}
# 'as.stars' <- function(obj) as_stars(obj=obj) ## if 'stars' is class, then 'as' is function
