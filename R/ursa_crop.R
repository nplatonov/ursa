# 'subset.ursaRaster' <- function(...) .syn('ursa_crop',0,...)
#'subset.ursaRaster' <- function(x,...) ursa_crop(x,...)

'ursa_crop' <- function(obj,condition,border=0,expand=1,resetGrid=TRUE
                                      ,verbose=FALSE)
{
  # 20171111 ++ 'discolor(cond)'
   border <- as.integer(round(rep(border,length=4)))
   expand <- rep(expand,length=4)
   if (is.null(obj$value))
      return(NULL)
   if (missing(condition))
      a <- as.data.frame(discolor(obj))
   else {
      if (FALSE) ## longtime
         a <- as.data.frame(obj[condition])
      else {
         a <- as.data.frame(discolor(condition))
      }
   }
   if (!nrow(a)) {
      if (verbose) {
         opW <- options(warn=0)
         warning("Crop cannot be applied due to blank image. Return full spatial domain.")
         options(opW)
      }
      return(obj)
   }
   g1 <- g2 <- obj$grid
   g1$minx <- min(a$x)-g1$resx/2-border[2]*g1$resx
   g1$miny <- min(a$y)-g1$resy/2-border[1]*g1$resy
   g1$maxx <- max(a$x)+g1$resx/2+border[4]*g1$resx
   g1$maxy <- max(a$y)+g1$resy/2+border[3]*g1$resy
   dx <- g1$maxx-g1$minx
   dy <- g1$maxy-g1$miny
   if (T)
      dx <- dy <- sqrt(dx*dy)
   g1$maxx <- g1$maxx+(expand[4]-1)*dx
   g1$minx <- g1$minx-(expand[2]-1)*dx
   g1$miny <- g1$miny-(expand[1]-1)*dy
   g1$maxy <- g1$maxy+(expand[3]-1)*dy
   g1$columns <- as.integer(.round(with(g1,(maxx-minx)/resx)))
   g1$rows <- as.integer(.round(with(g1,(maxy-miny)/resy)))
   regrid(obj,g1,resetGrid=resetGrid,resample=0,verbose=verbose)
}
