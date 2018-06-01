# 'subset.ursaRaster' <- function(...) .syn('ursa_crop',0,...)
#'subset.ursaRaster' <- function(x,...) ursa_crop(x,...)

'ursa_crop' <- function(obj,condition,border=0,resetGrid=TRUE
                                      ,verbose=FALSE)
{
  # 20171111 ++ 'discolor(cond)'
   border <- as.integer(round(rep(border,length=4)))
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
   g1 <- g2 <- obj$grid
   g1$minx <- min(a$x)-g1$resx/2-border[1]*g1$resx
   g1$miny <- min(a$y)-g1$resy/2-border[2]*g1$resy
   g1$maxx <- max(a$x)+g1$resx/2+border[3]*g1$resx
   g1$maxy <- max(a$y)+g1$resy/2+border[4]*g1$resy
   g1$columns <- as.integer(.round(with(g1,(maxx-minx)/resx)))
   g1$rows <- as.integer(.round(with(g1,(maxy-miny)/resy)))
   regrid(obj,g1,resetGrid=resetGrid,resample=0,verbose=verbose)
}
