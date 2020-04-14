'panel_shading' <- function(obj,level=NA,col=NULL,density=25,angle=c(-45,45)
                          ,lwd=1,lty=1,verbose=TRUE)
{
   if (.skipPlot(TRUE))
      return(NULL)
   if (!is.ursa(obj))
      return(NULL)
   g1 <- session_grid()
   if (is.null(col)) {
      if (.is.colortable(obj))
         col <- ursa_colortable(obj)
      else if (.is.colortable(col))
         col <- ursa_colortable(col)
      else
         col <- "#0000002F"
   }
   col <- range(col)
   if ((is.numeric(level))&&(level>0)) {
      obj <- discolor(obj)
      obj[abs(obj)<level] <- NA
   }
   obj <- as.data.frame(obj)
   if (!nrow(obj))
      return(NULL)
  # scale <- getOption("ursaPngScale")
  # res <- with(g1,sqrt(resx*resy))
   dens <- density #  (1/scale)*density#/par()$cex
   if (verbose)
   {
     # print(c(original=density,applied=dens,cex=par()$cex,scale=scale,res=res/1e3))
      pb <- ursaProgressBar(min=0,max=nrow(obj),tail=TRUE)
     # cat("dashing start...")
   }
   for (i in seq(nrow(obj)))
   {
      x1 <- obj$x[i]-g1$resx/2
      x2 <- obj$x[i]+g1$resx/2
      y1 <- obj$y[i]-g1$resy/2
      y2 <- obj$y[i]+g1$resy/2
      s <- sign(obj[i,3])>=0
      for (j in angle)
         polygon(c(x1,x1,x2,x2,x1),c(y1,y2,y2,y1,y1)
                ,density=dens,angle=j #sign(obj[i,3])*45*j
                ,border=NA,col=ifelse(!s,col[1],col[2]),lwd=0.8*lwd,lty=lty)
      if (verbose)
         setUrsaProgressBar(pb,i)
   }
   if (verbose) {
      close(pb)
     # cat("done!\n")
   }
   NULL
}
