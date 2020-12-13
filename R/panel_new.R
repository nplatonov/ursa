##~ '.panel_new' <- function(dymmy=NULL,
                ##~ col.blank=NA,asp=1,density=NA,angle=NA,lwd=1,mar=rep(0,4),...)
##~ {
   ##~ 0L
##~ }
'panel_new' <- function(...)
{
   if (.skipPlot())
      return(invisible(NULL))
   figN <- getOption("ursaPngFigure")+1L
   if (figN>getOption("ursaPngLayout")$image) {
      options(ursaPngSkip=TRUE)
      return(invisible(NULL))
   }
   g1 <- session_grid()
   if (figN>1) {
      .panel_attribution()
      if (getOption("ursaPngBox"))
         panel_box()
   }
   options(ursaPngFigure=figN)
   arglist <- list(...)
   kwd <- "blank"
   density <- .getPrm(arglist,name="density",kwd=kwd,default=NA_real_)
   angle <- .getPrm(arglist,name="angle",kwd=kwd,default=NA_real_)
   def.col <- if ((any(!is.na(density)))&&(any(!is.na(angle)))) "grey80" else "chessboard"
   col <- .getPrm(arglist,name="(^$|bg|blank|fill)",kwd=kwd
                    ,default=ifelse(nchar(g1$crs)>0,def.col,"white")) # grey90
   lwd <- .getPrm(arglist,name="lwd",kwd=kwd,default=1)
   lty <- .getPrm(arglist,name="lty",class=c("character","integer")
                 ,kwd=kwd,default=1)
   mar <- .getPrm(arglist,name="mar",kwd=kwd,default=rep(0,4))
   asp <- .getPrm(arglist,name="asp",kwd=kwd,default=NA_real_) # default=1?
   verbose <- .getPrm(arglist,name="verb(ose)*",kwd=kwd,default=FALSE)
   .panel_new(col=col,density=density,angle=angle,lwd=lwd,lty=lty,mar=mar
             ,asp=asp,verbose=verbose)
}
'.panel_new' <- function(col="chessboard",density=NA,angle=NA,lwd=1,lty=1
                        ,asp=NA,mar=rep(0,4),verbose=FALSE) {
   if (.skipPlot())
      return(invisible(NULL))
   if (verbose)
      str(list(col=col,density=density,angle=angle,lwd=lwd,lty=lty,mar=mar,asp=asp))
   g1 <- session_grid()
   scale <- getOption("ursaPngScale")
   par(mar=rep(mar,length=4),xaxs="i",yaxs="i")#,xaxt="n",yaxt="n")
   xlim <- with(g1,c(minx,maxx))
   ylim <- with(g1,c(miny,maxy))
   xlim[1] <- xlim[1]-1*g1$resx/scale
   ylim[2] <- ylim[2]+1*g1$resy/scale
   plot(0,0,type="n",xlim=xlim,ylim=ylim,axes=FALSE,asp=asp,xlab="",ylab="")
   if (col=="chessboard") {
      sc <- getOption("ursaPngScale")
      if (!is.numeric(sc))
         sc <- 1
      g2 <- regrid(mul=sc/8,resetGrid=TRUE,tolerance=0.999) ## let rough grid
      dima <- dim(g2)
      minc <- 247/255
      maxc <- 1.000
      s1 <- rep(c(minc,maxc),length=dima["samples"])
      s2 <- rep(c(maxc,minc),length=dima["samples"])
      a <- matrix(NA,nrow=dima["lines"],ncol=dima["samples"],byrow=TRUE)
      ind1 <- seq(floor(dima["lines"]/2))*2
      ind2 <- seq(nrow(a))[-ind1]
      m1 <- matrix(rep(s1,length(ind1)),byrow=TRUE,ncol=ncol(a))
      m2 <- matrix(rep(s2,length(ind2)),byrow=TRUE,ncol=ncol(a))
      a[ind1,] <- m1
      a[ind2,] <- m2
      panel_plot(as.raster(a),interpolate=FALSE)
      session_grid(g1)
      col <- "white"
   }
   else {
      opEnd <- par(lend="square")
      for (an in angle)
         rect(min(xlim),min(ylim),max(xlim),max(ylim)
             ,col=col,border="transparent",density=density,angle=an,lwd=lwd,lty=lty)
      par(opEnd)
   }
   options(ursaPngPanel=col,ursaPngPanelGrid=session_grid())
   invisible(NULL)
}
