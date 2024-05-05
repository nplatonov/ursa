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
   if (figN>1) {
      .panel_attribution()
      if (getOption("ursaPngBox"))
         panel_box()
   }
   options(ursaPngFigure=figN)
   arglist <- list(...)
   kwd <- "blank"
   g1 <- .getPrm(arglist,name="(grid|ref|dim)",kwd=kwd,class="ursaGrid",default=NULL)
   if (is.null(g1))
      g1 <- .panel_grid()
   else
      .panel_grid(g1)
   crs <- if (is.null(g1)) "" else g1$crs
   density <- .getPrm(arglist,name="density",kwd=kwd,default=NA_real_)
   angle <- .getPrm(arglist,name="angle",kwd=kwd,default=NA_real_)
   def.col <- if ((any(!is.na(density)))&&(any(!is.na(angle)))) "grey80" else "chessboard"
   if (devel5 <- FALSE) {
      col <- .getPrm(arglist,name="(^$|fill)",kwd=kwd,class="character"
                    ,default="brown",verbose=TRUE)
      str(col)
      q()
   }
   col <- .getPrm(arglist,name="(^$|bg|blank|fill)",kwd=kwd
                    ,default=ifelse(nchar(crs)>0,def.col,"white")
                    ,class=c("character","ursaColorTable")) # grey90
   alpha <- .getPrm(arglist,name="alpha",kwd=kwd,default=1)
   lwd <- .getPrm(arglist,name="lwd",kwd=kwd,default=1)
   lty <- .getPrm(arglist,name="lty",class=c("character","integer")
                 ,kwd=kwd,default=1)
   mar <- .getPrm(arglist,name="mar",kwd=kwd,default=rep(0,4))
   asp <- .getPrm(arglist,name="asp",kwd=kwd,default=NA_real_) # default=1?
   verbose <- .getPrm(arglist,name="verb(ose)*",kwd=kwd,default=FALSE)
   .panel_new(grid=g1,col=col,alpha=alpha,density=density,angle=angle
             ,lwd=lwd,lty=lty,mar=mar,asp=asp,verbose=verbose)
}
'.panel_new' <- function(col="chessboard",alpha=NA,density=NA,angle=NA
                        ,lwd=1,lty=1,asp=NA,mar=rep(0,4),grid=NULL
                        ,verbose=FALSE) {
   if (.skipPlot())
      return(invisible(NULL))
   if (verbose)
      str(list(col=col,alpha=alpha,density=density,angle=angle,lwd=lwd,lty=lty
              ,mar=mar,asp=asp))
   g1 <- .compose_grid()
   if (noGrid <- is.null(grid))
      grid <- session_grid()
   sc <- getOption("ursaPngScale")
   if (!is.numeric(sc))
      sc <- 1
   if (T) {
      mul <- 1
      cl <- getOption("ursaPngLayout")
      m <- cl$layout
      m[m!=getOption("ursaPngFigure")] <- 0
      indC <- which(colSums(m)>0)
      indR <- which(rowSums(m)>0)
      if ((length(indC))&&(length(indR))) {
         if (length(indC)>1)
            indC <- seq(min(indC),max(indC))
         if (length(indR)>1)
            indR <- seq(min(indR),max(indR))
         ref0 <- c(sum(cl$size$r[indR]),sum(cl$size$c[indC]))
         if (!identical(as.integer(.round(ref0)),unname(dim(grid)))) {
           # print(ref0)
            grid <- consistent_grid(grid,ref=ref0,verbose=!TRUE)
            options(ursaPngPanelGrid=grid)
         }
         else if (noGrid)
            options(ursaPngPanelGrid=grid)
      }
   }
   par(mar=rep(mar,length=4),xaxs="i",yaxs="i")#,xaxt="n",yaxt="n")
   xlim <- with(grid,c(minx,maxx))
   ylim <- with(grid,c(miny,maxy))
   xlim[1] <- xlim[1]-1*grid$resx/sc
   ylim[2] <- ylim[2]+1*grid$resy/sc
   plot(0,0,type="n",xlim=xlim,ylim=ylim,axes=FALSE,asp=asp,xlab="",ylab="")
   if (col=="chessboard") {
     # print(session_grid())
     # if (!is.numeric(sc))
     #    sc <- 1
      g2 <- regrid(grid,mul=sc/8,resetGrid=!TRUE,tolerance=0.999) ## let rough grid
      dima <- dim(g2)
      if (isTRUE(getOption("ursaPngBackground") %in% c("black","#000000"))) {
         minc <- 21/255
         maxc <- 0.000
      }
      else {
         minc <- 247/255
         maxc <- 1.000
      }
      s1 <- rep(c(minc,maxc),length=dima["samples"])
      s2 <- rep(c(maxc,minc),length=dima["samples"])
      a <- matrix(NA,nrow=dima["lines"],ncol=dima["samples"],byrow=TRUE)
      ind1 <- seq(floor(dima["lines"]/2))*2
      ind2 <- seq(nrow(a))[-ind1]
      m1 <- matrix(rep(s1,length(ind1)),byrow=TRUE,ncol=ncol(a))
      m2 <- matrix(rep(s2,length(ind2)),byrow=TRUE,ncol=ncol(a))
      a[ind1,] <- m1
      a[ind2,] <- m2
     # a[] <- runif(prod(dim(a)),min=0.6,max=1)
      panel_plot(as.raster(a),interpolate=FALSE)
     # session_grid(g3)
      col <- "white"
   }
   else {
      a <- col2rgb(col,alpha=TRUE)
      col <- rgb(a[1,],a[2,],a[3,],.round(a[4,]*alpha),maxColorValue=255)
      opEnd <- par(lend="square")
      for (an in angle)
         rect(min(xlim),min(ylim),max(xlim),max(ylim)
             ,col=col,border="transparent",density=density,angle=an,lwd=lwd,lty=lty)
      par(opEnd)
   }
   options(ursaPngPanel=col) # ,ursaPngPanelGrid=g3
   invisible(NULL)
}
