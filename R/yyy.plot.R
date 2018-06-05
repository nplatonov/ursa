'.makeFileout' <- function(obj,fileout="")
{
   if (!nchar(fileout))
   {
      if (!is.na(obj$con$fname))
      {
         f <- obj$con$fname
         string <- "\\.(unpacked(.*)~|bin|bingz|envi|enviz)$"
         if (.lgrep(string,f))
            f <- .gsub(string,"",f)
         fileout <- paste0(basename(f),.maketmp(),".png")
         rm(f)
      }
      else
         fileout <- .maketmp(ext="png")
   }
   if (!.lgrep("\\.png$",fileout))
      fileout <- paste0(fileout,".png")
   fileout
}
'.repairForScatterPlot' <- function() {
   g1 <- session_grid()
   if ((nchar(g1$proj4)==0)||(is.na(g1$proj4)))
   {
      cexx <- cexy <- 1
      x <- seq(g1,"x")
      y <- seq(g1,"y")
      img <- list(x=x,y=y)
      if (!is.null(g1$labx)) {
         if (length(g1$seqx)!=g1$columns)
            g1$seqx <- seq(g1,"x")
      }
      if (!is.null(g1$laby)) {
         if (length(g1$seqy)!=g1$rows)
            g1$seqy <- seq(g1,"y")
      }
      if ((!is.null(g1$seqx))&&(length(g1$seqx)))
      {
         if (length(g1$labx))
         {
            if (length(names(g1$labx))==length(g1$labx))
               xlab <- names(g1$labx)
            else
               xlab <- g1$labx
            xo <- data.frame(at=NA,lab=xlab,stringsAsFactors=FALSE)
            for (i in seq(nrow(xo)))
            {
               v <- g1$labx[i]
               i2 <- which(v<g1$seqx)[1]
               if ((is.na(i2))||(i2<2))
                  next
               i1 <- i2-1
               sc <- (v-g1$seqx[i1])/(g1$seqx[i2]-g1$seqx[i1])
               xo$at[i] <- img$x[i1]+sc*(img$x[i2]-img$x[i1])
            }
           # xo <- subset(xo,!is.na(at))
            xo <- xo[!is.na(xo$at),]
         }
         else
         {
            xo <- .prettyLabel(img$x,ncol=11)
           # xo <- subset(xo,at>=g1$minx & at<=g1$maxx)
            xo <- xo[xo$at>=g1$minx & xo$at<=g1$maxx,]
            if (0 %in% xo$at)
               xo$at <- xo$at+1L
            if (all(xo$at>0))
               xo$lab <- g1$seqx[xo$at]
            xo$at <- xo$at-0.5
         }
      }
      else
      {
         xt <- axTicks(1)
         xo <- data.frame(at=xt,lab=as.character(xt),stringsAsFactors=FALSE)
        # cexx <- 0.5
      }
      if ((!is.null(g1$seqy))&&(length(g1$seqy)))
      {
         if (length(g1$laby))
         {
            if (length(names(g1$laby))==length(g1$laby))
               ylab <- names(g1$laby)
            else
               ylab <- g1$laby
            yo <- data.frame(at=NA,lab=ylab,stringsAsFactors=FALSE)
            for (i in seq(nrow(yo)))
            {
               v <- g1$laby[i]
               i2 <- which(v<g1$seqy)[1]
               if ((is.na(i2))||(i2<2))
                  next
               i1 <- i2-1
               sc <- (v-g1$seqy[i1])/(g1$seqy[i2]-g1$seqy[i1])
               yo$at[i] <- img$y[i1]+sc*(img$y[i2]-img$y[i1])
            }
           # yo <- subset(yo,!is.na(at))
            yo <- yo[!is.na(yo$at),]
         }
         else
         {
            yo <- .prettyLabel(img$y,ncol=11)
           # yo <- subset(yo,at>=g1$miny & at<=g1$maxy)
            yo <- yo[yo$at>=g1$miny & yo$at<=g1$maxy,]
            if (0 %in% yo$at)
               yo$at <- yo$at+1L
            if (all(yo$at>0))
               yo$lab <- g1$seqy[yo$at]
            yo$at <- yo$at-0.5
         }
      }
      else
      {
         yt <- axTicks(2)
         yo <- data.frame(at=yt,lab=as.character(yt),stringsAsFactors=FALSE)
        # cexy <- 0.5
      }
      abline(v=xo$at,h=yo$at,lty=2,col="#0000002F")
      panel <- options()[.grep("^ursaPng.+",names(options()))]
      layout <- panel[["ursaPngLayout"]][["layout"]]
      figure <- panel[["ursaPngFigure"]]
      isTop <- figure %in% layout[3,]
      isBottom <- figure %in% layout[nrow(layout)-2,]
      isLeft <- figure %in% layout[,3]
      isRight <- figure %in% layout[,ncol(layout)-2]
      rm(panel,layout,figure)
     # axis(side=1,at=xo$at,labels=NA,tck=-0.2,col="red")
     # axis(side=2,at=yo$at,labels=NA,tck=-0.2,col="blue")
      width <- strwidth(paste(xo$lab,collapse=" "),units="inches",cex=cexy)
      if (width>0.5*par()$pin[1])
      {
         i0 <- which(seq(nrow(xo))%%2==0)
         i1 <- which(seq(nrow(xo))%%2==1)
         if (isTop)
            mtext(side=3,text=xo$lab[i0],at=xo$at[i0],padj=-0.25,adj=0.5,line=0
                 ,cex=cexx,las=1)
         if (isBottom)
            mtext(side=1,text=xo$lab[i1],at=xo$at[i1],padj=0.5,adj=0.5
                 ,cex=cexx,las=1)
         rm(i0,i1)
      }
      else if (isBottom)
         mtext(side=1,text=xo$lab,at=xo$at,padj=0.5,adj=0.5,cex=cexx,las=1)
      if (isLeft)
         mtext(side=2,text=yo$lab,at=yo$at,padj=0.4,adj=1.0,line=0.4,cex=cexy,las=1)
      xu <- attr(g1$columns,"units")
      if ((isBottom)&&(is.character(xu)))
      {
         xu <- as.expression(substitute(bold(u),list(u=xu)))
         mtext(xu,side=1,padj=1,adj=0.5,las=1,col="black",cex=cexx,line=0.85)
      }
      yu <- attr(g1$rows,"units")
      if ((isLeft)&&(is.character(yu)))
      {
         width <- max(strwidth(yo$lab,units="inches",cex=cexy))
         height <- max(strheight(yo$lab,units="inches",cex=cexy))
         yu <- as.expression(substitute(bold(u),list(u=yu)))
         mtext(yu,side=2,padj=0,adj=0.5,las=3,col="black",cex=cexy
              ,line=0.8+width/height)
      }
      return(NULL)
   }
   NULL
}
'.getSide' <- function()
{
   figN <- getOption("ursaPngFigure")
   figP <- getOption("ursaPngLayout")$image
   while (figN<figP) {
      panel_new()
      figN <- figN+1L
   }
   panel <- getOption("ursaPngLayout")
   if (figN==panel$image) {
      .panel_attribution()
      if (getOption("ursaPngBox"))
         panel_box()
   }
   figN <- figN+1L
   options(ursaPngFigure=figN)
   mat <- panel$layout
   ind <- which(c(mat)==figN)
   indr <- c(row(mat))[ind]
   indc <- c(col(mat))[ind]
   if (all(indr==1))
      side <- 3L
   else if (all(indr==nrow(mat)))
      side <- 1L
   else if (all(indc==ncol(mat)))
      side <- 4L
   else if (all(indc==1))
      side <- 2L
   else
      stop("cannot identify side")
   side
}
'.panel_attribution' <- function(pos=ifelse(vertical,"bottomright","bottomright")
                                ,vertical=TRUE) {
   if (isWindows <- getOption("ursaPngDevice")=="windows")
      windowsFonts('Arial Narrow'=windowsFont("TT Arial Narrow"))
   ann <- paste0("",paste(unique(getOption("ursaPngCopyright")),collapse=" | "))
  # ann <- paste(c(getOption("ursaPngCopyright")),collapse="\n")
   if (nchar(ann))
      panel_annotation(ann,pos=pos,cex=0.7,font="Arial Narrow"
                      ,fg=sprintf("#000000%s","4F"),vertical=vertical)
   options(ursaPngCopyright=NULL)
}
