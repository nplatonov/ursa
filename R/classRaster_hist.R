'hist.ursaRaster' <- function(x,...) {
   v <- ursa_value(x)
   class(v) <- paste0(".",class(v))
   hist(v,...)
}
'histogram' <- function(...) .syn('ursa_hist',0,...)
'ursa_hist' <- function(obj,width=800,height=600,...) {
   rel <- as.list(match.call())
   if (!is.ursa(obj)) {
      if ((is.character(obj))&&(!is.matrix(obj)))
         obj <- if (envi_exists(obj)) read_envi(obj,...) else read_gdal(obj,...)
      if ((is.character(obj))||(is.factor(obj))) {
         stop("histogram of categories is not implemented yet")
      }
      if ((is.numeric(obj))&&(is.null(dim(obj)))) {
         g1 <- getOption("ursaSessionGrid")
         dim(obj) <- c(length(obj),1)
      }
      if(!.try(obj <- as.ursa(obj)))
         return(NULL)
     # obj <- as.ursa(obj)
   }
   rel[["obj"]] <- obj
   if (length(ind <- (.grep("verbose",names(rel)))))
      verbose <- eval(rel[[ind]])
   else
      verbose <- FALSE
  # if (!.lgrep("tail",names(rel)))
  #    rel$tail <- 0.001
  # p <- colorize(obj)
   p <- do.call("colorize",rel[-1],quote=TRUE)
   ct <- p$colortable
   ta <- as.table(p)
   va <- .deintervale(ct)
  # ind <- match(names(ta),seq_along(va)-1L)
  # if (any(is.na(ind)))
  #    ct <- rep(NA_character_,)
   if (is.character(va))
      va <- seq(length(ta))
  # else
  #    va <- as.numeric(.deintervale(ct))
   d <- mean(diff(va))
   ##~ if (length(ta)==length(va)+1) {
      ##~ breaks <- c(min(va)-d,va,max(va)+d)
   ##~ }
   ##~ else {
      ##~ breaks <- c(va-d/2,max(va)+d/2)
   ##~ }
   ##~ if (TRUE)
      ##~ breaks <- c(0,seq(length(ct)))
   adjy <- as.numeric(names(ta))
   dify <- diff(adjy)
   toDensity <- .is.eq(dify)
   if (toDensity) {
      rngy <- range(adjy)+c(-1,1)*mean(dify)/2
      breaks <- seq(rngy[1],rngy[2],by=mean(dify))
   }
   else
      breaks <- c(0,seq(length(ta)))
   mids <- breaks[-1]-d/2
   counts <- as.integer(ta)
   g0 <- session_grid()
   g1 <- .grid.skeleton()
   g1$minx <- min(breaks)
   g1$maxx <- max(breaks)
   g1$miny <- 0
   g1$maxy <- 1
   g1$columns <- width
   g1$rows <- height
   g1$resx <- with(g1,(maxx-minx)/columns)
   g1$resy <- with(g1,(maxy-miny)/rows)
   session_grid(g1)
   maxy <- 0.95
   histValue <- list(breaks=breaks,counts=as.numeric(counts)
                    ,intensities=as.numeric(counts/max(counts))
                    ,density=maxy*as.numeric(counts/max(counts))
                    ,mids=mids
                    ,zname="manual histogram"
                    ,equidist=TRUE)
   class(histValue) <- "histogram"
   if (verbose)
      str(histValue,digits=12)
   options(ursaPngAuto=TRUE)
   compose_open(legend=list("bottom","left"),...)
   panel_new(asp=NA,col="white")
   panel_lines(histValue,col="grey80",lwd=5
        ,main=NULL,axes=FALSE,freq=FALSE) # ,xlab=NULL
   isCT <- .is.colortable(obj)
   if (isCT) {
      ct <- obj$colortable
      val <- .deintervale(ct)
      isChar <- is.character(val)
   }
   if (toDensity) { ## carefully
      if (!((isCT)&&(!isChar)&&(sd(diff(ct))>0.1))) {
         if (isCT)
            z <- try(density(na.omit(reclass(obj)$value),n=2^11,...))
         else {
            if (FALSE) {
               arglist1 <- as.list(args(density.default))
               str(arglist1)
               arglist2 <- list(...)
               str(arglist2)
               q()
            }
            opW <- options(warn=-10)
            z <- try(density(na.omit(c(obj$value)),n=2^11,...))
            options(opW)
         }
         if (!inherits(z,"try-error")) {
            z$x <- c(min(z$x),z$x,max(z$x))
            z$y <- c(-1,z$y,-1)
            z$y <- maxy*z$y*max(histValue$density)/max(z$y)
            panel_polygon(z,lwd=3,lty=5,border="grey20") #border=tail(myBrewer("Spectral"),1)
         }
         else
            cat("density was not defined")
      }
   }
   arglist <- list(...)
   ind <- .grep("^las$",names(arglist))
   if (!length(ind)) {
      arglist$las <- if (isCT) 3L else 1L
   }
  # str(c(list(p),arglist))
  # compose_legend(p,...)
   xlab <- .getPrm(arglist,name="(^xlab.*|lab.*x$)",default="")
   ylab <- .getPrm(arglist,name="(^ylab.*|lab.*y$)",default="")
   if (!nchar(ylab)) {
      do.call("legend_colorbar",list(ursa_colortable(p),units=xlab))
      do.call("legend_mtext",list(ylab))
     # leg <- c(list(p),arglist)
     # do.call("compose_legend",leg)
   }
   else {
      bg <- sum(c(col2rgb(getOption("ursaPngBackground")))*c(0.30,0.59,0.11))
      tcol <- ifelse(bg<128,"#FFFFFF","#000000")
      scol <- paste0(tcol,"7F")
      las <- 1
      adj <- 1
      cex <- .getPrm(arglist,name="cex",default=1)
      if (getOption("ursaPngDevice") %in% c("windows"))
         toE <- TRUE
      else {
         opWE <- options(warn=2)
         toE1 <- .try(abbreviate(xlab,minlength=2,strict=TRUE),silent=TRUE)
         toE2 <- .try(abbreviate(ylab,minlength=2,strict=TRUE),silent=TRUE)
         if (!toE1)
            message(paste("Note: unable to make bold caption for",.dQuote(xlab)))
         if (!toE2)
            message(paste("Note: unable to make bold caption for",.dQuote(ylab)))
         toE <- toE1 & toE2
         options(opWE)
      }
      if (toE) {
         xlab <- as.expression(substitute(bold(u),list(u=xlab)))
         ylab <- as.expression(substitute(bold(u),list(u=ylab)))
      }
      family <- getOption("ursaPngFamily")
      if (nchar(xlab)) {
         side <- 1
         y <- .deintervale(p)
         mwidth <- max(par()$fin)
         labels <- 21
         repeat({
            label <- pretty(y,n=labels)
            labelW <- label
            width <- max(strwidth(paste0("Wwwwwii",labelW)
                             ,units="inches",cex=cex,family=family))
            if (width*length(label)<mwidth)
               break
            labels <- labels-1
         })
         label <- with(session_grid(),label[label>=minx & label<=maxx])
         axis(side=side,col=NA,col.ticks=scol,at=label,tcl=-0.3
             ,labels=NA,lty=1,lwd=0.5)
         mtext(side=side,text=format(label),at=label,las=las
              ,line=0.3,padj=0.5,adj=0.5
              ,cex=cex,col=tcol)
         mtext(xlab,side=side,padj=1,adj=0.5,las=1,col=tcol,cex=cex
              ,line=1.2)
        # da <- .prettyLabel(.deintervale(p),ncol=10)
        # print(da)
      }
      if (nchar(ylab)) {
         side <- 2
         nlab <- c(11,10,12,9,13,8,14,7,15,6,16,5,4,3,2,1)
         print(table(counts))
         for (n in nlab[order(abs(nlab-11))]) {
            da <- .prettyLabel(counts,ncol=n)
            if (all(abs(da$at-round(da$at))<1e-11))
               break
         }
         da$at <- maxy*da$at/max(da$at)
         width <- max(strwidth(paste0("Ww",da$lab)
                          ,units="inches",cex=cex,family=family))
         height <- 1.5*strheight("Mg",units="inches",cex=cex,family=family)
         axis(side=side,at=da$at,labels=NA,col=NA,col.ticks=scol,tcl=-0.2
             ,lty=1,lwd=0.5)
        # mtext(side=2,at=at,text=lab,padj=0.4,adj=1,line=0.6,cex=2,col=tcol)
         mtext(text=da$lab,at=da$at,las=las,line=0.5
              ,side=side,padj=ifelse(las %in% c(0,3),0.2,0.4)
              ,adj=ifelse(las %in% c(0,3),0.5,adj),cex=cex,col=tcol)
         mtext(ylab,side=side,padj=0,adj=0.5,las=3,col=tcol,cex=cex
              ,line=ifelse(las %in% c(1,2),0.1+width/height,height+1.5))
      }
   }
   compose_close(...)
   session_grid(g0)
   0L
}
'.cmd.hist' <- function() {
   do.call("histogram",.args2list())
}
