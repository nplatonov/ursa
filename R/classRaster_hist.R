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
      if(!.try(obj <- as.ursa(obj)))
         return(NULL)
      obj <- as.ursa(obj)
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
   if (TRUE)
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
   histValue <- list(breaks=breaks,counts=as.numeric(counts)
                    ,intensities=as.numeric(counts/max(counts))
                    ,density=0.95*as.numeric(counts/max(counts))
                    ,mids=mids
                    ,zname="manual histogram"
                    ,equidist=TRUE)
   class(histValue) <- "histogram"
   if (verbose)
      str(histValue,digits=12)
   options(ursaPngAuto=TRUE)
   compose_open(side=1,...)
   panel_new(asp=NA,col="white")
   panel_lines(histValue,col="grey80",lwd=5
        ,main=NULL,axes=FALSE,freq=FALSE) # ,xlab=NULL
   isCT <- .is.colortable(obj)
   if (isCT) {
      ct <- obj$colortable
      val <- .deintervale(ct)
      isChar <- is.character(val)
   }
   if (FALSE) { ## many bugs
      if (!((isCT)&&(!isChar)&&(sd(diff(ct))>0.1))) {
         if (isCT)
            z <- density(na.omit(reclass(obj)$value),n=2048)
         else {
            z <- density(na.omit(obj$value),n=2048)
         }
         z$x <- c(min(z$x),z$x,max(z$x))
         z$y <- c(-1,z$y,-1)
         z$y <- 0.95*z$y*max(histValue$density)/max(z$y)
         panel_polygon(z,lwd=3,lty=5,border="grey20") #border=tail(myBrewer("Spectral"),1)
      }
   }
   arglist <- list(...)
   ind <- .grep("^las$",names(arglist))
   if (!length(ind)) {
      arglist$las <- if (isCT) 3L else 1L
   }
  # str(c(list(p),arglist))
  # compose_legend(p,...)
   do.call("compose_legend",c(list(p),arglist))
   compose_close(...)
   session_grid(g0)
   0L
}
'.cmd.hist' <- function() {
   do.call("histogram",.args2list())
}
