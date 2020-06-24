'panel_scalebar' <- function(...) {
   if (.skipPlot(TRUE))
      return(invisible(NULL))
   arglist <- list(...)
   kwd <- "(ruler|scalebar)"
   scalebar <- .getPrm(arglist,name=gsub("\\)","|decor)$",kwd)
                      ,class=list("integer","logical")
                      ,default=TRUE)
   if (is.integer(scalebar))
      scalebar <- getOption("ursaPngFigure") %in% scalebar
   if (!scalebar) {
     # scalebar <- .getPrm(arglist,name=kwd,coerce=FALSE,verb=TRUE,default=NULL)
     # str(scalebar)
      return(NULL)
   }
   position <- .getPrm(arglist,name="pos(ition)*",kwd=kwd
                      ,class=list("character","numeric"),default="---")
   g0 <- session_grid()
   canScale <- .lgrep("(epsg:3857|\\+proj=(merc|zzzzz)\\s)",g0$crs)>0
   if ((all(position=="---"))&&(canScale)) {
      lat <- with(g0,.project(rbind(c(minx,miny),c(maxx,maxy)),crs,inv=TRUE))[,2]
      sc <- sort(1/cos(lat*pi/180))
      if (sc[2]/sc[1]>1.25)
         position <- c("bottomleft","topleft")
   }
   if (position[1]=="---")
      position <- "bottomleft"
   w <- .getPrm(arglist,name="w",kwd=kwd,class=list("numeric","character")
               ,default=NA_real_)
   cex <- .getPrm(arglist,name="cex",kwd=kwd,default=0.75)
   bg <- sum(c(col2rgb(getOption("ursaPngBackground")))*c(0.30,0.59,0.11))
   defcol <- ifelse(bg<128,"#FFFFFF2F","#0000002F") 
   col <- .getPrm(arglist,name="col",kwd=kwd,default=defcol)
   bg <- .getPrm(arglist,name="bg",kwd=kwd,default="transparent")
   fill <- .getPrm(arglist,name="fill",kwd=kwd,default="transparent") # "#FFFFFF3F"
   language <- .getPrm(arglist,name="lan(g(uage)*)*",kwd=kwd,default=NA_character_)
   verbose <- .getPrm(arglist,name="verb(ose)*",kwd=kwd,default=FALSE)
   if (is.character(position)) {
      for (pos in position)
         .panel_scalebar(position=pos,w=w,cex=cex,col=col,bg=bg,fill=fill
                        ,language=language,verbose=verbose)
   }
   else {
      .panel_scalebar(position=position,w=w,cex=cex,col=col,bg=bg,fill=fill
                     ,language=language,verbose=verbose)
   }
   invisible(NULL)
}
'.panel_scalebar' <- function(position="bottomleft",w=NA,cex=0.85
                             ,col="#0000002F",bg="transparent",fill="#FFFFFF2F"
                             ,language=NA,verbose=FALSE) {
   if (.skipPlot(TRUE))
      return(invisible(NULL))
   indirect <- getOption("ursaPngAuto")
   if (!is.logical(indirect))
      indirect <- FALSE
   if (verbose)
      str(list(position=position,w=w,cex=cex,col=col,bg=bg,fill=fill,verbose=verbose))
   g1 <- session_grid()
   paperScale <- getOption("ursaPngPaperScale")
   if (is.na(language)) {
      if (TRUE) {
         ctype <- Sys.getlocale("LC_TIME")
         if (.lgrep("Russian",ctype))
            language <- "ru"
      }
      else
         language <- Sys.getenv("LANGUAGE")
   }
   pos <- position
   if (is.factor(pos))
      pos <- as.character(pos)
   if (is.character(pos))
      pos <- switch(pos,center=c(0.5,0.5),top=c(0.5,1),bottom=c(0.5,0)
                   ,left=c(0,0.5),right=c(1,0.5),topleft=c(0,1),topright=c(1,1)
                   ,bottomleft=c(0,0),bottomright=c(1,0),{
                       message(sprintf("'%s' is invalid keyword",pos))
                       match.arg(pos,c("center","top","bottom","left","right"
                                      ,"topleft","topright"
                                      ,"bottomleft","bottomright"))
                   })
   if (!((is.numeric(pos))&&(length(pos)==2))) {
      opW <- options(warn=1)
      warning("Unable to detect 'position'. Skipping scalebar")
      options(opW)
      return(invisible(NULL))
   }
   isLonLat <- .lgrep("(\\+proj=longlat|epsg:4326)",g1$crs)>0
   if ((isLonLat)||((TRUE)&&(!nchar(g1$crs))))
      return(invisible(NULL))
   isGeo <- nchar(g1$crs)>0
   if ((indirect)&&(!isGeo))
      return(invisible(NULL))
   dx <- with(g1,maxx-minx)
   dy <- with(g1,maxy-miny)
   isMerc <- .lgrep("(\\+proj=merc|epsg\\:3857)",g1$crs)>0
   if (isMerc) {
      x3 <- pos[1]
      if (pos[1]<0.1)
         x3 <- 0.1
      else if (pos[1]>0.9)
         x3 <- 0.9
      y3 <- pos[2]
      if (pos[2]<0.05)
         y3 <- 0.05
      else if (pos[2]>0.95)
         y3 <- 0.95
      lat <- with(g1,.project(cbind(minx+x3*(maxx-minx),miny+y3*(maxy-miny))
                             ,g1$crs,inv=TRUE))#[1,2]
      lat_ts <- .gsub2("\\+lat_ts=(\\S+)\\s","\\1",g1$crs)
      lat_ts <- ifelse(lat_ts==g1$crs,0,as.numeric(lat_ts))
      sc <- if (is.null(lat)) 1 else 1/cos((lat[1,2]-lat_ts)*pi/180)
   }
   else
      sc <- 1
   mycex <- cex/par()$cex
   w0 <- 15*strheight("M",units="user",cex=mycex)/sc
  # w0 <- (180/sc)*g1$resx/getOption("ursaPngScale") ## desired width 180 pxls 
  # print(c(new=w1,old=w0,r=w0/w1))
   if (w0>0.6*dx) {
      w0 <- 0.6*dx
   }
   w0 <- w0/3000
   if (is.character(w)) {
      wVal <- as.numeric(.gsub("^(\\d+(\\.\\d+)*)([A-Za-z\\s])*$","\\1",w))
      wUn <- .gsub("^(\\d+(\\.\\d+)*)\\s*([A-Za-z]+)*$","\\3",w)
      w <- wVal*switch(wUn,km=1,m=1e-3,mm=1e-6)
   }
   if (verbose)
      print(c(manual=w,auto=w0))
   if ((is.na(w))||(w<=0)) {
      scW <- 10
     # if (isMerc)
     #    w <- (0.08/sc)*dx/1000
     # else 
         w <- w0
     # print(c(sc=sc,scF=scF,resx=g1$resx,w=w,lw=g1$columns*scF
     #        ,pr=(180/3)*g1$resx/1000/scF))
     # q()
     # w <- (0.08/sc)*max(dx,dy)/1000
      manualW <- FALSE
   }
   else {
      scW <- 1
      manualW <- TRUE
   }
   if (TRUE) {
      if (!manualW) {
         w0 <-  c(10,12,15,16,20,25,30,40,50,60,70,80,90)
         for (i in c(-6:6)) {
            if (w<=10^i)
               break
         }
         w1 <- w0*10^(i-2)
         w <- w1[which.min(abs(w1-w))]
      }
      if (w<1e-6) {
        # print(w)
         message(paste0("Nano scale of scalebar length (",w,") is ignored"))
         return(invisible(NULL))
      }
      else if (w<1e-3) {
         w0 <- w*1e6
         un <- switch(language,ru="\u043C\u043C","mm")
      }
      else if (w<1) {
         w0 <- w*1000
         un <- switch(language,ru="\u043C","m")
      }
      else {
         w0 <- w
         un <- switch(language,ru="\u043A\u043C","km")
      }
   }
   else
      w0 <- w*1000
   if ((!manualW)&&(paperScale[1]!=0)) { ## 20171019 ++ (!manualW)
      pw <- par()$fin[1]*2.54
      w1 <- paperScale[2]/(pw*sc) ## 1cm or 'paperScale[2] cm'
      if (w1<1)
      {
         w1 <- w1*dx
         if (w1<1000) {
            if (FALSE) {
               wP <- pretty(w1,n=20)
               w0 <- wP[which.min(abs(wP-w1))[1]]
            }
            else if (paperScale[1]>0) {
               w0 <- paperScale[1]*1e3
            }
            else {
               e <- 1e0
               w0 <- round(w1/e,0)*e
            }
            message(paste("Composer: figure width for"
                          ,sprintf("1:%.0f",w0*100/paperScale[2]),"scale is"
                          ,pw*(w1/paperScale[2])/(w0),"cm"))
            un <- switch(language,ru="\u043C","m")
         }
         else {
            if (paperScale[1]>0)
               w0 <- paperScale[1]
            else
               w0 <- round(w1/1000,0) ## ,1 for tuning
           # if (as.integer(w0)!=w0)
            message(paste("Composer: figure width for"
                         ,sprintf("1:%.0f",w0*100000/paperScale[2]),"scale is"
                         ,pw*(w1/paperScale[2])/(1000*w0),"cm"))
            un <- switch(language,ru="\u043A\u043C","km")
         }
         w <- w1/1000
        # print(w)
        # print(w0)
        # print(un)
      }
   }
   if (!nchar(g1$crs))
      un <- ""
   w <- w*sc
   ##~ if (pos[1]<0.5) {
      ##~ x1 <- g1$minx+pos[1]*dx
      ##~ x2 <- x1+3*w*1000
   ##~ }
   ##~ else {
      ##~ x2 <- g1$maxx-(1-pos[1])*dx
      ##~ x1 <- x2-3*w*1000
   ##~ }
   h <- strheight("M",units="user",cex=mycex)
   lu <- strwidth(paste0("n",un),units="user",cex=mycex)
   lw <- strwidth(paste0(w0,"|"),units="user",cex=mycex)/2
   ls <- strwidth("n",units="user",cex=mycex)/2
   lw2 <- strwidth(paste0(2*w0,""),units="user",cex=mycex)/2
   lw3 <- max(lw2,lu)
   h2 <- 0.25*h
   hs <- 0.3*h
   h3 <- 0.8*h
   h4 <- h2+1.7*h
   x1 <- g1$minx+pos[1]*dx-1.5*w*1000
   x2 <- x1+3.0*w*1000
   if ((x1-lw)<g1$minx) {
      x1 <- g1$minx+lw
      x2 <- x1+3.0*w*1000
   }
   else if ((x2+lw3)>g1$maxx) {
      x2 <- g1$maxx-lw3
      x1 <- x2-3.0*w*1000
   }
   y1 <- g1$miny+pos[2]*dy
   if ((y1-h3)<g1$miny)
      y1 <- g1$miny+h3
   else if ((y1+h4)>g1$maxy)
      y1 <- g1$maxy-h4
  # print(pos)
  # print(c(x1=x1,x2=x2,y0=y1))
  # print(g1)
   xo <- seq(x1,x2,length=7)[-c(4,6)]#+ifelse(pos[1]>=0.5,-max(lu,lw2),lw)
   yo <- seq(y1-hs,y1+hs,length=3)#+ifelse(pos[2]>=0.5,-h-3*h2,h)
   rect(min(xo)-1.0*lw,y1-h3,max(xo)+1.0*lw3,max(yo)+h4
       ,col=bg,border="transparent")
   if (TRUE)
      rect(xo[1],yo[1],xo[5],yo[3],lwd=1,border=col,col=fill)
   else {
      rect(xo[1],yo[1],xo[2],yo[2],lwd=1,border=col,col="transparent")
      rect(xo[2],yo[2],xo[3],yo[3],lwd=1,border=col,col="transparent")
      rect(xo[3],yo[1],xo[4],yo[2],lwd=1,border=col,col="transparent")
      rect(xo[4],yo[2],xo[5],yo[3],lwd=1,border=col,col="transparent")
   }
   rect(xo[1],yo[2],xo[2],yo[3],lwd=1,border="transparent",col=col)
   rect(xo[2],yo[1],xo[3],yo[2],lwd=1,border="transparent",col=col)
   rect(xo[3],yo[2],xo[4],yo[3],lwd=1,border="transparent",col=col)
   rect(xo[4],yo[1],xo[5],yo[2],lwd=1,border="transparent",col=col)
   text(xo[1],yo[3]+h2,w0,col=col,cex=mycex,adj=c(0.5,-0.2))
   text(xo[3],yo[3]+h2,0,col=col,cex=mycex,adj=c(0.5,-0.2))
   text(xo[4],yo[3]+h2,w0,col=col,cex=mycex,adj=c(0.5,-0.2))
   text(xo[5],yo[3]+h2,2*w0,col=col,cex=mycex,adj=c(0.5,-0.2))
   text(xo[5]+ls,yo[2],un,col=col,cex=mycex,adj=c(0,0.5)) ## adj=c(0,0.6)
   invisible(NULL)
}
