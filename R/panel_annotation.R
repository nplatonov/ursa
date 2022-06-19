##~ 'layout.label' <- function(...) .syn('panel_annotation',0,...)
## 'panel_annotation'??
'panel_annotation' <- function(...) {
   if (.skipPlot(TRUE))
      return(invisible(NULL))
   arglist <- list(...)
   if ((length(arglist))&&(is_spatial(arglist[[1]]))) {
      obj <- spatial_transform(arglist[[1]],session_crs())
      xy <- spatial_coordinates(spatial_centroid(obj))
      da <- spatial_data(obj)
      noLabel <- !ncol(da)
     # if (noLabel)
     #    da <- data.frame(label=rep("*",nrow(xy)))
      if (!noLabel)
         da <- da[[1]]
      return(invisible(lapply(seq_len(spatial_count(obj)),function(i) {
         do.call(panel_annotation,c(x=xy[i,1],y=xy[i,2]
                                   ,label=if (noLabel) NULL else da[i]
                                   ,arglist[-1]))
      })))
   }
   kwd <- "(caption|ann(otation)*)"
   annotation <- .getPrm(arglist,name=kwd,default=TRUE)
   if (!is.logical(annotation))
      annotation <- TRUE
   if (!annotation)
      return(NULL)
   label <- .getPrm(arglist,name="($|label|text)",kwd=kwd
                   ,class=rev(c("expression","character","array","matrix"))
                   ,default=expression()) ## expression(degree*C) ## "May"
   if (!length(label))
      return(NULL)
   if ((!is.expression(label))&&(all(is.na(label))))
      return(NULL)
  # label <- .getPrm(arglist,name="label",kwd=kwd,default="May")
  # font <- .getPrm(arglist,name="font",kwd=kwd,default=par("family"))
   position <- .getPrm(arglist,name="pos(ition)*",kwd=kwd
                      ,class=list("character","numeric"),default="topright")
   cex <- .getPrm(arglist,name="cex",kwd=kwd,default=1.2-0.2)
   adjust <- .getPrm(arglist,name="adj(ust)*",kwd=kwd,default=0.5)
   fg <- .getPrm(arglist,name="fg",kwd=kwd,default="#000000AF")
   bg <- .getPrm(arglist,name="bg",kwd=kwd,default=NA_character_)
   fill <- .getPrm(arglist,name="fill",kwd=kwd,default=NA_character_)
   buffer <- .getPrm(arglist,name="buf(fer)*",kwd=kwd,default=1)
   lon <- .getPrm(arglist,name="lon(gitude)*",kwd=kwd,default=NA_real_)
   lat <- .getPrm(arglist,name="lat(itude)*",kwd=kwd,default=NA_real_)
   x <- .getPrm(arglist,name="x$",kwd=kwd,default=NA_real_)
   y <- .getPrm(arglist,name="y$",kwd=kwd,default=NA_real_)
  # font <- .getPrm(arglist,name="font",kwd=kwd,default=par("family"))
   font <- .getPrm(arglist,name="font",kwd=kwd,default=getOption("ursaPngFamily"))
   ##~ vertical <- .getPrm(arglist,name="vert(ical)*"
                      ##~ ,class=c("logical","numeric"),kwd=kwd,default=FALSE)
  # vertical <- .getPrm(arglist,name="vert(ical)*",kwd=kwd,default=FALSE)
   vertical <- .getPrm(arglist,name="vert(ical)*",kwd=kwd,default=0)
   if (vertical==1)
      vertical <- 90
   alpha <- .getPrm(arglist,name="(alpha|transp(aren(cy)*)*)",kwd=kwd,default=1)
   interpolate <- .getPrm(arglist,name="interp(olate)*",kwd=kwd,default=FALSE)
   resample <- .getPrm(arglist,name="resample",kwd=kwd,default=FALSE)
   verbose <- .getPrm(arglist,name="verb(ose)*",kwd=kwd,default=FALSE)
   .panel_annotation(label=label,position=position,lon=lon,lat=lat,x=x,y=y
                                   ,cex=cex,adjust=adjust
                                   ,fg=fg,bg=bg,buffer=buffer,fill=fill
                                   ,font=font,vertical=vertical
                                   ,alpha=alpha,verbose=verbose)
}
'.panel_annotation' <- function(label=expression()#Omega^alpha)
                               ,position="bottomright",lon=NA,lat=NA,x=NA,y=NA
                               ,cex=1,adjust=0.5
                               ,fg="#000000",bg="#FFFFFF1F",buffer=1,fill="#FFFFFF7F"
                               ,font=par("family"),vertical=FALSE
                               ,alpha=1,interpolate=FALSE,resample=FALSE
                               ,verbose=FALSE,...) {
   if (verbose)
      str(list(label=label,position=position,cex=cex,adjust=adjust,fg=fg,bg=bg
              ,fill=fill,buffer=buffer,vertical=vertical,verbose=verbose))
   opt <- par(family=font)
   g1 <- session_grid()
   minx <- g1$minx
   miny <- g1$miny
   maxx <- g1$maxx
   maxy <- g1$maxy
   centerx <- 0.5*(minx+maxx)
   centery <- 0.5*(miny+maxy)
   device <- getOption("ursaPngDevice")
   vadj0 <- if ((!is.null(device))&&(device=="windows")) 0.35 else 0.40
   isPicture <- inherits(label,c("array","matrix"))
   srt <- if (is.logical(vertical)) ifelse(vertical,90,0) else vertical
   if (length(adjust)==2) {
      vadj <- adjust[2]
      adjust <- adjust[1]
   }
   else if (!isPicture) {
      vadj <- ifelse(any(as.numeric(unlist(gregexpr("\\\n",label)))<0),vadj0,0.5)
   }
   if ((!anyNA(lon))&&(!anyNA(lat))) {
      xy <- .project(cbind(lon,lat),g1$crs)
      pos <- c((xy[1,1]-minx)/(maxx-minx),(xy[1,2]-miny)/(maxy-miny))
   }
   else if ((!is.na(x))&&(!is.na(y))) {
      if ((x<minx)||(x>maxx)||(y<miny)||(y>maxy))
         return(NULL)
      pos <- c((x-minx)/(maxx-minx),(y-miny)/(maxy-miny))
   }
   else
      pos <- position
  # scale <- getOption("ursaPngScale")
   if (F)
      print(data.frame(cex=cex,par=par()$cex,retina=getOption("ursaPngRetina")
                      ,pointsize=getOption("ursaPngPointsize"),default=12))
   mycex <- cex/par()$cex#*getOption("ursaPngRetina")*getOption("ursaPngPointsize")/12
   if (!isPicture)
      if ((length(label)>1)&&(length(label)>=getOption("ursaPngLayout")$image))
         label <- label[getOption("ursaPngFigure")]
   isE <- is.expression(label)
   if (!isPicture) {
      if (!isE)
         label <- paste(label,collapse="\n")
      if (!nchar(label))
         return (10L)
   }
   if (is.factor(pos))
      pos <- as.character(pos)
   isCaption <- is.character(pos)
   if (isCaption) {
      pos <- switch(pos,center=c(0.5,0.5),top=c(0.5,1),bottom=c(0.5,0)
                   ,left=c(0,0.5),right=c(1,0.5),topleft=c(0,1),topright=c(1,1)
                   ,bottomleft=c(0,0),bottomright=c(1,0),{
                       message(sprintf("'%s' is invalid keyword",pos))
                       match.arg(pos,c("center","top","bottom","left","right"
                                      ,"topleft","topright"
                                      ,"bottomleft","bottomright"))
                   })
   }
   if (length(pos)==1)
      pos <- c(1,1)
  # print(pos)
   if (!length(pos)) {
      cat("Unable to detect position for annotation\n")
      return(invisible(NULL))
   }
   if ((isCaption)&&(is.na(fill)))
      fill <- "#FFFFFF7F"
   if ((!isCaption)&&(is.na(bg)))
      bg <- "#FFFFFF3F"
   if (isPicture) {
      sc <- getOption("ursaPngScale")
      g2 <- if (is.numeric(sc)) regrid(g1,mul=sc,resample=resample) else g1
      dima <- dim(label)
     # nb <- length(dima) ## -- 20170919
      nb <- dima[3] ## ++ 20170919
      if (is.character(alpha)) {
         alpha <- as.numeric(as.hexmode(alpha))/255
      }
      if (alpha>1)
         alpha <- alpha/255
      if (alpha<1) {
         if (nb==3) {
            label <- c(label,rep(alpha,prod(dima[1:2])))
            dim(label) <- c(dima[1:2],nb+1)
         }
         else if (nb==4) {
            label[,,4] <- label[,,4]*alpha
         }
      }
      width <- dima[2]*g2$resx*cex
      height <- dima[1]*g2$resy*cex
   }
   else {
      height <- 1.5*strheight(paste0(label,"(Cjyp^~_)"),units="user",cex=mycex)
      if (is.expression(label))
         width <- strwidth(label,units="user",cex=mycex)+0.2*height
      else
         width <- strwidth(paste(label,"|",sep=""),units="user",cex=mycex)
   }
   if (vertical) {
      .w <- width
      width <- height
      height <- .w*1.05
      rm(.w)
   }
  # print(c(width=width,height=height))
   coord <- c(NA,NA,NA,NA)
  # rect(coord[1],coord[2],coord[3],coord[4],col="red")
   coord[1] <- minx+pos[1]*(maxx-minx)-0.5*width
   coord[2] <- miny+pos[2]*(maxy-miny)-0.5*height
   coord[3] <- minx+pos[1]*(maxx-minx)+0.5*width
   coord[4] <- miny+pos[2]*(maxy-miny)+0.5*height
   if (coord[1]<minx)
   {
      coord[1] <- minx-ifelse(isE,width/3.5,0)
      coord[3] <- coord[1]+width
   }
   if (coord[3]>maxx)
   {
      coord[3] <- maxx+ifelse(isE,0,0) ## ifelse(isE,width/3.5,0)
      coord[1] <- coord[3]-width
   }
   if (coord[2]<miny)
   {
      coord[2] <- miny
      coord[4] <- coord[2]+height
   }
   if (coord[4]>maxy)
   {
      coord[4] <- maxy-ifelse(isE,height/6,0) # ifelse(isE,height,0)
      coord[2] <- coord[4]-height
   }
  # rect(coord[1],coord[2],coord[3],coord[4],col="#0000FF50")
   if (isPicture) {
      graphics::rasterImage(grDevices::as.raster(label,max=1)
                           ,coord[1],coord[2],coord[3],coord[4]
                           ,interpolate=interpolate)
      par(opt)
      return(invisible(NULL))
   }
   if (TRUE) { ## 20180625
      if (adjust>=0.48)
         adjust <- (adjust-0.5)*width/height+0.5 
   }
  # print(c(scale=scale,cex=mycex))
   x <- 0.5*(coord[1]+coord[3])
   y <- 0.5*(coord[2]+coord[4])
   x <- x+(adjust-0.5)*(coord[3]-coord[1])
   if ((!is.na(fill))&&(is.character(fill))) ##(isCaption)
      rect(coord[1],coord[2],coord[3],coord[4],border="transparent",col=fill)
   if ((!is.na(bg))&&(is.character(bg))) ##(!isCaption)
   {
      if (verbose)
         .elapsedTime("label:start")
      sc <- getOption("ursaPngScale")
      b <- buffer*sc
      w <- seq(-b,b,length=ceiling(b)*2+1)*with(g1,sqrt(resx*resy))/sc
      for (dx in w)
         for (dy in w)
            text(x=x+dx,y+dy,adj=c(adjust,vadj),label,cex=mycex,col=bg,srt=srt)
      if (verbose)
         .elapsedTime("label:finish")
   }
   text(x=x,y=y,adj=c(adjust,vadj),labels=label,cex=mycex,col=fg,srt=srt)
   par(opt)
   invisible(NULL)
}
