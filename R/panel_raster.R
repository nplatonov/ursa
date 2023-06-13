'panel_raster' <- function(...)
{
   if (.skipPlot(TRUE))
      return(NULL)
   arglist <- list(...)
   kwd <- "raster"
   obj <- .getPrm(arglist,name="",default=NULL
                 ,class=list(c("list","ursaRaster"),"ursaRaster","ggmap","character"))
   verbose <- .getPrm(arglist,name="verb(ose)*",kwd=kwd,default=FALSE)
   if (is.character(obj)) {
      cache <- .getPrm(arglist,name="cache",class=c("logical","character"),default=TRUE)
      obj <- .geomap(style=obj,cache=cache,verbose=verbose)
   }
   if (inherits(obj,"ggmap"))
      obj <- as.ursa(obj)
   if (is.null(obj))
      return(invisible(NULL))
   useRaster <- .getPrm(arglist,name="useRaster",kwd=kwd,default=NA)
   interpolate <- .getPrm(arglist,name="interp(olate)*",kwd=kwd,default=FALSE)
   alpha <- .getPrm(arglist,name="(alpha|transp(aren(cy)*)*)"
                   ,kwd=kwd,class=list("numeric","character"),default=NA)
   attribution <- .getPrm(arglist,name="(ann(otat)*|attr)",kwd=kwd,default="bottomright")
   if (verbose)
      str(list(obj=class(obj),useRaster=useRaster,interpolate=interpolate
              ,alpha=alpha))
   if (.is.ursa_stack(obj)) { ## recursive!!! (20160404 has bugs)
      if (verbose)
         print(match.call())
      rel <- as.list(match.call()) ## try mget(names(match.call())[-1])
      fun <- as.character(rel[1])
      rel <- rel[-1]
      rel <- rel[nchar(names(rel))>0]
      lastCT <- NULL
      for (i in seq_along(obj)) {
         rel[["obj"]] <- obj[[i]]
         lastCT <- do.call(fun,rel)
      }
      return(invisible(lastCT))
   }
   isRGB <- nband(obj) %in% c(2,3,4) & all(band_nNA(obj)>=0) # '==0' is NA used for RGB?
   if (isRGB) {
      if ((nband(obj)==4)&&(all(band_stat(obj)$sd[1:3]==0))) { ## ++ 20170718
         if (TRUE)
            obj <- c(obj[1:3]*obj[4]/255,obj[4])
         else {
            obj <- obj[4]
         }
         if ((is.numeric(alpha))&&(alpha<1))
            obj <- round(obj*alpha)
      }
      if ((is.numeric(alpha))&&(alpha<1)) {
         if (nband(obj) %in% c(1,3)) {
            g3 <- session_grid()
            session_grid(obj)
            obj <- c(obj,ursa_new(round(alpha*255),bandname=paste("Band",nband(obj)+1)))
            session_grid(g3)
         }
         else if (nband(obj) %in% c(4)) {
            obj[4] <- round(obj[4]*alpha)
         }
      }
      a <- with(ursa_grid(obj),rasterImage(as.raster(obj),minx,miny,maxx,maxy
                                      ,interpolate=interpolate))
      ann <- attr(obj,"copyright")
      if ((is.character(ann))&&(nchar(.gsub("\\s","",ann))>1)) {
         ##~ panel_annotation(ann,pos=attribution,cex=0.7,font="Arial Narrow"
                         ##~ ,fg=sprintf("#000000%s","4F"))
         options(ursaPngCopyright=c(getOption("ursaPngCopyright"),ann))
      }
      return(invisible(NULL))
   }
  # .panel_raster(colorize(obj,...),useRaster=useRaster
  #              ,interpolate=interpolate,alpha=alpha,verbose=verbose)
   ##~ .panel_raster(obj,useRaster=useRaster
                ##~ ,interpolate=interpolate,alpha=alpha,verbose=verbose,...)
##~ }
##~ '.panel_raster' <- function(obj,useRaster=NA,interpolate=FALSE,alpha=NA
                           ##~ ,verbose=FALSE,...) {
   ##~ if (.skipPlot(TRUE))
      ##~ return(NULL)
  # if (!getOption("ursaBackground"))
  #    panel_new()
  # print(obj)
  # q()
  # print(c(isCT=.is.colortable(obj),isC=obj$category))
   isCT <-  .is.colortable(obj) & .is.category(obj) # & attr(obj$value,"category")
   if (isCT)
      ct <- ursa_colortable(obj)
   scale <- getOption("ursaPngScale")
   e <- band_nNA(obj)
   isRGB <- nband(obj) %in% c(2,3,4) & all(band_nNA(obj)>=0) # '==0' is NA used for RGB?
   g1 <- getOption("ursaPngPanelGrid")
   if (ursa_crs(g1)!=ursa_crs(obj))
      obj <- .gdalwarp(obj,grid=g1)
   toResample <- floor(1/scale)>1 & !isRGB
   if (is.na(useRaster)) {
      cond1 <- getOption("ursaPngDevice")!="windows"
      cond2 <- !(tolower(gsub(".*\\.(\\S+)$","\\1"
                                    ,getOption("ursaPngFileout"))) %in% c("svg"))
      useRaster <- cond1 && cond2
   }
   if (verbose)
      str(list(isRGB=isRGB,isCT=isCT,toResample=toResample,isColorTable=isCT
              ,useRasrer=useRaster))
   if (toResample)
   {
     # obj <- contract(obj,size=sc,verbose=TRUE)
     # obj <- resampl4(obj,resize=1/sc,area=0,verbose=TRUE)
      g2 <- ursa_grid(obj)
     # if (isCT) {
     #    obj <- reclass(obj)
     # }
     # if (isCT)
     #    print(as.table(obj))
      obj <- regrid(obj,mul=scale,resample=ifelse(isCT,-1,1)
                   ,verbose=verbose)#,resetGrid=TRUE)
     # if ((TRUE)&&(isCT)) { ## FALSE?
     #    obj <- reclass(discolor(obj),ct) ## round(obj)?
     # }
   }
   ##~ if (isRGB) {
      ##~ with(ursa_grid(obj),rasterImage(as.raster(obj),minx,miny,maxx,maxy
                                      ##~ ,interpolate=interpolate))
      ##~ return(NULL)
   ##~ }
   arglist <- list(...)
   if (!isCT) {
      if (!.is.colortable(obj)) {
         arglist[[1]] <- quote(obj)
         obj <- do.call("colorize",arglist)
      }
      else if (!.is.category(obj)) { # attr(obj$value,"category")
         obj <- reclass(obj)
      }
   }
   else if (all(is.na(ct))) {
      val <- .deintervale(ct)
      if (is.numeric(val)) {
         if (length(val)==length(ct)) {
            arglist$value <- val
         }
         else {
            stop("TODO: interval")
         }
      }
      else {
         arglist$value <- seq(length(ct))-1L ## val
         arglist$name <- names(ct)
      }
      arglist$stretch <- ".onetoone"
      arglist <- c(quote(obj),arglist)
      arglist[[1]] <- quote(obj)
      obj <- do.call("colorize",arglist)
   }
   if (!is.na(alpha)) { ## ?adjustcolor
      if ((is.numeric(alpha))&&(alpha>=0)&&(alpha<=1))
         alpha <- paste0(toupper(sprintf("%x",round(alpha*255))))
      else if ((is.numeric(alpha))&&(alpha>=0)&&(alpha<=255))
         alpha <- paste0(toupper(sprintf("%x",round(alpha))))
      else if (is.character(alpha)) {
         a <- as.numeric(paste0("0x",alpha))
         if ((a<0)||(a>255))
            alpha <- NA
      }
      else
         alpha <- NA
      if (!is.na(alpha)) {
         if (nchar(alpha)<2)
            alpha <- paste0("0",alpha)
         ctname <- names(obj$colortable)
         ursa_colortable(obj) <- paste0(substr(ursa_colortable(obj),1,7),alpha)
         names(obj$colortable) <- ctname
     }
   }
   img <- as.matrix(obj,coords=TRUE)
   g1 <- ursa_grid(obj) #session_grid()
   xo <- with(g1,seq(minx,maxx,len=columns+1L))
   yo <- with(g1,seq(miny,maxy,len=rows+1L))
   if (!FALSE)
   {
      xo <- xo[-1]-g1$resx/2
      yo <- yo[-1]-g1$resy/2
   }
   if ((FALSE)&&(useRaster))
   {
      xo <- xo-0.5*g1$resx/scale
      yo <- yo+0.5*g1$resy/scale
   }
   if (.is.colortable(attr(img,"colortable")))
   {
      col <- as.character(attr(img,"colortable"))
      zlim <- c(1,length(col))-1
   }
   else ## color ramp
   {
      col <- 1:255
      if (any(!is.na(img$z)))
         zlim <- range(img$z,na.rm=TRUE)
      else
         zlim <- range(col)
   }
   if ((verbose)&&(!useRaster))
      .elapsedTime(paste0("useRaster=",useRaster,"--start"))
  # require(grid)
  # a <- grDevices::as.raster(t(img$z)/max(img$z,na.rm=TRUE))
  # grid.raster(a,x=unit(0.5,"npc"),y=unit(0.5,"npc"),interpolate=FALSE)
   image(x=xo,y=yo,z=img$z,col=col,asp=NA,zlim=zlim,useRaster=useRaster,add=TRUE)
  ##### rasterImage(img$z,min(xo),min(yo),max(xo),max(yo),)
   if ((verbose)&&(!useRaster))
      .elapsedTime(paste0("useRaster=",useRaster,"--finish"))
   if (toResample)
      session_grid(g2)
   col1 <- col2rgb(obj$colortable,alpha=TRUE)/255
   isAlpha <- any(col1[4,]!=1)
   ann <- c(getOption("ursaPngCopyright"),attr(obj,"copyright"))
   if (sum(nchar(ann)))
      options(ursaPngCopyright=ann)
  # if ((is.character(ann))&&(nchar(.gsub("\\s","",ann))>1))
  #    panel_annotation(ann,pos="bottomright",cex=0.7,font="Arial Narrow"
  #                    ,fg=sprintf("#000000%s",ifelse(isAlpha,alpha,"4F")))
   if (!isAlpha)
      return(invisible(obj$colortable))
   if (is.na(alpha))
      col1[4,] <- 1-col1[4,]
   bg <- col2rgb(getOption("ursaPngPanel"),alpha=TRUE)/255
   col2 <- apply(col1,2,function(x) x*c(bg))
   if ((TRUE)||(identical(col1,col2))) ## 20170522 added TRUE
      return(invisible(obj$colortable))
   alphaPatch <- ifelse(!is.na(alpha),1,1-1e-2) ## desured to be 1
   col3 <- rgb(col2[1,],col2[2,],col2[3,],col2[4,]*alphaPatch,maxColorValue=1)
   ct1 <- ursa_colortable(obj)
   ct2 <- ursa_colortable(col3)
   names(ct2) <- names(ct1)
  # str(obj$colortable)
  # print(col1)
  # print(col2)
  # str(ct2)
   invisible(ct2)
}
