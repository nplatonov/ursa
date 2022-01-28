'compose_open' <- function(...) {
   if (T & .isKnitr()) {
      retina0 <- 2
   }
   else {
      retina0 <- getOption("ursaRetina")
      if (!is.numeric(retina0))
         retina0 <- 1
   }
   arglist <- list(...)
   mosaic <- .getPrm(arglist,name="",default=NA,class="")
   fileout <- .getPrm(arglist,name="fileout",default="")
   retina <- .getPrm(arglist,name="retina",default=ifelse(nchar(fileout),1,retina0))
   if ((!is.numeric(retina))||(is.na(retina)))
      retina <- retina0
   dpi <- .getPrm(arglist,name="dpi"
                 ,default=as.integer(round(ifelse(.isKnitr(),150*retina,96*retina))))
   pointsize <- .getPrm(arglist,name="pointsize",default=NA_real_)
   scale <- .getPrm(arglist,name="^scale$",class="",default=NA_real_)
   width <- .getPrm(arglist,name="width",class=list("integer","character"),default=NA_real_)
   height <- .getPrm(arglist,name="height",class=list("integer","character"),default=NA_real_)
   indent <- .getPrm(arglist,name="(space|offset|indent)",default=NA_real_)
  # frame <- .getPrm(arglist,name="(frame|colorbar|strip)(height)*",default=NA_real_)
   frame <- .getPrm(arglist,name="((frame|strip)(height)*|colorbar$)",default=NA_real_)
   box <- .getPrm(arglist,name="box",default=TRUE)
   delafter <- .getPrm(arglist,name="(del|remove)after",default=NA)
   wait <- .getPrm(arglist,name="wait",default=switch(.Platform$OS.type,windows=1,3))
   dtype <- if (.Platform$OS.type=="windows") c("cairo","windows","agg","cairo-png")
            else c("cairo","cairo-png","Xlib","quartz")
   device <- .getPrm(arglist,name="^(device|type)",valid=dtype)
   antialias <- .getPrm(arglist,name="antialias",valid=c("default","none","cleartype"))
  # font <- .getPrm(arglist,name="(font|family)",valid=ifelse(device=="windows","sans","Tahoma"))
   font <- .getPrm(arglist,name="(^font$|family)"
                  ,default=ifelse(device=="windows","sans","sans"))
   background <- .getPrm(arglist,name="(background|nodata)",default="white")
   dev <- .getPrm(arglist,name="^dev(el)*$",default=FALSE)
   fixed <- .getPrm(arglist,name="^(fixed)",default=FALSE)
   verbose <- .getPrm(arglist,name="verb(ose)*",kwd="open",default=FALSE)
   options(ursaPngWebCartography=FALSE)
   if (is_spatial(mosaic))
      session_grid(regrid(spatial_grid(mosaic),border=27))
   if (is.ursa(mosaic)) {
      cr <- attr(mosaic,"copyright")
      if ((is.character(cr))&&(nchar(cr)>1)) {
         mosaic <- compose_design(layout=c(1,1),legend=NULL)
        # print("WEB #1")
         options(ursaPngWebCartography=TRUE)
         scale <- 1
      }
   }
   else if ((.lgrep("\\+proj=merc",session_crs()))&&
           (!is.na(.is.near(session_cellsize(),2*6378137*pi/(2^(1:21+8)))))) {
     # print("WEB #2")
      arglist <- as.list(match.call())
      if (!("scale" %in% names(arglist))) {
        # options(ursaPngWebCartography=TRUE) ## -- 20201125
         scale <- 1
      }
      if (.is.integer(scale)) ## ++ 20201125
         options(ursaPngWebCartography=TRUE) 
   }
   if (isTRUE(getOption("ursaPngWebCartography"))) {
      retina1 <- session_grid()$retina
      if ((retina>1)&&(!is.na(retina1))&&(retina1>1)) {
         dpi <- round(dpi*retina1/retina)
         retina <- 2
      }
   }
   if ((is.character(mosaic))&&(mosaic=="rgb"))
      mosaic <- compose_design(layout=c(1,1),legend=NULL)
   else if (!inherits(mosaic,"ursaLayout"))
      mosaic <- compose_design(...)
   if ((isTRUE(fixed))&&(is.na(scale)))
      scale <- 1
   .compose_open(mosaic=mosaic,fileout=fileout,dpi=dpi,pointsize=pointsize
                ,scale=scale,width=width,height=height
                ,indent=indent,frame=frame,box=box,delafter=delafter,wait=wait
                ,device=device,antialias=antialias,font=font
                ,background=background,retina=retina,dev=dev,verbose=verbose)
   if (dev) {
      options(ursaPngPlot=TRUE)
      compose_close(...)
   }
   invisible(fileout)
}
'.compose_open' <- function(mosaic=NULL,fileout="",dpi=NA,pointsize=NA,scale=NA
                          ,width=NA,height=NA
                          ,indent=NA,frame=NA,box=TRUE,delafter=NA,wait=5
                          ,device=NA,antialias=NA,font=NA,background="white"
                          ,retina=NA,dev=FALSE,verbose=FALSE) {
   if (is.na(retina)) {
      retina <- getOption("ursaRetina")
      if (!is.numeric(retina))
         retina <- 1
   }
  # session_pngviewer()
   if (FALSE) {
      str(list(mosaic=if (is.list(mosaic)) sapply(mosaic,class) else class(mosaic)
              ,fileout=fileout,dpi=dpi,pointsize=pointsize,scale=scale
              ,width=width,height=height,indent=indent,frame=frame
              ,box=box,delafter=delafter,wait=wait,device=device
              ,antialias=antialias,font=font,background=background,dev=dev
              ,verbose=verbose))
   }
   if (!nchar(fileout))
   {
     # print(c(batch=.isRscript()))
     # print(commandArgs(FALSE))
     # print(c(tempdir=session_tempdir()))
      if ((FALSE)&&((!.isRscript())||(!session_pngviewer())||(.isKnitr())))
         fileout <- file.path(tempdir(),.maketmp()) ## CRAN Repository Policy
      else if (!TRUE)
         fileout <- file.path(tempdir(),basename(.maketmp()))
      else if (.isKnitr()) {
         bname <- basename(.maketmp())
         bname <- gsub("_[0-9a-f]+"
                  ,paste0("_",knitr::opts_current$get()$label),bname,ignore.case=TRUE)
         fileout <- file.path(knitr::opts_current$get()$fig.path,bname)
      }
      else
         fileout <- .maketmp()
      if (is.na(delafter))
         delafter <- TRUE
   }
   else if (is.na(delafter))
      delafter <- FALSE
   fileext <- if (.lgrep("\\.(jpeg|jpg)$",fileout)) "jpeg" 
              else if (.lgrep("\\.(webp)$",fileout)) "webp"
              else "png"
   isJPEG <- fileext %in% "jpeg"
   isWEBP <- fileext %in% "webp"
   if ((!isJPEG)&&(!isWEBP)&&(!.lgrep("\\.png$",fileout)))
      fileout <- paste0(fileout,".png")
   g1 <- session_grid()
  # scale1 <- (18.5*96)/(g1$rows*2.54)
  # scale2 <- (23.7*96)/(g1$columns*2.54)
   paperScale <- 0
   if (is.character(scale)) {
      patt <- "\\D*((\\d+(\\.\\d+)*)(\\:))*(\\d+(\\.\\d+)*)\\D*"
      .s0 <- .gsub(patt,"\\2",scale)
      .s <- as.numeric(.gsub(patt,"\\5",scale))
      .un <- .gsub(patt,"\\1 \\2 \\3 \\4 \\5 \\6",scale)
      if (nchar(.s0))
         .s <- .s/100000
      .s0 <- ifelse(nchar(.s0),as.numeric(.s0),1)
      if (.lgrep("\\+proj=merc",g1$crs)) {
         lat <- with(session_grid(),.project(cbind(0.5*(maxx+minx),0.5*(maxy+miny))
                                        ,crs,inv=TRUE))[1,2]
         sc <- 1/cos(lat*pi/180)
      }
      else
         sc <- 1
      scale <- NA
      dx <- with(g1,maxx-minx)/1000
      width <- round((1/sc)*dx/.s*dpi/2.54)-1 ## defines correctness of scalebar
      height <- 1e6
      paperScale <- c(.s,.s0)
   }
   else if ((!is.na(width))&&(is.na(height))) {
      if (is.character(width)) {
         .w <- as.numeric(.gsub("\\D*(\\d+(\\.\\d+)*)\\D*","\\1",width))
         width <- dpi*.w/2.54
         paperScale <- c(-1,1)
      }
      height <- 1e6
   }
   else if ((!is.na(height))&&(is.na(width))) {
      if (is.character(height)) {
         .h <- as.numeric(.gsub("\\D*(\\d+(\\.\\d+)*)\\D*","\\1",height))
         height <- dpi*.h/2.54
         paperScale <- c(-1,1) # TRUE
      }
      width <- 1e6
   }
  # print(c(width=width,s=ifelse(is.na(width),900,width),r=ifelse(is.na(width),900,width)/g1$columns))
  # print(c(height=height,s=ifelse(is.na(height),600,height),r=ifelse(is.na(height),600,height)/g1$rows))
   scale1 <- ifelse(is.na(height),600*retina,height)/g1$rows
   scale2 <- ifelse(is.na(width),900*retina,width)/g1$columns
   rescale <- mosaic$image^(0.1)
   autoscale <- min(scale1,scale2)
   if ((is.na(height))&&(is.na(width)))
      autoscale <- autoscale/mosaic$image^0.25
   if (is.na(scale))
      scale <- autoscale
  # dpiscale <- scale*(2.54/96.0) # 2.54cm per inch / 96 dpi screen
   dpiscale <- scale*(2.54/dpi)
   mainc <- g1$columns*dpiscale
   mainr <- g1$rows*dpiscale
   if (verbose)
      print(c(v=scale1,h=scale2,autoscale=autoscale,scale=scale,c=g1$columns,r=g1$rows
             ,retina=retina,digits=3))
   pointsize0 <- ifelse(.isKnitr(),round(12*retina,0),round(12*retina,0))
   if (is.na(pointsize)) {
     # print(c(pointsize0=pointsize0,dpi=dpi,scale=scale,scale0=autoscale))
     # pointsize <- pointsize0*96/dpi*scale/autoscale ## removed 20161217
      pointsize <- pointsize0*96/dpi ## added 20161217 
   }
   if (FALSE) { ## removed 20161201
      if (is.na(frame))
         frame <- 0.028*mainr*rescale
      else
         frame <- 0.028*mainr*rescale*frame
      if (is.na(indent))
         indent <- 0.008*mainr*rescale
      else
         indent <- 0.008*mainr*rescale*indent
   }
   else { ## added 20161201
      if (is.na(frame))
         frame <- 0.021*pointsize*rescale
      else
         frame <- 0.021*pointsize*rescale*frame
      if (is.na(indent))
         indent <- 0.007*pointsize*rescale
      else
         indent <- 0.007*pointsize*rescale*indent
   }
   simage <- seq(mosaic$image)
   slegend <- max(simage)+seq(mosaic$legend)
   panel <- mosaic$layout
   sizec <- rep(indent,ncol(panel))
   sizer <- rep(indent,nrow(panel))
   'indmatch' <- function(x,fun=c("all","any"),ind)
   {
      fun <- match.arg(fun)
      x <- x[x>0]
      if (!length(x))
         return(FALSE)
      do.call(fun,list(x %in% ind))
   }
   sizer[apply(panel,1,indmatch,"all",slegend)] <- frame
   sizec[apply(panel,2,indmatch,"all",slegend)] <- frame
   sizer[apply(panel,1,indmatch,"any",simage)] <- mainr
   sizec[apply(panel,2,indmatch,"any",simage)] <- mainc
   if ((TRUE)||(box))
   {
      sizec <- sizec+1*2.54/dpi
      sizer <- sizer+1*2.54/dpi
   }
   png_height <- round(sum(sizer)*dpi/2.54+5*dpi/96+5*dpi*pointsize/pointsize0)
   png_width <- round(sum(sizec)*dpi/2.54+5*dpi/96+5*dpi*pointsize/pointsize0)
   dname <- dirname(fileout)
   if ((dname!=".")&&(!dir.exists(dname)))
      dir.create(dname,recursive=TRUE)
   if (verbose)
      print(c(png_width=png_width,png_height=png_height
             ,scale=scale,autoscale=autoscale,pointsize=pointsize,dpi=dpi))
   if (.isJupyter())
      options(jupyter.plot_mimetypes=ifelse(isJPEG,'image/jpeg','image/png'))
   if ((device=="agg")&&(requireNamespace("ragg",quietly=.isPackageInUse()))) {
      a <- try(ragg::agg_png(filename=fileout
                             ,width=png_width,height=png_height,res=dpi
                             ,bg=background,pointsize=pointsize
                            # ,antialias=antialias
                            # ,family=font
                             ))
   }
  # else if ((device=="CairoPNG")&&(requireNamespace("Cairo",quietly=.isPackageInUse()))) {
  #    a <- try(Cairo::CairoPNG(filename=fileout
  #            ,width=png_width,height=png_height,res=dpi
  #            ,bg=background,pointsize=pointsize
  #            #,type="png"
  #            ,antialias=antialias,family=font))
  # }
   else {
     # if (device=="default")
     #    device <- "cairo"
      if ((device %in% c("cairo","cairo-png"))&&(!capabilities("cairo"))) {
         if (.Platform$OS.type=="windows")
            device <- "windows"
         else
            device <- if (capabilities("aqua")) "quartz" else "Xlib"
      }
      a <- try(png(filename=fileout,width=png_width,height=png_height,res=dpi
              ,bg=background,pointsize=pointsize
              ,type=device,antialias=antialias,family=font))
   }
   if ((inherits(a,"try-error"))&&(.Platform$OS.type=="windows")) { ## 20180117 patch for conda without cairo
      device <- "windows"
      if (isJPEG)
         png(filename=fileout,width=png_width,height=png_height,res=dpi
            ,bg=background,pointsize=pointsize
            ,type=device,antialias=antialias,family=font)
      else
         png(filename=fileout,width=png_width,height=png_height,res=dpi
            ,bg=background,pointsize=pointsize
            ,type=device,antialias=antialias,family=font)
   }
     # ,family=c("Tahoma","Verdana","Georgia","Calibri","sans")[1]
   nf <- layout(panel,widths=lcm(sizec)
                               ,heights=lcm(sizer),respect=TRUE)
   options(ursaPngScale=scale,ursaPngDpi=dpi,ursaPngLayout=mosaic
          ,ursaPngFileout=fileout,ursaPngBox=box ## ,ursaPngLegend=mosaic$legend
          ,ursaPngFigure=0L,ursaPngDelafter=delafter ## ,ursaPngBar=frame
          ,ursaPngPlot=!dev,ursaPngPaperScale=paperScale 
          ,ursaPngFamily=font,ursaPngWaitBeforeRemove=wait
          ,ursaPngDevice=device,ursaPngShadow=""
          ,ursaPngBackground=background,ursaPngPanel="",ursaPngSkip=FALSE
          ,ursaPngRetina=retina
          ,ursaPngPointsize=pointsize,ursaPngComposeGrid=session_grid())
  # if (.isKnitr()) {
  #   # if (knitr::opts_knit$get(""))
  #    fileout <- paste0("file:///",fileout)
  # }
   invisible(fileout)
}
'.compose_open.example' <- function() {
   a <- pixelsize()
   compose_open(side=4,scale="1:50000000",pointsize=12,dpi=300)
   panel_new()
   panel_raster(a)
   panel_coastline()
   panel_graticule()
   for (y in seq(0,1,len=5))
      panel_scalebar(x=0.5,y=y,col="black")
   compose_close()
}
