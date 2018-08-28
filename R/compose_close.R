'compose_close' <- function(...) {
   if (.skipPlot(FALSE))
      return(invisible(NULL))
   arglist <- list(...)
   kind <- .getPrm(arglist,name="(^$|crop|kind)",valid=c("crop","crop2","nocrop"))
   border <- .getPrm(arglist,name="(border|frame)",default=5L)
   bpp <- .getPrm(arglist,name="bpp",valid=c(8L,24L)
              ,default=switch(getOption("ursaPngDevice"),windows=8L,cairo=24L))
   execute <- .getPrm(arglist,name="(execute|view|open|render)",default=!.isShiny())
   verbose <- .getPrm(arglist,name="verb(ose)*",kwd="close",default=FALSE)
  # wait <- .getPrm(arglist,name="wait",default=NA_real_)
   .compose_close(kind=kind,border=border,bpp=bpp,execute=execute#,wait=wait
                 ,verbose=verbose)
}
'.compose_close' <- function(kind=c("crop2","crop","nocrop")
                            ,border=5,bpp=0,execute=TRUE#,wait=NA
                            ,verbose=FALSE){
   if (verbose)
      str(list(kind=kind,border=border,bpp=bpp,execute=execute,verbose=verbose))
   toOpen <- session_pngviewer()
   if (FALSE) {
      message(paste(commandArgs(FALSE),collapse=" "))
      print(Sys.getenv()[grep("^(_)*R_",names(Sys.getenv()))])
   }
   delafter <- getOption("ursaPngDelafter")
   fileout <- getOption("ursaPngFileout")
   if (!(bpp %in% c(8,24)))
      bpp <- switch(getOption("ursaPngDevice"),windows=8,cairo=24)
   on.exit({
      op <- options()
      if (length(ind <- .grep("^ursaPng.+",names(op))))
         options(lapply(op[ind],function(x) NULL))
      NULL
   })
   syswait <- FALSE
   if ((FALSE)&&(execute)) { ## patch for "shell" 
      con <- showConnections(all=TRUE)
      ind <- which(!is.na(match(con[,"class"],"file")))
      if (length(ind)) {
         con <- con[ind,,drop=FALSE]
         unable <- grep("\\.unpacked(.*)\\~$",con[,"description"])
         if (length(unable)) {
            opW <- options(warn=1)
            w <- paste0("Detected opened connection(s): "
                       ,paste(.dQuote(basename(con[unable,"description"]))
                             ,collapse=", ")
                       ,".")
            warning(w)
            w <- "Script is suspended until close of image viewer."
            message(w)
            options(opW)
            syswait <- TRUE
         }
      }
   }
   if (!toOpen) {
      if (delafter)
         delafter <- .normalizePath(dirname(fileout))!=.normalizePath(tempdir())
      message(paste("Use",.sQuote("session_pngviewer(TRUE)")
             ,"\nto open",.sQuote(.normalizePath(fileout))
             ,"in external software."))
   }
   if (getOption("ursaPngFigure")==0L) ## plot layout only
   {
      if (TRUE) {## introduced 20150916 to deprecate 'dev' arg in compose_open()
         mosaic <- getOption("ursaPngLayout")
        # print(mosaic)
        # print(fileout)
         layout.show(max(mosaic$layout))
        # str(par())
        # plot(ann,add=TRUE)
         grDevices::dev.off()
         if (proposed <- TRUE) {
           # print(c(knitr=.isKnitr(),shiny=.isShiny(),jupyter=.isJupyter()))
            if (.isShiny()) {  ## in 'imageOutput'/'renderImage'
               return(if (execute) list(src=fileout) else fileout)
            }
            if ((execute)&&(.isKnitr())) {
               execute <- FALSE
               retK <- knitr::include_graphics(fileout,dpi=getOption("ursaPngDpi"))
               return(retK)
            }
            if (.isJupyter()) {
               execute <- FALSE
               return(invisible(IRdisplay::display_png(file=fileout)))
            }
         }
         if (execute) {
            if (!toOpen) {
               op <- par(mar=c(0,0,0,0))
               plot(grDevices::as.raster(png::readPNG(fileout)))
               if (TRUE) {
                  fann <- .dir(path=system.file("optional/sponsorship",package="ursa")
                              ,pattern="\\.png$",full.names=TRUE)
                  ann <- png::readPNG(sample(fann,1))
                  plot(grDevices::as.raster(ann),add=TRUE)
               }
               par(op)
            }
            else {
               if (TRUE)
                  browseURL(normalizePath(fileout))
               else if (.Platform$OS.type=="unix")
                  system2("xdg-open",c(.dQuote(fileout)),wait=!.isRscript())
               else
                  system2("R",c("CMD","open",.dQuote(fileout)),wait=!.isRscript())
              # system2("open",list(fileout),wait=!.isRscript()) ## wait=syswait
              # stop("How to implement file association in Unix-like systems?")
            }
         }
         if (delafter)
         {
            if (execute) {
               wait <- getOption("ursaPngWaitBeforeRemove")
               Sys.sleep(wait)
            }
            file.remove(fileout)
         }
      }
      else { ## original
         graphics.off()
         if ((delafter)&&(file.exists(fileout)))
            file.remove(fileout)
      }
      return(invisible(NULL))
   }
   if (getOption("ursaPngFigure")==getOption("ursaPngLayout")$image) {
      .panel_attribution()
      if (getOption("ursaPngBox"))
         panel_box()
   }
   grDevices::dev.off()
  # kind <- match.arg(kind)
   do.call(paste0(".",kind),list(fileout,border,verbose))
   n <- 999L
   if (!(bpp %in% c(8,24))) {
      requireNamespace("png",quietly=.isPackageInUse())
      x <- png::readPNG(fileout,native=TRUE,info=FALSE)
      if (verbose)
         .elapsedTime("uniqueColor:start")
      n <- length(unique(c(x)))
      if (verbose)
         .elapsedTime(paste0("uniqueColor:finish (",n,")"))
      bpp <- ifelse(n<256,8,24)
   }
   if (bpp==8) {
      if (nchar(Sys.which("i_view32"))) {
         FoutTmp <- ifelse(dirname(fileout)==".",fileout,normalizePath(fileout))
         cmd <- paste("i_view32",FoutTmp,"/bpp=8",paste0("/convert=",FoutTmp))
      }
      else if ((nchar(im <- Sys.getenv("R_IMAGEMAGICK"))>0)&&(file.exists(im))) {
        # print("R_IMAGEMAGICK is found")
         cmd <- paste(im,fileout,ifelse(n>=255,"-colors 255","")
                     ,paste0("png8:",fileout))
      } 
      else if ((nchar(imdisplay <- Sys.which("imdisplay"))>0)) {
        # imagemagick ver>=7.0.7 "magick" instead of "convert"
        # print("imagemagick is in PATH")
         cmd <- paste(file.path(dirname(imdisplay),"convert")
                     ,fileout,ifelse(n>=255,"-colors 255","")
                     ,paste0("png8:",fileout))
      }
      else if ((nchar(im <- Sys.which("convert"))>0)&& ##
                     (toupper(normalizePath(dirname(dirname(im))))!=
                      toupper(normalizePath(Sys.getenv("WINDIR"))))) {
        # print("convert is not in Windows directory")
         cmd <- paste(im,fileout,ifelse(n>=255,"-colors 255","")
                     ,paste0("png8:",fileout))
      }
      else {## else if... (other ways to force to bpp=8
        # print("imagemagick is not found")
         cmd <- ""
      }
      if (nchar(cmd)) {
         if (verbose)
            message(cmd)
         system(cmd)
      }
   }
   if (proposed <- TRUE) {
      if (.isShiny()) {  ## in 'imageOutput'/'renderImage'
         return(if (execute) list(src=fileout) else fileout)
      }
      if ((execute)&&(.isKnitr())) {
         execute <- FALSE
         retK <- knitr::include_graphics(fileout,dpi=getOption("ursaPngDpi"))
         return(retK)
      }
      if (.isJupyter()) {
         execute <- FALSE
         return(invisible(IRdisplay::display_png(file=fileout)))
        # return(invisible(do.call("IRdisplay::display_png",list(file=fileout))))
        # retK <- IRdisplay::display_png(file=fileout)
        # return(invisible(retK))
      }
   }
   if (execute)
   {
      if (!toOpen) {
         op <- par(mar=c(0,0,0,0))
         if (TRUE)
            plot(grDevices::as.raster(png::readPNG(fileout)))
         else { ## failed asp=1
            img <- png::readPNG(fileout,native=TRUE)
            dima <- dim(img)
            plot(0,0,type="n",axes=FALSE,xlim=c(0,dima[1]),ylim=c(0,dima[2]),asp=1,xlab="",ylab="")
            rasterImage(img,0,0,dima[1],dima[2])
         }
         par(op)
      }
      else {
         if (TRUE)
            browseURL(normalizePath(fileout))
         else if (.Platform$OS.type=="unix")
            system2("xdg-open",c(.dQuote(fileout)),wait=!.isRscript())
         else
            system2("R",c("CMD","open",.dQuote(fileout)),wait=TRUE)
        # system2("R cmd open",list(,fileout),wait=TRUE) #!.isRscript()) ## wait=syswait
        # stop("How to implement file association in Unix-like systems?")
      }
      wait <- getOption("ursaPngWaitBeforeRemove")
      if (delafter)
         Sys.sleep(wait)
   }
   if (delafter) {
      if (FALSE) {
         fpath <- dirname(fileout)
         isTempDir <- tempdir()==normalizePath(fpath)
         file.remove(fileout)
         if (isTempDir) {
            a <- .dir(path=fpath)
            print(fpath)
            if (!length(a)) {
               res <- unlink(fpath,force=TRUE)
               print(res)
            }
         }
      }
      else
         file.remove(fileout)
   }
   if (file.exists(fileout)) {
      return(invisible(fileout))
   }
   invisible(NULL)
}
