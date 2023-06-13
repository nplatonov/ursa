'compose_panel' <- function(...,silent=FALSE)
{
   if (.skipPlot(TRUE))
      return(NULL)
   arglist <- list(...)
   img <- .getPrm(arglist,name="",default=NULL
                 ,class=list(c("list","ursaRaster"),"ursaRaster"))
   isList <- .is.ursa_stack(img)
   if ((is.null(img))||((!isList)&&(all(is.na(ursa_value(img)))))) { ## was 'missing'
  # if (is.null(img)) { 
      ret <- NULL
      if ((is.ursa(img))||(!length(arglist))) {
         panel_new(...)
         panel_decor(...)
      }
      else {
         aname <- names(arglist)
         indB <- .grep("^blank",aname)
         do.call("panel_new",arglist[indB])
        # indSP <- which(sapply(arglist,inherits,"Spatial"))
        # indSF <- which(sapply(arglist,inherits,c("sfc","sf")))
         indSP <- which(sapply(arglist,.isSP))
         indSF <- which(sapply(arglist,.isSF))
         other <- seq_along(arglist)
         indS <- c(sp=indSP,sf=indSF)
        # print(indS)
         if (length(indS))
            other <- other[-indS]
         if (length(indSP)) {
           # panel_plot(...)
            ret <- do.call("panel_plot",c(arglist[indSP],arglist[other]))#$col
         }
         if (length(indSF)) {
           # str(arglist[other])
            ret <- do.call("panel_plot",c(arglist[indSF],arglist[other]))#[[1]]#$col
           # if (inherits(ret,"ursaLegend"))
           #    NULL # ret <- ret[[1]]$col
           # if ((!is_ursa(ret,"colortable"))||(!inherits(ret,"ursaLegend")))
           #    ret <- ret$col
           # str(ret)
           # q()
         }
         if ((!length(indS))&&(length(other))) {
            ret <- do.call("panel_plot",c(arglist[other]))$col
         }
         indD <- .grep("^(decor|coast|grid|graticul|scale|ruler)",aname)
         do.call("panel_decor",arglist[indD])
         indA <- .grep("^(caption|ann|label)",aname)
         do.call("panel_annotation",arglist[indA])
      }
      return(invisible(ret))
   }
  # annotation <- .getPrm(arglist,name="annotation",default=NA)#_character_)
  # decor <- .getPrm(arglist,name="decor",default=TRUE)
  # scalebar <- .getPrm(arglist,name="scalebar",default=FALSE)
   verbose <- .getPrm(arglist,name="verb",kwd="plot",default=NA) ## FALSE?
   forceRGB <- .getPrm(arglist,name="rgb",kwd="plot",default=FALSE)
  # if ((!is.na(verbose))&&(verbose))
  #    str(list(annotation=annotation,verbose=verbose))
  # isBox <- getOption("ursaPngBox")
  # skip <- getOption("ursaPngSkip")
   np <- getOption("ursaPngLayout")$image
   if (isList)
   {
      ng <- length(img)
      nb <- sum(unlist(sapply(img,nband)))
      ln <- unlist(lapply(unname(img),bandname))
      units <- names(img)
      if (is.null(units))
         units <- if (FALSE) rep("",ng) else unname(ln)
      canRGB <- FALSE
   }
   else
   {
      ng <- 1L
      nb <- nband(img)
      ln <- bandname(img)
      units <- if (nb==1) ln[1] else ""
      canRGB <- .is.rgb(img) ## can improve RGB detection?
   }
   isRGB <- nb/3==np || nb/4==np || nb/2==np
   if ((!.is.integer(nb/np))&&(!isRGB)&&(canRGB))
      isRGB <- TRUE
   if (forceRGB)
      isRGB <- TRUE
   if (isRGB)
      nl <- nb/np ## ??? not used after
  # print(img)
  # print(c(nb=nb,np=np,ng=ng,isRGB=as.integer(isRGB)))
   annotation <- nb>1 & !isRGB #& !isList
   if (is.na(verbose))
      verbose <- nb>2
   txt <- NULL
   if (is.character(annotation))
      txt <- if (length(annotation)==nb) annotation else rep(annotation,nb)
   else if ((is.logical(annotation))&&(annotation))
      txt <- ln
   else
      txt <- ""
   annotation <- is.character(txt)
   k <- 0L
   myname <- names(arglist)
   if (is.null(myname))
      myname <- ""
   arglist <- arglist[nchar(myname)>0]
   blankAnnotation <- .lgrep("(caption|ann(otation)*)\\.label",myname)==0
   ct <- vector("list",ng)
  # names(ct) <- if (isList) ln else ""
   if (length(ct)==length(units))
      names(ct) <- units
   else
      names(ct) <- rep("",length(ct))
   ll <- do.call("compose_graticule",arglist)
   coast <- do.call("compose_coastline",arglist)
   for (j in seq(ng))
   {
      if (isList) {
         obj <- img[[j]]
         p <- do.call("colorize",c(list(obj),arglist))
         ct[[j]] <- p$colortable
         nl <- nband(obj)
      }
      else if (!isRGB) {
        # obj <- img
         if ((is.ursa(img,"colortable"))&&(!anyNA(ursa(img,"colortable"))))
            p <- img
         else
            p <- do.call("colorize",c(list(img),arglist))
         ct[[j]] <- p$colortable
         nl <- nband(p)
      }
      if (isRGB)
         nl <- 1
      for (i in seq(nl))
      {
         if ((verbose)&&(k==0)) {
            pb <- ursaProgressBar(min=0,max=ifelse(isRGB,nl,nb),silent=silent,tail=TRUE)
           # setUrsaProgressBar(pb,k)
         }
         k <- k+1L
        # if (i %in% skip)
        #    next
         panel_new(...)
         if (isRGB) {
            if (isList)
               panel_raster(img[[j]],...)
            else
               panel_raster(img,...)
           # panel_raster(colorize(obj,...),...)
         }
         else {
           # panel_raster(colorize(obj[i],...),...)
            if (isList) {
              # p <- do.call("colorize",c(list(obj[i]),arglist))
              # ct[[j]] <- p$colortable
              # do.call("panel_raster",c(list(p),arglist))
               do.call("panel_raster",c(list(p[i]),arglist))
            }
            else {
               do.call("panel_raster",c(list(p[i]),arglist))
            }
         }
         if (FALSE) {
            do.call("panel_coastline",arglist)
            do.call("panel_graticule",arglist)
         }
         else {
            panel_coastline(coast)
            panel_graticule(ll)
         }
         if (blankAnnotation) {
            arglist[["caption.label"]] <- if (length(txt)==nl) txt[i] else txt
         }
         do.call("panel_annotation",arglist) ## only through 'do.call'
         do.call("panel_scalebar",arglist) ## panel_scalebar(...)
         if (verbose)
            setUrsaProgressBar(pb,k)
      }
   }
   if (verbose)
      close(pb)
   if (isRGB)
      return(invisible(NULL))
   if (FALSE) {
     # compose_legend(...)
     # compose_legend(img,...)
      do.call("compose_legend",c(list(ct),arglist))
     # do.call("compose_legend",c(img,arglist)) ## FAIL
      return(NULL)
   }
   invisible(ct)
}
