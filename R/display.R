'.plot.ursaRaster' <- function(obj,...) display(obj,...)
#'glance' <- function(obj,...) UseMethod("glance") ## 'broom::glance'
#'glance.ursaRaster' <- function(obj,...) display(obj,...)
'display' <- function(obj,...)
{
   arglist <- list(...)
   isBlank <- missing(obj)
   if (isBlank)
      obj <- ursa_new()
   verbose <- .getPrm(arglist,name="verb(ose)*",default=FALSE)
   toStack <- .getPrm(arglist,name="stack",default=FALSE)
   toBrick <- .getPrm(arglist,name="brick",default=FALSE)
   isStack <- .is.ursa_stack(obj)
   if (verbose)
      print(c(toStack=toStack,toBrick=toBrick,isStack=isStack))
   if (TRUE) { ## keep to read *.bz2 etc
      if ((is.character(obj))&&(!is.matrix(obj))) {
         if (isURL <- .lgrep("^(http://|https://|ftp://|file:///)",obj)>0) {
            fname <- tempfile()
           # download.file(obj,fname,method="curl")
            ind <- .grep("(method|mode|cache|extra)",names(arglist))
            args2 <- c(url=obj,destfile=fname,arglist[ind])
           # do.call("download.file",args2)
            fname <- do.call(".ursaCacheDownload",c(url=obj,arglist[ind]))
            obj <- read_gdal(fname,...)
           # try(file.remove(fname))
         }
         else {
            obj <- if (envi_exists(obj)) read_envi(obj,...) else read_gdal(obj,...)
         }
      }
   }
   if ((!isStack)&&(!.try(obj <- as.ursa(obj)))) {
      if (is.ursa(obj,"grid")) {
         g0 <- session_grid()
         session_grid(obj)
         ret <- display()
         session_grid(g0)
         return(ret)
      }
      return(NULL)
   }
   if (.is.rgb(obj)) {
      if (verbose)
         print("display_rgb")
      return(display_rgb(obj,...))
   }
   if ((toStack)||(.is.ursa_stack(obj))) {
      if (verbose)
         print("display stack")
      return(display_stack(obj,...))
   }
   ind <- pmatch("verb",names(arglist))
   if ((!is.na(ind))&&(is.logical(arglist[[ind]]))&&(arglist[[ind]]))
      verb <- TRUE
   else
      verb <- !.isPackageInUse()
   if ((toBrick)||(.is.brick(obj,verbose=verb))) {
      if (!TRUE) {
         arglist <- list(...)
         if ((!.lgrep("decor",names(arglist)))&&(nband(obj)==1))
            arglist$decor <- TRUE
         return(invisible(do.call("display_brick",c(list(obj),arglist))))
      }
      else if (isBlank) {
         if (verbose)
            print("display blank")
         compose_open(legend=NULL,...)
         panel_new(...)
         panel_decor(...)
         if (TRUE) { #(.isPackageInUse())
            fann <- .dir(path=system.file("optional/sponsorship",package="ursa")
                         ,pattern="\\.png$",full.names=TRUE)
            ann <- png::readPNG(sample(fann,1))
            panel_annotation(ann,alpha=0.5,pos="bottomright",cex=0.5)
         }
         if (.isKnitr())
            return(compose_close(...))
         compose_close(...)
         return(invisible(NULL))
      }
      else {
         if (verbose)
            print("display_brick")
         if (.isKnitr())
            return(display_brick(obj,...))
         return(invisible(display_brick(obj,...)))
      }
   }
   else {
      if (verbose)
         print("display stack")
      if (.isKnitr())
         return(display_stack(obj,...))
      return(invisible(display_stack(obj,...)))
   }
   stop("unreachable code")
}
'.is.brick' <- function(obj,tol=0.8,verbose=!.isPackageInUse()) {
  # verbose <- TRUE
   if (nband(obj)==1)
      return(TRUE)
   e <- band_stat(obj)
   if (!TRUE) {
      e.var <- e$sd^2/e$n
      n <- nrow(e)
      p <- rep(NA,n)
      for (i in seq(n)) {
         j <- if (i==1) n else i-1L
         p[i] <- (e$mean[i]-e$mean[j])/sqrt(e.var[i]+e.var[j])
      }
      if (verbose)
         print(p)
      p <- na.omit(abs(p))
      print(all(p<500)) ## return
   }
   e.avg <- na.omit(e$mean)
   e.sd <- na.omit(e$sd)
   e.min <- na.omit(e$min)
   e.max <- na.omit(e$max)
   if (!length(e.avg))
      return(TRUE)
   ##~ sd.avg <- sd(e.avg)
   ##~ sd.sd <- sd(e.sd)
   ##~ sd.min <- sd(e.min)
   ##~ sd.max <- sd(e.max)
   mean.avg <- mean(e.avg)
   mean.sd <- mean(e.sd)
   mean.min <- mean(e.min)
   mean.max <- mean(e.max)
   range.avg <- diff(range(e.avg))
   range.sd <- diff(range(e.sd))
   range.min <- diff(range(e.min))
   range.max <- diff(range(e.max))
   p.avg <- abs(range.avg/mean.avg)
   p.sd <- abs(range.sd/mean.sd)
   p.min <- abs(range.min/mean.min)
   p.max <- abs(range.max/mean.max)
   ##~ p.avg <- sd.avg/mean.avg
   ##~ p.sd <- sd.sd/mean.sd
   ##~ p.min <- sd.min/mean.min
   ##~ p.max <- sd.max/mean.max
   ##~ res.mean <- c(avg=mean.avg,sd=mean.sd,min=mean.min,max=mean.max)
   ##~ res.sd <- c(avg=sd.avg,sd=sd.sd,min=sd.min,max=sd.max)
   res <- c(avg=p.avg,sd=p.sd,min=p.min,max=p.max,tol=tol)
   res[is.na(res)] <- 0
   if (verbose) {
     # print(res.mean)
     # print(res.sd)
      print(res)
   }
   ret <- unname((res["min"]<tol & res["max"]<tol) | res["sd"]==0)
   ret
}
'.cmd.display' <- function() {
   do.call("display",.args2list())
}
