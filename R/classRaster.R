'.raster.skeleton' <- function()
{
   obj <- list(grid=NA,con=NA,value=NA,dim=NA,name=NA,colortable=character(0))
   class(obj$value) <- "ursaNumeric"
   class(obj$colortable) <- "ursaColorTable"
   class(obj) <- "ursaRaster"
   obj
}
'is_ursa' <- function(obj,ref=NULL) is.ursa(obj=obj,ref=ref)
'is.ursa' <- function(obj,ref=NULL) {
   if (is.null(ref))
      return(inherits(obj,"ursaRaster"))
   if (.lgrep("(raster|brick|ursa)",ref)>0)
      return(inherits(obj,"ursaRaster"))
   if (.lgrep("grid",ref)>0)
      return(.is.grid(obj))
   if (.lgrep("(ct|color|table)",ref)>0)
      return(.is.colortable(obj))
   if (.lgrep("stack",ref)>0)
      return(.is.ursa_stack(obj))
   if (.lgrep("con",ref)>0)
      return(.is.con(obj))
   if (.lgrep("val",ref)>0)
      return(inherits(obj,"ursaNumeric") || inherits(obj,"ursaCategory"))
   if (.lgrep("cat",ref)>0)
      return(.is.category(obj))
   FALSE
}
'.str.ursaRaster' <- function(x,grid=NA,con=NA,...) {
   NULL
}
'str.ursaRaster' <- function(object,...) {
   arglist <- list(...)
   Rgrid <- .getPrm(arglist,name="grid",default=NA)
   con <- .getPrm(arglist,name="con",default=NA)
   os <- object.size(object)
   .con <- con
   if (is.na(con))
      con <- FALSE
   if (is.na(Rgrid)) {
      if (is.na(.con))
         Rgrid <- TRUE
      else
         Rgrid <- FALSE
   }
   if (!con) {
      if (!is.na(nodata <- ursa_nodata(object)))
         object$nodata <- nodata
      if (identical(dim(object$value),object$dim))
         object$dim <- NULL
      if (!length(object$colortable)) {
         object$colortable <- NULL
        # attr(object$value,"category") <- NULL
      }
      if (!is.na(object$con$posZ[1]))
         object$name <- object$name[object$con$posZ]
      object$con <- NULL
   }
   if (!Rgrid)
      object$grid <- NULL
   object$object.size <- format(os,units="MB")
   class(object) <- paste0(".",class(object))
   NextMethod("str",object,...)
}
'print.ursaRaster' <- function(x,digits=NA,grid=FALSE,raw=FALSE,caption=FALSE,...)
{
   ellipsis <- c(">","\u2026")[2]
   e <- band_stat(x,grid=grid,raw=raw)
   if (grid)
      return(format(e,digits=digits,...))
   if ((is.null(e))||(!nrow(e)))
      return(e)
   if (isTRUE(caption)) {
      caption <- as.character(as.expression(as.list(match.call())[["x"]]))
   }
   else if (!(is.character(caption))||(!nchar(caption)))
      caption <- ""
   else
      caption <- ""
   if (nchar(caption))
      cat(paste0(caption,":\n"))
   if (is.na(digits)) {
      ln <- e$name
      lmax <- max(nchar(ln))
      cn <- colnames(e)
      len <- if (lmax>13) seq(lmax,13,by=-1) else lmax
      nmax <- getOption("width")-max(nchar(rownames(e)))-1
      for (i in seq_along(len)) {
         e$name <- substr(ln,1,len[i])
         mn <- ifelse(i==1,2,1)
         for (d in 6:mn) {
            f <- format(e,digits=d,scientific=FALSE)
            f2 <- rbind(cn,as.matrix(f))
            f5 <- apply(f2,2,function(x){max(nchar(x))})
            n <- length(f5)-1+sum(f5)
            if (n<nmax) {
               l <- substr(ln,1,len[i]+nmax-n-1)
               ind <- which(nchar(ln)!=nchar(l))
               if (length(ind)) {
                  b <- nchar(l[ind])
                  substr(l[ind],b,b) <- ellipsis
               }
               f$name <- format(l)
               return({
                  if ((FALSE)||(.isKnitr()))
                     print(knitr::kable(f,format="pandoc"))
                  else {
                     ret <- print(f,quote=FALSE)
                     if (candidate <- TRUE) {
                        ct <- ursa(x,"category")
                        if (!is.null(ct)) {
                           o <- paste(ct,collapse=", ")
                           if (nchar(o)>72)
                              o <- paste0(substr(o,1,71),"\u2026")
                           cat("   Classes: ",o,"\n")
                        }
                     }
                     ret
                  }
               })
            }
         }
         for (d in 3:mn) {
            f <- format(e,digits=d,scientific=TRUE)
            f2 <- rbind(cn,as.matrix(f))
            f5 <- apply(f2,2,function(x){max(nchar(x))})
            n <- length(f5)-1+sum(f5)
            if (n<nmax) {
               l <- substr(ln,1,len[i]+nmax-n-1)
               ind <- which(nchar(ln)!=nchar(l))
               if (length(ind)) {
                  b <- nchar(l[ind])
                  substr(l[ind],b,b) <- ellipsis
               }
               f$name <- format(l)
               return(print(f,quote=FALSE))
            }
         }
      }
      digits <- 3
   }
   if (.isKnitr())
      knitr::kable(format(e,digits=digits,...))
   else
      print(format(e,digits=digits,...),quote=FALSE)
}
