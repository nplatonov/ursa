'as.raster.ursaRaster' <- function(x,...) .as.raster2(obj=x,...)
'.as.raster1' <- function(obj,max=255) { # slower
   verbose <- TRUE
   if (verbose)
      .elapsedTime("as.raster -- start")
   nb <- nband(obj)
   if (nb>4)
      return(NULL)
   a <- round(obj*255/max)
   dimv <- obj$dim
   if (nb %in% c(1,3)) {
      v <- sum(a*c(65536,256,1),cover=1)
      b <- sprintf("#%06X",as.hexmode(ursa_value(v)))
   }
   else {
      if (nb==4) {
         v1 <- a[1]*256+a[2]
         v2 <- a[3]*256+a[4]
         b <- sprintf("#%04X%04X",as.hexmode(ursa_value(v1))
                                 ,as.hexmode(ursa_value(v2)))
      }
      else if (nb==2) {
         v <- sum(a[1]*c(65536,256,1),cover=1)
         b <- sprintf("#%06X%02X",as.hexmode(ursa_value(v))
                                 ,as.hexmode(ursa_value(a[2])))
      }
   }
   b[grep("NA",b)] <- NA
   dim(b) <- unname(dim(obj)[1:2])
   class(b) <- "raster"
   if (verbose)
      .elapsedTime("as.raster -- finish")
   return(b)
}

'.as.raster2' <- function(obj,...) { # faster
   maxv <- .getPrm(list(...),name=".*",default=255)
   verbose <- FALSE
   nb <- nband(obj)
   if (verbose)
      .elapsedTime("as.raster -- start")
   s <- ursa_value(!is.na(sum(obj,cover=1)))
   if (nb %in% c(3,4)) {
      a <- ursa_value(obj)
   }
   else if (nb==1)
      a <- ursa_value(obj[rep(1,3)])
   else if (nb==2)
      a <- ursa_value(obj[c(1,1,1,2)])
   else {
      warning("not recognized as RGB(A) or Grayscale")
      return(NULL)
   }
   dim(a) <- c(1,dim(a))
   ind <- which(!is.na(c(s)))
   res <- rep(NA_character_,prod(dim(s)))
   res[ind] <- c(grDevices::as.raster(a[,ind,,drop=FALSE],max=maxv))
   dim(res) <- dim(obj)[c(1,2)]
   class(res) <- class(as.raster(1))
   if (verbose)
      .elapsedTime("as.raster -- finish")
   res
}
