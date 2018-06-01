'c.ursaRaster' <- function(...)
{
   args <- list(...)
  # str(args)
  # print(names(args))
   if (length(args)<2)
   {
      myname <- names(args)[1]
      res <- args[[1]]
      if ((is.character(myname))&&(nchar(myname)))
      {
         bandname(res) <- rep(myname,length(bandname(res)))
      }
      return(res)
   }
   x <- args[[1]]
   xname <- names(args[1])
   z <- x$con$posZ
   ##~ if (((length(x$name)==1)||(!is.na(z[1]))&&(length(z)==1))&&(!is.null(xname)))
   if (is.null(xname))
      xname <- ""
  # if ((length(x$name)==1)&&(nchar(xname))) ## before 2015-03-05 'if (!is.null(xname))'
  #    x$name <- xname
   if (nchar(xname))
   {
      if (is.na(z[1])) {
         x$name <- rep(xname,nband(x)) ## x$name <- xname
      }
      else if (length(z)==1L)
         x$name[z] <- xname
     # x$name <- xname
   }
   if (!is.na(z[1]))
   {
      x$dim[2] <- length(z)
      x$name <- x$name[z]
      nz <- length(z)
      x$con$posZ <- rep(NA,nz)
      x$con$indexZ <- seq(nz)
      x$con$bands <- nz
   }
   clValue <- class(x$value)
   isDiskX <- is.null(dim(x$value))
   bandInd <- vector("list",length(args))
   bandInd[[1]] <- if (is.na(x$con$bands[1])) 1L else seq(x$con$bands)
   if (!is.na(x$con$posZ[1]))
      bandInd[[1]] <- x$con$posZ
   for (i in seq(along=args))
   {
      if (i==1L)
         next
      y <- args[[i]]
      if (!is.ursa(y)){
         if (is.na(y))
            y <- ursa_new()
         else if (is.numeric(y))
            y <- ursa_new(value=y)
      }
      isDiskY <- is.null(dim(y$value))
      if (isDiskY)
         next
      n1 <- seq(x$dim[2])
      x$dim[2] <- x$dim[2]+y$dim[2]
      n2 <- seq(x$dim[2])
      indy <- which(is.na(match(n2,n1)))
      bandInd[[i]] <- indy
      z <- y$con$posZ
      yname <- names(args[i])
      if (is.na(z[1]))
      {
         if (is.null(yname))
            yname <- ""
         if (nchar(yname)) {
            if (length(y$name)==1) ## before 2015-03-05 'if (!is.null(yname))'
               y$name <- yname
            else
               y$name <- rep(yname,nband(y))
         }
         x$name <- c(x$name,y$name)
      }
      else
      {
        # op <- options(warn=0)
        # warning("probably incorrect naminig")
        # options(op)
         if (length(y$name[z])==1)
         {
            if (is.null(yname))
               yname <- ""
            if (nchar(yname)) ## before 2015-03-05 'if (!is.null(yname))'
            {
               y$name <- yname
               z <- 1L
            }
            else
            {
              # y$name <- y$name[z]
            }
         }
         x$name <- c(x$name,y$name[z]) ## before 2015-03-05 'c(x$name,y$name)'
      }
      if (!is.na(x$con$bands))
      {
         if (!is.na(y$con$bands))
            x$con$bands <- x$con$bands+y$con$bands
         else
            x$con$bands <- x$con$bands+y$dim[2]
      }
      if (!is.na(x$con$indexZ[1L]))
      {
         x$con$indexZ <- c(x$con$indexZ,indy)
      }
      if ((!isDiskX)&&(!isDiskY)) {
         x$value <- cbind(x$value,y$value)
         class(x$value) <- clValue ## 'clValue' '"ursaNumeric"'
      }
   }
   if ((!isDiskX)&&(!isDiskY))
   {
      x$dim[2] <- x$con$bands <- ncol(x$value)
      x$con$indexZ <- seq(x$con$bands)
   }
   if (!isDiskX)
      return(x);
   stop("TODO (low-level image manipulation)")
   .write.hdr(x)
   for (i in seq(along=args))
   {
      if (i==1L)
         next
      y <- args[[i]]
   }
   x
}
