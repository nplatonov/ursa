'.crop' <- function(fileout,border=5,verbose=FALSE) {
   isJPEG <- .lgrep("(jpg|jpeg)",gsub(".*\\.(.+$)","\\1",fileout))>0
   frame <- as.integer(round(border))
   requireNamespace("png",quietly=.isPackageInUse())
   if (isJPEG)
      requireNamespace("jpeg",quietly=.isPackageInUse())
   x <- png::readPNG(fileout,native=FALSE,info=TRUE)
   dimx <- dim(x)
   b <- .Cursa("internalMargin",x=as.numeric(x),dim=as.integer(dimx)
          ,indr=integer(dimx[1]),indc=integer(dimx[2]),NAOK=TRUE)
   indentc <- rep(which(b$indc==1)[1],frame)
   d <- diff(b$indc)
   s1 <- which(d==-1)+1L
   s2 <- which(d==1)
   if (b$indc[1]==0)
      s1 <- c(1L,s1)
   if (b$indc[length(b$indc)]==0)
      s2 <- c(s2,length(b$indc))
   indc <- c(indentc,s1[1]:s2[length(s2)],indentc)
   indentr <- rep(which(b$indr==1)[1],frame)
   d <- diff(b$indr)
   s1 <- which(d==-1)+1L
   s2 <- which(d==1)
   if (b$indr[1]==0)
      s1 <- c(1L,s1)
   if (b$indr[length(b$indr)]==0)
      s2 <- c(s2,length(b$indr))
   indr <- c(indentr,s1[1]:s2[length(s2)],indentc)
   att <- attr(x,"info")
   if (isJPEG)
      jpeg::writeJPEG(x[indr,indc,],fileout)
   else
      png::writePNG(x[indr,indc,],fileout,dpi=att$dpi,text=c(source=R.version.string))
   0L
}
'.crop2' <- function(fileout,border=5,verbose=FALSE) {
   if (verbose)
      .elapsedTime("crop2:start")
   frame <- as.integer(round(border))
   isJPEG <- .lgrep("(jpg|jpeg)",gsub(".*\\.(.+$)","\\1",fileout))>0
   requireNamespace("png",quietly=.isPackageInUse())
   if (isJPEG)
      requireNamespace("jpeg",quietly=.isPackageInUse())
   x <- png::readPNG(fileout,native=FALSE,info=TRUE)
   dimx <- dim(x)
   b <- .Cursa("internalMargin",x=as.numeric(x),dim=as.integer(dimx)
          ,indr=integer(dimx[1]),indc=integer(dimx[2]),NAOK=TRUE)
   indentc <- rep(which(b$indc==1)[1],frame)
   d <- diff(b$indc)
   s1 <- which(d==-1)+1L
   s2 <- which(d==1)
   if (b$indc[1]==0)
      s1 <- c(1L,s1)
   if (b$indc[length(b$indc)]==0)
      s2 <- c(s2,length(b$indc))
   d <- s1[2:length(s1)]-s2[1:(length(s2)-1)]
   ind <- which(d>frame)
   s1 <- s1[c(1,ind+1)]
   s2 <- s2[c(ind,length(s2))]
   indc <- indentc
   for (i in seq_along(s1))
      indc <- c(indc,s1[i]:s2[i],indentc)
   indentr <- rep(which(b$indr==1)[1],frame)
   d <- diff(b$indr)
   s1 <- which(d==-1)+1L
   s2 <- which(d==1)
   if (b$indr[1]==0)
      s1 <- c(1L,s1)
   if (b$indr[length(b$indr)]==0)
      s2 <- c(s2,length(b$indr))
   d <- s1[2:length(s1)]-s2[1:(length(s2)-1)]
   ind <- which(d>frame)
   s1 <- s1[c(1,ind+1)]
   s2 <- s2[c(ind,length(s2))]
   indr <- indentr
   for (i in seq_along(s1))
      indr <- c(indr,s1[i]:s2[i],indentr)
   att <- attr(x,"info")
   if (isJPEG)
      jpeg::writeJPEG(x[indr,indc,],fileout)
   else
      png::writePNG(x[indr,indc,],fileout,dpi=att$dpi,text=c(source=R.version.string))
   if (verbose)
      .elapsedTime("crop2:finish")
   0L
}
'.nocrop' <- function(fileout,border,verbose) {
   isJPEG <- .lgrep("(jpg|jpeg)",gsub(".*\\.(.+$)","\\1",fileout))>0
   if (!isJPEG)
      return(NULL)
   requireNamespace("png",quietly=.isPackageInUse())
   requireNamespace("jpeg",quietly=.isPackageInUse())
   jpeg::writeJPEG(png::readPNG(fileout,native=FALSE,info=TRUE),fileout)
   NULL
}
