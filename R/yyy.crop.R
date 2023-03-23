'.crop' <- function(fileout,border=5,verbose=FALSE) {
   isJPEG <- .lgrep("(jpg|jpeg)",gsub(".*\\.(.+$)","\\1",fileout))>0
   isWEBP <- .lgrep("(webp)",gsub(".*\\.(.+$)","\\1",fileout))>0
   isSVG <- .lgrep("(svg)",gsub(".*\\.(.+$)","\\1",fileout))>0
   isPNG <- .lgrep("(png)",gsub(".*\\.(.+$)","\\1",fileout))>0
   frame <- as.integer(round(border))
   if (!isSVG)
      requireNamespace("png",quietly=.isPackageInUse())
   if (isPNG)
      NULL
   else if (isJPEG)
      isJPEG <- requireNamespace("jpeg",quietly=.isPackageInUse())
   else if (isWEBP)
      isWEBP <- requireNamespace("webp",quietly=.isPackageInUse())
   else if (isSVG) {
      if (!(isSVG <- requireNamespace("magick",quietly=.isPackageInUse())))
         return(0L)
     # print("A")
     # imread <- purrr::safely(magick::image_read)
     # a <- imread(fileout)
     # print("B")
     # str(a)
     # q()
     # tempf <- tempfile()
     # sink(tempf)
      a <- magick::image_raster(magick::image_read(fileout))
     # sink()
     # file.remove(tempf)
      x <- col2rgb(a$col)/255
      dim(x) <- c(dim(x)[1],max(a$x),max(a$y))
      x <- aperm(x,c(3,2,1))
      rm(a)
   }
   if (!isSVG)
      x <- png::readPNG(fileout,native=FALSE,info=TRUE)
   dimx <- dim(x)
   isBlack <- isTRUE(getOption("ursaPngBackground") %in% (c("black","#000000")))
   if (isBlack)
      b <- .Cursa(C_internalMargin,x=1-as.numeric(x),dim=as.integer(dimx)
             ,indr=integer(dimx[1]),indc=integer(dimx[2]),NAOK=TRUE)
   else
      b <- .Cursa(C_internalMargin,x=as.numeric(x),dim=as.integer(dimx)
             ,indr=integer(dimx[1]),indc=integer(dimx[2]),NAOK=TRUE)
   indentc <- rep(which(b$indc==1)[1],frame)
   d <- diff(b$indc)
   s1 <- which(d==-1)+1L
   s2 <- which(d==1)
   if (b$indc[1]==0)
      s1 <- c(1L,s1)
   if (b$indc[length(b$indc)]==0)
      s2 <- c(s2,length(b$indc))
   seqc <- s1[1]:s2[length(s2)]
   indc <- c(indentc,seqc,indentc)
   indentr <- rep(which(b$indr==1)[1],frame)
   d <- diff(b$indr)
   s1 <- which(d==-1)+1L
   s2 <- which(d==1)
   if (b$indr[1]==0)
      s1 <- c(1L,s1)
   if (b$indr[length(b$indr)]==0)
      s2 <- c(s2,length(b$indr))
   seqr <- s1[1]:s2[length(s2)]
   indr <- c(indentr,seqr,indentc)
   att <- attr(x,"info")
   if (is.null(att$dpi)) { ## e.g. after Cairo::CairoPNG
      dpi <- getOption("ursaPngDpi")
      if (is.numeric(dpi))
         att$dpi <- dpi
   }
   if (isSVG) {
      sc <- 0.75
      ##~ print(range(seqc))
      ##~ print(range(seqr))
      lenc <- round(length(seqc)*sc,2)
      lenr <- round(length(seqr)*sc,2)
      seqc <- round(range(seqc)*sc,2)
      seqr <- round(range(seqr)*sc,2)
      ##~ print(seqc)
      ##~ print(seqr)
      ##~ print(lenc)
      ##~ print(lenr)
      content <- readLines(fileout)
      patt <- paste0("^(.+)(width=\\S+\\sheight=\\S+\\s)(viewBox=(\\\"|'))"
                    ,"\\S+\\s\\S+\\s\\S+\\s\\S+((\\\"|').+)$")
      if (length(ind <- grep(patt,head(content,4)))==1) {
         ##~ print(content[ind])
         content[ind] <- gsub(patt,paste0("\\1\\3"
                                         ,seqc[1]-frame," "
                                         ,seqr[1]-frame," "
                                         ,lenc[1]+frame+frame," "
                                         ,lenr[1]+frame+frame
                                         ,"\\5"),content[ind])
         ##~ print(content[ind])
        # file.copy(fileout,paste0(fileout,"~"))
         writeLines(content,paste0(fileout,""))
      }
   }
   else if (isJPEG)
      jpeg::writeJPEG(x[indr,indc,],fileout)
   else if (isWEBP)
      webp::write_webp(x[indr,indc,],fileout)
   else
      png::writePNG(x[indr,indc,],fileout,dpi=att$dpi,text=c(source=R.version.string))
   0L
}
'.crop2' <- function(fileout,border=5,verbose=FALSE) {
   if (verbose)
      .elapsedTime("crop2:start")
   frame <- as.integer(round(border))
   isJPEG <- .lgrep("(jpg|jpeg)",gsub(".*\\.(.+$)","\\1",fileout))>0
   isWEBP <- .lgrep("(webp)",gsub(".*\\.(.+$)","\\1",fileout))>0
   requireNamespace("png",quietly=.isPackageInUse())
   if (isJPEG)
      isJPEG <- requireNamespace("jpeg",quietly=.isPackageInUse())
   if (isWEBP)
      isWEBP <- requireNamespace("webp",quietly=.isPackageInUse())
   x <- png::readPNG(fileout,native=FALSE,info=TRUE)
   dimx <- dim(x)
   b <- .Cursa(C_internalMargin,x=as.numeric(x),dim=as.integer(dimx)
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
   else if (isWEBP)
      webp::write_webp(x[indr,indc,],fileout)
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
   isJPEG <- requireNamespace("jpeg",quietly=.isPackageInUse())
   if (!isJPEG)
      return(NULL)
   requireNamespace("png",quietly=.isPackageInUse())
   jpeg::writeJPEG(png::readPNG(fileout,native=FALSE,info=TRUE),fileout)
   NULL
}
