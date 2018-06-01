invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   if (TRUE) { ## manual classification
      a <- as.ursa(round(matrix(runif(100,min=0.5,max=3.5),ncol=10)))
      print(as.table(a))
      b <- reclass(a,src=c(3,1,2),dst=round(runif(3),2))
     # str(b)
     # q()
      print(as.table(b))
      print(c(src=a,dst=b))
   }
   if (!TRUE) { ## similarity
      a <- ursa_dummy(nband=2,min=-1,max=1)
      b1 <- colorize(a[1],value=seq(-1,1,length=21),pal.rich=240,pal.rotate=0)
      b2 <- reclass(a[2],b1)
      b3 <- reclass(a[2],ursa_colortable(b2))
      b <- c(b1,b2,b3)
      print(reclass(b))
   }
   if (!TRUE) { ## data compress with lost
      a <- pixelsize(NULL)
      b <- reclass(a,byte=TRUE,tail=0) ## try 'byte=FALSE'
     # str(b)
     # q()
      a2 <- reclass(b)
      res <- c(source=a,as_category=a2,difference=a-a2)
      print(res)
      message(paste("RMS error: ",format(sqrt(band_sum(res[3]^2)/band_n(res[3])))))
      prefix <- names(res)[1:2]
      fname <- file.path(tempdir(),paste0(prefix,".envi"))
      s <- data.frame(object.size=sapply(list(a,b),object.size))
      rownames(s) <- prefix
      print(s)
      write_envi(a,fname[1])
      write_envi(b,fname[2])
      f <- file.info(dir(path=tempdir()
                        ,pattern=paste0("(",prefix,")\\.(envi|hdr)",sep="|")
                        ,full.names=TRUE))[,"size",drop=FALSE]
      colnames(f) <- "file.size"
      print(f)
      envi_remove(fname)
   }
   if (!TRUE) { ## dont include
      a <- ursa_dummy(nband=2,min=-1,max=1)
      print(a)
      b1 <- colorize(a[1],value=seq(-1,1,length=201),pal.rich=240,pal.rotate=0)
      b1 <- colorize(b1,ramp=FALSE)
      ursa_plot(b1)
     # q()
      d1 <- reclass(a[1],value=seq(-1,1,length=201))
      write_envi(d1,"tmp1.envi")
      e1 <- read_envi("tmp1")
      display(e1,ramp=FALSE)
   }
})
