'ursa_dummy' <- function(nband=3L,minvalue=0,maxvalue=255,mul=1,elements=8L
                         ,bandname=NULL,nodata=TRUE,resetGrid=FALSE) {
  # 
   if (resetGrid)
      session_grid(NULL)
   if (mul!=1)
      session_grid(regrid(session_grid(),mul=mul))
   g1 <- session_grid()
   ratio <- max(with(g1,c(columns,rows)/(elements-0.5)))
   ratio[ratio<1] <- 1
   g2 <- session_grid(regrid(g1,mul=1/ratio))
   if (is.character(bandname))
      nband <- length(bandname)
   d <- with(g2,c(columns,rows,nband))
   b <- array(runif(prod(d),min=minvalue,max=maxvalue),dim=d)
   b <- as.ursa(b,flip=FALSE,permute=FALSE)
   if (is.character(bandname))
      bandname(b) <- bandname
   else {
      lT <- Sys.getlocale("LC_TIME")
      Sys.setlocale("LC_TIME","C") # "English_United States.1252"
      bandname(b) <- format(Sys.time()+(seq(nband)-1)*24*60*60,"%A %d")
      Sys.setlocale("LC_TIME",lT)
   }
   if (ratio>1) {
      repeat({
         b <- regrid(b,mul=2,resample=1)
         g3 <- ursa_grid(b)
         if ((g3$columns>g1$columns)||(g3$rows>g1$rows))
            break
      })
      b <- regrid(b,g1,resample=1)
   }
   .gc()
   if (is.numeric(nodata))
      ursa_nodata(b) <- nodata
   else if ((is.na(nodata))||((is.logical(nodata))&&(!nodata)))
      ursa_nodata(b) <- NA
   b
}
