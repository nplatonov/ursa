invisible({
  # plutil::ursula(0)
   require(ursa)
   session_grid(NULL)
   ## 1. Prepare data
   session_grid(NULL)
   fname <- ursa:::.maketmp(2)
   a <- create_envi(fname[1],nband=3,ignorevalue=-99)
   for (i in seq(nband(a)))
     a[i] <- pixelsize()^(1/i)
   close(a)
   rm(a)
   
   ## 2. Read 
   a <- open_envi(fname[1])
   chB <- chunk_band(a,2)
   print(str(chB))
   for (i in chB)
      print(a[i])
   chL <- chunk_line(a,2.5)
   print(str(chL))
   for (j in chL)
      print(a[,j])
   
   ## 3. Filtering with partial reading
   b <- create_envi(a,fname[2])
   fsize <- 15
   for (j in chL) {
      k <- chunk_expand(j,fsize)
      b[,j] <- focal_mean(a[,k$src],size=fsize)[,k$dst]
   }
   d1 <- b[]
   
   ##  4. Filtering in memory
   d2 <- focal_mean(a[],size=fsize)
   close(a,b)
   envi_remove(fname)
   print(d1-d2)
})
