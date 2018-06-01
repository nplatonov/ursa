invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   a <- as.integer(round(ursa_dummy(nband=3)))
   
   print(a) ## print data
   ursa_info(a,digits=1) ## print metadata
   
   fname <- tempfile()
   write_envi(a,fname,compress=FALSE)
   b1 <- open_envi(fname)
   ursa_info(b1)
   close(b1)
   b2 <- colorize(read_envi(fname),ncolor=7)
   
   ursa_info(b2)
   
  # print ENVI header
   sapply(c(" -------------- begin --------------",readLines(paste0(fname,".hdr"))
           ," --------------- end ---------------"),message)
   
   envi_remove(fname)
})
