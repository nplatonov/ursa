invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   
   fname <- "c:/tmp/res1" # tempfile()
   a <- ursa_dummy()
   bandname(a) <- c("first","second","third")
   write_envi(a,fname,compress=!FALSE)
   
   print(read_envi(fname))
   print(read_envi(fname,c(1,3)))
   print(read_envi(fname,-c(1,3)))
   print(read_envi(fname,c("first","third")))
   print(read_envi(fname,"iR"))
   
   print(session_grid())
   g <- regrid(session_grid(),mul=1/2.3)
   b <- read_envi(fname,ref=g)
   print(session_grid())
   print(b)
   
   envi_remove(fname)
})
