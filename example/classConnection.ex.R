invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
  # \dontrun{
   print(methods(class="ursaConnection"))#}

  # \dontrun{
   print(ursa:::.con.skeleton())#}

   a <- pixelsize()
   write_envi(rep(a,5),"tmp1",compress=FALSE)
   ## change spatial domain for cropping example
   g <- session_grid(regrid(lim=c(-1200000,-1400000,1600000,1800000)))
   print(g)
   b <- open_envi("tmp1")
   d <- b[,30:70]
   print(ursa(d[2:3],"con"))
   close(b)
   envi_remove("tmp1")
})
