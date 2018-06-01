invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   'printCR' <- function(obj) print(with(ursa_grid(obj),c(c=columns,r=rows)))
   g0 <- session_grid()
   a <- pixelsize()
   th <- with(ursa_grid(a),resx*resy*1e-6)
   a0 <- a[a>th*0.9]
   print(session_grid())
   printCR(a0)
   print(a0)
   a1 <- ursa_crop(a0,resetGrid=TRUE)
   print(session_grid())
   printCR(a1)
   print(a1)
   a2 <- ursa_crop(a0,resetGrid=FALSE)
   print(session_grid())
   printCR(a2)
   print(a2)
   a3 <- a[a>=th*0.85 & a<=th*1.01]
   b1 <- ursa_dummy(nband=3,min=0,max=255)
   print(b1)
   b2 <- ursa_crop(b1[a3>0],border=10)
   print(b2)
   printCR(b2)
  # \dontrun{
   b2[is.na(b2)] <- 255
   display_rgb(b2)#}
   b3 <- ursa_crop(b1,a3,border=0)
   print(b3)
   printCR(b3)
})
