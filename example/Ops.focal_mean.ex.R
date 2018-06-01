invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
  # g1 <- session_grid()
  # a <- regrid(ursa_dummy(nband=1,mul=1/8),g1,resample=0)
   a <- ursa_dummy(nband=1,mul=1/8,elements=0)
   a[a<80] <- NA
   print(a)
   b1 <- focal_mean(a,size=6,cover=0.5,saveMargin=FALSE)
   b2 <- focal_mean(a,size=6,cover=0.5,saveMargin=TRUE)
   b3 <- focal_mean(a,size=6,cover=0.5,saveMargin=TRUE,fillNA=TRUE)
   print(b3-a)
   display(c(a,b1,b2,b3),blank.angle=c(-45,45),blank.density=20)
})
